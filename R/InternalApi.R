.jobStatusFromJson <- function(json) {
	validate = function(value) {
		is.character(value$projectName) && is.integer(value$revision) && is.integer(value$rowCount) && is.character(value$state) && 
			(is.null(value$result) || is.character(value$result)) && 
			(is.null(value$error) || is.character(value$error)) &&
			(is.null(value$result) || is.null(value$error))
	}
	
	value = list()
	
	tryCatch(
		{
			value = list(
				projectName = json$projectName,
				revision = json$revision,
				rowCount = json$rowCount,
				state = json$state,
				result = json$result,
				error = json$error
			)
			if(!validate(value))
				message("Invalid json received, can't deserialize into jobStatus")
		},
			error = function(cond) {
				message("Failed to deserialize ")
			}
	)

	attr(value, "class") <- "jobStatus"
	value
}

.predictResultFromJson = function(json){
	validate = function(value) {
		if(!is.null(value$result) && !is.null(value$error)) {
			stop("Predict result contains both result and error")
		}
	}
	
	value = list()
	
	tryCatch(
		{
			value = list(
				result = json$result,
				error = json$error,
				executionId = json$executionId,
				asyncMode = is.null(json$result) && !is.null(json$executionId)
			)
			validate(value)
		},
		error = function(cond) {
			message(paste("Failed to deserialize predict result:", cond))
		}
	)
	
	attr(value, "class") <- "predictResult"
	value
}

.enrichResultFromJson = function(json){
	validate = function(value) {
		if(!is.null(value$result) && !is.null(value$error)) {
			stop("Enrich result contains both result and error")
		}
	}
	
	value = list()
	
	tryCatch(
		{
			value = list(
				result = json$result,
				error = json$error,
				executionId = json$executionId,
				asyncMode = is.null(json$result) && !is.null(json$executionId)
			)
			validate(value)
		},
		error = function(cond) {
			message(paste("Failed to deserialize enrich result:", cond))
		}
	)
	
	attr(value, "class") <- "enrichResult"
	value
}


.getPredictJobStatus = function(jobId) {
	tryCatch(
		url <- paste0(getSBserverDomain(),"/rapi/predict/", jobId),
		error = function(cond) {
			message(paste("Failed to get predict job status. Error:", cond))
		}
	)
	res = httr::GET(url, httr::content_type_json())
	res <- jsonlite::fromJSON(txt=httr::content(res, as="text"), simplifyDataFrame=TRUE)
	jobStatus = .jobStatusFromJson(res)
}

.getEnrichJobStatus = function(jobId) {
	tryCatch(
		url <- paste0(getSBserverDomain(),"/rapi/enrich/", jobId),
		error = function(cond) {
			message(paste("Failed to get enrich job status. Error:", cond))
		}
	)
	res = httr::GET(url, httr::content_type_json())
	res <- jsonlite::fromJSON(txt=httr::content(res, as="text"), simplifyDataFrame=TRUE)
	jobStatus = .jobStatusFromJson(res)
}

.IsHttrRetryDefined = tryCatch({
		f = httr::RETRY1
		TRUE
	}, error = function(e) FALSE
)

.download = function(url, localFile = NA_character_, description = NA_character_) {
	tempFileName = "contexts-test_timeSeries_using_problem_definition-5.zip"
	tempFileCreated = FALSE
	downloadDescription = ifelse(!is.na(description), description, 
											 ifelse(!is.na(localFile), localFile, "..."))
	if(is.na(localFile)) {
		tempFileCreated = TRUE
		localFile = tempFileName
	}
	
	message(paste("Downloading", downloadDescription))
	attempts = 3
	downloadResult = NA_integer_
	
	if(.IsHttrRetryDefined) {
		downloadResult = httr::RETRY("GET", url = url, times = attemtps, config = httr::progress(type = "down"), httr::write_disk(localFile, overwrite = TRUE))
	} else {
		succeeded = FALSE
		while (!succeeded && attempts > 0 ) {
			downloadResult = httr::GET(url, config = httr::progress(type = "down"), httr::write_disk(localFile, overwrite = TRUE))
			attempts = attempts - 1
			succeeded = httr::status_code(downloadResult) == 200
			if(!succeeded && attempts>0) {
				message(paste("Download failed with status:", httr::status_code(downloadResult)))
				message("Retrying")
			}
		}
	}
	
	if(httr::status_code(downloadResult) == 200) {
		if(tempFileCreated) {
			contentDispositionFileName <- stringr::str_match(httr::headers(downloadResult)$`content-disposition`, "\"(.*)\"")[2]
			if(!is.na(contentDispositionFileName)) {
				file.rename(tempFileName, contentDispositionFileName)
				localFile = contentDispositionFileName
			} else {
				stop(paste0("Error in download from ", url, ": Local file name is not defined"))
			}
		}
		message(paste("File", localFile, "saved to:", normalizePath(localFile)))
	} else {
		stop(paste("Failed to download a file from:", url))
	}
	normalizePath(localFile)
}

.downloadFile = function(projectName, revision, pathOnServer, saveToPath) {
	url = paste0(getSBserverDomain(),"/api2/downloadFile/",projectName,"/",revision, "/",pathOnServer)
	downloadResult = httr::GET(url)
	if (downloadResult$status == 200) {
		writeBin(httr::content(downloadResult), saveToPath)
		normalizePath(saveToPath)
	} else {
		warning(paste("Failed to download a file from", pathOnServer, "for project:", projectName, ",revision:", revision, 
									". Request failed with status:", downloadResult$status))
		NULL
	}
}

.downloadDataFrame = function(projectName, revision, pathOnServer, saveToPath) {
	localFile = .downloadFile(projectName, revision, pathOnServer, saveToPath)
	if(!is.null(localFile)) {
		read.table(localFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
	} else {
		NULL
	}
}

.getSupportedAlgorithms = function() {
	url = paste0(getSBserverDomain(), "/api2/algorithms")
	res = httr::GET(url)
	jsonlite::fromJSON(txt=httr::content(res, as="text"), simplifyDataFrame=TRUE)['name']$name
}

.algorithmsCompatibility = (function() {
	backwardCompatibilityAlgorithmNamesMapping = list(
		"RXGBoost" = c("RXGBoostRegressor", "RXGBoostClassifier"),
		"RRpartDecisionTree" = c("RRpartDecisionTreeClassifier", "RRpartDecisionTreeRegressor"),
		"RLinearEnsembleGBM_with_Rpart" = c("RLinearEnsembleGBM_with_RpartClassifier", "RLinearEnsembleGBM_with_RpartRegressor"),
		"SciKitLearnSGD" = c("SciKitLearnSGDRegressor", "SciKitLearnSGDClassifier"),
		"MLlibDecisionTree" = c("MLlibDecisionTreeRegressor", "MLlibDecisionTreeClassifier"),
		"SciKitLearnGradientBoosting" = c("SciKitLearnGradientBoostingRegressor", "SciKitLearnGradientBoostingClassifier"),
		"RCaretGBM" = c("RCaretGBMRegressor", "RCaretGBMClassifier"),
		"SciKitLearnDecisionTree" = c("SciKitLearnDecisionTreeRegressor", "SciKitLearnDecisionTreeClassifier", "SciKitLearnDecisionTreeGiniClassifier"),
		"RRandomForest" = c("RRandomForestClassifier", "RRandomForestRegressor"),
		"SciKitLearnBagging" = c("SciKitLearnBaggingRegressor", "SciKitLearnBaggingClassifier"),
		"RStackingEnsembleGBM_of_GBM_with_Rpart" = c("RStackingEnsembleGBM_of_GBM_with_RpartRegressor", "RStackingEnsembleGBM_of_GBM_with_RpartClassifier"),
		"SciKitLearnRandomForest" = c("SciKitLearnRandomForestClassifier", "SciKitLearnRandomForestRegressor"),
		"MLlibLinearModel" = c("MLlibLogisticRegression", "MLlibLinearRegression"),
		"RRidgeGlmNet" = c("RRidgeLinearRegressionGlmnetRegressor", "RRidgeLogisticRegressionGlmnetClassifier"),
		"KerasDeepLearning" = c("KerasDeepLearningClassifier", "KerasDeepLearningRegressor"),
		"SciKitLearnAdaBoost" = c("SciKitLearnAdaBoostClassifier", "SciKitLearnAdaBoostRegressor"),
		"SciKitLearnSVM" = c("SciKitLearnSVMRegressor", "SciKitLearnSVMClassifier"),
		"RLassoGlmNet" = c("RLinearRegressionLMRegressor", "RLassoLogisticRegressionGlmnetClassifier", "RLassoLinearRegressionGlmnetRegressor"),
		"MLlibRandomForest" = c("MLlibRandomForestRegressor", "MLlibRandomForestClassifier")
	)
	
	list(
		adaptExtraModels = function(original) {
			if(.isServerVersionOlderThan("1.8")) {
				adapted = list()
				originalNames = names(original)
				for(n in originalNames) {
					if (n %in% names(backwardCompatibilityAlgorithmNamesMapping) ) { 
						oldNames = backwardCompatibilityAlgorithmNamesMapping[[n]]
						for(oldName in oldNames) {
							adapted[[oldName]] <- original[[n]]
						}
					} else {
						adapted[[n]] <- original[[n]]
					}
				}
				adapted
			} else {
				original
			}
		},
		adaptWhiteList = function(original) {
			if(.isServerVersionOlderThan("1.8")) {
				compatibleAlgNamesList = sapply(
					original,
					function(algName) {
					 	ifelse(
					 		algName %in% names(backwardCompatibilityAlgorithmNamesMapping), 
					 		backwardCompatibilityAlgorithmNamesMapping[algName], 
					 		algName
					 	)
				 	}, 
				 	simplify = TRUE, USE.NAMES = FALSE)
				unlist(compatibleAlgNamesList)
			} else {
				original
			}
		}
	)
})()

.isServerVersionOlderThan = function(version) {
	compareVersion(SBServerVersion, version) < 0
}

.showReport = function(report_path = NA, projectName, revision){ #confine to a specific list
	"\\code{report_path} path of report to show"        
	#htmlSource = paste0(artifact_loc,"/reports/", subFolder(report_name), "/", report_name, ".html")
	suppressWarnings({
		url <- paste0(getSBserverDomain(),paste0("/getToken")) 
		res = httr::GET(url)
		token = httr::content(res, as="text")
		htmlSource = paste0(getSBserverDomain(), "/analytics/report/", projectName, "/", revision, "/", report_path,"?token=", token)
		browseURL(htmlSource)
	})
}
