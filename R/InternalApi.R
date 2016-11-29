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
	url <- paste0(getSBserverDomain(),"/rapi/predict/", jobId)

	res = .executeRequest(
        function() httr::GET(url, httr::content_type_json()),
        errorHandling = .withErrorHandling(onError = .onErrorBehavior$MESSAGE),
        responseSerializer = .responseSerializers$JSON
    )
	jobStatus = .jobStatusFromJson(res)
}

.getEnrichJobStatus = function(jobId) {
	url <- paste0(getSBserverDomain(),"/rapi/enrich/", jobId)

	res = .executeRequest(
		function() httr::GET(url, httr::content_type_json()),
		errorHandling = .withErrorHandling(onError = .onErrorBehavior$MESSAGE),
		responseSerializer = .responseSerializers$JSON
	)
	jobStatus = .jobStatusFromJson(res)
}

.getNotificationLogReport = function(projectName, revision, path, responseSerializer = .responseSerializers$JSON, onError = .onErrorBehavior$STOP) {
	url = paste0(getSBserverDomain(),"/rapi/notificationsLog/",projectName,"/",revision, "?path=", path)
	.executeRequest(
		function() httr::GET(url),
		errorHandling = .withErrorHandling(onError = onError),
		responseSerializer = responseSerializer
	)
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
    downloadResult = .executeRequest(
    	function() httr::GET(url, config = httr::progress(type = "down"), httr::write_disk(localFile, overwrite = TRUE)),
      errorHandling = .withErrorHandling(retries = 2)
    )
	
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
	downloadResult = .executeRequest(
		function() httr::GET(url),
		errorHandling = .withErrorHandling(retries = 2)
	)
	writeBin(httr::content(downloadResult), saveToPath)
	normalizePath(saveToPath)
}

.downloadDataFrame = function(projectName, revision, pathOnServer, saveToPath) {
	localFile = .downloadFile(projectName, revision, pathOnServer, saveToPath)
	read.table(localFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
}

.exportModelToPredictionBox = function(project, revision,
																			 predictionBoxUrl, authKey = "",
																			 uploadExtractor = TRUE, uploadModel = TRUE, uploadContexts = TRUE,
																			 overwriteExisting = TRUE, overrideTargetGroupName = NULL) {
	url = paste0(getSBserverDomain(), "/api2/predictionBoxClient/uploadModel")
	params = list(project = project, revision = revision,
								predictionBoxUrl = predictionBoxUrl, authKey = authKey,
								uploadExtractor = uploadExtractor, uploadModel = uploadModel, uploadContexts = uploadContexts,
								overwriteExisting = overwriteExisting, overrideTargetGroupName = overrideTargetGroupName)
	.executeRequest(function() httr::POST(url, body = rjson::toJSON(params), httr::content_type_json()))
	message(paste0("Model from project: ", projectName, ", revision: ", revision, " was exported successfully"))
}

.handleContexts = function(contextDatasets, projectName, useEscaping=TRUE, uncompressedUpload=FALSE) {
	fileUploadThreshold = ifelse(uncompressedUpload, NA, 0)
	isContextObjectDefinition = function(c) "contextObject" %in% class(c) || "contextDefinition" %in% class(c)
	if (!is.null(contextDatasets)) {
		if (class(contextDatasets) != "list") {
			warning("contextDatasets should be a list")
			if (!isContextObjectDefinition(contextDatasets)) error("contextDatasets are not of class contextObject")
			contextDatasets=list(contextDatasets)
		}
		contextDatasets = as.list(contextDatasets)
		if (!all(sapply(contextDatasets, isContextObjectDefinition))) stop("Not all provided context objects are of type 'contextObject'")
		contextDatasets = lapply(contextDatasets, function(context) {
			if("contextDefinition" %in% class(context)) {  # the new way of defining contexts
				createContextObject = function(data=NULL, contextProvider=NULL, contextTypes=NULL, name = NULL, keyColumns = list(), timeColumn = NULL, graphSourceNodeColumn = NULL,graphTargetNodeColumn= NULL) {
					context = contextObject(data, contextTypes, name, keyColumns, timeColumn, graphSourceNodeColumn ,graphTargetNodeColumn)
					if(!is.null(contextProvider)) {
						context$contextProvider = contextProvider
					}
					context
				}
				
				if("lookupTableContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold),
															keyColumns = context$keyColumns,
															name = context$name,
															contextTypes = list("LookupTables"))
				} else if("textIndexContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					keyColumns = if(is.null(context$keyColumn)) {
						list()
					} else {
						list(context$keyColumn)
					}
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold),
															keyColumns = keyColumns,
															name = context$name,
															contextTypes = list("TextIndex"))
				} else if("graphContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold),
															graphSourceNodeColumn = context$sourceNodeColumn,
															graphTargetNodeColumn = context$targetNodeColumn,
															name = context$name,
															contextTypes = list("Graph"))
				} else if("timeSeriesContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					contextType = ifelse(length(context$keyColumns)>0, "TimeSeriesMap", "TimeSeries")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold),
															timeColumn = context$timeColumn,
															keyColumns = context$keyColumns,
															name = context$name,
															contextTypes = list(contextType))
				} else if("codeFileContextDefinition" %in% class(context)) {
					createContextObject(contextProvider = list(url = context$url, name = "Code file", jsonClass = "com.sparkbeyond.runtime.data.transform.CodeFile"),
															name = context$name)
				} else if("word2VecBasedOnDataContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold),
															name = context$name,
															keyColumns = context$keyColumns,
															contextTypes = list("Word2Vec"))
				} else if("word2VecPretrainedS3ContextDefinition" %in% class(context)) {
					# S3source was renamed to source in 1.8, depracated, left for backward compatibility
					createContextObject(contextProvider = list(S3source = context$modelName, source = context$modelName, name = context$name, jsonClass = "com.sparkbeyond.runtime.data.transform.Word2Vec"),
															name = context$name,
															contextTypes = list("Word2VecPretrainedS3"))
				} else if("word2VecPretrainedLocalContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold),
															name = context$name,
															contextTypes = list("Word2VecPretrainedLocal"))
				} else if("featuresFromRevisionContextDefinition" %in% class(context)) {
					createContextObject(contextProvider = list(revisionId = context$revision, name = "FeaturesFromRevision", jsonClass = "com.sparkbeyond.runtime.data.transform.FeaturesFromRevision"),
															name = context$name)
				} else if("openStreetMapContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name), "")
					serverPath = uploadFileToServer(filePath = context$filePath, projectName = projectName, name = paste0("context", contextName))
					createContextObject(contextProvider = list(path = serverPath, jsonClass = "com.sparkbeyond.runtime.data.transform.OpenStreetMapFileInput"),
															name = context$name,
															contextTypes = contextTypesList(osmFile = TRUE))
				} else if("shapeFileContextDefinition" %in% class(context)) {
					findFilesRelatedToShapeFile = function(shapeFilePath) {
						stopifnot(tools::file_ext(shapeFilePath) == "shp")
						
						fileNameWithoutExtention = tools::file_path_sans_ext(shapeFilePath)
						possibleExts = c("cpg", "dbf", "prj", "shx")
						relatedFiles = paste0(fileNameWithoutExtention,".",possibleExts)
						existingRelatedFiles = relatedFiles[file.exists(relatedFiles)]
						
						requiredExts = c("dbf", "shx")
						requiredFiles = paste0(fileNameWithoutExtention,".",requiredExts)
						if(!all(requiredFiles %in% existingRelatedFiles)) {
							vec2String = function(vec) paste(vec, collapse = ", ")
							stop(paste0("Missing some of the mandatory files for shape file: ", shapeFilePath, ".\n Required: ", vec2String(requiredFiles), ".\n Found: ", vec2String(existingRelatedFiles)))
						}
						relatedFiles
					}
					
					if(! tools::file_ext(context$filePath) == "shp") {
						stop(paste("filePath in shapeFile context should point to a file with .shp extension. Instead received:", context$filePath))
					}
					
					shapeComplimentaryFiles = findFilesRelatedToShapeFile(context$filePath)
					uploadFiles = Vectorize(uploadFileToServer, c("filePath"))
					
					contextName = ifelse(!is.null(context$name), paste0("_", context$name), "")
					uploadFiles(shapeComplimentaryFiles, projectName = projectName, name = paste0("context", contextName), generateHash=FALSE)
					serverPath = uploadFileToServer(filePath = context$filePath, projectName = projectName, name = paste0("context", contextName), generateHash=FALSE)
					createContextObject(contextProvider = list(path = serverPath, name = context$name, jsonClass = "com.sparkbeyond.runtime.data.transform.ShapeFileInput"),
															name = context$name,
															contextTypes = list("SpatialIndexFromShapes"))
				}
			} else {
				#Handle data.frame input
				contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
				context$data = uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
								 							 , useEscaping = useEscaping, directUploadThreshold = fileUploadThreshold)
				context
			}
		})
	}
	contextDatasets
}

.getSupportedAlgorithms = function() {
	url = paste0(getSBserverDomain(), "/api2/algorithms")
	algorithmsDF = .executeRequest(
		function() httr::GET(url),
		responseSerializer = .responseSerializers$DATA_FRAME
		)
	algorithmsDF['name']$name
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

.contextTypesCompatibility = function(contextTypes) {
	# if(.isServerVersionOlderThan("1.9")) {
	# 	lapply(contextTypes, function(name) { ifelse(name=="TextIndex", "InvertedIndex", name) })
	# } else {
		contextTypes
	# }
}

.isServerVersionOlderThan = function(version) {
	compareVersion(SBServerVersion, version) < 0
}


.showReport = function(report_path = NA, projectName, revision){ #confine to a specific list
	"\\code{report_path} path of report to show"        
	suppressWarnings({
		url <- paste0(getSBserverDomain(),paste0("/getToken"))
		token = .executeRequest(
			function() httr::GET(url),
			responseSerializer = .responseSerializers$TEXT
		)
		htmlSource = paste0(getSBserverDomain(), "/analytics/report/", projectName, "/", revision, "/", report_path,"?token=", token)
		browseURL(htmlSource)
	})
}

	# Serialization
.responseSerializers = list(
	NONE = "NONE",
	JSON = "JSON",
	TEXT = "TEXT",
	DATA_FRAME = "DATA_FRAME"
)

# Error handling
.onErrorBehavior = list(
	STOP = "stop",
	WARNING = "warning",
	MESSAGE = "message",
	SILENT = "silent"
)

.withErrorHandling = function(retries = 0, onError = .onErrorBehavior$STOP, message = NA_character_) {
	structure(
		class = c("ErrorHandlingSettings"),
		list(retries = retries, onError = onError, message = message)
	)
}

.signalApiCondition = function(type, message, call = sys.call(-1), isRetriable=TRUE) {
	exc = structure(
		class = c(type, "RequestFailedError", "condition"),
		list(message = message, isRetriable = isRetriable, call = call)
	)
	stop(exc)
}

.sdkError = function(message, call) {
	structure(
		class = c("SdkError", "condition"),
		list(message = message, call = call)
	)
}

.executeRequest = function(httpCall, errorHandling = .withErrorHandling(), responseSerializer = .responseSerializers$NONE) {
	externalCallStack = sys.call(-1)
	error = NA_character_
	trimUnparsableContentInErrorMessagesLength = 400
	handleError = function(message, onError, stackTrace) {
		if(onError == .onErrorBehavior$STOP) {
			stop(.sdkError(message, stackTrace))
		} else if(onError == .onErrorBehavior$WARNING) {
			warning(message)
		} else if(onError == .onErrorBehavior$MESSAGE) {
			message(message)
		} else if(onError == .onErrorBehavior$SILENT) {
			NULL
		} else {
			stop("Incorrect onError behavior defined, should be one of the following: [stop, warning, message, silent]")
		}
	}
	
	exceptions = list(
		CONNECTION_CURL = "ConnectionCurl",
		AUTHENTICATION = "Authentication",
		AUTHORIZATION = "Authorization",
		APPLICATION_FAILURE = "ApplicationFailure",
		APPLICATION_ERROR = "ApplicationError",
		UNEXPECTED_RESPONSE_FORMAT = "ResponseFormatError"
	)
	
	extractApplicationError = function(httpResponse, expectJson=FALSE) {
		tryCatch({
			text = suppressMessages(httr::content(httpResponse, as="text"))
			res <- jsonlite::fromJSON(txt = text, simplifyDataFrame=TRUE)
			if(!is.null(res$error)) {
				res$error
			} else if(!is.null(res$message)) {
				res$message
			} else {
				NA_character_
			}
		}, error = function(e) {
			if(expectJson) {
				.signalApiCondition(exceptions$UNEXPECTED_RESPONSE_FORMAT, "Failed to parse the response")
			} else {
				NA_character_
			}
		})
	}
	
	responseSerializersFactory = list(
		NONE = function(response) { response },
		JSON = function(response) { 
			responseText = NA_character_
			tryCatch({
					responseText <- suppressMessages(httr::content(response, as="text"))
					jsonlite::fromJSON(txt=httr::content(response, as="text"), simplifyDataFrame=TRUE)
				},error = function(e) {
					truncatedResponse = ifelse(is.na(responseText), "Failed to parse context as text", strtrim(responseText, trimUnparsableContentInErrorMessagesLength))
					if(nchar(responseText)>trimUnparsableContentInErrorMessagesLength) {
						truncatedResponse = paste0(truncatedResponse, "...")
					}
					.signalApiCondition(exceptions$APPLICATION_FAILURE, paste0("Failed to parse response as json for url: ", response$url, 
																																		 "\nHTTP Status: ", httr::status_code(response), 
																																		 "\nResponse: ", truncatedResponse), 
															call = externalCallStack, isRetriable = FALSE)
				})
			},
		TEXT = function(response) { httr::content(response, as="text") },
		DATA_FRAME = function(response) { suppressWarnings(read.table(textConnection(httr::content(response, as="text")),sep="\t", header=TRUE, stringsAsFactors = FALSE, quote = "", comment.char = "")) }
	)
	
	processRequest = function(httpCall) {
		# Request execution
		response = tryCatch(
			httpCall(), 
			error = function(e) {
				.signalApiCondition(exceptions$CONNECTION_CURL, paste0(e$message, ", usually indicates a connectivity problem"), call = externalCallStack)
			}
		)
		
		# Checking HTTP status
		status = httr::status_code(response)
		statusMessage = httr::http_status(response)$message
		applicationError = extractApplicationError(response)
		if(status==401) {
			.signalApiCondition(exceptions$AUTHENTICATION, "Authentication error, please login", call = externalCallStack, isRetriable = FALSE)
		} else if(status==403) {
			.signalApiCondition(exceptions$AUTHORIZATION, "Unauthorized: you don't have the required permissions", call = externalCallStack, isRetriable = FALSE)
		} else if(status >= 400) {
			ifelse(is.na(applicationError),
						 { 
						 	responseText = strtrim(suppressMessages(httr::content(response, as="text")), trimUnparsableContentInErrorMessagesLength)
						 	.signalApiCondition(exceptions$APPLICATION_FAILURE, paste0(statusMessage, ", for url: ", response$url, "\nerror: ", responseText), call = externalCallStack, isRetriable = FALSE)
						 	},
						 .signalApiCondition(exceptions$APPLICATION_ERROR, applicationError, isRetriable = FALSE))
		} else if(status == 303) {
			if(grepl(response$headers$location, "login")) {
				.signalApiCondition(exceptions$AUTHENTICATION, "Authentication error, please login", call = externalCallStack, isRetriable = FALSE)
			} else {
				warning(paste("Redirecting from:", response$url, "to:", response$headers$location))
			}
		}
		
		# Handling application errors in successfull HTTP requests
		if(!is.na(applicationError)) {
			.signalApiCondition(exceptions$APPLICATION_ERROR, applicationError, call = externalCallStack)
		}
		
		response
	}
	
	attempts = 0
	requestFinished = FALSE
	result = NULL
	while (!requestFinished && attempts <= errorHandling$retries) {
		tryCatch( {
				response = processRequest(httpCall)
				requestFinished = TRUE
				responseSerializerImpl = responseSerializersFactory[[responseSerializer]]
				result = responseSerializerImpl(response)
				# print(paste("Result:", result))
			},
			RequestFailedError = function(e) {
				# print(paste("RequestFailedError handler called. Error:", e))
				if(attempts >= errorHandling$retries || !e$isRetriable) {
					requestFinished <<- TRUE
					errorMessage = e$message
					if(!is.na(errorHandling$message)) {
						errorMessage = paste0(errorHandling$message, ":\n", errorMessage)
					}
					result = handleError(errorMessage, errorHandling$onError, stackTrace = e$call)
				} else {
					attempts <<- attempts + 1
					message(paste0("Request failed with error: ", e$message, ", retrying in 5 seconds"))
					Sys.sleep(5)
				}
			},
			error = function(e) {
				requestFinished <<- TRUE
				if("SdkError" %in% class(e)) {
					stop(e)
				} else {
					errorMessage = e$message
					if(!is.na(errorHandling$message)) {
						errorMessage = paste0(errorHandling$message, ":\n", errorMessage)
					}
					result = stop(errorMessage)
				}
			}
		)
	}
	result
}

.assertUserAuthenticated = function() {
	url <- paste0(getSBserverDomain(), "/currentUser")
	userInfo = .executeRequest(
		function() httr::GET(url, encode = "form"),
		responseSerializer = .responseSerializers$JSON
	)
}
