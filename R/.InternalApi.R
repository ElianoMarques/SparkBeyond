
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
	
	attr(value, "class") <- "predcitResult"
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
	if(!is.na(localFile)) {
		read.table(localFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
	} else {
		NULL
	}
}
