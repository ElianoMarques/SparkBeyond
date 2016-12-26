library(R6)

#' SB object that encapsulates an enrichment job
#' 
#' @examples
#' # Enrichment example
#' \donttest{
#' # Learn
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' # Non blocking enrich
#' enrichment = session$enrich(getData("titanic_test"), async=TRUE)
#' enrichment$currentStatus()
#' data = enrichment$data()
#' head(data)
#' }
EnrichmentJob = R6Class("EnrichmentJob",
	 lock_objects = TRUE,
	 lock_class = TRUE,
	 cloneable = FALSE,
	 private = list(
	 	jobId = NA_character_,
	 	totalRows = NA_integer_,
	 	job = NULL,
	 	dataFrame = NULL,
	 	printCurrentState = function(state) {
	 		if(state$isEnqueued) {
	 			message("Enrichment job is waiting in queue")
	 		} else if(state$isRunning) {
	 			status = .enrichPredictStatusFromJson(state$status)
	 			if(!status$builtContext) {
	 				message(paste("Enrichment job is running. Parsing contexts..."))
	 			} else {
	 				totalRowsMessage = ifelse(is.na(private$totalRows), "", paste0("out of ", private$totalRows))
	 				cat("\r", "Enrichment job is running. So far processed", status$processedRows, totalRowsMessage, "rows", "\r")
	 			}
	 		} else if(state$isCompleted) {
	 			result = .jobResultFromJson(state$result)
	 			status = .enrichPredictStatusFromJson(state$status)
	 			if(result$isSucceeded) {
	 				totalRowsMessage = ifelse(is.na(private$totalRows), "", paste0("out of ", private$totalRows))
	 				message(paste("Enrichment has finished successfully. Processed", status$processedRows, totalRowsMessage, "rows"))
	 			} else {
	 				stop(paste("Enrichment failed:", result$error))
	 			}
	 		} else {
	 			stop(paste("Unexpected job state:", state))
	 		}
	 	}
	 ),
	 public = list(
	 	initialize = function(jobId, totalRows = NA_integer_) {
	 		"Initializes an EnrichmentJob with jobId."
	 		private$jobId = jobId
	 		private$totalRows = totalRows
	 		private$job = .initJob(jobId)
	 	},
	 	
	 	print = function(...) {
	 		cat("Enrichment job id: ", private$jobId, sep = "")
	 		invisible(self)
	 	},
	 	
	 	currentStatus = function() {
	 		"Get Enrichment job status - Started/Finished, number of lines processed, etc."
	 		if(.isServerVersionOlderThan("1.10")) {
		 		"Get Enrichment job status - Started/Finished, number of lines processed, etc."
		 		jobStatus = .getEnrichJobStatus(private$jobId)
		 		if(jobStatus$state == "Finished" && !is.null(jobStatus$error)) {
		 			message(paste("Enrichment has finished with an error:", jobStatus$error))
		 		} else if(jobStatus$state == "Finished" && !is.null(jobStatus$result)) {
		 			message("Enrichment has finished successfully. Call Enrichment$getData() to get the results")
		 		} else if(jobStatus$state == "Started" || jobStatus$state == "Running") {
		 			message(paste("Enrichment job is running. So far processed", jobStatus$rowCount, "rows"))
		 		} else {
		 			warning("Unexpected jobStatus: ", jobStatus)
		 		}
	 		} else {
	 			jobState = private$job$currentState()
	 			private$printCurrentState(jobState)
	 		}
	 	},
	 	
	 	data = function(localFileName = NA_character_, runBlocking=TRUE) {
	 		"If \\code{runBlocking} is TRUE, block until the job finishes while showing processed rows counter, and return the result when available. If \\code{runBlocking} is FALSE return the data if available, else return NULL"
	 		outputName = ifelse(is.na(localFileName), paste("enriched", private$jobId, sep = "-"), localFileName)
	 		if(!is.null(private$dataFrame)) {
	 			private$dataFrame
	 		} else if(runBlocking) {
	 			if(.isServerVersionOlderThan("1.10")) {
		 			jobStatus = .getEnrichJobStatus(private$jobId)
		 			state = jobStatus$state
		 			if(state!="Finished") {
		 				message("Blocking the R console until enrichment is finished.")
		 			}
		 			
		 			while(state!="Finished") {
		 				Sys.sleep(5)
		 				jobStatus = .getEnrichJobStatus(private$jobId)
		 				processed = min(jobStatus$rowCount, private$totalRows, na.rm = TRUE)
		 				cat("\r", "Processed rows: ", processed, "\r")
		 				state = jobStatus$state
		 				# progressBar$update(processed)
		 			}
		 			# progressBar$onFinish()
		 			if(!is.null(jobStatus$error)) {
		 				stop(paste("Enrichment failed with error:", jobStatus$error))
		 			}
		 			result = jobStatus$result
		 			projectName = jobStatus$projectName
		 			revision = jobStatus$revision
		 			localFile = .downloadFile(projectName, revision, pathOnServer = result, saveToPath = paste0(outputName, ".tsv.gz"))
		 			enrichedData = if (!is.null(localFile)) {
		 				.loadEnrichedDataFrame(localFile)
		 			} else {
		 				NULL
		 			}
		 			
		 			if(!is.null(enrichedData)) {
		 				private$dataFrame = enrichedData
		 				private$dataFrame
		 			} else {
		 				message("Failed to download the enriched data")
		 				NULL
		 			} 
		 		}	else {
		 				jobState = private$job$currentState()
		 				if(!jobState$isCompleted) {
		 					message("Blocking the R console until enrichment is finished.")
		 				}
		 				
		 				private$job$pollState(function(state) {
		 					private$printCurrentState(state)
		 					
		 					if(state$isCompleted) {
		 						result = .jobResultFromJson(state$result)
		 						if(result$isSucceeded) {
		 							downloadUrl = paste0(getSBserverDomain(),"/api/downloadJobResult/", private$jobId)
		 							enrichedFile = .download(downloadUrl, localFile = paste0(outputName, ".tsv.gz"), description = paste("enrichment result for job:", private$jobId))
		 							private$dataFrame = .loadEnrichedDataFrame(enrichedFile)
		 						} else {
		 							stop(paste("Enrichment failed with an error:", result$failure))
		 						}
		 						
		 						FALSE
		 					} else {
		 						TRUE
		 					}
		 				})
		 				private$dataFrame
		 			}
	 			} else {
	 			message("Data isn't available locally. Use blockWaitingForData=TRUE to get the data when enriched data is ready")
	 			NULL
	 		}
	 	},
	 	
	 	cancel = function() {
	 	  "Cancel the job"
	 		if(.isServerVersionOlderThan("1.10")) {
	 			stop("cancel is available starting from server version 1.10. Currently used version: ", SBServerVersion)
	 		}
	 		
	 		private$job$cancel()
	 	}
	 )
)

.loadEnrichedDataFrame = function(filePath) {
	df = read.table(filePath, sep="\t", quote = "",header=FALSE, skip = 1, comment.char = "")
	headers = readLines(filePath , n=1)
	cols = strsplit(headers, '\t')
	colnames(df) = cols[[1]]
	
	for(i in 1:ncol(df)) {
		if (length(levels(df[[i]])) == 1 && (levels(df[[i]]) == "false" || levels(df[[i]]) == "true")) {
			df[,i] = as.logical(as.character(df[,i]))
		} else if (length(levels(df[[i]])) == 2 && (levels(df[[i]]) == c("false","true"))) {
			df[,i] = as.logical(as.character(df[,i]))
		}
	}
	df
}