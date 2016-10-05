library(R6)

#' SB object that encapsulates an enrichment job
#' 
#' @examples
#' # Enrichment example
#' \donttest{
#' # Learn
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' # Non blocking predict
#' enrichment = session$enrich(getData("titanic_test"), async=TRUE)
#' enrichment$currentStatus()
#' enrichment = prediction$getData()
#' head(data)
#' }
Enrichment = R6Class("Enrichment",
	 lock_objects = TRUE,
	 lock_class = TRUE,
	 cloneable = FALSE,
	 private = list(
	 	executionId = NA_character_,
	 	totalRows = NA_integer_,
	 	data = NULL
	 ),
	 public = list(
	 	initialize = function(executionId, totalRows = NA_integer_) {
	 		"Initializes an Enrichment object using executionId."
	 		private$executionId = executionId
	 		private$totalRows = totalRows
	 	},
	 	
	 	currentStatus = function() {
	 		"Get prediction job status - Started/Finished, number of lines processed, etc."
	 		jobStatus = .getEnrichJobStatus(private$executionId)
	 		if(jobStatus$state == "Finished" && !is.null(jobStatus$error)) {
	 			message(paste("Enrichment has finished with an error:", jobStatus$error))
	 		} else if(jobStatus$state == "Finished" && !is.null(jobStatus$result)) {
	 			message("Enrichment has finished successfully. Call Enrichment$getData() to get the results")
	 		} else if(jobStatus$state == "Started" || jobStatus$state == "Running") {
	 			message(paste("Enrichment job is running. So far processed", jobStatus$rowCount, "rows"))
	 		}
	 	},
	 	
	 	getData = function(localFileName = NA_character_, runBlocking=TRUE) {
	 		"If \\code{runBlocking} is TRUE, block until the job finishes while showing processed rows counter, and return the result when available. If \\code{runBlocking} is FALSE return the data if available, else return NULL"
	 		outputName = ifelse(is.na(localFileName), paste("enriched", private$executionId, sep = "-"), localFileName)
	 		if(!is.null(private$data)) {
	 			private$data
	 		} else if(runBlocking) {
	 			jobStatus = .getEnrichJobStatus(private$executionId)
	 			state = jobStatus$state
	 			if(state!="Finished") {
	 				message("Blocking the R console until enrichment is finished.")
	 			}
	 			
	 			while(state!="Finished") {
	 				Sys.sleep(5)
	 				jobStatus = .getEnrichJobStatus(private$executionId)
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
	 				private$data = enrichedData
	 				private$data
	 			} else {
	 				message("Failed to download the enriched data")
	 				NULL
	 			} 
	 		} else {
	 			message("Data isn't available locally. Use blockWaitingForData=TRUE to get the data when enriched data is ready")
	 			NULL
	 		}
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