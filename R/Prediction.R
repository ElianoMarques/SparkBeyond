#' SB object that encapsulates a prediction job
#' 
#' @field executionId prediction job id
#' @examples
#' # Prediction example
#' \donttest{
#' # Create a Session object from scratch
#' session = Session("project name", revision_id)
#' # Learn
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' # Non blocking predict
#' prediction = session$predict(getData("titanic_test"), async=TRUE)
#' prediction$currentStatus()
#' data = prediction$getData()
#' head(data)
#' }
Prediction = setRefClass("Prediction",
		fields = list(
			executionId = "character",
			.totalRows = "numeric",
			.data = "data.frame"
		),
		methods = list(
			initialize = function(executionId, totalRows = NA_integer_) {
				"Initializes a Prediction object using executionId."
				executionId <<- executionId
				.totalRows <<- totalRows
			},
			
			currentStatus = function() {
				"Get prediction job status - Started/Finished, number of lines processed, etc."
				jobStatus = .getPredictJobStatus(executionId)
				if(jobStatus$state == "Finished" && !is.null(jobStatus$error)) {
					message(paste("Prediction has finished with an error:", jobStatus$error))
				} else if(jobStatus$state == "Finished" && !is.null(jobStatus$result)) {
					message("Prediction has finished successfully. Call Prediction$getData() to get the results")
				} else if((jobStatus$state == "Started" || jobStatus$state == "Running") && !is.na(.totalRows) && jobStatus$rowCount>=.totalRows) {
					message("Generating reports")
				} else if(jobStatus$state == "Started" || jobStatus$state == "Running") {
					message(paste("Prediction job is running. So far processed", jobStatus$rowCount, "rows"))
				}
			},
			
			getData = function(localFileName = NA_character_, runBlocking=TRUE) {
				"If \\code{runBlocking} is TRUE, block until the job finishes while showing processed rows counter, and return the result when available. If \\code{runBlocking} is FALSE return the data if available, else return NULL"
				outputName = ifelse(is.na(localFileName), paste("predicted", executionId, "-"), localFileName)
				if(nrow(.data)!=0) {
					.data
				} else if(runBlocking) {
					jobStatus = .getPredictJobStatus(executionId)
					state = jobStatus$state
					if(state!="Finished") {
						message("Blocking the R console until prediction is finished.")
					}
					
					displayedGeneratingReportsMessage = FALSE
					while(state!="Finished") {
						Sys.sleep(5)
						jobStatus = .getPredictJobStatus(executionId)
						processed = min(jobStatus$rowCount, .totalRows, na.rm = TRUE)
						if(!is.na(.totalRows) && processed==.totalRows) {
							if(!displayedGeneratingReportsMessage) {
								cat("\r", "Processed rows: ", .totalRows, "\r")
								message("Generating reports")
								displayedGeneratingReportsMessage = TRUE
							}
						} else {
							cat("\r", "Processed rows: ", processed, "\r")
						}
						state = jobStatus$state
						# progressBar$update(processed)
					}
					# progressBar$onFinish()
					if(!is.null(jobStatus$error)) {
						stop(paste("Prediction failed with error:", jobStatus$error))
					}
					result = jobStatus$result
					projectName = jobStatus$projectName
					revision = jobStatus$revision
					predictedData = .downloadDataFrame(projectName, revision, pathOnServer = result, saveToPath = paste0(outputName, ".tsv.gz"))
					
					if(!is.null(predictedData)) {
						.data <<- predictedData
						.data
					} else {
						message("Failed to download the prediction result")
						NULL
					} 
				} else {
					message("Data isn't available locally. Use blockWaitingForData=TRUE to get the data when prediction is ready")
					NULL
				}
			}
		)
)