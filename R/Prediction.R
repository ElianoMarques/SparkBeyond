

Prediction = setRefClass("Prediction",
		fields = list(
			executionId = "character",
			totalRows = "numeric",
			data = "data.frame"
		),
		methods = list(
			initialize = function(executionId, totalRows = NA_integer_) {
				"initializes a prediction object using executionId."
				executionId <<- executionId
				totalRows <<- totalRows
			},
			
			currentStatus = function() {
				jobStatus = .getPredictJobStatus(executionId)
				if(jobStatus$state == "Finished" && !is.null(jobStatus$error)) {
					message(paste("Prediction has finished with an error:", jobStatus$error))
				} else if(jobStatus$state == "Finished" && !is.null(jobStatus$result)) {
					message("Prediction has finished successfully. Call Prediction$getData() to get the results")
				} else if(jobStatus$state == "Started" || jobStatus$state == "Running") {
					message(paste("Prediction job is running. So far processed", jobStatus$rowCount, "rows"))
				}
			},
			
			getData = function(localFileName = "predicted", runBlocking=TRUE) { 
				if(nrow(data)!=0) {
					data
				} else if(runBlocking) {
					jobStatus = .getPredictJobStatus(executionId)
					state = jobStatus$state
					if(state!="Finished") {
						message("Blocking the R console until prediction is finished.")
					}
					
					while(state!="Finished") {
						Sys.sleep(5)
						jobStatus = .getPredictJobStatus(executionId)
						processed = jobStatus$rowCount
						cat("\r", "Processed rows: ", processed, "\r")
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
					predictedData = .downloadDataFrame(projectName, revision, pathOnServer = result, saveToPath = paste0(localFileName, ".tsv.gz"))
					if(!is.null(predictedData)) {
						data <<- predictedData
						data
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