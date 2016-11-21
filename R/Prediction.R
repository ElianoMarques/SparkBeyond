library(R6)

#' SB object that encapsulates a prediction job
#' 
#' @field executionId prediction job id
#' @examples
#' # Prediction example
#' \donttest{
#' # Learn
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' # Non blocking predict
#' prediction = session$predict(getData("titanic_test"), async=TRUE)
#' prediction$currentStatus()
#' data = prediction$data()
#' head(data)
#' }
PredictionJob = R6Class("PredictionJob",
		lock_objects = TRUE,
		lock_class = TRUE,
		cloneable = FALSE,
		private = list(
			executionId = NA_character_,
			totalRows = NA_integer_,
			dataFrame = NULL
		),
		public = list(
			initialize = function(executionId, totalRows = NA_integer_) {
				"Initializes a Prediction object using executionId."
				private$executionId <- executionId
				private$totalRows <- totalRows
			},
			
			print = function(...) {
				cat("Prediction job id: ", private$executionId, sep = "")
				invisible(self)
			},
			
			currentStatus = function() {
				"Get prediction job status - Started/Finished, number of lines processed, etc."
				jobStatus = .getPredictJobStatus(private$executionId)
				if(jobStatus$state == "Finished" && !is.null(jobStatus$error)) {
					message(paste("Prediction has finished with an error:", jobStatus$error))
				} else if(jobStatus$state == "Finished" && !is.null(jobStatus$result)) {
					message("Prediction has finished successfully. Call Prediction$data() to get the results")
				} else if((jobStatus$state == "Started" || jobStatus$state == "Running") && !is.na(private$totalRows) && jobStatus$rowCount>=private$totalRows) {
					message("Generating reports")
				} else if(jobStatus$state == "Started" || jobStatus$state == "Running") {
					message(paste("Prediction job is running. So far processed", jobStatus$rowCount, "rows"))
				}
			},
			
			data = function(localFileName = NA_character_, runBlocking=TRUE) {
				"If \\code{runBlocking} is TRUE, block until the job finishes while showing processed rows counter, and return the result when available. If \\code{runBlocking} is FALSE return the data if available, else return NULL"
				outputName = ifelse(is.na(localFileName), paste("predicted", private$executionId, sep = "-"), localFileName)
				if(!is.null(private$dataFrame)) {
					private$dataFrame
				} else if(runBlocking) {
					jobStatus = .getPredictJobStatus(private$executionId)
					state = jobStatus$state
					if(state!="Finished") {
						message("Blocking the R console until prediction is finished.")
					}
					
					reachedTotalRows = FALSE
					displayedGeneratingReportsMessage = FALSE
					while(state!="Finished" && !displayedGeneratingReportsMessage) { # if reports are generated then the data is ready to download
						Sys.sleep(5)
						jobStatus = .getPredictJobStatus(private$executionId)
						processed = min(jobStatus$rowCount, private$totalRows, na.rm = TRUE)
						if(!is.na(private$totalRows) && processed==private$totalRows) {
							if(!reachedTotalRows) {
								reachedTotalRows = TRUE
								cat("\r", "Processed rows: ", private$totalRows, "\r")
							} else if(reachedTotalRows && !displayedGeneratingReportsMessage) {
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
						private$dataFrame = predictedData
						private$dataFrame
					} else {
						message("Failed to download the prediction result")
						NULL
					} 
				} else {
					message("Data isn't available locally. Use blockWaitingForData=TRUE to get the data when prediction is ready")
					NULL
				}
			}
			
			################################## lift #####################			
			, liftPlot = function(predictionResult = NA, overrideDesiredClass = NA, title = "test", percentOfPopulationToPlot = 0.2, outputName = "lift", fileEscaping = TRUE, ...) { #TODO: change documentation
				"Returns lift from a created model and generates three plots. \\code{predictionResult} is a dataframe to be analyzed, \\code{overrideDesiredClass} the class in the label column to check the lift for (e.g. '1'), \\code{title} optional: a title for the plot. \\code{percentOfPopulationToPlot} optional: limit the plot to the top percent of the data (x axis)."
				title = ifelse(title == "test", paste0("test-",private$executionId), title) #unless a non-default title is provided
				
				#if no prediction input was given use the one on the server first - change default to NA
				jobStatus = .getPredictJobStatus(private$executionId)
				projectName = jobStatus$projectName
				revision = jobStatus$revision
				## check if plot is available - problematic because of plotName - won't be done now
				# doesFileExistOnServer(projectName, paste0("/",revision,"/reports/predictions/test-", private$executionId, "/"))
				# else check if in generating reports status
				# else call lift calculations

				extraParams = list(...)        
				remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
				
				#check if predicion file exists on the server instead of uploading it again - currently impossible - only uploaded folder can be used.
				#requestedPredictionDataPath = paste0("/",revision,"/reports/predictions/test-", private$executionId, "/predictions.tsv.gz")
				#predicionDataPath = ifelse (doesFileExistOnServer(projectName, requestedPredictionDataPath), requestedPredictionDataPath , {
				#if (is.na(modelBuilt) || !modelBuilt) warning("Lift requires full model building using learn")  check if classification problem
				uncompressedUpload = ifelse(!is.null(extraParams$uncompressedUpload), extraParams$uncompressedUpload, FALSE)
				fileUploadThreshold = ifelse(uncompressedUpload, NA, 0)
				if (is.na(predictionResult)) predictionResult = self$data() # getting (hopefully) the local prediction data frame
				datapath = ifelse (remoteMode, {
						uploadedPath = uploadToServer(data = predictionResult, projectName = projectName, name = "lift", useEscaping = fileEscaping, directUploadThreshold = fileUploadThreshold)
						if(is.na(uploadedPath)) stop("failed to upload file to plots lifts to server")
						uploadedPath        
					},
					writeToServer(data, prefix = "lift", useEscaping = fileEscaping) 
				)
				
				params <-list(modelPath = paste0(projectName, "/", revision), #artifact_loc, #this is not available at this point
											predictionPath = datapath,
											title = title,
											percentOfPopulationToPlot= percentOfPopulationToPlot,
											fileEscaping = fileEscaping,
											externalPrefixPath = ifelse(remoteMode, NA, getSBserverIOfolder())
				)
				params = params[!is.na(params)]
				
				url <- paste0(getSBserverDomain(),"/rapi/liftFromPredictionResults")
				message(paste("Calling:", url))
				body = rjson::toJSON(params)
				res = httr::POST(url, body = body, httr::content_type_json())
				res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
				finalRes = if (is.null(res$error) && !is.null(res$result)) {
					plotName = res$result
					subFolder = gsub("\\s+","_", title)
					resultsLocation = paste0("predictions/", subFolder, "/")
					.showReport(paste0(resultsLocation, "CumulativeGain_counts_", plotName,".html"), projectName, revision) #cummGain_counts
					.showReport(paste0(resultsLocation, "CumulativeGain_percent_", plotName,".html"), projectName, revision) #cummGain_percent
					.showReport(paste0(resultsLocation, "Lift_plot_", plotName,".html"), projectName, revision) #lift plot
					
					url = paste0(getSBserverDomain(),"/api2/downloadFile/",projectName,"/",revision, "/reports/", resultsLocation, "lift_table_",plotName, ".tsv.gz")
					res2 = httr::GET(url)
					if (res2$status == 200) {
						localfile = paste0(outputName, ".tsv.gz")
						writeBin(httr::content(res2), localfile)
						message(paste0("Results written to: ", getwd(),"/", localfile))
						read.table(localfile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
					} else NA 
				} else {
					message = paste("Lift failed: ", res$error)
					message(message)
					stop(message)
				}
				message("Done.")
				return(finalRes)
			}
		)
)
