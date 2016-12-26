library(R6)

#' SB object that encapsulates a prediction job
#' 
#' @field jobId prediction job id
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
			jobId = NA_character_,
			totalRows = NA_integer_,
			job = NULL,
			dataFrame = NULL,
			reportJobsCache = list(),
			
			printCurrentState = function(state) {
				if(state$isEnqueued) {
					message("Prediction job is waiting in queue")
				} else if(state$isRunning) {
					status = .enrichPredictStatusFromJson(state$status)
					if(!status$builtContext) {
						message(paste("Prediction job is running. Parsing contexts..."))
					} else {
						totalRowsMessage = ifelse(is.na(private$totalRows), "", paste0("out of ", private$totalRows))
						cat("\r", "Prediction job is running. Processed", status$processedRows, totalRowsMessage, "rows", "\r")
					}
				} else if(state$isCompleted) {
					result = .jobResultFromJson(state$result)
					status = .enrichPredictStatusFromJson(state$status)
					if(result$isSucceeded) {
						totalRowsMessage = ifelse(is.na(private$totalRows), "", paste0("out of ", private$totalRows))
						message(paste0("Prediction has finished successfully. Processed ", status$processedRows, totalRowsMessage, " rows",
													 ". Started: ", as.POSIXct(state$startedMillis, origin="1970-01-01"),
													 ". Ended: ", as.POSIXct(state$endedMillis, origin="1970-01-01")))
					} else {
						stop(paste0("Prediction failed: ", result$error, ". Worker ID: ", state$workerId, 
												". Started: ", as.POSIXct(state$startedMillis, origin="1970-01-01"),
												". Ended: ", as.POSIXct(state$endedMillis, origin="1970-01-01")))
					}
				} else {
					stop(paste("Unexpected job state:", state))
				}
			},
			
			executeReportsGeneration = function(percentOfPopulationToPlot = 0.2) {
				printCurrentEvaluationState = function(state) {
					if(state$isEnqueued) {
						message("Prediction reports job is waiting in queue")
					} else if(state$isRunning) {
						message("Prediction reports job is running...")
					} else if(state$isCompleted) {
						result = .jobResultFromJson(state$result)
						if(result$isSucceeded) {
							message("Prediction reports were generated successfully.")
						} else {
							stop(paste("Prediction reports generation failed:", result$error))
						}
					} else {
						stop(paste("Unexpected job state:", state))
					}
				}
				
				cachedJobId = private$reportJobsCache[[as.character(percentOfPopulationToPlot)]]
				
				cachedJobId = if(!is.null(cachedJobId)) {
					job = .initJob(cachedJobId)
					state = job$currentState()
					if(!is.null(state) && state$isCompleted) {
						cachedJobId
					} else {
						private$reportJobsCache[[as.character(percentOfPopulationToPlot)]] <- NULL
						NULL
					}
				}
				
				if(!is.null(cachedJobId)) {
					cachedJobId
				} else {
					jobState = .generatePredictionReports(private$jobId, percentOfPopulationToPlot = percentOfPopulationToPlot)
					evaluationJob = .initJob(jobState$jobId)
					reportsPath = NULL
					
					successfulEvaluationJobId = NULL
					
					evaluationJob$poll(function(state) {
						printCurrentEvaluationState(state)
						
						if(state$isCompleted) {
							result = .jobResultFromJson(state$result)
							if(result$isSucceeded && !is.null(result$success$createdFilenames)) {
								private$reportJobsCache[[as.character(percentOfPopulationToPlot)]] = evaluationJob$jobId
								successfulEvaluationJobId <<- evaluationJob$jobId
							} else {
								stop(paste("Prediction reports generation failed with an error:", result$failure))
							}
						
							FALSE
						} else {
							TRUE
						}
					})
				
					successfulEvaluationJobId
				}
			}
		),
		public = list(
			initialize = function(jobId, totalRows = NA_integer_) {
				"Initializes a PredictionJob with jobId."
				private$jobId <- jobId
				private$totalRows <- totalRows
				private$job = .initJob(jobId)
			},
			
			print = function(...) {
				cat("Prediction job id: ", private$jobId, sep = "")
				invisible(self)
			},
			
			currentStatus = function() {
				"Get prediction job status - Started/Finished, number of lines processed, etc."
				if(.isServerVersionOlderThan("1.10")) {
					jobStatus = .getPredictJobStatus(private$jobId)
					if(jobStatus$state == "Finished" && !is.null(jobStatus$error)) {
						message(paste("Prediction has finished with an error:", jobStatus$error))
					} else if(jobStatus$state == "Finished" && !is.null(jobStatus$result)) {
						message("Prediction has finished successfully. Call PredictionJob$data() to get the results")
					} else if((jobStatus$state == "Started" || jobStatus$state == "Running") && !is.na(private$totalRows) && jobStatus$rowCount>=private$totalRows) {
						message("Generating reports")
					} else if(jobStatus$state == "Started" || jobStatus$state == "Running") {
						message(paste("Prediction job is running. So far processed", jobStatus$rowCount, "rows"))
					} else {
						warning("Unexpected jobStatus: ", jobStatus)
					}
				} else {
					jobState = private$job$currentState()
					private$printCurrentState(jobState)
				}
			},
			
			data = function(localFileName = NA_character_, runBlocking=TRUE) {
				"Set \\code{localFileName} to change the default result file name. If \\code{runBlocking} is TRUE, block until the job finishes while showing processed rows counter, and return the result when available. If \\code{runBlocking} is FALSE return the data if available, else return NULL"
				outputName = ifelse(is.na(localFileName), paste("predicted", private$jobId, sep = "-"), localFileName)
				if(!is.null(private$dataFrame)) { # todo: check status and download
					private$dataFrame
				} else if(runBlocking) {
					if(.isServerVersionOlderThan("1.10")) {
						jobStatus = .getPredictJobStatus(private$jobId)
						state = jobStatus$state
						if(state!="Finished") {
							message("Blocking the R console until prediction is finished.")
						}
						
						reachedTotalRows = FALSE
						displayedGeneratingReportsMessage = FALSE
						while(state!="Finished" && !displayedGeneratingReportsMessage) { # if reports are generated then the data is ready to download
							Sys.sleep(5)
							jobStatus = .getPredictJobStatus(private$jobId)
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
						jobState = private$job$currentState()
						if(!jobState$isCompleted) {
							message("Blocking the R console until prediction is finished.")
						}
						
						private$job$pollState(function(state) {
							private$printCurrentState(state)
							
							if(state$isCompleted) {
								result = .jobResultFromJson(state$result)
								if(result$isSucceeded) {
									downloadUrl = paste0(getSBserverDomain(),"/api/downloadJobResult/", private$jobId)
									predictedFile = .download(downloadUrl, localFile = paste0(outputName, ".tsv.gz"), description = paste("prediction result for job:", private$jobId))
									private$dataFrame = read.table(predictedFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
								} else {
									stop(paste("Prediction failed with an error:", result$failure))
								}
								
								FALSE
							} else {
								TRUE
							}
						})
						private$dataFrame
					}
				} else {
					message("Data isn't available locally. Use blockWaitingForData=TRUE to get the data when prediction is ready")
					NULL
				}
			},
			
			downloadReports = function(percentOfPopulationToPlot = 0.2, outputName = "prediction_reports") {
				"Download all reports as a zip archive. \\code{percentOfPopulationToPlot} percentage of population for lift plots. Set \\code{outputName} to change the default result file name"
				if(.isServerVersionOlderThan("1.10")) {
					stop("downloadReports is available starting from server version 1.10. Currently used version: ", SBServerVersion)
				}
				
				completedReportsGenerationJobId = private$executeReportsGeneration(percentOfPopulationToPlot)
				downloadUrl = paste0(getSBserverDomain(),"/api/downloadJobResult/", completedReportsGenerationJobId)
				.download(downloadUrl, localFile = paste0(outputName, ".zip"), description = paste("prediction reports for job:", completedReportsGenerationJobId))
			},
			
			browseReports = function(percentOfPopulationToPlot = 0.2) {
				"Show interactive list of the available reports - enter a report number to browse to the report. \\code{percentOfPopulationToPlot} percentage of population for lift plots."
				if(.isServerVersionOlderThan("1.10")) {
					stop("browseReports is available starting from server version 1.10. Currently used version: ", SBServerVersion)
				}
				
				completedReportsGenerationJobId = private$executeReportsGeneration(percentOfPopulationToPlot)
				evaluationJob = .initJob(completedReportsGenerationJobId)
				state = evaluationJob$currentState()
				result = .jobResultFromJson(state$result)
				reportList = result$success$createdFilenames
				names(reportList) = NULL
				reportListWithIndex = cbind(paste0("[",1:length(reportList),"]"), reportList)
				reportListWithIndex = apply(reportListWithIndex,1,paste, collapse=" ")
				writeLines(paste(unlist(reportListWithIndex), collapse="\n"))
				n <- readline("Enter a number of report to show or <enter> otherwise:")
				n <- ifelse(grepl("\\D",n),-1,as.integer(n))
				if(!is.na(n) && n >= 1 && n <= length(reportList)) {
					message(paste("Showing",n))
					.showPredictResultReport(evaluationJob$jobId, reportList[n])
				}										
				reportList
			},
			
			evaluate = function() {
				"Returns an evaluation object containing various information on the run including evaluation metric that was used, evaluation score, precision, confusion matrix, number of correct and incorrect instances, AUC information and more."
				if(.isServerVersionOlderThan("1.10")) {
					stop("downloadReports is available starting from server version 1.10. Currently used version: ", SBServerVersion)
				}
				
				completedReportsGenerationJobId = private$executeReportsGeneration()
				evaluation = .getPredictEvaluation(completedReportsGenerationJobId)
				if (evaluation$isRegression) {
					writeLines(evaluation$evaluation$summary)
				} else {
					writeLines(evaluation$evaluation$classDetails)
				}
				evaluation
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
