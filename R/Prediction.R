library(R6)

#' @title
#' PredictionJob
#' @description 
#' \section{SB object that encapsulates a prediction job}
#' 
#' \section{\bold{Methods}}{
#' 
#' \subsection{\code{currentStatus()}}{
#' \emph{Print the current job status to console}
#' }
#' 
#' \subsection{\code{data(localFileName, runBlocking)}}{
#' \emph{Retreive the job result as a DataFrame. By default, if the data is not currently available, the function will block until the result is ready, and download it}
#' \itemize{
#'   \item \code{localFileName} - chose the name of the file the result will be stored in 
#'   \item \code{runBlocking} - if TRUE - wait for the result to be available and downloaded, and then return the result. If FALSE - return the data immediately if available, return NULL otherwise. Defaults to TRUE.  
#' }
#' }
#' 
#' \subsection{\code{cancel()}}{
#' \emph{Cancel the job}
#' }
#' 
#' \subsection{\code{downloadReports(percentOfPopulationToPlot, outputName)}}{
#' \emph{Download all the prediction reports as an archived file.}
#' You can get several reports for prediction results: lift plots, evaluation, etc.
#' Most of the reports require the target column to be present in prediction input
#' \itemize{
#'   \item \code{percentOfPopulationToPlot} - percentage of population for lift plots. Defaults to 0.2.
#'   \item \code{outputName} - chose the name of the file the result will be stored in.  
#' }
#' }
#' 
#' \subsection{\code{browseReports(percentOfPopulationToPlot)}}{
#' \emph{Browse the list of the available reports.}
#' You can chose to view a specific report from the list of the available reports.
#' \itemize{
#'   \item \code{percentOfPopulationToPlot} - percentage of population for lift plots. Defaults to 0.2.
#' }
#' }
#' 
#' \subsection{\code{evaluate()}}{
#' \emph{Get prediction evaluation. Also available via downloadReports and browseReports.}
#' }
#' }
#' 
#' @usage
#' # Prediction example
#' \donttest{
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' 
#' # predict function returns a PredictionJob object
#' predictionJob = session$predict(getData("titanic_test"))
#' 
#' # currentStatus() function can be used to show job's status
#' predictionJob$currentStatus()
#' 
#' # use data() function to retreive the job result as a DataFrame
#' data = predictionJob$data()
#' head(data)
#' 
#' # You can use downloadReports() to download a zip file containing all reports
#' predictionJob$downloadReports()
#' 
#' # You can also browse the list of the available reports using browseReports() function and chose to download a specific report 
#' predictionJob$browseReports()
#' 
#' # Prediction evaluation is available both as one of the reports and via dedicated function, evaluate()
#' predictionJob$evaluate()
#' }
#' 
#' # A job can be canceled using the cancel() function
#' \donttest{
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' predictionJob = session$predict(getData("titanic_test"))
#' predictionJob$cancel()
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
						cat("\r", "Prediction job is running. So far processed", status$processedRows, totalRowsMessage, "rows", "\r")
					}
				} else if(state$isCompleted) {
					result = .jobResultFromJson(state$result)
					status = .enrichPredictStatusFromJson(state$status)
					if(result$isSucceeded) {
						totalRowsMessage = ifelse(is.na(private$totalRows), "", paste0(" out of ", private$totalRows))
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
			
			data = function(localFileName = NA_character_, runBlocking=TRUE, ...) {
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
									predictedResult = read.table(predictedFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
									extraParams = list(...)
									originalInput = extraParams$originalInput
									if(.overrideOriginalColumnsInPredict() && !is.null(originalInput)) { # Workaround for 1.10-1.10.1
										res_col_names = names(predictedResult)
										input_column_names = names(originalInput)
										original_column_names_in_result = res_col_names[res_col_names %in% input_column_names]
										original_columns = originalInput[input_column_names %in% original_column_names_in_result]
										result_without_original_columns = predictedResult[!(res_col_names %in% original_column_names_in_result)]
										private$dataFrame = cbind(original_columns, result_without_original_columns)
									} else {
										private$dataFrame = predictedResult
									}
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
				if(.isServerVersionOlderThan("1.10.2")) {
					stop("downloadReports is available starting from server version 1.10.2. Currently used version: ", SBServerVersion)
				}
				
				completedReportsGenerationJobId = private$executeReportsGeneration(percentOfPopulationToPlot)
				downloadUrl = paste0(getSBserverDomain(),"/api/downloadJobResult/", completedReportsGenerationJobId)
				.download(downloadUrl, localFile = paste0(outputName, ".zip"), description = paste("prediction reports for job:", completedReportsGenerationJobId))
			},
			
			browseReports = function(percentOfPopulationToPlot = 0.2) {
				"Show interactive list of the available reports - enter a report number to browse to the report. \\code{percentOfPopulationToPlot} percentage of population for lift plots."
				if(.isServerVersionOlderThan("1.10.2")) {
					stop("browseReports is available starting from server version 1.10.2. Currently used version: ", SBServerVersion)
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
				if(.isServerVersionOlderThan("1.10.2")) {
					stop("downloadReports is available starting from server version 1.10.2. Currently used version: ", SBServerVersion)
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
				
				res = private$job$cancel()
			}
		)
)
