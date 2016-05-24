library(methods) #to support RScript


#' SB object that encapsulates a session
#' @field projectName the name of the project that this Session object is part of
#' @field revision an incremental number assigned to each learning iteration for this project
#' @examples
#' # Learning example
#' \donttest{
#' # Create a Session object from scratch
#' session = Session("project name", revision_id)
#' # Learn
#' session = learn("titanic", getData("titanic_train"), target="survived")
#' # Enrich
#' enriched = session$enrich(getData("titanic_train"), featureCount = 10)
#' colnames(enriched)
#' # Predict
#' predicted = session$predict(getData("titanic_test"))
#' colnames(predicted)
#' predicted[1:5,c("survived_predicted", "probability_0", "probability_1")]
#' #Evaluate
#' eval = session$evaluate()
#' #Show reports
#' session$reports()
#' session$showFeaturesTrain()
#' session$showConfusionMatrix()
#' }

Session = setRefClass("Session",
    fields = list(
      artifact_loc = "character",
      modelBuilt = "logical",       #TODO: replace to a function call
      jobId = "numeric",
      projectName = "character",
      revision = "numeric"
    ),
    methods = list(
      initialize = function(nameOfProject = NA, revisionNumber = NA, artifact_loc = NA, modelBuilt = TRUE, jobId = -1) {
        "initializes a session using a projectName and revision number."
        if (!is.na(nameOfProject) && !is.na(revisionNumber)) {
        	artifact_loc <<- paste0(nameOfProject,"/",revisionNumber)
        	projectName <<- nameOfProject
        	revision <<- as.numeric(revisionNumber)
        	jobs = showJobs(projectName = projectName) #currently not using revision for backward compatibility 
        	if ("revision" %in% colnames(jobs)) {
        		jobId <<- as.numeric(jobs[jobs$revision == revision,]$id)
        	}
        } else {
	        artifact_loc <<- artifact_loc
	        tokens = strsplit(x = artifact_loc, split = "/")[[1]]
	        projectName <<- tokens[length(tokens)-1]
	        revision <<- as.numeric(tokens[length(tokens)])
	        jobId <<- jobId
        }
        modelBuilt <<- modelBuilt
      },

      waitForProcess = function(...) { #TODO: number of mintues as parameter?
        "Blocking the R console until session is finished."
        extraParams = list(...)
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
        serverResponded = FALSE
        i = 0
        curStreamingLine = 0
        curStatus = ""
        hasShownInputSchema = FALSE
        hasShownFeatures = FALSE
        isRunning = FALSE
        lastQueuePosition = -1
        message("You are now running in a Blocking Mode")
				message("In order to see the job queue, terminate the current command and run showJobs(status='queued')")
                
        readStreamingAPI = function(prevLine = 0) {
        	#/rapi/notificationsLog/:project/:revision?skipLines=x
        	url = paste0(getSBserverDomain(),"/rapi/notificationsLog/",projectName,"/",revision, "?path=UInotification.log&skipLines=",prevLine)					
        	res = httr::GET(url)
					if (res$status == 200) {
						txt = httr::content(res, as="text")
						if (nchar(txt) > 0) {
							message(txt)
							length(strsplit(x = txt, split = "\n")[[1]]) # returning the number of lines read
						} else 0
					} else 0
        }
        
				printFile = function(filename, ...) {
					extraParams = list(...)
					remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
					if (remoteMode){
						url = paste0(getSBserverDomain(),"/rapi/notificationsLog/",projectName,"/",revision, "?path=/reports/", filename)
						res = httr::GET(url)
						if (res$status == 200) {
							writeLines(httr::content(res, as="text"))
							TRUE
						} else FALSE
					} else {
						file = paste0(artifact_loc,"/reports/",filename)
						if (file.exists(file)) {
							writeLines(readLines(file, warn = FALSE))
							TRUE
						} else FALSE
					}					
				}
				
				queuedStatus = function() {
					queuedJobs = showJobs(status = "queued")
					if (!is.null(queuedJobs) && !is.na(jobId) && length(which(queuedJobs$id == jobId)) > 0) {
						curQueuePosition = which(queuedJobs$id == jobId)
    				if (curQueuePosition != lastQueuePosition) {
							message(paste("Learning job (", jobId ,") position in the queue is", curQueuePosition))
    				}
						curQueuePosition
					}
				}
				
				runningStatus = function(curStreamingLineParam) {
					curStreamingLineAggregate = curStreamingLineParam + readStreamingAPI(curStreamingLineParam)
					
					curStatus = status(remoteMode = remoteMode)
					
					internalHasShownInputSchema = hasShownInputSchema
					if (!internalHasShownInputSchema) {
						internalHasShownInputSchema = printFile("preProcessing/inputSchema.txt", remoteMode=remoteMode)
					}
					internalHasShownFeatures = hasShownFeatures
					if (!internalHasShownFeatures) {
						f = tryCatch(features(remoteMode=remoteMode), error = function(cond) NULL)
						if (!is.null(f)) {
							featuresCount = nrow(f)
							cntToShow = min(featuresCount, 50)
							message(paste("Printing top", cntToShow, "features out of", featuresCount))
							print(f[1:cntToShow,c("idx","feature","RIG", "lin..score", "support")]) #TODO: need to understand how to show this properly as a message
							internalHasShownFeatures = TRUE
						}
					}
					
			#		if (curStatus != "NOT_FOUND" && i %% 10 == 0)	print(paste("Current status:", curStatus))
					
					list(curStatus = curStatus,
							 curStreamingLine = curStreamingLineAggregate,
							 hasShownInputSchema=internalHasShownInputSchema,
							 hasShownFeatures=internalHasShownFeatures
					) 
				}
					
				jobFinished = FALSE
				finalStatus = NA
        while(!jobFinished) {
          i = i + 1
          
          if (!is.na(jobId)) { # new discover platform version
          	newCurStatus = showJobById(jobId)$status
          	switch(newCurStatus, 
          				 queued = {
          				 		lastQueuePosition = queuedStatus()
          				 },
          				 running = {
          				 		retStatuses = runningStatus(curStreamingLine)
          				 		curStreamingLine = retStatuses$curStreamingLine
          				 		hasShownInputSchema = retStatuses$hasShownInputSchema
          				 		hasShownFeatures = retStatuses$hasShownFeatures
          				 },
          				 failed = {
          				 		finalStatus = "Failed"
          				 		writeLines(showJobById(jobId)$error)
          				 		jobFinished = TRUE
          				 },
          				 canceled = {
          				 		message ("Learning session was cancelled")
          				 		finalStatus = "Cancelled"
          				 		jobFinished = TRUE
          				 },
          				 done = {
          				 		runningStatus(curStreamingLine) # check to see if there are any last prints to do
          				 		printFile("model/evaluation.txt", remoteMode=remoteMode)
          				 		finalStatus = "Done"
          				 		jobFinished = TRUE
          				 }
          	)
          } else {  # old discovery platform version
          	runningStatus()
          }
           
          if (!jobFinished) {
          	secs = min(i*(3), 10)
          	Sys.sleep(secs)
          }
        }
				finalStatus
      },

      status = function(...) {
        "Checking the status of the session."
        extraParams = list(...)
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
        
        if (!isServerAlive()) stop(paste("Server", getSBserverHost(), "is unavailable."))
        
        if (remoteMode){""
# 	        url = paste0(getSBserverDomain(),"/api2/state/",projectName,"/",revision)					
# 	        res = httr::GET(url)
# 	        if (res$status == 200) {
# 	        	curStatus = jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
# 	        	if (!is.null(curStatus$error)) showJobById(jobId = jobId)$status
# 	        	else{
# 	        		curState = "Preprocessing"
# 	        		if (curStatus$featureSearch == TRUE) curState = "feature search"
# 	        		if (curStatus$featureSelection == TRUE) curState = "feature selection"
# 	        		if (curStatus$enrichment == TRUE) curState = "enrichment"
# 	        		if (curStatus$modelBuild == TRUE) curState = "model build"
# 	        		if (curStatus$dead == TRUE) curState = "error occurred"
# 	        		curState
# 	        	}
# 	        } else "server is unavailable"	
        } else {
	        checkIfError = function(status) {
	          errorFile = paste0(artifact_loc,"/learningFailed.txt")
	          if (file.exists(errorFile)){
	            errorLines = readLines(errorFile)
	            writeLines(errorLines)
	            paste(errorLines, collapse = '\n')
	          } else {
	            if (status == FALSE) "Unknown error"
	            else "Detecting types" #This may occur in the very begining of the run - status.json was not created and there is no error
	          }
	        }
	
	        currentState = function(statusJson) {
	          curState = "Feature search"
	          if (curStatus$fastFeatures == TRUE) curState = "Model building"
	          if (curStatus$model == TRUE) curState = "Model evaluation"
	          if (curStatus$evaluation == TRUE) curState = "Model evaluation completed"
	          if (curStatus$errors == TRUE) curState = "Errors occurred"
	          curState
	        }
	
	        statusFile = paste0(artifact_loc,"/json/status.json")
	        finalStatus = if (file.exists(statusFile)) {
	          curStatus = jsonlite::fromJSON(paste(readLines(statusFile, warn=FALSE), collapse=""))
	          if (curStatus$evaluation == TRUE) return("Model evaluation completed")
	          else if (curStatus$alive == FALSE) return(checkIfError(FALSE))
	          currentState() 
	        } else checkIfError(TRUE)
        }
      },

      statusException = function() {
        #curStatus = status()
        #if (curStatus != "Model evaluation completed") stop(paste("Session was not completed - ", curStatus))
      	TRUE #TODO: relies on an accurate job id
      },

			################################## enrich #####################
      enrich = function(data, featureCount = NA, enrichedColumnsOnly = TRUE, columnsWhiteList = NA, outputName = "enriched", fileEscaping = TRUE, ...) {
        "Returns a data frame containing the enrichedData. \\code{data} is a dataframe to be enriched. Set \\code{featureCount} in order to limit the number of returned features. Set \\code{writePredictionColumnsOnly} to TRUE to return only prediction and probabily columns rather than the entire dataset."
        extraParams = list(...)
        
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
                
        datapath = ifelse (remoteMode, 
					{
						uploadedPath = uploadToServer(data = data, projectName = projectName, name = "enrich", useEscaping = fileEscaping)
						if(is.na(uploadedPath)) stop("failed to upload file to enrich to server")
						uploadedPath        
					},
					writeToServer(data, prefix = "enrich", useEscaping = fileEscaping) #project name
        )        
 
        outputPath = ifelse(remoteMode,
        								outputName,		#potentially get a name for the enriched output
        								tempfile(pattern = "data", tmpdir = getSBserverIOfolder(), fileext = ".tsv.gz"))

        params <-list(modelPath = artifact_loc,
                      dataPath = datapath,
                      featureCount = featureCount,
                      outputName = outputPath,
                      enrichedColumnsOnly = enrichedColumnsOnly,
        							columnsWhiteList = columnsWhiteList,
        							fileEscaping = fileEscaping,
                      externalPrefixPath = ifelse (remoteMode, NA, getSBserverIOfolder())
        							#addBooleanNumericExtractors        							
        )
        params = params[!is.na(params)]

        message (paste("Enriching ",params$dataPath))
        url <- paste0(getSBserverDomain(),"/rapi/enrich")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result)) {
        	if (remoteMode) {
        		url = paste0(getSBserverDomain(),"/api2/downloadFile/",projectName,"/",revision,"/", res$result)
        		res2 = httr::GET(url)
        		if (res2$status == 200) {
        			localfile = paste0(outputName, ".tsv.gz")
        			writeBin(httr::content(res2), localfile)
        			message(paste0("Results written to: ", getwd(),"/", localfile))
        			df = read.table(localfile, sep="\t", quote = "",header=FALSE, skip = 1)
        			headers = readLines(localfile , n=1)
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
        		} else NA        		
        	} else {
	          df = read.table(outputPath, header = TRUE, quote = "",sep="\t")
	          for(i in 1:ncol(df)) {
	            if (length(levels(df[[i]])) == 1 && (levels(df[[i]]) == "false" || levels(df[[i]]) == "true")) {
	              df[,i] = as.logical(as.character(df[,i]))
	            } else if (length(levels(df[[i]])) == 2 && (levels(df[[i]]) == c("false","true"))) {
	              df[,i] = as.logical(as.character(df[,i]))
	            }
	          }
	          df
        	}
        } else {
          message = paste("Enrichment failed: ", res$error)
          message(message)
          stop(message)
        }
        message("Done.")
        return(finalRes)
      },

			################################## predict #####################
      predict = function(data, contextDatasets = NULL, predictionColumnsOnly = TRUE, columnsWhiteList = NA, outputName = "predicted", fileEscaping = TRUE, ...) { 
        "Returns prediction on a created model. \\code{data} is a dataframe to be predicted. contextDatasets - list of contextObject(s) with context information unique to the prediction (see more information in learn()). Set \\code{predictionColumnsOnly} to TRUE to return only prediction and probabily columns rather than the entire dataset."
        #if(!currentUser(FALSE)) stop("Please login")
                
        statusException()
        if (is.na(modelBuilt) || !modelBuilt) warning("Prediction requires full model building using learn")
        
        extraParams = list(...)        
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
        
        datapath = ifelse (remoteMode, 
					{
        		uploadedPath = uploadToServer(data = data, projectName = projectName, name = "predict", useEscaping = fileEscaping)
        		if(is.na(uploadedPath)) stop("failed to upload file to predict to server")
        		uploadedPath        
        	},
        	writeToServer(data, prefix = "predict", useEscaping = fileEscaping) #project name
				)
        
        if (!is.null(contextDatasets)){
	        if (!all(sapply(contextDatasets, function(x) class(x) == "contextObject"))) stop("Not all provided context objects are of type 'contextObject'")
	        for (i in 1:length(contextDatasets)) {
	        	contextName = ifelse(!is.null(contextDatasets[[i]]$name), paste0("_", contextDatasets[[i]]$name),"")
	        	contextDatasets[[i]]$data = 
	        		ifelse(!remoteMode,
	        					 writeToServer(contextDatasets[[i]]$data, 
	        					 							prefix = paste0(projectName,"_context", contextName),
	        					 							useEscaping = preProcessingCtrl$fileEscaping
	        					 ),
	        					 uploadToServer(data = contextDatasets[[i]]$data, projectName = projectName, name = paste0("context", contextName)
	        					 							 , useEscaping = preProcessingCtrl$fileEscaping)
	        		)
	        }
        }

        params <-list(modelPath = artifact_loc,
                      dataPath = datapath,
                      writePredictionColumnsOnly = predictionColumnsOnly,
        							columnsWhiteList = columnsWhiteList,
        							fileEscaping = fileEscaping,
        							predictContexts = contextDatasets,
                      externalPrefixPath = ifelse(remoteMode, NA, getSBserverIOfolder())        							
        )        							
        params = params[!is.na(params)] 							

        message (paste("Predicting ",params$dataPath))
        url <- paste0(getSBserverDomain(),"/rapi/predict")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"), simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result)) { 
        	if (remoteMode) {
        		url = paste0(getSBserverDomain(),"/api2/downloadFile/",projectName,"/",revision, "/",res$result)
        		res2 = httr::GET(url)
        		if (res2$status == 200) {
        			localfile = paste0(outputName, ".tsv.gz")
        			writeBin(httr::content(res2), localfile)
        			message(paste0("Results written to: ", getwd(),"/", localfile))
        			read.table(localfile, sep="\t", header=TRUE, stringsAsFactors = FALSE)
        		} else NA 
        	} else {
	          table = read.table(res$result, header = TRUE, sep="\t")
	          resultsLocation = paste0(artifact_loc, "/reports/predictions/test/")
	          message (paste("Predictions and plots are available at:", resultsLocation))
	          files = sapply(list.files(resultsLocation), function(f) grepl(".html", f))
	          if (length(files) > 0) {
	            htmlFilesInd = which(files)
	            if (length(htmlFilesInd>0)) {
	              htmlFiles = names(htmlFilesInd)
	              for(f in htmlFiles) file.show(paste0(resultsLocation, f))
	            }
	          }
	          table
        	}
        } else {
          message = paste("Prediction failed: ", res$error)
          message(message)
          stop(message)
        }
        message("Done.")
        return(finalRes)
      },

			################################## lift #####################			
      liftFromPrediction = function(predictionResult, overrideDesiredClass = NA, title = "test", percentOfPopulationToPlot = 0.2, outputName = "lift", fileEscaping = TRUE, ...) { #TODO: change documentation
        "Returns lift from a created model and generates three plots. \\code{predictionResult} is a dataframe to be analyzed, \\code{overrideDesiredClass} the class in the label column to check the lift for (e.g. '1'), \\code{title} optional: a title for the plot. \\code{percentOfPopulationToPlot} optional: limit the plot to the top percent of the data (x axis)."
        extraParams = list(...)        
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
        
        if (is.na(modelBuilt) || !modelBuilt) warning("Lift requires full model building using learn") #TODO: verify classification problem

        datapath = ifelse (remoteMode, {
							uploadedPath = uploadToServer(data = predictionResult, projectName = projectName, name = "lift", useEscaping = fileEscaping)
							if(is.na(uploadedPath)) stop("failed to upload file to plots lifts to server")
							uploadedPath        
						},
						writeToServer(data, prefix = "lift", useEscaping = fileEscaping) 
				)

        params <-list(modelPath = artifact_loc,
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
          showReport(paste0(resultsLocation, "CumulativeGain_counts_", plotName,".html")) #cummGain_counts
          showReport(paste0(resultsLocation, "CumulativeGain_percent_", plotName,".html")) #cummGain_percent
          showReport(paste0(resultsLocation, "Lift_plot_", plotName,".html")) #lift plot
          
          url = paste0(getSBserverDomain(),"/api2/downloadFile/",projectName,"/",revision, "/reports/", resultsLocation, "lift_table_",plotName, ".tsv.gz")
          res2 = httr::GET(url)
          if (res2$status == 200) {
          	localfile = paste0(outputName, ".tsv.gz")
          	writeBin(httr::content(res2), localfile)
          	message(paste0("Results written to: ", getwd(),"/", localfile))
          	read.table(localfile, sep="\t", header=TRUE, stringsAsFactors = FALSE)
          } else NA 
        } else {
          message = paste("Lift failed: ", res$error)
          message(message)
          stop(message)
        }
        message("Done.")
        return(finalRes)
      },

			####################################### createPackage
      createPackage = function(sampleData = NULL, createRestAPIpackage = FALSE, fileEscaping = TRUE, ...) { #
        "Create a sharable package for the model. \\code{sampleData} can be used to a sample data to the package and test it. Only first 20 rows of the sample data will be used. \\code{createRestAPIpackage} is a boolean indicator for whether to create a package for prediction via command line (set to FALSE) or via programmatic REST API call(TRUE)."
        if (is.na(modelBuilt) || !modelBuilt) warning("createPackage requires full model building using learn")

        sampleDataFilename = if (is.null(sampleData)) NA else {		        	
          writeToServer(if ("data.frame" %in% class(data)) sampleData[1:20,] else sampleData, prefix="createPackage_sample", useEscaping = fileEscaping) #projectName
        }
        extraParams = list(...)
        
        params <-list(modelPath = artifact_loc,
                      createRestAPIpackage = createRestAPIpackage,
                      dataPath = sampleDataFilename,
                      debugMode = if(!is.null(extraParams$debugMode)) extraParams$debugMode else FALSE,
        							fileEscaping = fileEscaping,
                      externalPrefixPath = getSBserverIOfolder())

        params = params[!is.na(params)]

        url <- paste0(getSBserverDomain(),"/rapi/createPackage")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
       # print(body)
        res = httr::POST(url, body = body, httr::content_type_json())
        
        finalRes = if (res$status == 200) {
        	print("Saving zipped file")
        	fileName = paste0(getwd(), "/cli_runner.zip")
        	writeBin(httr::content(res), fileName)
        	message ("Package created successfully")
        	message(paste0("Package is saved to: ", fileName))
        	print(paste0("Saved zipped file to ", fileName))
        	TRUE
        } else {
        	errorFile = paste0(artifact_loc,"/package-errors.txt")
        	if (file.exists(errorFile)) {
        		errorLines = readLines(errorFile)
        		writeLines(errorLines)
        	}
        	FALSE
        }

        return(finalRes)
      },

      buildNumber = function() {
        "Returns the build number in which the model was generated."
        filename = paste0(artifact_loc,"/jenkinsBuild.txt")
        if (file.exists(filename)) {
          lines = paste(readLines(filename, warn=FALSE), collapse="")
          lines
        } else NA
      },

      evaluate = function(...) {
        "Returns an evaluation object containing various information on the run including evaluation metric that was used, evaluation score, precision, confusion matrix, number of correct and incorrect instances, AUC information and more."
        #statusException()
        #if (is.na(modelBuilt) || !modelBuilt) warning("Evaluation requires full model building using learn")
        extraParams = list(...)        
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
        
       	finalEvaluation = if (remoteMode) {
        		url = paste0(getSBserverDomain(),"/rapi/notificationsLog/",projectName,"/",revision, "?path=/json/evaluation.json")
        		res = httr::GET(url)
        		if (res$status == 200) {
        			jsonlite::fromJSON(txt=httr::content(res, as="text"))
        		} else NULL        	
        } else {
	        evaluationFile = paste0(artifact_loc,"/json/evaluation.json")
	        evaluation = if (file.exists(evaluationFile)) {
	          lines = paste(readLines(evaluationFile, warn=FALSE), collapse="")
	          jsonlite::fromJSON(gsub("NaN", 0.0, lines), flatten=TRUE)
	        } else {stop(paste("Evaluation file does not exist in ", evaluationFile))}
        }
        
        if (finalEvaluation$isRegression) {
        	writeLines(finalEvaluation$evaluation$summary)
        } else {
        	writeLines(finalEvaluation$evaluation$classDetails)
        }
        
        finalEvaluation
      },

      features = function(...) {
        "Returns a dataset with top feature information"
        extraParams = list(...)
        remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
        
        if (remoteMode) {
        	url = paste0(getSBserverDomain(),"/rapi/notificationsLog/",projectName,"/",revision, "?path=/reports/features/train_features.tsv")
        	res = httr::GET(url)
        	txt = httr::content(res, as="text")
        	if (res$status == 200) suppressWarnings(read.table(textConnection(txt),sep="\t", header=TRUE, stringsAsFactors = FALSE, quote = ""))
        	else NULL
        } else {
	        featuresFile = paste0(artifact_loc,"/reports/features/train_features.tsv")
	        features = if (file.exists(featuresFile)){
	          suppressWarnings(read.table(featuresFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, quote = ""))
	        } else NULL	        
        }        
      },

      showReport = function(report_name = NA){ #confine to a specific list
        "\\code{report_name} name of report to show"        
        #htmlSource = paste0(artifact_loc,"/reports/", subFolder(report_name), "/", report_name, ".html")
      	suppressWarnings({
	      	url <- paste0(getSBserverDomain(),paste0("/getToken")) 
	      	res = httr::GET(url)
	      	token = httr::content(res, as="text")
	        htmlSource = paste0(getSBserverDomain(), "/analytics/report/", projectName, "/", revision, "/", report_name,"?token=", token)
	      	browseURL(htmlSource)
      	})
      },

      showExtractors = function() {
        "Shows extractors."
        showReport("extractor")
      },
			showContextObjects = function() {
				"Shows context objects report."
				showReport("features/contextObjects.html")
			},
      showFeaturesTrain = function() {
        "Shows features performance on train."
        showReport("features/train_features.html")
      },
      showFeaturesTest = function() {
        "Shows features performance on test."
        showReport("features/test_unweighted_features.html")
      },
			showFeatureStability = function() {
				"Shows features stability report."
				showReport("features/featureStability.html")
			},
      showFields = function() {
        "Shows fields."
        showReport("features/field.html")
      },
      showFunctions = function() {
        "Shows functions."
        showReport("features/function.html")
      },
      showInputSchema = function() {
        "Shows the input schema."
        showReport("preProcessing/InputSchema.html")
      },

      #require model methods
      showConfusionMatrix = function(normalized = FALSE) { #verify that this was a classification problem
        "Shows a confusion matrix of a model."
        showReport(if (normalized) "model/confusionMatrix_normalized.html" else "model/confusionMatrix.html")
      },
      showModelComparison = function() {
        "Shows cross validation of various algorithms tested to create a model."
        showReport("model/modelComparison.html")
      },
#       showROC = function(){
#         "Shows ROC of the model."
#         showReport("roc_best") #problematic to show in internal browser non local resources
#       },
#       showROC_CV = function(){
#         "Shows ROC of cross validation of various algorithms tested to create a model."
#         showReport("roc_CV")
#       },
      showFeatureClusters = function() {
        "Shows the representative feature clusters pdf report."
				showReport("/features/featureClusters/allFeatures.pdf")
      },
			reports = function() {	
				"Shows all reports applicable for the current analysis."
				url = paste0(getSBserverDomain(),"/analytics/file/", projectName,"/",revision,"/reportsStructure.json")
				res = httr::GET(url)
				if (res$status == 200 && res$url == url) {
					reportList = unlist(jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE))
					names(reportList) = NULL
					reportListWithIndex = cbind(paste0("[",1:length(reportList),"]"), reportList)
					reportListWithIndex = apply(reportListWithIndex,1,paste, collapse=" ")
					writeLines(paste(unlist(reportListWithIndex), collapse="\n"))
					n <- readline("enter a number of report to show or <enter> otherwise:")
					n <- ifelse(grepl("\\D",n),-1,as.integer(n))
					if(!is.na(n) && n >= 1 && n <= length(reportList)) {
						message(paste("showing",n))
						showReport(reportList[n])
					}										
					reportList
				}else NULL
			},
			webView = function (show=TRUE){
				"Show a dynamic web view of the analysis."
				suppressWarnings({
					url <- paste0(getSBserverDomain(),paste0("/getToken")) 
					res = httr::GET(url)
					token = httr::content(res, as="text")
					htmlSource = paste0(getSBserverDomain(), "/?token=", token,"#/visualPipeline/", projectName, "?revision=", revision, "&forceByRevision=true")
					if (show == T) browseURL(htmlSource)
					htmlSource
				})
			},
			revisions = function (){
				"Show previous revisions of a project."
				projectRevisions(projectName = projectName)
			},
			cancel = function(){
				"Cancel a queued / running job of this model"
				cancelJob(jobId = jobId)
			}

# save and load are probably more confusing at this time hence commented out. Just use regular save and load.
#       ,
#       save = function(filename) {
#         'Save the current object to \\code{filename} in R external object format.'
#         SBmodelSerializeVar = .self
#         base::save(SBmodelSerializeVar, file = filename)
#       },
#
#       load = function(filename) {
#         'Loads a saved SparkBeyond model saved using \\code{$save}. Paramter: \\code{filename} where the model was saved.'
#         base::load(filename)
#         loadedModel = SBmodelSerializeVar
#         rm(SBmodelSerializeVar)
#         return(loadedModel)
#       }

      # add columnSubSetSize
      # add customColumnSubset
      # sessionBlackList

      # add dashboard json
      # lift, regression plots
      # feature clusters report
      # featurePlots
      # add S3 methods of print, predict
  )
);





