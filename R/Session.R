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
      jobId = "character",
      projectName = "character",
      revision = "numeric"
    ),
    methods = list(
      initialize = function(nameOfProject = NA, revisionNumber = NA, artifact_loc = NA, modelBuilt = TRUE, jobId = NA_character_) {
        "initializes a session using a projectName and revision number."
        if (!is.na(nameOfProject) && !is.na(revisionNumber)) {
        	artifact_loc <<- paste0(nameOfProject,"/",revisionNumber)
        	projectName <<- nameOfProject
        	revision <<- as.numeric(revisionNumber)
        	jobs = showJobs(projectName = projectName) #currently not using revision for backward compatibility 
        	if ("revision" %in% colnames(jobs)) {
        		jobId <<- as.character(jobs[jobs$revision == revision,]$id)
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
        	#/api/notificationsLog/:project/:revision?skipLines=x
        	url = paste0(getSBserverDomain(),"/api/notificationsLog/",projectName,"/",revision, "?path=UInotification.log&skipLines=",prevLine)
        	txt = .executeRequest(
        		function() httr::GET(url),
        		errorHandling = .withErrorHandling(onError = .onErrorBehavior$SILENT),
        		responseSerializer = .responseSerializers$TEXT
        	)
        	
        	if (!is.null(txt) && nchar(txt) > 0) {
        		message(txt)
        		length(strsplit(x = txt, split = "\n")[[1]]) # returning the number of lines read
        	} else {
        	    0
        	}
        }
        
        printFile = function(filename, onError = .onErrorBehavior$WARNING) {
            reportPath = paste0("/reports/", filename)
            reportText = .getNotificationLogReport(projectName, revision, path = reportPath, responseSerializer = .responseSerializers$TEXT, onError = onError)
            if (!is.null(reportText)) {
                writeLines(reportText)
                TRUE
            } else {
                FALSE
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
                internalHasShownInputSchema = printFile("preProcessing/inputSchema.txt", onError = .onErrorBehavior$SILENT)
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
          				 		printFile("model/evaluation.txt")
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
      },

      statusException = function() {
        #curStatus = status()
        #if (curStatus != "Model evaluation completed") stop(paste("Session was not completed - ", curStatus))
      	TRUE #TODO: relies on an accurate job id
      },

			################################## enrich #####################
      enrich = function(data, featureCount = NA,  contextDatasets = NULL , includeOriginals = FALSE, columnsWhiteList = NA, outputName = "enriched", fileEscaping = TRUE, runBlocking = TRUE, ...) {
        "Returns a data frame containing the enrichedData. \\code{data} is a dataframe to be enriched. Set \\code{featureCount} in order to limit the number of returned features. Set \\code{includeOriginals} to TRUE in order to include the original columns in the enriched dataset.\
				\\code{columnsWhiteList} - specify the original columns to be included in the result , set \\code{outputName} to change the default result file name"
      	
        extraParams = list(...)
        uncompressedUpload = ifelse(!is.null(extraParams$uncompressedUpload), extraParams$uncompressedUpload, FALSE)
        fileUploadThreshold = ifelse(uncompressedUpload, NA, 0)
        async = ifelse(!is.null(extraParams$async), extraParams$async, FALSE)
        enrichedColumnsOnly = ifelse(!is.null(extraParams$enrichedColumnsOnly), extraParams$enrichedColumnsOnly, TRUE) #Removed in 1.10
        includeOriginals = includeOriginals || !enrichedColumnsOnly #Added in 1.10
        
                
        datapath = {
					uploadedPath = uploadToServer(data = data, projectName = projectName, name = "enrich", useEscaping = fileEscaping, directUploadThreshold = fileUploadThreshold)
					if(is.na(uploadedPath)) stop("failed to upload file to enrich to server")
					uploadedPath        
				}
       
        contextDatasets = .handleContexts(contextDatasets, projectName = projectName,uncompressedUpload = uncompressedUpload)
        
        params <-list(projectName = projectName,
        							revision = revision,
        							modelPath = artifact_loc,  #not needed since 1.10
        							dataPath = datapath,
        							featureCount = featureCount,
        							outputName = outputName,
        							enrichedColumnsOnly = !includeOriginals,
        							includeOriginals = includeOriginals,
        							columnsWhiteList = columnsWhiteList,
        							fileEscaping = fileEscaping,
        							contextDatasets = contextDatasets,
        							async = async
        							#addBooleanNumericExtractors        							
        )
        params = params[!is.na(params)]

        message (paste("Enriching ",params$dataPath))
        url <- paste0(getSBserverDomain(),"/api/enrich")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = .executeRequest(
        	function() httr::POST(url, body = body, httr::content_type_json()),
        	errorHandling = .withErrorHandling(message = "Enrichment failed"),
        	responseSerializer = .responseSerializers$JSON
        )
        
        if(.isServerVersionOlderThan("1.10")) {
	        enrichResult = .enrichResultFromJson(res)
	
	        if(enrichResult$asyncMode) {
	        	# Async mode
	        	executionId = enrichResult$executionId
	        	total = nrow(data)
	
	        	enrichment = EnrichmentJob$new(executionId, totalRows = total)
	        	quoted = function(str) paste0('"', str, '"')
	        	message(paste("Enrichment job ID is:", executionId))
	        	message(paste0("You can get back to following this enrichment job by running: ",
	        								 "EnrichmentJob$new(", quoted(executionId), ")"))
	
	        	if(runBlocking) {
	        		unusedRefToData = enrichment$data(localFileName = outputName)
	        	}
	
	
	        	message("Done.")
	        	enrichment
	        } else {
	        	#	Sync mode for server version < 1.8, doesn't support async mode
	        	if(is.null(res$result)) {
	        		stop("Enrichment failed.")
	        	}
	        	localFile = .downloadFile(projectName, revision, pathOnServer = res$result, saveToPath = paste0(outputName, ".tsv.gz"))
	        	data = if (!is.null(localFile)) {
	        		.loadEnrichedDataFrame(localFile)
	        	} else {
	        		stop("Enrichment failed: failed to download results")
	        	}
	
	        	message("Done.")
	        	return(data)
	        }
        } else {
        	jobState = .jobStateFromJson(res)
        	enrichJobId = jobState$jobId
        	total = nrow(data)
        	enrichmentJob = EnrichmentJob$new(enrichJobId, totalRows = total)
        	quoted = function(str) paste0('"', str, '"')
        	message(paste("Enrich job ID is:", enrichJobId))
        	message(paste0("You can get back to following this enrichment job by running: ", 
        								 "EnrichmentJob$new(jobId=", quoted(enrichJobId), ")"))
        	
        	if(runBlocking) {
        		unusedRefToData = enrichmentJob$data(localFileName = outputName)
        	}
        	
        	enrichmentJob        	
        }
      },

			################################## predict #####################
      predict = function(data, contextDatasets = NULL, includeOriginals = FALSE, includeEnriched = FALSE, columnsWhiteList = NA, outputName = "predicted", fileEscaping = TRUE, runBlocking = TRUE, ...) {
        "Returns prediction on a created model. \\code{data} is a dataframe to be predicted. contextDatasets - list of contextObject(s) with context information unique to the prediction (see more information in learn()). Set \\code{includeOriginals} to TRUE to include the original columns in the result. Set \\code{includeEnriched} to TRUE to include the enriched columns in the result.\
				\\code{columnsWhiteList} - specify the original columns to be included in the result (\\code{includeOriginals} is required to be TRUE), set \\code{outputName} to change the default result file name"

        if (is.na(modelBuilt) || !modelBuilt) warning("Prediction requires full model building using learn")
        
        extraParams = list(...)
        uncompressedUpload = ifelse(!is.null(extraParams$uncompressedUpload), extraParams$uncompressedUpload, FALSE)
        fileUploadThreshold = ifelse(uncompressedUpload, NA, 0)
        async = ifelse(!is.null(extraParams$async), extraParams$async, FALSE)
        predictionColumnsOnly = ifelse(!is.null(extraParams$predictionColumnsOnly), extraParams$predictionColumnsOnly, TRUE)
        includeOriginals = includeOriginals || !predictionColumnsOnly
        includeEnriched = includeEnriched
        predictionColumnsOnly = !(includeEnriched || includeOriginals)
        
        if(!.isServerVersionOlderThan("1.10") && (!is.na(columnsWhiteList) && length(columnsWhiteList)>0) && includeOriginals == FALSE) {
        	warning("includeOriginals has to be set to TRUE in order to enable columnsWhiteList")
        }
        
        datapath = {
      		uploadedPath = uploadToServer(data = data, projectName = projectName, name = "predict", useEscaping = fileEscaping, directUploadThreshold = fileUploadThreshold)
      		if(is.na(uploadedPath)) stop("failed to upload file to predict to server")
      		uploadedPath
      	}
      
        contextDatasets = .handleContexts(contextDatasets, projectName = projectName,uncompressedUpload = uncompressedUpload)

        params <-list(projectName = projectName,
        							revision = revision,
        							modelPath = artifact_loc,
        							dataPath = datapath,
        							includeOriginals = includeOriginals, # From 1.10
        							includeEnriched = includeEnriched, # From 1.10
        							writePredictionColumnsOnly = predictionColumnsOnly, # Redundant since 1.10
        							columnsWhiteList = columnsWhiteList,
        							fileEscaping = fileEscaping,
        							contextDatasets = contextDatasets,
        							# Starting from version 1.10 not in use
        							# Starting from version 1.8 this will activate async mode, older versions will ignore it
        							# If async is TRUE and server version is >=1.8, successful response to this request will contain executionId
        							# If the response will not contain executionId, old predict flow will be executed
        							async = async
        							)        							
        params = params[!is.na(params)] 							

        message (paste("Predicting ",params$dataPath))
        url <- paste0(getSBserverDomain(),"/api/predict")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = .executeRequest(
        	function() httr::POST(url, body = body, httr::content_type_json()),
        	errorHandling = .withErrorHandling(message = "Prediction failed"),
        	responseSerializer = .responseSerializers$JSON
        )
        
        if(.isServerVersionOlderThan("1.10")) {
        	predictResult = .predictResultFromJson(res)
        	
        	if(predictResult$asyncMode) {
        		# Async mode
        		executionId = predictResult$executionId
        		total = nrow(data)
        		
        		prediction = PredictionJob$new(executionId, totalRows = total)
        		quoted = function(str) paste0('"', str, '"')
        		message(paste("Predict job ID is:", executionId))
        		message(paste0("You can get back to following this prediction job by running: ", 
        									 "PredictionJob$new(", quoted(executionId), ")"))
        		
        		if(runBlocking) {
        			unusedRefToData = prediction$data(localFileName = outputName)
        		}
        		
        		prediction
        	} else {
        		#	Sync mode for server version < 1.8, doesn't support async mode
        		if(is.null(res$result)) {
        			stop("Prediction failed.")
        		}
        		data	= .downloadDataFrame(projectName, revision, pathOnServer = res$result, saveToPath = paste0(outputName, ".tsv.gz"))
        		if(!is.null(data)) {
        			message("Done.")
        		}
        		
        		return(data)
        	}
        } else {
        	jobState = .jobStateFromJson(res)
        	predictJobId = jobState$jobId
        	total = nrow(data)
        	predictionJob = PredictionJob$new(predictJobId, totalRows = total)
        	quoted = function(str) paste0('"', str, '"')
        	message(paste("Predict job ID is:", predictJobId))
        	message(paste0("You can get back to following this prediction job by running: ", 
        								 "PredictionJob$new(jobId=", quoted(predictJobId), ")"))
        	
        	if(runBlocking) {
        		unusedRefToData = predictionJob$data(localFileName = outputName)
        	}
        	
        	predictionJob
        }
      },

			################################## lift #####################			
      liftFromPrediction = function(predictionResult, overrideDesiredClass = NA, title = "test", percentOfPopulationToPlot = 0.2, outputName = "lift", fileEscaping = TRUE, ...) { #TODO: change documentation
        "Returns lift from a created model and generates three plots. \\code{predictionResult} is a dataframe to be analyzed, \\code{overrideDesiredClass} the class in the label column to check the lift for (e.g. '1'), \\code{title} optional: a title for the plot. \\code{percentOfPopulationToPlot} optional: limit the plot to the top percent of the data (x axis)."
      	if(!.isServerVersionOlderThan("1.10")) {
      		stop('liftPlot is deprecated starting from server version 1.10. Use "PredictionJob" functions "downloadReports", "browseReports" and "evaluate" instead')
      	}
      	
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

        url <- paste0(getSBserverDomain(),"/api/liftFromPredictionResults")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = .executeRequest(
        	function() httr::POST(url, body = body, httr::content_type_json()),
        	responseSerializer = .responseSerializers$JSON
        )

        finalRes = if (!is.null(res$result)) {
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
          	read.table(localfile, sep="\t", header=TRUE, stringsAsFactors = FALSE, comment.char = "")
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
        	uploadToServer(if ("data.frame" %in% class(data)) sampleData[1:20,] else sampleData, projectName = projectName, name ="createPackage_sample", useEscaping = fileEscaping)
        }
        extraParams = list(...)
        
        params <-list(modelPath = artifact_loc,
                      createRestAPIpackage = createRestAPIpackage,
                      dataPath = sampleDataFilename,
                      debugMode = if(!is.null(extraParams$debugMode)) extraParams$debugMode else FALSE,
        							fileEscaping = fileEscaping,
                      externalPrefixPath = getSBserverIOfolder())

        params = params[!is.na(params)]

        url <- paste0(getSBserverDomain(),"/api/createPackage")
        message(paste("Calling:", url))

        body = rjson::toJSON(params)
       # print(body)
        res = httr::POST(url, body = body, httr::content_type_json())
        
        finalRes = if (res$status == 200) {
            # Extract file name from 'content-disposition' header
        	contentDispositionHeader = httr::headers(res)$'content-disposition'
        	fileName = strsplit(contentDispositionHeader, "filename=")[[1]][2]

        	fullFileName = paste0(getwd(), "/", fileName)
        	writeBin(httr::content(res), fullFileName)
        	print(paste0("Package was saved at: ", fullFileName))
        	TRUE
        } else if (res$status == 501) {
        	res <- jsonlite::fromJSON(txt=httr::content(res, as="text"))
        	if(!is.null(res$error)) {
        		warning(res$error)
        	}
        	FALSE
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

      evaluate = function(...) {
        "Returns an evaluation object containing various information on the run including evaluation metric that was used, evaluation score, precision, confusion matrix, number of correct and incorrect instances, AUC information and more."
       	finalEvaluation = .getNotificationLogReport(projectName, revision, path = "/json/evaluation.json")
        
        if (finalEvaluation$isRegression) {
        	writeLines(finalEvaluation$evaluation$summary)
        } else {
        	writeLines(finalEvaluation$evaluation$classDetails)
        }
        
        finalEvaluation
      },

      features = function(...) {
        "Returns a dataset with top feature information"
      	.getNotificationLogReport(projectName, revision, path = "/reports/features/train_features.tsv", responseSerializer = .responseSerializers$DATA_FRAME, onError = .onErrorBehavior$SILENT)
      },

      showReport = function(report_name = NA){ #confine to a specific list
        "\\code{report_name} name of report to show"        
        .showReport(report_name, projectName, revision)
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
				res = .executeRequest(
					function() httr::GET(url),
					responseSerializer = .responseSerializers$JSON
				)
				
				reportList = unlist(res)
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
			},
			exportModel = function(includeContexts = TRUE) {
				"Export model for prediction box. If prediction requires to supply ALL new contexts and no original contexts are needed, download of the contexts can be skipped be setting \\code{includeContexts} to FALSE"
				featureExtractorUrl = paste0(getSBserverDomain(), "/analytics/rawFile/", projectName,"/",revision,"/extcode.dat")
				path = .download(url = featureExtractorUrl, localFile = "extcode.dat", description = "feature extractor")
				message(paste("Successfully exported feature extractor to:", path))
				
				modelUrl = paste0(getSBserverDomain(), "/analytics/rawFile/", projectName,"/",revision,"/model.ser")
				path = .download(url = modelUrl, localFile = "model.ser", description = "model")
				message(paste("Successfully exported model to:", path))
				
				if(includeContexts) {
					contextsUrl = paste0(getSBserverDomain(), "/api2/downloadContexts/", projectName,"/",revision)
					path = .download(url = contextsUrl, description = "contexts")
					message(paste("Successfully exported contexts to:", path))
				}
			},
			exportModelToPredictionBox = function(predictionBoxUrl, authKey = "", uploadContexts = TRUE,
																						overwriteExisting = TRUE, overrideTargetGroupName = NULL) {
				"Export model directly to prediction box. predictionBoxUrl - prediction box URL, authKey - prediction box authentication key, overwriteExisting - override existing model, overrideTargetGroupName - save the model under a different name in porediction box (by default project name is used as model identifier)"
				.exportModelToPredictionBox(project = projectName, revision = revision, 
																		predictionBoxUrl = predictionBoxUrl, authKey = authKey, 
																		uploadContexts = uploadContexts,
																		overwriteExisting = overwriteExisting, overrideTargetGroupName = overrideTargetGroupName)
			},
			webView = function (show=TRUE){
				"Show a dynamic web view of the analysis."
				suppressWarnings({
					url <- paste0(getSBserverDomain(),paste0("/getToken"))
					token = .executeRequest(
						function() httr::GET(url),
						responseSerializer = .responseSerializers$TEXT
					)
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





