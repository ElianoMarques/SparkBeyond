library(methods) #to support RScript


#' SB object that encapsulates a session
#' @field artifact_loc String location pointing to the model artifact.
#' @field modelBuilt Indication for whether only a feature search was performed or a full model was created.
#' @examples
#' # Learning example
#' \donttest{
#' session = learn("titanic", getData("titanic_train"), target="survived",algorithmsWhiteList = list("xgboostClassifier"),  scoreOnTestSet = TRUE, useCachedFeatures=TRUE)
#' #session = featureSearch("titanic", getData("titanic_train"), target="survived")
#' enriched = session$enrich(getData("titanic_train"), featureCount = 10)
#' colnames(enriched)
#' predicted = session$predict(getData("titanic_test"))
#' colnames(predicted)
#' predicted[1:5,c("survived_predicted", "probability_0", "probability_1")]
#' eval = session$evaluate()
#' #session$showFeatures()
#' #session$showConfusionMatrix()
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
      initialize = function(artifact_loc, modelBuilt = FALSE, jobId = -1) {
        "initializes a session using a string provided as \\code{loc}."
        artifact_loc <<- artifact_loc
        modelBuilt <<- modelBuilt
        jobId <<- jobId
        tokens = strsplit(x = artifact_loc, split = "/")[[1]]
        projectName <<- tokens[length(tokens)-1]
        revision <<- as.numeric(tokens[length(tokens)])
      },

      waitForProcess = function() { #TODO: number of mintues as parameter?
        "Blocking the R console until session is finished."
        serverResponded = FALSE
        i = 0
        curStreamingLine = 0
        curStatus = ""
        hasShownInputSchema = FALSE
        hasShownFeatures = FALSE
        isRunning = FALSE
        lastQueuePosition = -1
        print("You are now running in a Blocking Mode")
				print("In order to see the job queue, terminate the current command and run showJobs(status='queued')")
                
        readStreamingAPI = function(prevLine = 0){
        	#TODO: keep project name in Session
        	#/rapi/notificationsLog/:project/:revision?skipLines=x
        	url = paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/notificationsLog/",projectName,"/",revision, "?skipLines=",prevLine)					
        	res = httr::GET(url)
					if (res$status == 200) {
						txt = httr::content(res, as="text")
						writeLines(txt)
						length(strsplit(x = txt, split = "\n")[[1]]) # returning the number of lines read
					} else 0
        }
        
				printFile = function(filename) {
					file = paste0(artifact_loc,"/reports/",filename)
					if (file.exists(file)) {
						writeLines(readLines(file, warn = FALSE))
						TRUE
					}else FALSE
				}
				
				queuedStatus = function() {
					queuedJobs = showJobs(status = "queued")
					if (!is.null(queuedJobs) && !is.na(jobId) && length(which(queuedJobs$id == jobId)) > 0){
						curQueuePosition = which(queuedJobs$id == jobId)
    				if (curQueuePosition != lastQueuePosition){
							print(paste("Learning job (", jobId ,") position in the queue is", curQueuePosition))
    				}
						curQueuePosition
					}
				}
				
				runningStatus = function() {
					curStreamingLine = readStreamingAPI(curStreamingLine)
					
					curStatus = status()
					
					internalHasShownInputSchema = hasShownInputSchema
					if (!internalHasShownInputSchema){
						internalHasShownInputSchema = printFile("preProcessing/inputSchema.txt")
					}
					internalHasShownFeatures = hasShownFeatures
					if (!internalHasShownFeatures){
						f = tryCatch(features(), error = function(cond) NA)
						if (length(f)>1 || !is.na(f)){
							featuresCount = nrow(f)
							cntToShow = min(featuresCount, 50)
							print(paste("Printing top", cntToShow, "features out of", featuresCount))
							print(f[1:cntToShow,c("idx","feature","RIG", "lin..score", "support")])
							internalHasShownFeatures = TRUE
						}
					}
					
					print(paste(curStatus, "-" ,i))
					list(curStatus = curStatus, 
							 hasShownInputSchema=internalHasShownInputSchema,
							 hasShownFeatures=internalHasShownFeatures
					) 
				}
					
				jobFinished = FALSE
				finalStatus = NA
        while(!jobFinished) {
          i = i+1
          
          if (!is.na(jobId)){ # new discover platform version
          	newCurStatus = showJobById(jobId)$status
          	switch(newCurStatus, 
          				 queued = {
          				 		lastQueuePosition = queuedStatus()
          				 },
          				 running = {
          				 		retStatuses = runningStatus()
          				 		hasShownInputSchema = retStatuses$hasShownInputSchema
          				 		hasShownFeatures = retStatuses$hasShownFeatures
          				 },
          				 failed = {
          				 		finalStatus = "Failed"
          				 		writeLines(showJobById(jobId)$error)
          				 		jobFinished = TRUE
          				 },
          				 canceled = {
          				 		print ("Learning session was cancelled")
          				 		finalStatus = "Cancelled"
          				 		jobFinished = TRUE
          				 },
          				 done = {
          				 		runningStatus() # check to see if there are any last prints to do
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

      status = function() {
        "Checking the status of the session."

        if (!isServerAlive()) stop(paste("Server", getSBserverHost(), "is unavailable."))
        checkIfError = function(status) {
          errorFile = paste0(artifact_loc,"/learningFailed.txt")
          if (file.exists(errorFile)){
            errorLines = readLines(errorFile)
            writeLines(errorLines)
            paste(errorLines, collapse = '\n')
          }else {
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
        finalStatus = if (file.exists(statusFile)){
          curStatus = jsonlite::fromJSON(paste(readLines(statusFile, warn=FALSE), collapse=""))
          if (curStatus$evaluation == TRUE) return("Model evaluation completed")
          else if (curStatus$alive == FALSE) return(checkIfError(FALSE))
          return(paste(currentState())) 
        } else return(checkIfError(TRUE))
      },

#       hasModelBuilt = function() {
#         statusFile = paste0(artifact_loc,"/json/status.json")
#         status = if (file.exists(statusFile)){ #currently assuming that application won't crush before file is created
#           statusContent = jsonlite::fromJSON(paste(readLines(statusFile, warn=FALSE), collapse=""))
#           statusContent$model
#         } else FALSE
#         return (status)
#       },

      statusException = function() {
        curStatus = status()
        if (curStatus != "Model evaluation completed") stop(paste("Session was not completed - ", curStatus))
      },

      enrich = function(data, featureCount = NA, writePredictionColumnsOnly = TRUE) {
        "Returns a data frame containing the enrichedData. \\code{data} is a dataframe to be enriched. Set \\code{featureCount} in order to limit the number of returned features. Set \\code{writePredictionColumnsOnly} to TRUE to return only prediction and probabily columns rather than the entire dataset."
        statusException()
        datapath = writeToServer(data, prefix = "enrich") # projectName?
        enrich.file(datapath, featureCount, writePredictionColumnsOnly)
      },

      enrich.file = function(file, featureCount = NA, writePredictionColumnsOnly = TRUE) {
        "Returns a data frame containing the enrichedData. \\code{file} is a path to the file to be enriched. Set \\code{featureCount} in order to limit the number of returned features. Set \\code{writePredictionColumnsOnly} to TRUE to return only prediction and probabily columns rather than the entire dataset."
        statusException()
        isLatestVersion()
        outputPath = tempfile(pattern = "data", tmpdir = getSBserverIOfolder(), fileext = ".tsv.gz") #TODO: complement with params

        params <-list(modelPath = artifact_loc,
                      dataPath = file,
                      featureCount = featureCount,
                      outputPath = outputPath,
                      writePredictionColumnsOnly = writePredictionColumnsOnly,
                      externalPrefixPath = getSBserverIOfolder())

        params = params[!is.na(params)]

        print (paste("Enriching ",params$dataPath))
        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/enrich")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result) && res$result == "OK"){
          df = read.table(outputPath, header = TRUE, sep="\t")
          for(i in 1:ncol(df)){
            if (length(levels(df[[i]])) == 1 && (levels(df[[i]]) == "false" || levels(df[[i]]) == "true")) {
              df[,i] = as.logical(as.character(df[,i]))
            } else if (length(levels(df[[i]])) == 2 && (levels(df[[i]]) == c("false","true"))) {
              df[,i] = as.logical(as.character(df[,i]))
            }
          }
          df
        } else {
          message = paste("Enrichment failed: ", res$error)
          print(message)
          stop(message)
        }
        print("Done.")
        return(finalRes)
      },

      predict = function(data, writePredictionColumnsOnly = TRUE) { #
        "Returns prediction on a created model. \\code{data} is a dataframe to be predicted. Set \\code{writePredictionColumnsOnly} to TRUE to return only prediction and probabily columns rather than the entire dataset."
        statusException()
        if (is.na(modelBuilt) || !modelBuilt) warning("Prediction requires full model building using learn")
        datapath = writeToServer(data, prefix = "predict") #project name
        predict.file(datapath, writePredictionColumnsOnly)
      },

      predict.file = function(file, writePredictionColumnsOnly = TRUE) {
        "Returns prediction on a created model. \\code{file} is the path of the file to be predicted. Set \\code{writePredictionColumnsOnly} to TRUE to return only prediction and probabily columns rather than the entire dataset."
        statusException()
        isLatestVersion()
        if (is.na(modelBuilt) || !modelBuilt) warning("Prediction requires full model building using learn")

        SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
        if (!grepl(SBdir, file)) file = paste0(getSBserverIOfolder(), file)
        if (!file.exists(file)) stop(print(paste("Predict file:", file, "does not exist")))

        params <-list(modelPath = artifact_loc,
                      dataPath = file,
                      writePredictionColumnsOnly = writePredictionColumnsOnly,
                      externalPrefixPath = getSBserverIOfolder())

        print (paste("Predicting ",params$dataPath))
        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/predict")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result)){
          table = read.table(res$result, header = TRUE, sep="\t")
          resultsLocation = paste0(artifact_loc, "/reports/predictions/test/")
          print (paste("Predictions and plots are available at:", resultsLocation))
          files = sapply(list.files(resultsLocation), function(f) grepl(".html", f))
          if (length(files) > 0){
            htmlFilesInd = which(files)
            if (length(htmlFilesInd>0)) {
              htmlFiles = names(htmlFilesInd)
              for(f in htmlFiles) file.show(paste0(resultsLocation, f))
            }
          }
          table
        } else {
          message = paste("Prediction failed: ", res$error)
          print(message)
          stop(message)
        }
        print("Done.")
        return(finalRes)
      },

      liftFromPrediction = function(predictionResult, overrideDesiredClass = NA, title = NA, percentOfPopulationToPlot = 0.2) { #TODO: change documentation
        "Returns lift from a created model and generates three plots. \\code{predictionResult} is a dataframe to be analyzed, \\code{overrideDesiredClass} the class in the label column to check the lift for (e.g. '1'), \\code{title} optional: a title for the plot. \\code{percentOfPopulationToPlot} optional: limit the plot to the top percent of the data (x axis)."
        statusException()
        isLatestVersion()
        if (is.na(modelBuilt) || !modelBuilt) warning("Lift requires full model building using learn")

        SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
        params <-list(modelPath = artifact_loc,
                      predictionPath = writeToServer(predictionResult, prefix = "lift"), #projectName? 
                      title = title,
                      percentOfPopulationToPlot= percentOfPopulationToPlot,
                      externalPrefixPath = getSBserverIOfolder())

        params = params[!is.na(params)]

        print (paste("Calculating lift for ",params$predictionPath))

        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/liftFromPredictionResults")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result)){
          plotName = res$result
          subFolder = if (is.na(title)) "test" else gsub("\\s+","_", title)
          resultsLocation = paste0(artifact_loc, "/reports/predictions/", subFolder, "/")
          print (paste("Plots are available at:", resultsLocation))
          table = read.table(paste0(resultsLocation, "lift_table_",plotName, ".tsv.gz"), header = TRUE, sep="\t")
          files = sapply(list.files(resultsLocation), function(f) grepl(".html", f))
          if (length(files) > 0){
            htmlFilesInd = which(files)
            if (length(htmlFilesInd>0)) {
              htmlFiles = names(htmlFilesInd)
              for(f in htmlFiles) file.show(paste0(resultsLocation, f))
            }
          }
          table
        } else {
          message = paste("Lift failed: ", res$error)
          print(message)
          stop(message)
        }
        print("Done.")
        return(finalRes)
      },

      createPackage = function(sampleData = NULL, createRestAPIpackage = FALSE, ...) { #
        "Create a sharable package for the model. \\code{sampleData} can be used to a sample data to the package and test it. Only first 20 rows of the sample data will be used. \\code{createRestAPIpackage} is a boolean indicator for whether to create a package for prediction via command line (set to FALSE) or via programmatic REST API call(TRUE)."
        if (is.na(modelBuilt) || !modelBuilt) warning("createPackage requires full model building using learn")

        sampleDataFilename = if (is.null(sampleData)) NA else {		        	
          writeToServer(if ("data.frame" %in% class(data)) sampleData[1:20,] else sampleData, prefix="createPackage_sample") #projectName
        }
        extraParams = list(...)
        
        params <-list(modelPath = artifact_loc,
                      createRestAPIpackage = createRestAPIpackage,
                      dataPath = sampleDataFilename,
                      debugMode = if(!is.null(extraParams$debugMode)) extraParams$debugMode else FALSE,
                      externalPrefixPath = getSBserverIOfolder())

        params = params[!is.na(params)]

        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/createPackage")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
       # print(body)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
        finalRes = if (is.null(res$error) && !is.null(res$result) && res$result == "OK"){
          print ("Package created successfully")
          TRUE
        } else{
          errorFile = paste0(artifact_loc,"/package-errors.txt")
          if (file.exists(errorFile)){
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
        if (file.exists(filename)){
          lines = paste(readLines(filename, warn=FALSE), collapse="")
          lines
        } else NA
      },

      evaluate = function() {
        "Returns an evaluation object containing various information on the run including evaluation metric that was used, evaluation score, precision, confusion matrix, number of correct and incorrect instances, AUC information and more."
        statusException()
        if (is.na(modelBuilt) || !modelBuilt) warning("Evaluation requires full model building using learn")
        evaluationFile = paste0(artifact_loc,"/json/evaluation.json")
        evaluation = if (file.exists(evaluationFile)){
          lines = paste(readLines(evaluationFile, warn=FALSE), collapse="")
          eval = jsonlite::fromJSON(gsub("NaN", 0.0, lines),flatten = TRUE)
          writeLines(eval$evaluation$classDetails)
          eval
        } else {stop(paste("Evaluation file does not exist in ", evaluationFile))}
        return (evaluation)
      },

      features = function() {
        "Returns a dataset with top feature information"
        #statusException() # TODO: check if features were generated already
        featuresFile = paste0(artifact_loc,"/reports/features/train_features.tsv")
        features = if (file.exists(featuresFile)){
          suppressWarnings(read.table(featuresFile, sep="\t", header=TRUE, stringsAsFactors = FALSE, quote = ""))
        } else {stop(paste("Features file does not exist in ", featuresFile))}
        return (features)
      },

      showReport = function(report_name = NA){ #confine to a specific list
        "\\code{report_name} should be one of the following: confusionMatrix, confusionMatrix_normalized, extractor, train_features, train_unweighted_features, test_unweighted_features,field, function, InputSchema, modelComparison, roc_best, roc_CV"
        preProcessingReports = c("InputSchema")
        featuresReports = c("extractor", "train_features", "train_unweighted_features", "test_unweighted_features","field", "function")
        modelReports = c("confusionMatrix", "confusionMatrix_normalized", "modelComparison", "roc_best", "roc_CV")
        validReports = c(preProcessingReports,featuresReports, modelReports)
        if (is.na(report_name) || ! report_name %in% validReports ) stop("Report name is not valid")
        if ((is.na(modelBuilt) || !modelBuilt) && report_name %in% modelReports) warning("This report requires full model building using learn")
        #verify model created, check if classification, file exists
        statusException() #TODO: can be more refined here per report
        subFolder = function(name) {
          if (name %in% preProcessingReports) "preProcessing"
          else if (name %in% modelReports) "model"
          else "features"
        }
        #http://localhost:9000/analytics/report/timeWindowNew2/6/preProcessing/inputSchema.html
        if (!report_name %in% c("roc_best", "roc_CV")){
          #htmlSource = paste0(artifact_loc,"/reports/", subFolder(report_name), "/", report_name, ".html")
          htmlSource = paste0(getSBserverHost(), ":", getSBserverPort(), "/analytics/report/", projectName, "/", revision, "/", subFolder(report_name), "/", report_name, ".html")
        	browseURL(htmlSource)
        }
        else { #TODO: change to serving mode
          modelLocation = paste0(artifact_loc,"/reports/model/")
          relFiles = names(which(sapply(list.files(modelLocation), function(f) grepl(report_name, f))))
          for(f in relFiles) file.show(paste0(modelLocation, f))
        }

      },

      showExtractors = function(){
        "Shows extractors."
        showReport("extractor")
      },
      showFeaturesTrain = function(){
        "Shows features performance on train."
        showReport("train_features")
      },
      showFeaturesTest = function(){
        "Shows features performance on test."
        showReport("test_unweighted_features")
      },
      showFields = function(){
        "Shows fields."
        showReport("field")
      },
      showFunctions = function(){
        "Shows functions."
        showReport("function")
      },
      showInputSchema = function(){
        "Shows the input schema."
        showReport("InputSchema")
      },

      #require model methods
      showConfusionMatrix = function(normalized = FALSE){ #verify that this was a classification problem
        "Shows a confusion matrix of a model."
        showReport(if (normalized) "confusionMatrix_normalized" else "confusionMatrix")
      },
      showModelComparison = function(){
        "Shows cross validation of various algorithms tested to create a model."
        showReport("modelComparison")
      },
      showROC = function(){
        "Shows ROC of the model."
        showReport("roc_best") #problematic to show in internal browser non local resources
      },
      showROC_CV = function(){
        "Shows ROC of cross validation of various algorithms tested to create a model."
        showReport("roc_CV")
      },
      showFeatureClusters = function(){
        "Shows the representative feature clusters pdf report."
        pdfReport <- paste0(artifact_loc,"/reports/featureClusters/representativeFeatureIds.pdf")
        if (file.exists(pdfReport)) viewer(pdfReport)
        else {stop(paste("Feature clusters file does not exist in ", pdfReport, " (did you set the produceFeatureClusteringReport parameter in your learn call?)"))}
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





