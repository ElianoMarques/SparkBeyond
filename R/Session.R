library(methods) #to support RScript

#' SB object that encapsulates a session
#' @field artifact_loc String location pointing to the model artifact.
#' @field modelBuilt Indication for whether only a feature search was performed or a full model was created.
#' @examples
#' # Learning example
#' session = learn("titanic", getTitanicData(train = TRUE), target="survived",algorithmsWhiteList = list("RRandomForest"))
#' #session = featureSearch("titanic", getTitanicData(train = TRUE), target="survived")
#' enriched = session$enrich(getTitanicData(train = FALSE), featureCount = 10)
#' colnames(enriched)
#' predicted = session$predict(getTitanicData(train = FALSE))
#' colnames(predicted)
#' predicted[1:5,c("survived_predicted", "X_0_probability", "X_1_probability")]
#' eval = session$evaluate()
#' #session$showFeatures()
#' #session$showConfusionMatrix()
Session = setRefClass("Session",
    fields = list(
      artifact_loc = "character",
      modelBuilt = "logical"       #TODO: replace to a function call
    ),
    methods = list(
      initialize = function(artifact_loc, modelBuilt = FALSE) {
        "initializes a session using a string provided as \\code{loc}."
        artifact_loc<<- artifact_loc
        modelBuilt <<- modelBuilt
      },

      waitForProcess = function() { #TODO: number of mintues as parameter?
        "Blocking the R console until session is finished."
        serverResponded = FALSE
        i = 0
        curStatus = ""
        hasShownInputSchema = FALSE
        hasShownFeatures = FALSE
        finalStatus = repeat {
          i = i+1
#           if (i > 10 && !serverResponded) {
#             res = "Server didn't respond for too long... terminating."
#             print(res)
#             #stop(res)
#           }
          printFile = function(filename) {
            file = paste0(artifact_loc,"/reports/",filename)
            if (file.exists(file)) {
              writeLines(readLines(file, warn = FALSE))
              TRUE
            }else FALSE
          }

          curStatus = status()
          if(curStatus == "Done") {serverResponded = TRUE
                                   printFile("evaluation.txt")
                                   print ("Done")
                                   return ("Done")}
          else if (curStatus == "Detecting types") serverReponded = TRUE
          else if (grepl("Session in progress: " , curStatus)) serverResponded = TRUE
          else if (curStatus == "Unknown error") serverReponded = return (curStatus)
          else {
            serverResponded = TRUE
            return (curStatus)
          }



          if (!hasShownInputSchema){
            hasShownInputSchema = printFile("inputSchema.txt")
          }
          if (!hasShownFeatures){
            f = tryCatch(features(), error = function(cond) NA)
            if (length(f)>1 || !is.na(f)){
              featuresCount = nrow(f)
              cntToShow = min(featuresCount, 50)
              print(paste("Printing top", cntToShow, "features out of", featuresCount))
              print(f[1:cntToShow,.(idx,feature,RIG,support)])
              hasShownFeatures = TRUE
            }
          }

          print(paste(curStatus, "-" ,i))
          secs = min(i*2, 10)
          Sys.sleep(secs)
        }
        return (finalStatus)
      },

      status = function() {
        "Checking the status of the session."

        if (!isServerAlive()) stop("Server is down. Check server for errors (e.g., out of memory).")
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
          curState = "Creating features"
          if (curStatus$fastFeatures == TRUE) curState = "Building model"
          if (curStatus$model == TRUE) curState = "Evaluating model"
          if (curStatus$errors == TRUE) curState = "errors occurred"
          curState
        }

        statusFile = paste0(artifact_loc,"/json/status.json")
        finalStatus = if (file.exists(statusFile)){ #currently assuming that application won't crush before file is created
          serverResponded = TRUE
          curStatus = jsonlite::fromJSON(paste(readLines(statusFile, warn=FALSE), collapse=""))
          if (curStatus$evaluation == TRUE) return ("Done")
          if (curStatus$alive == FALSE) return(checkIfError(FALSE))

          return(paste("Session in progress: ",currentState()))   #TODO: further parse status and refine to feature search, evaluation
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
        if (grepl("Session in progress: " , curStatus)) stop("Processing still didn't finish") #TODO: or more refined
        if (curStatus != "Done") stop(paste("Session was not completed - ", curStatus))
      },

      enrich = function(data, featureCount = NA) { #TODO: change documentation
        "Returns a data frame containing the enrichedData. \\code{data} is a dataframe to be enriched."
        statusException()

        outputPath = tempfile(pattern = "data", tmpdir = getSBserverIOfolder(), fileext = ".tsv.gz") #TODO: complement with params



        params <-list(modelPath = artifact_loc,
                      dataPath = writeToServer(data),
                      featureCount = featureCount,
                      outputPath = outputPath,
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
        return(finalRes)
      },

      predict = function(data) { #TODO: change documentation
        "Returns prediction on a created model. \\code{data} is a dataframe to be predicted."
        statusException()
        if (!modelBuilt) stop("Prediction requires full model building using learn")

        SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
        outputPath = tempfile(pattern = "data", tmpdir = SBdir, fileext = ".tsv.gz") #TODO: complement with params
        params <-list(modelPath = artifact_loc,
                      dataPath = writeToServer(data),
                      outputPath = outputPath,
                      externalPrefixPath = getSBserverIOfolder())

        print (paste("Predicting ",params$dataPath))
        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/predict")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result) && res$result == "OK"){
          read.table(outputPath, header = TRUE, sep="\t")
        } else {
          message = paste("Prediction failed: ", res$error)
          print(message)
          stop(message)
        }
        return(finalRes)
      },
      predict.file = function(file) {
        "Returns prediction on a created model. \\code{file} is a dataframe to be predicted."
        statusException()
        if (!modelBuilt) stop("Prediction requires full model building using learn")

        SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
        if (!grepl(SBdir, file)) file = paste0(getSBserverIOfolder(), file)
        if (!file.exists(file)) stop(print(paste("Predict file:", file, "does not exist")))

        outputPath = tempfile(pattern = "data", tmpdir = SBdir, fileext = ".tsv.gz") #TODO: complement with params
        params <-list(modelPath = artifact_loc,
                      dataPath = file,
                      outputPath = outputPath,
                      externalPrefixPath = getSBserverIOfolder())

        print (paste("Predicting ",params$dataPath))
        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/predict")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result) && res$result == "OK"){
          read.table(outputPath, header = TRUE, sep="\t")
        } else {
          message = paste("Prediction failed: ", res$error)
          print(message)
          stop(message)
        }
        return(finalRes)
      },

      liftFromPrediction = function(data, labelColumn, probabilityColumn, desiredClass, title = "", percentOfPopulationToPlot = 0.1) { #TODO: change documentation
        "Returns lift from a created model. \\code{data} is a dataframe to be analyzed."
        statusException()
        if (!modelBuilt) stop("Lift requires full model building using learn")

        SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
        params <-list(modelPath = artifact_loc,
                      dataPath = writeToServer(data),
                      labelColumn = labelColumn,
                      probabilityColumn = probabilityColumn,
                      desiredClass = desiredClass,
                      title=  title,
                      percentOfPopulationToPlot= percentOfPopulationToPlot,
                      externalPrefixPath = getSBserverIOfolder())

        print (paste("Lift for ",params$dataPath))
        url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/liftFromPredictionResults")
        print(paste("Calling:", url))

        body = rjson::toJSON(params)
        res = httr::POST(url, body = body, httr::content_type_json())
        res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)

        finalRes = if (is.null(res$error) && !is.null(res$result) && res$result == "OK"){
          read.table(paste0(artifact_loc, "/results/lift.tsv.gz"), header = TRUE, sep="\t")
        } else {
          message = paste("Lift failed: ", res$error)
          print(message)
          stop(message)
        }
        return(finalRes)
      },
      evaluate = function() {
        "Returns an evaluation object containing various information on the run including evaluation metric that was used, evaluation score, precision, confusion matrix, number of correct and incorrect instances, AUC information and more."
        statusException()
        if (!modelBuilt) stop("Evaluation requires full model building using learn")
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
        featuresFile = paste0(artifact_loc,"/reports/train_features.tsv")
        features = if (file.exists(featuresFile)){
          fread(featuresFile, sep="\t", header=TRUE)
        } else {stop(paste("Features file does not exist in ", featuresFile))}
        return (features)
      },

      showReport = function(report_name = NA){ #confine to a specific list
        "\\code{report_name} should be one of the following: confusionMatrix, confusionMatrix_normalized, extractor, train_features, train_unweighted_features, test_unweighted_features,field, function, InputSchema, modelComparison, roc_best, roc_CV"
        validReports = c("confusionMatrix", "confusionMatrix_normalized", "extractor", "features", "field",
                         "function", "InputSchema", "modelComparison", "roc_best", "roc_CV")
        if (is.na(report_name) || ! report_name %in% validReports ) stop("Report name is not valid")
        reportsThatRequireModel = c("confusionMatrix", "confusionMatrix_normalized", "modelComparison", "roc_best", "roc_CV")
        if (!modelBuilt && report_name %in% reportsThatRequireModel) stop("This report requires full model building using learn")
        #verify model created, check if classification, file exists
        statusException() #TODO: can be more refined here per report
        htmlSource <- paste0(artifact_loc,"/reports/", report_name, ".html")
        file.show(htmlSource)
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





