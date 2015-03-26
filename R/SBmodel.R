library(methods) #to support RScript

#' SB object that encapsulates a model result
#'
#' @field artifactLoc String location pointing to the model artifact.
#' @examples
#' #model learn
#' model = SBlearn("titanic", getTitanicFilename(train = TRUE), "survived",algorithmsWhiteList = list("RRandomForest"))
#' #model = SBfeatureSearchOnly("titanic", getTitanicFilename(train = TRUE), "survived")
#' enriched = model$enrich(getTitanicFilename(train = FALSE), paste(getwd(),"titanic_test_enriched.tsv.gz",sep="/"), featureCount = 10)
#' colnames(enriched)
#' predicted = model$predict(getTitanicFilename(train = FALSE), paste(getwd(),"titanic_test_predicted.tsv.gz",sep="/"))
#' colnames(predicted)
#' predicted[1:5,c("survived_predicted", "X_0_probability", "X_1_probability")]
#' eval = model$evaluate()
#' #model$showFeatures()
#' #model$showConfusionMatrix()
#'
SBmodel = setRefClass("SBmodel",
  fields = list(
    artifact_loc = "character",
    server_port = "numeric",
    modelBuilt = "logical"
  ),
  methods = list(
    initialize = function(artifact_loc, server_port = 9000) {
      "initializes a model using a string provided as \\code{loc}."
      artifact_loc<<- artifact_loc
      server_port <<- server_port
      modelBuilt <<- FALSE
    },

    predict = function(dataPath, outputPath) {
      "Returns prediction on a created model. \\code{dataPath} is the path to the file to be tested. \\code{outputPath} is the path to write the results of the prediction."
      if (!modelBuilt) stop("Prediction requires full model building using SBlearn")
      url <- paste("http://127.0.0.1:",server_port,"/rapi/predict", sep="")
      params <-list(modelPath = artifact_loc,
                    dataPath = dataPath,
                    outputPath = outputPath)

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

    enrich = function(dataPath, outputPath, featureCount = NA) {
      "Returns a data frame containing the enrichedData. \\code{dataPath} is the path to the file to be tested. \\code{outputPath} is the path to write the results of the prediction. featureCount Integer value signaling how many enriched features would be returned. NA by default - marking maximum number possible (based on the number of features requested in modeling)."
      url <- paste("http://127.0.0.1:",server_port,"/rapi/enrich", sep="")
      params <-list(modelPath = artifact_loc,
                    dataPath = dataPath,
                    featureCount = featureCount,
                    outputPath = outputPath)

      params = params[!is.na(params)]

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

    evaluate = function() {
      "Returns an evaluation object containing various information on the run including evaluation metric that was used, evaluation score, precision, confusion matrix, number of correct and incorrect instances, AUC information and more."
      if (!modelBuilt) stop("Evaluation requires full model building using SBlearn")
      evaluationFile = paste(artifact_loc,"/json/evaluation.json", sep="")
      evaluation = if (file.exists(evaluationFile)){
        lines = paste(readLines(evaluationFile, warn=FALSE), collapse="")
        eval = jsonlite::fromJSON(gsub("NaN", 0.0, lines),flatten = TRUE)
        writeLines(eval$evaluation$classDetails)
        eval
      } else {stop(paste("Evaluation file does not exist in ", artifact_loc))}
      return (evaluation)
    },

    showReport = function(report_name = NA, showInIDE = TRUE){ #confine to a specific list
      "\\code{report_name} should be one of the following: confusionMatrix, confusionMatrix_normalized, extractor, features, field, function, InputSchema, modelComparison, roc_best, roc_CV"
      validReports = c("confusionMatrix", "confusionMatrix_normalized", "extractor", "features", "field",
                       "function", "InputSchema", "modelComparison", "roc_best", "roc_CV")
      if (is.na(report_name) || ! report_name %in% validReports ) stop("Report name is not valid")
      reportsThatRequireModel = c("confusionMatrix", "confusionMatrix_normalized", "modelComparison", "roc_best", "roc_CV")
      if (!modelBuilt && report_name %in% reportsThatRequireModel) stop("This report requires full model building using SBlearn")
      if (showInIDE && (report_name == "roc_best" || report_name == "roc_CV")) print ("Graphs cannot be shown in the IDE and will be displayed in the external browser")
      #verify model created, check if classification, file exists
      htmlSource <- paste(artifact_loc,"/reports/", report_name, ".html",sep="")
      viewer <- getOption("viewer")
      if (!is.null(viewer) && showInIDE){
        x <- paste(readLines(htmlSource, warn = F), collapse = '\n')
        temp.f <- tempfile("plot", fileext=c(".html"))
        writeLines(x, con = temp.f)
        viewer(temp.f)
      }
      else
        utils::browseURL(htmlSource)
    },

    showExtractors = function(showInIDE = TRUE){
      "Shows extractors used by a model in the IDE viewer on in the web browser."
      showReport("extractor",showInIDE)
    },
    showFeatures = function(showInIDE = TRUE){
      "Shows features used by a model in the IDE viewer on in the web browser."
      showReport("features",showInIDE)
    },
    showFields = function(showInIDE = TRUE){
      "Shows fields used by a model in the IDE viewer on in the web browser."
      showReport("field",showInIDE)
    },
    showFunctions = function(showInIDE = TRUE){
      "Shows functions used by a model in the IDE viewer on in the web browser."
      showReport("function",showInIDE)
    },
    showInputSchema = function(showInIDE = TRUE){
      "Shows the input schema a model in the IDE viewer on in the web browser."
      showReport("InputSchema",showInIDE)
    },

    #require model methods
    showConfusionMatrix = function(normalized = FALSE, showInIDE = TRUE){ #verify that this was a classification problem
      "Shows a confusion matrix of a model in the IDE viewer on in the web browser."
      showReport(if (normalized) "confusionMatrix_normalized" else "confusionMatrix",showInIDE)
    },
    showModelComparison = function(showInIDE = TRUE){
      "Shows cross validation of various algorithms tested to create a model in the IDE viewer on in the web browser."
      showReport("modelComparison",showInIDE)
    },
    showROC = function(){
      "Shows ROC of the model in the IDE viewer on in the web browser."
      showReport("roc_best",FALSE) #problematic to show in internal browser non local resources
    },
    showROC_CV = function(){
      "Shows ROC of cross validation of various algorithms tested to create a model in the IDE viewer on in the web browser."
      showReport("roc_CV",FALSE) #problematic to show in internal browser non local resources
    },
    save = function(file) {
      'Save the current object on the file in R external object format.'
      SBmodelSerializeVar = .self
      base::save(SBmodelSerializeVar, file = file)
    }

    # add stub function
    # non blocking
    # add groupBy column, time column
    # serialize / deserialize
    # add features json
    # lift, regression plots
    # feature clusters report
    # featurePlot
    # add S3 methods of print, predict

  )
);

#To enable tab completion on SBmodel classes
.DollarNames.SBmodel <- function(x, pattern){
   grep(pattern, getRefClass(class(x))$methods(), value=TRUE)
}



# S3 functions (De facto constructors of SBmodel)


#' Run SparkBeyond feature enrichment and learning process.
#' @param sessionName String of the session name.
#' @param trainingFilePath String of the path to the file to be trained on.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param testFilePath: Optional. String of the path to an independent test file to test the prediction on. NA by default.
#' @param trainTestSplitRatio: Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param weightColumn: Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth: Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. NA by default
#' @param hints: Optional. A list of strings that reprents a set of hints that will be used to guide the feature search. NA by default.
#' @param useGraph: Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount: Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". "PREC" by default.
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @param server_port Optional. the port to be accessed in the SparkBeyond API server. 9000 by default.
#' @return SBmodel object the encapsulate the prediction.
#' @examples
#' model = SBlearn("titanic", getTitanicFilename(train = TRUE), "survived", algorithmsWhiteList = list("RRandomForest"))
SBlearn <- function(sessionName, trainingFilePath, target,
                    testFilePath = NA,
                    trainTestSplitRatio = 0.8,
                    weightColumn = NA,
                    maxDepth = 2,
                    algorithmsWhiteList = NA, #list available algorithms
                    hints = NA,
                    useGraph = FALSE,
                    maxFeaturesCount = 300, #make it a list
                    evaluationMetric = "PREC", #add all options
                    scoreOnTestSet = FALSE,
                    crossValidation = 5,
                    runBlocking = TRUE,
                    server_port = 9000){

  url <- paste("http://127.0.0.1:",server_port,"/rapi/learn", sep="")
  params <-list("sessionName" = sessionName,
                "trainingFilePath" = trainingFilePath,
                target = target,
                testFilePath = testFilePath,
                trainTestSplitRatio = trainTestSplitRatio,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                algorithmsWhiteList = algorithmsWhiteList,
                hints = hints,
                useGraph = useGraph,
                globalFeatureIterations = maxFeaturesCount,
                evaluationMetric = evaluationMetric,
                scoreOnTestSet = scoreOnTestSet,
                crossValidation = crossValidation
  )

  params = params[!is.na(params)]

  body = rjson::toJSON(params)
  res = httr::POST(url, body = body, httr::content_type_json())
  res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
  if (!is.null(res$error)) {
    res = paste("Train error: ", res$error, " - terminating.")
    print(res)
    stop(res)
  }

  print(paste("Artifact location was created at:", res$artifactPath))

  serverResponded = FALSE
  i = 0
  repeat {
    print(paste("In modeling process... please notice that this process may take some time... ",i))
    flush.console()
    i = i+1
    if (i >= 3 && !serverResponded) {
      res = "Server didn't respond for 30 seconds... check that server is up... terminating."
      print(res)
      flush.console()
      stop(res)
    }
    Sys.sleep(10)
    statusFile = paste(res$artifactPath,"/json/status.json", sep="")
    if (file.exists(statusFile)){ #currently assuming that application won't crush before file is created
      serverResponded = TRUE
      status = jsonlite::fromJSON(paste(readLines(statusFile, warn=FALSE), collapse=""))
      if (i %% 6 == 0 ) {
        print(i)
        print(status)
        flush.console()
      }
      if (status$alive == FALSE || status$evaluation == TRUE) {break}
    }
  }
  model = SBmodel(res$artifactPath, as.numeric(server_port))
  model$modelBuilt = TRUE
  return(model)
}



#' Run SparkBeyond feature enrichment and learning process.
#' @param sessionName String of the session name.
#' @param trainingFilePath String of the path to the file to be trained on.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param hints Optional. A list of strings that reprents a set of hints that will be used to guide the feature search. NA by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param server_port Optional. the port to be accessed in the SparkBeyond API server. 9000 by default.
#' @return SBmodel object that encapsulate the feature search result.
#' @examples
#' #model = SBfeatureSearchOnly("titanic", titanic_train_filename, "survived")
SBfeatureSearchOnly <- function(sessionName, trainingFilePath, target,
                    weightColumn = NA,
                    maxDepth = 2,
                    hints = NA,
                    useGraph = FALSE,
                    maxFeaturesCount = 300, #make it a list
                    runBlocking = TRUE,
                    server_port = 9000){

  params <-list("sessionName" = sessionName,
                "trainingFilePath" = trainingFilePath,
                target = target,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                algorithmsWhiteList = list("ZeroR"),
                hints = hints,
                useGraph = useGraph,
                maxFeaturesCount = maxFeaturesCount,
                runBlocking = runBlocking,
                server_port = server_port)
  model = do.call(SBlearn,c(params))
  model$modelBuilt = FALSE
  model
}

#' load saved SparkBeyond model saved using $save
#' @param file filename where the model was saved
#' @return SBmodel object
#' @examples
#' #tf = paste(tempfile(), ".Rdata", sep="")
#' #model$save(tf)
#' #SBloadModel(tf)
SBloadModel = function(file) {
  base::load(file)
  loadedModel = SBmodelSerializeVar
  rm(SBmodelSerializeVar)
  return(loadedModel)
}
