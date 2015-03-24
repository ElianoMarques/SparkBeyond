#' SB object that encapsulates a model result
#'
#' @field artifactLoc String location pointing to the model artifact.
SBmodel = setRefClass("SBmodel",
  fields = list(
    artifact_loc = "character",
    server_port = "numeric"
  ),
  methods = list(
    initialize = function(artifact_loc, server_port = 9000) {
      "initializes a model using a string provided as \\code{loc}."
      artifact_loc<<- artifact_loc
      server_port <<- server_port
    },

    predict = function(dataPath, outputPath) {
      "Returns prediction on a created model. \\code{dataPath} is the path to the file to be tested. \\code{outputPath} is the path to write the results of the prediction."
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
      if (is.na(report_name) || ! reportName %in% validReports ) stop("Report name is not valid")
      if (showInIDE && (reportName == "roc_best" || reportName == "roc_CV")) print ("Graphs cannot be shown in the IDE and will be displayed in the external browser")
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

    showConfusionMatrixReport = function(normalized = FALSE, showInIDE = TRUE){
      "Shows a confusion matrix of a model in the IDE viewer on in the web browser."
      showReport(if (normalized) "confusionMatrix_normalized", "confusionMatrix",showInIDE)
    },
    showModelExtractorsReport = function(showInIDE = TRUE){
      "Shows extractors used by a model in the IDE viewer on in the web browser."
      showReport("extractor",showInIDE)
    },
    showFeaturesReport = function(showInIDE = TRUE){
      "Shows features used by a model in the IDE viewer on in the web browser."
      showReport("features",showInIDE)
    },
    showFieldsReport = function(showInIDE = TRUE){
      "Shows fields used by a model in the IDE viewer on in the web browser."
      showReport("field",showInIDE)
    },
    showFunctionReport = function(showInIDE = TRUE){
      "Shows functions used by a model in the IDE viewer on in the web browser."
      showReport("function",showInIDE)
    },
    showInputSchemaReport = function(showInIDE = TRUE){
      "Shows the input schema a model in the IDE viewer on in the web browser."
      showReport("InputSchema",showInIDE)
    },
    showModelComparisonReport = function(showInIDE = TRUE){
      "Shows cross validation of various algorithms tested to create a model in the IDE viewer on in the web browser."
      showReport("modelComparison",showInIDE)
    },
    showROCReport = function(){
      "Shows ROC of the model in the IDE viewer on in the web browser."
      showReport("roc_best",FALSE) #problematic to show in internal browser non local resources
    },
    showROC_CVReport = function(){
      "Shows ROC of cross validation of various algorithms tested to create a model in the IDE viewer on in the web browser."
      showReport("roc_CV",FALSE) #problematic to show in internal browser non local resources
    }

    # add all reports + showReport
    # add features json
    # serialize / deserialize
    # non blocking
    # lift, regression plots
    # feature clusters
    # featurePlot

  )
);








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
#' @param globalFeatureIterations: Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". "PREC" by default.
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @param server_port Optional. the port to be accessed in the SparkBeyond API server. 9000 by default.
#' @return model path of the prediction on \code{trainingFilePath}.
#' @examples
#' modelRes = SBlearn("titanic", titanic_train_filename, "survived")
SBlearn <- function(sessionName, trainingFilePath, target,
                    testFilePath = NA,
                    trainTestSplitRatio = 0.8,
                    weightColumn = NA,
                    maxDepth = 2,
                    algorithmsWhiteList = NA, #list available algorithms
                    hints = NA,
                    useGraph = FALSE,
                    globalFeatureIterations = 300, #make it a list
                    evaluationMetric = "PREC", #add all options
                    scoreOnTestSet = FALSE,
                    crossValidation = 5,
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
                globalFeatureIterations = globalFeatureIterations,
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
  m = SBmodel(res$artifactPath, server_port)
  return(m)
}
