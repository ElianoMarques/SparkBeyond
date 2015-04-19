# S3 functions (De facto constructors of SBmodel)

#' Run SparkBeyond feature enrichment and learning process.
#' @param sessionName Optional string of the session name. Setting a session name is highly recommended. "temp" by default.
#' @param trainingFilePath String of the path to the file to be trained on.
#' @param target String of the column name of in the training file that contains the target of the prediction.
#' @param testFilePath: Optional. String of the path to an independent test file to test the prediction on. NA by default.
#' @param trainTestSplitRatio: Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param weightColumn: Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth: Optional. Integer < 8 which represent the maximum number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. NA by default
#' @param hints: Optional. A list of strings that represents a set of hints that will be used to guide the feature search. NA by default.
#' @param useGraph: Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount: Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". "PREC" by default.
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @return SBmodel object the encapsulate the prediction.
#' @examples
#' model = SBlearn("titanic", getTitanicFilename(train = TRUE), "survived", algorithmsWhiteList = list("RRandomForest"))
SBlearn <- function(sessionName = "temp",
                    trainingFilePath,
                    target,
                    testFilePath = NA,
                    trainTestSplitRatio = 0.8,
                    weightColumn = NA,
                    maxDepth = 2,
                    algorithmsWhiteList = NA, #list available algorithms
                    hints = NA,
                    useGraph = FALSE,
                    maxFeaturesCount = 300, #TODO: make it a list
                    evaluationMetric = "PREC", #add all options
                    scoreOnTestSet = FALSE,
                    crossValidation = 5,
                    runBlocking = FALSE){

  url <- paste(getSBserverHost(),":",getSBserverPort(),"/rapi/learn", sep="")
  print(paste("Calling:", url))

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
  model = SBmodel(artifact_loc = res$artifactPath, TRUE)
  if (runBlocking) model$waitForProcess()
  return(model)
}

#' Run SparkBeyond feature enrichment and learning process.
#' @param sessionName Optional string of the session name. Setting a session name is highly recommened. "temp" by default.
#' @param trainingFilePath String of the path to the file to be trained on.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param hints Optional. A list of strings that reprents a set of hints that will be used to guide the feature search. NA by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @return SBmodel object that encapsulate the feature search result.
#' @examples
#' #model = SBfeatureSearchOnly("titanic", titanic_train_filename, "survived")
SBfeatureSearchOnly <- function(sessionName = "temp",
                                trainingFilePath,
                                target,
                                weightColumn = NA,
                                maxDepth = 2,
                                hints = NA,
                                useGraph = FALSE,
                                maxFeaturesCount = 300, #TODO: make it a list
                                runBlocking = FALSE){

  params <-list("sessionName" = sessionName,
                "trainingFilePath" = trainingFilePath,
                target = target,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                algorithmsWhiteList = list("ZeroR"),
                hints = hints,
                useGraph = useGraph,
                maxFeaturesCount = maxFeaturesCount,
                runBlocking = runBlocking)
  model = do.call(SBlearn,c(params))
  model$modelBuilt = FALSE
  model
}

#' A function to write a dateframe to the server. Useful for passing a dataframe to the server for feature search / learning purposes.
#' @param data Data frame or table to export to the server.
#' @return A filepath to the file on the server that was created.
writeDataToServer = function(data){
  filename = tempfile("data_in",  tmpdir = getSBserverIOfolder(), fileext="tsv")
  write.table(data, file=filename, row.names = FALSE, sep='\t') #TODO: write things using the new write function
  return (filename)
}

#' A function to restart server
#' @return The response from the server.
restartServer = function() {
  url <- paste(getSBserverHost(),":",getSBserverPort(),"/rapi/die", sep="")
  res = httr::POST(url, body = FALSE, httr::content_type_json())
  res
}

# #' Run SparkBeyond prediction on a result of SBlearn.
# #' @param modelPath path to the model file returned by SBlearn.
# #' @param dataPath  path to the file to be tested.
# #' @param outputPath path to write the results of the prediction.
# #' @param server_port the port to be accessed in the SparkBeyond API server. 9000 by default.
# #' @return A data frame containing the prediction.
# #' @examples
# #' predictRes = SBpredict(modelRes$artifactPath, titanic_test_filename, "./titanic_test.tsv.gz")
# SBpredict <- function(modelPath, dataPath, outputPath, server_port = 9000){
#   url <- paste("http://127.0.0.1:",server_port,"/rapi/predict", sep="")
# 	params <-list(modelPath = modelPath,
# 		dataPath = dataPath,
# 		outputPath = outputPath)
#
# 	body = rjson::toJSON(params)
# 	res = httr::POST(url, body = body, httr::content_type_json())
#   res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
#
#   finalRes = if (is.null(res$error) && !is.null(res$result) && res$result == "OK"){
#   	read.table(outputPath, header = TRUE, sep="\t")
#   } else {
#     message = paste("Prediction failed: ", res$error)
#     print(message)
#     stop(message)
#   }
# 	return(finalRes)
# }

