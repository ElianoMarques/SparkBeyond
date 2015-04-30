# S3 functions (De facto constructors of SBmodel)

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName: Optional string of the session name. Setting a session name is highly recommended. "temp" by default.
#' @param trainData: A data frame to analyze.
#' @param trainDataFilename: Optional. define a name to save the data to. Useful in combination with {overridePreviousFiles} to cache files written to server.
#' @param target String of the column name of in the training file that contains the target of the prediction.
#' @param testData: Optional. An independent test data frame to test the prediction on. NA by default.
#' @param testDataFilename: Optional. define a name to save the data to. Useful in combination with {overridePreviousFiles} to cache files written to server.
#' @param overridePreviousFiles: An indicator whether to override train / test files that were previously written to the server.
#' @param trainTestSplitRatio: Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param weightColumn: Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth: Optional. Integer < 8 which represent the maximum number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. NA by default
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. NA by default.
#' @param useGraph: Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount: Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param columnSubsetSize: Optional. An integer denoting whether sets of columns should be looked at together. 1 by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. NA by default.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". "PREC" by default.
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @return SBmodel object the encapsulate the prediction.
#' @examples
#' model = SBlearn("titanic", getTitanicData(train = TRUE), target = "survived", algorithmsWhiteList = list("RRandomForest"), runBlocking = TRUE)
SBlearn <- function(projectName = "temp",
                    trainData,
                    trainDataFilename = "",
                    target,
                    testData = NA,
                    testDataFilename = "",
                    overridePreviousFiles = TRUE,
                    trainTestSplitRatio = 0.8,
                    weightColumn = NA,
                    maxDepth = 2,
                    algorithmsWhiteList = NA, #list available algorithms
                    functionsWhiteList = NA,
                    functionsBlackList = NA,
                    useGraph = FALSE,
                    maxFeaturesCount = 300, #TODO: make it a list
                    columnSubsetSize = 1,
                    customColumnSubsets = NA,
                    evaluationMetric = "PREC", #add all options
                    scoreOnTestSet = FALSE,
                    crossValidation = 5,
                    allocatedMemoryMB = 1000,
                    weightByClass = FALSE,
                    runBlocking = TRUE){

  url <- paste(getSBserverHost(),":",getSBserverPort(),"/rapi/learn", sep="")
  print(paste("Calling:", url))

  params <-list(projectName = projectName,
                trainingFilePath = writeToServer(trainData, trainDataFilename, overridePreviousFiles),
                target = target,
                testFilePath = if (!is.na(testData)) writeToServer(testData, testDataFilename, overridePreviousFiles) else NA,
                trainTestSplitRatio = trainTestSplitRatio,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                algorithmsWhiteList = algorithmsWhiteList,
                hints = functionsWhiteList,
                sessionBlackList = functionsBlackList,
                useGraph = useGraph,
                globalFeatureIterations = maxFeaturesCount,
                columnSubsetSize = columnSubsetSize,
                customColumnSubsets = customColumnSubsets,
                evaluationMetric = evaluationMetric,
                scoreOnTestSet = scoreOnTestSet,
                crossValidation = crossValidation,
                allocatedMemoryMB = allocatedMemoryMB,
                pathPrefix = getSBserverIOfolder()
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
#' @param projectName Optional string of the session name. Setting a session name is highly recommened. "temp" by default.
#' @param trainData: A data frame to analyze.
#' @param trainDataFilename: Optional. define a name to save the data to. Useful in combination with {overridePreviousFile} to cache files written to server.
#' @param overridePreviousFile: An indicator whether to override file that was previously written to the server.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. NA by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param columnSubsetSize: Optional. An integer denoting whether sets of columns should be looked at together. 1 by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. NA by default.
#' @return SBmodel object that encapsulate the feature search result.
#' @examples
#' #model = SBfeatureSearchOnly("titanic", titanic_train_filename, "survived")
SBfeatureSearchOnly <- function(projectName = "temp",
                                trainData,
                                trainDataFilename = "", #TODO:
                                overridePreviousFile = TRUE,
                                target,
                                weightColumn = NA,
                                maxDepth = 2,
                                functionsWhiteList = NA,
                                functionsBlackList = NA,
                                useGraph = FALSE,
                                maxFeaturesCount = 300, #TODO: make it a list
                                columnSubsetSize = 1,
                                customColumnSubsets = NA,
                                allocatedMemoryMB = 1000,
                                runBlocking = TRUE){

  params <-list(projectName = projectName,
                trainData = trainData,
                trainDataFilename = trainDataFilename,
                overridePreviousFiles = overridePreviousFile,
                target = target,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                algorithmsWhiteList = list("ZeroR"),
                functionsWhiteList = functionsWhiteList,
                functionsBlackList = functionsBlackList,
                useGraph = useGraph,
                maxFeaturesCount = maxFeaturesCount,
                columnSubsetSize = columnSubsetSize,
                customColumnSubsets = customColumnSubsets,
                allocatedMemoryMB = allocatedMemoryMB,
                runBlocking = runBlocking)
  model = do.call(SBlearn,c(params))
  model$modelBuilt = FALSE
  model
}

#' A function to write a dateframe to the server. Useful for passing a dataframe to the server for feature search / learning purposes.
#' @param data Data frame or table to export to the server.
#' @param filename: Optional. define a name to save the data to. Useful in combination with {overridePreviousFiles} to cache files written to server.
#' @param overridePreviousFile: An indicator whether to override file that was previously written to the server.
#' @return A filepath to the file on the server that was created.
writeToServer = function(data, filename = "", overridePreviousFile = TRUE){
  final_filename = if (filename == "") tempfile("data_in",  tmpdir = getSBserverIOfolder(), fileext=".tsv") else paste0(getSBserverIOfolder(), filename)
  if (!file.exists(final_filename) || overridePreviousFile)
    writeGroupedData(data, final_filename)
  return (final_filename)
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

