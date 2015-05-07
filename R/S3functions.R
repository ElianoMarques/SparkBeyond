# S3 functions (De facto constructors of SBmodel)

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName: Optional string of the session name. Setting a session name is highly recommended. "temp" by default.
#' @param trainData: A data frame to analyze.
#' @param target String of the column name of in the training file that contains the target of the prediction.
#' @param testData: Optional. An independent test data frame to test the prediction on. NA by default.
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
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 70K by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#'
#' @param runBlocking: Block the R console while the session is running. FALSE by default.
#' @return Session object that encapsulates the model.
#' @examples
#' session = learn("titanic", getTitanicData(train = TRUE), target = "survived", algorithmsWhiteList = list("RRandomForest"), runBlocking = TRUE)
learn <- function(projectName = "temp",
                  trainData,
                  target,
                  testData = NA,
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
                  maxCollectionSize = 70000,
                  weightByClass = FALSE,
                  produceFeatureClusteringReport = FALSE,
                  runBlocking = TRUE,
                  verbose = TRUE){

  params <-list(projectName = projectName,
                trainDataFilename = writeToServer(trainData),
                target = target,
                testDataFilename= if (!is.na(testData)) writeToServer(testData) else NA,
                trainTestSplitRatio = trainTestSplitRatio,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                algorithmsWhiteList = algorithmsWhiteList,
                functionsWhiteList = functionsWhiteList,
                functionsBlackList = functionsBlackList,
                useGraph = useGraph,
                maxFeaturesCount = maxFeaturesCount,
                columnSubsetSize = columnSubsetSize,
                customColumnSubsets = customColumnSubsets,
                evaluationMetric = evaluationMetric,
                scoreOnTestSet = scoreOnTestSet,
                crossValidation = crossValidation,
                allocatedMemoryMB = allocatedMemoryMB,
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                verbose = verbose)

  session = do.call(learn.file,c(params))
  session
}

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName: Optional string of the session name. Setting a session name is highly recommended. "temp" by default.
#' @param trainDataFilename: Define the path to the training data to use.
#' @param target String of the column name of in the training file that contains the target of the prediction.
#' @param testDataFilename: Optional. define a the path to the test data to use. NA by default.
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
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 70K by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @return Session object that encapsulates the model.
#' @examples
#' #session = learn("titanic", titanic_file_path, target = "survived", algorithmsWhiteList = list("RRandomForest"), runBlocking = TRUE)
learn.file <- function(projectName = "temp",
                    trainDataFilename,
                    target,
                    testDataFilename = NA,
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
                    maxCollectionSize = 70000,
                    weightByClass = FALSE,
                    produceFeatureClusteringReport = FALSE,
                    runBlocking = TRUE,
                    verbose = FALSE){

  SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
  if (!grepl(SBdir, trainDataFilename)) trainDataFilename = paste0(getSBserverIOfolder(), trainDataFilename)
  if (!file.exists(trainDataFilename)) stop(print(paste("Train file:", trainDataFilename, "does not exist")))
  if (!is.na(testDataFilename)) {
    if(!grepl(SBdir, testDataFilename)) testDataFilename = paste0(getSBserverIOfolder(), testDataFilename)
    if(!file.exists(testDataFilename)) stop(print(paste("Test file:", testDataFilename, "does not exist")))
    if (!is.na(trainTestSplitRatio)) print ("Note: test data was provided - ignoring trainTestSplitRatio defintion.")
  }

  print (paste("Training on ",trainDataFilename))
  url <- paste(getSBserverHost(),":",getSBserverPort(),"/rapi/learn", sep="")
  print(paste("Calling:", url))

  params <-list(projectName = projectName,
                trainingFilePath = trainDataFilename,
                target = target,
                testFilePath = testDataFilename,
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
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport
  )

  params = params[!is.na(params)]

  body = rjson::toJSON(params)
  if (verbose) print(body)
  res = httr::POST(url, body = body, httr::content_type_json())
  res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
  if (!is.null(res$error)) {
    res = paste("Train error: ", res$error, " - terminating.")
    print(res)
    stop(res)
  }

  print(paste("Artifact location was created at:", res$artifactPath))
  model = Session(artifact_loc = res$artifactPath, TRUE)
  if (runBlocking) model$waitForProcess()
  return(model)
}

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName Optional string of the session name. Setting a session name is highly recommened. "temp" by default.
#' @param trainData: A data frame to analyze.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. NA by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param columnSubsetSize: Optional. An integer denoting whether sets of columns should be looked at together. 1 by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 70K by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. NA by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @return Session object that encapsulate the feature search result.
#' @examples
#' #session = featureSearch("titanic", getTitanicData(train = TRUE), "survived")
featureSearch <- function(projectName = "temp",
                                trainData,
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
                                maxCollectionSize = 70000,
                                weightByClass = FALSE,
                                produceFeatureClusteringReport = FALSE,
                                runBlocking = TRUE){

  params <-list(projectName = projectName,
                trainData = trainData,
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
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                runBlocking = runBlocking)
    model = do.call(learn,c(params))
  model
}

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName Optional string of the session name. Setting a session name is highly recommened. "temp" by default.
#' @param trainDataFilename: Define the path to where the data is saved.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. NA by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param maxFeaturesCount Optional. An integer of how many features should be created by the SB engine. 300 by default.
#' @param columnSubsetSize: Optional. An integer denoting whether sets of columns should be looked at together. 1 by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 70K by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. NA by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @return Session object that encapsulate the feature search result.
#' @examples
#' #session = featureSearch.file ("titanic", titanic_train_filename, "survived")
featureSearch.file <- function(projectName = "temp",
                          trainDataFilename,
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
                          maxCollectionSize = 70000,
                          weightByClass = FALSE,
                          produceFeatureClusteringReport = FALSE,
                          runBlocking = TRUE){

  params <-list(projectName = projectName,
                trainDataFilename = trainDataFilename,
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
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                runBlocking = runBlocking)
  model = do.call(learn.file,c(params))
  model$modelBuilt = FALSE
  model
}

#' A function to write a dateframe to the server. Useful for passing a dataframe to the server for feature search / learning purposes.
#' @param data Data frame or table to export to the server.
#' @param filename: Optional. define a name to save the data to. NA by default.
#' @return A filepath to the file on the server that was created.
writeToServer = function(data, filename = NA){
  final_filename = if (is.na(filename)) tempfile("data_in",  tmpdir = getSBserverIOfolder(), fileext=".tsv") else paste0(getSBserverIOfolder(), filename)
  final_filename = gsub("/+", "/", final_filename)
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

