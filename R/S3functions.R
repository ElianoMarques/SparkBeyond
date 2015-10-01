# S3 functions (De facto constructors of Session)

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName: Optional string of the session name. Setting a session name is highly recommended. "temp" by default.
#' @param trainData: A data frame to analyze.
#' @param target String of the column name of in the training file that contains the target of the prediction.
#' @param testData: Optional. An independent test data frame to test the prediction on. NA by default.
#' @param trainTestSplitRatio: Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param weightColumn: Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth: Optional. Integer < 8 which represent the maximum number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param contextDatasets: Optional. A list of paths to context datasets to be added to the learning.
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. NA by default
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. Can also include function domains including('math','arithmetics', 'collections', 'booleanOperators', 'semantics', 'nlp', 'trigonometry', 'bitwise'). NA by default.
#' @param booleanNumericFeatures: A boolean indicating whether to transform all features to boolean values. TRUE by default.
#' @param useGraph: Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param useCustomGraphs: A boolean indicating whether custom graphs should be used. FALSE by default.
#' @param customGraphsWhiteList: Optional. A list that filters which domains should be used. NA by default.
#' @param customGraphsBlackList: Optional. A list that filters which custom graphs should be ignored. NA by default.
#' @param customFunctions: Optional. A list of additional functions that should be incorporated to the feature search. NA by default.
#' @param crossRowFeatureSearch. A booleean indicating whether to allow creating features using data collected from multiple rows together.FALSE by default.
#' @param maxFeaturesCount: Optional. A list of integers indicating how many features should be created by the SB engine. 300 by default.
#' @param autoColumnSubSets: Optional. A list of values contain any of the following: "CONCEPT", "NUMERIC_PAIRS", "ALL_PAIRS". "CONCEPT" will aim to generate column subset from fields that are from similar non-numeric types or combination of date and non-numeric elements. "NUMERIC_PAIRS" will create all column subsets for numeric columns. "ALL_PAIRS" will create subsets of size 2 from all columns. "CONCEPT" by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. In order to set a certain depth to the subset, add an element in the end of the customSubSet with one digit as a string representing the requested depth. If not such element was defined the regular depth definitions will be used. NA by default.
#' @param maxFeatureDuration: Optional. A numeric value representing the maximum allowed time a feature may take during search per row in milliseconds. 100 by default.
#' @param overrideMaxFeatureDurationForExternalData: A boolean indicating whether the maxFeatureDuration parameter should not be used for features that use external data. TRUE by default.
#' @param useCachedFeatures: Optional. A boolean indicating whether to use cached features (from previous run). FALSE by default.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". "AUC" by default.
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 80K by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @param fileEncoding: Optional. NA by default. Options are: "ISO-8859-1", "UTF-8", "US-ASCII".
#' @param autoSave: Optional. Automatically saves the generated session object to a file for future use. Good in cases where the connection between R and the server was interrupted or you would like to review previous models results. TRUE by default.
#' @param runBlocking: Block the R console while the session is running. FALSE by default.
#' @return Session object that encapsulates the model.
#' @examples
#' \donttest{
#' session = learn("titanic", getData("titanic_train"),
#'    target = "survived", algorithmsWhiteList = list("RRandomForest"), runBlocking = TRUE)
#'
#' flights = getData("flights_delay")
#' sampledFlights = sampleData(flights, 0.2)
#' flightsModel = learn(projectName = "flights_delay_example",
#'                     trainData = sampledFlights,
#'                     target = "weatherDelay",
#'                     algorithmsWhiteList = list("RRandomForestRegressor"),
#'                     functionsBlackList = list("trigonometry"),
#'                     maxFeaturesCount = list(20)
#'                )
#' head(flightsModel$features)
#' }

learn <- function(projectName = "temp",
                  trainData,
                  target,
                  testData = NA,
                  trainTestSplitRatio = 0.8,
                  weightColumn = NA,
                  maxDepth = 2,
                  contextDatasets = NA,
                  algorithmsWhiteList = NA, #list available algorithms
                  functionsWhiteList = NA,
                  functionsBlackList = NA,
                  booleanNumericFeatures = NA,
                  useGraph = FALSE,
                  useCustomGraphs = FALSE,
                  customGraphsWhiteList = NA,
                  customGraphsBlackList = NA,
                  customFunctions = NA,
                  crossRowFeatureSearch = FALSE,
                  maxFeaturesCount = list(300),
                  autoColumnSubSets = list("CONCEPT"),
                  customColumnSubsets = NA,
                  maxFeatureDuration = 100,
                  overrideMaxFeatureDurationForExternalData = TRUE,
                  useCachedFeatures = FALSE,
                  evaluationMetric = "AUC", #add all options
                  scoreOnTestSet = FALSE,
                  crossValidation = 5,
                  allocatedMemoryMB = 1000,
                  maxCollectionSize = 80000,
                  weightByClass = FALSE,
                  produceFeatureClusteringReport = FALSE,
                  fileEncoding = NA,
                  autoSave = TRUE,
                  runBlocking = TRUE,
                  verbose = FALSE,
                  numericEqualityFeatures = TRUE){

  params <-list(projectName = projectName,
                trainDataFilename = writeToServer(trainData),
                target = target,
                testDataFilename= if (class(testData) != "logical" && any(grep("data.frame", class(testData)))) writeToServer(testData) else NA,
                trainTestSplitRatio = trainTestSplitRatio,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                contextDatasets = contextDatasets,
                algorithmsWhiteList = algorithmsWhiteList,
                functionsWhiteList = functionsWhiteList,
                functionsBlackList = functionsBlackList,
                booleanNumericFeatures = booleanNumericFeatures,
                useGraph = useGraph,
                useCustomGraphs = useCustomGraphs,
                customGraphsWhiteList = customGraphsWhiteList,
                customGraphsBlackList = customGraphsBlackList,
                customFunctions = customFunctions,
                crossRowFeatureSearch = crossRowFeatureSearch,
                maxFeaturesCount = maxFeaturesCount,
                autoColumnSubSets = autoColumnSubSets,
                customColumnSubsets = customColumnSubsets,
                maxFeatureDuration = maxFeatureDuration,
                overrideMaxFeatureDurationForExternalData = overrideMaxFeatureDurationForExternalData,
                useCachedFeatures = useCachedFeatures,
                evaluationMetric = evaluationMetric,
                scoreOnTestSet = scoreOnTestSet,
                crossValidation = crossValidation,
                allocatedMemoryMB = allocatedMemoryMB,
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                autoSave = autoSave,
                fileEncoding = fileEncoding,
                verbose = verbose,
                numericEqualityFeatures = numericEqualityFeatures
                )

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
#' @param contextDatasets: Optional. A list of paths to context datasets to be added to the learning.
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. NA by default
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. Can also include function domains including('math','arithmetics', 'collections', 'booleanOperators', 'semantics', 'nlp', 'trigonometry', 'bitwise'). NA by default.
#' @param booleanNumericFeatures: A boolean indicating whether to transform all features to boolean values. TRUE by default.
#' @param useGraph: Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param useCustomGraphs: A boolean indicating whether custom graphs should be used. FALSE by default.
#' @param customGraphsWhiteList: Optional. A list that filters which domains should be used. NA by default.
#' @param customGraphsBlackList: Optional. A list that filters which custom graphs should be ignored. NA by default.
#' @param customFunctions: Optional. A list of additional functions that should be incorporated to the feature search. NA by default.
#' @param crossRowFeatureSearch. A booleean indicating whether to allow creating features using data collected from multiple rows together.FALSE by default.
#' @param maxFeaturesCount: Optional. A list of integers indicating how many features should be created by the SB engine. 300 by default.
#' @param autoColumnSubSets: Optional. A list of values contain any of the following: "CONCEPT", "NUMERIC_PAIRS", "ALL_PAIRS". "CONCEPT" will aim to generate column subset from fields that are from similar non-numeric types or combination of date and non-numeric elements. "NUMERIC_PAIRS" will create all column subsets for numeric columns. "ALL_PAIRS" will create subsets of size 2 from all columns. "CONCEPT" by default.
#' @param customColumnSubsets: Optional. A list of lists containing specific column subsets to examine. In order to set a certain depth to the subset, add an element in the end of the customSubSet with one digit as a string representing the requested depth. If not such element was defined the regular depth definitions will be used. NA by default.
#' @param useCachedFeatures: Optional. A boolean indicating whether to use cached features (from previous run). FALSE by default.
#' @param maxFeatureDuration: Optional. A numeric value representing the maximum allowed time a feature may take during search per row in milliseconds. 100 by default.
#' @param overrideMaxFeatureDurationForExternalData: A boolean indicating whether the maxFeatureDuration parameter should not be used for features that use external data. TRUE by default.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". "AUC" by default.
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 80K by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @param fileEncoding: Optional. NA by default. Options are: "ISO-8859-1", "UTF-8", "US-ASCII".
#' @param autoSave: Optional. Automatically saves the generated session object to a file for future use. Good in cases where the connection between R and the server was interrupted or you would like to review previous models results. TRUE by default.
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
                    contextDatasets = NA,
                    algorithmsWhiteList = NA, #list available algorithms
                    functionsWhiteList = NA,
                    functionsBlackList = NA,
                    booleanNumericFeatures = NA,
                    useGraph = FALSE,
                    useCustomGraphs = FALSE,
                    customGraphsWhiteList = NA,
                    customGraphsBlackList = NA,
                    customFunctions = NA,
                    crossRowFeatureSearch = FALSE,
                    maxFeaturesCount = list(300),
                    autoColumnSubSets = list("CONCEPT"),
                    customColumnSubsets = NA,
                    maxFeatureDuration = 100,
                    overrideMaxFeatureDurationForExternalData = TRUE,
                    useCachedFeatures = FALSE,
                    evaluationMetric = "AUC", #add all options
                    scoreOnTestSet = FALSE,
                    crossValidation = 5,
                    allocatedMemoryMB = 1000,
                    maxCollectionSize = 80000,
                    weightByClass = FALSE,
                    produceFeatureClusteringReport = FALSE,
                    fileEncoding = NA,
                    autoSave = TRUE,
                    runBlocking = TRUE,
                    verbose = FALSE,
                    numericEqualityFeatures = TRUE){

  isLatestVersion()
  isLatestRpackage()
  verifyList = function(l) {if(is.vector(l) && !is.na(l)) as.list(l) else l}
  algorithmsWhiteList = verifyList(algorithmsWhiteList)
  functionsWhiteList = verifyList(functionsWhiteList)
  functionsBlackList = verifyList(functionsBlackList)
  maxFeaturesCount = verifyList(maxFeaturesCount)

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
                contextDatasets = contextDatasets,
                algorithmsWhiteList = algorithmsWhiteList,
                hints = functionsWhiteList,
                sessionBlackList = functionsBlackList,
                booleanNumericFeatures = booleanNumericFeatures,
                useGraph = useGraph,
                useCustomGraphs = useCustomGraphs,
                customGraphsWhiteList = customGraphsWhiteList,
                customGraphsBlackList = customGraphsBlackList,
                customFunctions = customFunctions,
                crossRowFeatureSearch = crossRowFeatureSearch,
                globalFeatureIterations = maxFeaturesCount,
                autoColumnSubSets = autoColumnSubSets,
                customColumnSubsets = customColumnSubsets,
                maxTimePerRowMillis = maxFeatureDuration,
                overrideMaxFeatureDurationForExternalData = overrideMaxFeatureDurationForExternalData,
                useCachedFeatures = useCachedFeatures,
                evaluationMetric = evaluationMetric,
                scoreOnTestSet = scoreOnTestSet,
                crossValidation = crossValidation,
                allocatedMemoryMB = allocatedMemoryMB,
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                externalPrefixPath = getSBserverIOfolder(),
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                fileEncoding = fileEncoding,
                numericEqualityFeatures = numericEqualityFeatures
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
  session = Session(artifact_loc = res$artifactPath, !(length(algorithmsWhiteList) == 0 || tolower(algorithmsWhiteList[[1]]) == "zeror"))

  if (autoSave){
    tryCatch({
      tokens = strsplit(session$artifact_loc, "/")[[1]]
      varName = paste("backup", tokens[length(tokens)-1], tokens[length(tokens)], sep="_")
      saveFilename = paste0(getwd(),.Platform$file.sep,varName,".RData")
      assign(varName, session)
      base::save(list=varName, file = saveFilename) #auto-saving the model
      print (paste("auto saved Session object to a variable named '", varName,"'. To retrieve use:" ,paste0("load('",saveFilename,"').")))
    })
  }
  if (runBlocking) session$waitForProcess()
  return(session)
}

#' Run SparkBeyond feature enrichment and learning process.
#' @param projectName Optional string of the session name. Setting a session name is highly recommened. "temp" by default.
#' @param trainData: A data frame to analyze.
#' @param target String of the column name of in the training file that conatins the target of the prediction.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param maxDepth Optional. Integer < 8 which represent the maximun number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param contextDatasets: Optional. A list of paths to context datasets to be added to the learning.
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. Can also include function domains including('math','arithmetics', 'collections', 'booleanOperators', 'semantics', 'nlp', 'trigonometry', 'bitwise'). NA by default.
#' @param booleanNumericFeatures: A boolean indicating whether to transform all features to boolean values. TRUE by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param useCustomGraphs: A boolean indicating whether custom graphs should be used. FALSE by default.
#' @param customGraphsWhiteList: Optional. A list that filters which domains should be used. NA by default.
#' @param customGraphsBlackList: Optional. A list that filters which custom graphs should be ignored. NA by default.
#' @param customFunctions: Optional. A list of additional functions that should be incorporated to the feature search. NA by default.
#' @param crossRowFeatureSearch. A booleean indicating whether to allow creating features using data collected from multiple rows together.FALSE by default.
#' @param maxFeaturesCount: Optional. A list of integers indicating how many features should be created by the SB engine. 300 by default.
#' @param autoColumnSubSets: Optional. A list of values contain any of the following: "CONCEPT", "NUMERIC_PAIRS", "ALL_PAIRS". "CONCEPT" will aim to generate column subset from fields that are from similar non-numeric types or combination of date and non-numeric elements. "NUMERIC_PAIRS" will create all column subsets for numeric columns. "ALL_PAIRS" will create subsets of size 2 from all columns. "CONCEPT" by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 80K by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. In order to set a certain depth to the subset, add an element in the end of the customSubSet with one digit as a string representing the requested depth. If not such element was defined the regular depth definitions will be used. NA by default.
#' @param maxFeatureDuration: Optional. A numeric value representing the maximum allowed time a feature may take during search per row in milliseconds. 100 by default.
#' @param overrideMaxFeatureDurationForExternalData: A boolean indicating whether the maxFeatureDuration parameter should not be used for features that use external data. TRUE by default.
#' @param useCachedFeatures: Optional. A boolean indicating whether to use cached features (from previous run). FALSE by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @param fileEncoding: Optional. NA by default. Options are: "ISO-8859-1", "UTF-8", "US-ASCII".
#' @param autoSave: Optional. Automatically saves the generated session object to a file for future use. Good in cases where the connection between R and the server was interrupted or you would like to review previous models results. TRUE by default.
#' @return Session object that encapsulate the feature search result.
#' @examples
#' #session = featureSearch("titanic", getData("titanic_train"), "survived")
featureSearch <- function(projectName = "temp",
                                trainData,
                                target,
                                weightColumn = NA,
                                maxDepth = 2,
                                contextDatasets = NA,
                                functionsWhiteList = NA,
                                functionsBlackList = NA,
                                booleanNumericFeatures = TRUE,
                                useGraph = FALSE,
                                useCustomGraphs = FALSE,
                                customGraphsWhiteList = NA,
                                customGraphsBlackList = NA,
                                customFunctions = NA,
                                crossRowFeatureSearch = FALSE,
                                maxFeaturesCount = list(300),
                                autoColumnSubSets = list("CONCEPT"),
                                customColumnSubsets = NA,
                                maxFeatureDuration = 100,
                                overrideMaxFeatureDurationForExternalData = TRUE,
                                useCachedFeatures = FALSE,
                                allocatedMemoryMB = 1000,
                                maxCollectionSize = 80000,
                                weightByClass = FALSE,
                                produceFeatureClusteringReport = FALSE,
                                fileEncoding = NA,
                                autoSave = TRUE,
                                runBlocking = TRUE){

  params <-list(projectName = projectName,
                trainData = trainData,
                target = target,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                contextDatasets = contextDatasets,
                algorithmsWhiteList = list("ZeroR"),
                functionsWhiteList = functionsWhiteList,
                functionsBlackList = functionsBlackList,
                booleanNumericFeatures = booleanNumericFeatures,
                useGraph = useGraph,
                useCustomGraphs = useCustomGraphs,
                customGraphsWhiteList = customGraphsWhiteList,
                customGraphsBlackList = customGraphsBlackList,
                customFunctions = customFunctions,
                crossRowFeatureSearch = crossRowFeatureSearch,
                maxFeaturesCount = maxFeaturesCount,
                autoColumnSubSets = autoColumnSubSets,
                customColumnSubsets = customColumnSubsets,
                maxFeatureDuration = maxFeatureDuration,
                overrideMaxFeatureDurationForExternalData = overrideMaxFeatureDurationForExternalData,
                useCachedFeatures = useCachedFeatures,
                allocatedMemoryMB = allocatedMemoryMB,
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                fileEncoding = fileEncoding,
                autoSave = autoSave,
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
#' @param contextDatasets: Optional. A list of paths to context datasets to be added to the learning.
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. Can also include function domains including('math','arithmetics', 'collections', 'booleanOperators', 'semantics', 'nlp', 'trigonometry', 'bitwise'). NA by default.
#' @param booleanNumericFeatures: A boolean indicating whether to transform all features to boolean values. TRUE by default.
#' @param useGraph Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param useCustomGraphs: A boolean indicating whether custom graphs should be used. FALSE by default.
#' @param customGraphsWhiteList: Optional. A list that filters which domains should be used. NA by default.
#' @param customGraphsBlackList: Optional. A list that filters which custom graphs should be ignored. NA by default.
#' @param customFunctions: Optional. A list of additional functions that should be incorporated to the feature search. NA by default.
#' @param crossRowFeatureSearch. A booleean indicating whether to allow creating features using data collected from multiple rows together.FALSE by default.
#' @param maxFeaturesCount: Optional. A list of integers indicating how many features should be created by the SB engine. 300 by default.
#' @param autoColumnSubSets: Optional. A list of values contain any of the following: "CONCEPT", "NUMERIC_PAIRS", "ALL_PAIRS". "CONCEPT" will aim to generate column subset from fields that are from similar non-numeric types or combination of date and non-numeric elements. "NUMERIC_PAIRS" will create all column subsets for numeric columns. "ALL_PAIRS" will create subsets of size 2 from all columns. "CONCEPT" by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. 80K by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. In order to set a certain depth to the subset, add an element in the end of the customSubSet with one digit as a string representing the requested depth. If not such element was defined the regular depth definitions will be used. NA by default.
#' @param maxFeatureDuration: Optional. A numeric value representing the maximum allowed time a feature may take during search per row in milliseconds. 100 by default.
#' @param overrideMaxFeatureDurationForExternalData: A boolean indicating whether the maxFeatureDuration parameter should not be used for features that use external data. TRUE by default.
#' @param useCachedFeatures: Optional. A boolean indicating whether to use cached features (from previous run). FALSE by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @param fileEncoding: Optional. NA by default. Options are: "ISO-8859-1", "UTF-8", "US-ASCII".
#' @param autoSave: Optional. Automatically saves the generated session object to a file for future use. Good in cases where the connection between R and the server was interrupted or you would like to review previous models results. TRUE by default.
#' @return Session object that encapsulate the feature search result.
#' @examples
#' #session = featureSearch.file ("titanic", titanic_train_filename, "survived")
featureSearch.file <- function(projectName = "temp",
                          trainDataFilename,
                          target,
                          weightColumn = NA,
                          maxDepth = 2,
                          contextDatasets = NA,
                          functionsWhiteList = NA,
                          functionsBlackList = NA,
                          booleanNumericFeatures = TRUE,
                          useGraph = FALSE,
                          useCustomGraphs = FALSE,
                          customGraphsWhiteList = NA,
                          customGraphsBlackList = NA,
                          customFunctions = NA,
                          crossRowFeatureSearch = FALSE,
                          maxFeaturesCount = list(300),
                          autoColumnSubSets = list("CONCEPT"),
                          customColumnSubsets = NA,
                          maxFeatureDuration = 100,
                          overrideMaxFeatureDurationForExternalData = TRUE,
                          useCachedFeatures = FALSE,
                          allocatedMemoryMB = 1000,
                          maxCollectionSize = 80000,
                          weightByClass = FALSE,
                          produceFeatureClusteringReport = FALSE,
                          fileEncoding = NA,
                          autoSave = TRUE,
                          runBlocking = TRUE){

  params <-list(projectName = projectName,
                trainDataFilename = trainDataFilename,
                target = target,
                weightColumn = weightColumn,
                maxDepth = maxDepth,
                contextDatasets = contextDatasets,
                algorithmsWhiteList = list("ZeroR"),
                functionsWhiteList = functionsWhiteList,
                functionsBlackList = functionsBlackList,
                booleanNumericFeatures = booleanNumericFeatures,
                useGraph = useGraph,
                useCustomGraphs = useCustomGraphs,
                customGraphsWhiteList = customGraphsWhiteList,
                customGraphsBlackList = customGraphsBlackList,
                customFunctions = customFunctions,
                crossRowFeatureSearch = crossRowFeatureSearch,
                maxFeaturesCount = maxFeaturesCount,
                autoColumnSubSets = autoColumnSubSets,
                customColumnSubsets = customColumnSubsets,
                useCachedFeatures = useCachedFeatures,
                maxFeatureDuration = maxFeatureDuration,
                overrideMaxFeatureDurationForExternalData = overrideMaxFeatureDurationForExternalData,
                allocatedMemoryMB = allocatedMemoryMB,
                maxCollectionSize = maxCollectionSize,
                weightByClass = weightByClass,
                produceFeatureClusteringReport = produceFeatureClusteringReport,
                fileEncoding = fileEncoding,
                autoSave = autoSave,
                runBlocking = runBlocking)
  model = do.call(learn.file,c(params))
  model
}

#' A function to write a dateframe to the server. Useful for passing a dataframe to the server for feature search / learning purposes.
#' @param data Data frame or table to export to the server.
#' @param filename: Optional. define a name to save the data to. NA by default.
#' @return A filepath to the file on the server that was created.
writeToServer = function(data, filename = NA){
  final_filename = if (is.na(filename)) tempfile("data_in",  tmpdir = getSBserverIOfolder(), fileext=".tsv") else paste0(getSBserverIOfolder(), filename)
  final_filename = gsub("/+", "/", final_filename)
  writeToFile(data, final_filename)
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

