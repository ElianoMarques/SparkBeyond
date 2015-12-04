# S3 functions (De facto constructors of Session)

#' preProcessingContorl
#' 
#' @param emptyValuePolicy: Controls how empty values in the data are being handled NA by default will replace numeric value with median value, and strings with empty strings.
#' @param fileEncoding: Optional. Options are: "ISO-8859-1", "UTF-8", "US-ASCII". NA by default will try to automatically find the best encoding.
preProcessingControl = function(
	emptyValuePolicy = NA,
	fileEncoding = NA,
	trainTestSplitRatio = 0.8,
	temporalSplitColumn = NA
) {
	list(
		emptyValuePolicy = emptyValuePolicy,
		fileEncoding = fileEncoding,
		trainTestSplitRatio = trainTestSplitRatio,
		temporalSplitColumn = temporalSplitColumn
	)
}

#' featureGenerationContorl
#' 
#' @param maxDepth: Optional. Integer < 8 which represent the maximum number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param featureSearchMode: One of the following DEFAULT, ADVANCED, ADVANCED_WITH_PAIRS, DIG_DEEP
#' @param functionsWhiteList: Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList: Optional. A list of strings that represents a set of function that will be excluded from the feature search. Can also include function domains including('math','arithmetics', 'collections', 'booleanOperators', 'semantics', 'nlp', 'trigonometry', 'bitwise'). NA by default.
#' @param booleanNumericFeatures: A boolean indicating whether to transform all features to boolean values. (i.e., when FALSE the continuous value of the feature left-hand-side will be passed to the algorithm, without taking into account the specific cutoff chosen during the feature search phase). NA by default indicating that it will be TRUE for classification problems and FALSE for regression problems.
#' @param numericEqualityFeatures: A boolean indicator for whether to include features that compare numeric fields with specific values. TRUE by default.
#' @param allowRangeFeatures: A boolean indicator for whether to include features that define range over a set of numeric values. TRUE by default.

#' @param crossRowFeatureSearch. A booleean indicating whether to allow creating features using data collected from multiple rows together.FALSE by default.
#' @param maxFeaturesCount: Optional. A list of integers indicating how many features should be created by the SB engine. 300 by default.
#' @param autoColumnSubSets: Optional. A list of values contain any of the following: "CONCEPT", "NUMERIC_PAIRS", "ALL_PAIRS". "CONCEPT" will aim to generate column subset from fields that are from similar non-numeric types or combination of date and non-numeric elements. "NUMERIC_PAIRS" will create all column subsets for numeric columns. "ALL_PAIRS" will create subsets of size 2 from all columns. "CONCEPT" by default.
#' @param customColumnSubsets: Optional. A List of lists containing specific column subsets to examine. In order to set a certain depth to the subset, add an element in the end of the customSubSet with one digit as a string representing the requested depth. If not such element was defined the regular depth definitions will be used. NA by default.
#' @param maxFeatureDuration: Optional. A numeric value representing the maximum allowed time a feature may take during search per row in milliseconds. 100 by default.
#' @param overrideMaxFeatureDurationForExternalData: A boolean indicating whether the maxFeatureDuration parameter should not be used for features that use external data. TRUE by default.
#' @param allocatedMemoryMB: Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize: Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. NA by default corresponding to 200K.
#' @param useCachedFeatures: Optional. A boolean indicating whether to use cached features (from previous run). TRUE by default.
featureGenerationControl = function(
	maxDepth = 2,
	featureSearchMode = "DEFAULT", 
	functionsWhiteList = NA,
	functionsBlackList = NA,
	booleanNumericFeatures = NA,
	numericEqualityFeatures = TRUE,
	allowRangeFeatures = TRUE,
	
	crossRowFeatureSearch = FALSE,
	maxFeaturesCount = list(300),
	autoColumnSubSets = list("CONCEPT"),
	customColumnSubsets = NA,
	maxFeatureDuration = 100,
	overrideMaxFeatureDurationForExternalData = TRUE,
	allocatedMemoryMB = 1000,
	maxCollectionSize = NA,
	useCachedFeatures = TRUE
) {
	list(
		maxDepth = maxDepth,
		featureSearchMode = featureSearchMode,
		functionsWhiteList = functionsWhiteList,
		functionsBlackList = functionsBlackList,
		booleanNumericFeatures = booleanNumericFeatures,
		numericEqualityFeatures = numericEqualityFeatures,
		allowRangeFeatures = allowRangeFeatures,
		
		crossRowFeatureSearch = crossRowFeatureSearch,
		maxFeaturesCount = maxFeaturesCount,
		autoColumnSubSets = autoColumnSubSets,
		customColumnSubsets = customColumnSubsets,
		maxFeatureDuration = maxFeatureDuration,
		overrideMaxFeatureDurationForExternalData = overrideMaxFeatureDurationForExternalData,
		allocatedMemoryMB = allocatedMemoryMB,
		maxCollectionSize = maxCollectionSize,
		useCachedFeatures = useCachedFeatures
	)
}

#' knowledge Control
#' 
#' @param useGraph: Optional. A boolean indicating whether the knowledge graph should be used. FALSE by default.
#' @param useOpenStreetMap: Optional. A boolean indicating whether Open Street Map data should be included in feature search. FALSE by default.
#' @param useCustomGraphs: A boolean indicating whether custom graphs should be used. FALSE by default.
#' @param customGraphsWhiteList: Optional. A list that filters which domains should be used. NA by default.
#' @param customGraphsBlackList: Optional. A list that filters which custom graphs should be ignored. NA by default.
#' @param customFunctions: Optional. A list of additional functions that should be incorporated to the feature search. NA by default.
knowledgeControl = function(
	useGraph = FALSE,
	useOpenStreetMap = FALSE,
	useCustomGraphs = FALSE,	
	customGraphsWhiteList = NA,
	customGraphsBlackList = NA,
	customFunctions = NA
	) {
		list(
			useGraph = useGraph,
			useOpenStreetMap = useOpenStreetMap,
			useCustomGraphs = useCustomGraphs,			
			customGraphsWhiteList = customGraphsWhiteList,
			customGraphsBlackList = customGraphsBlackList,
			customFunctions = customFunctions
		)
}


#' modelBuildingControl
#' 
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. NA by default
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". NA by default automatically selects AUC for classification and RMSE for regression.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
modelBuildingControl = function(
	algorithmsWhiteList = NA, #list available algorithms
	evaluationMetric = NA,
	crossValidation = 5
) {
	list(
		algorithmsWhiteList = algorithmsWhiteList,
		evaluationMetric = evaluationMetric,
		crossValidation = crossValidation
	)
}

#' reportingContorl
#' 

#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @param produceReports: "EVALUATED_FUNCTIONS".
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
reportingControl = function(
	produceFeatureClusteringReport = FALSE,
	produceReports = NA,
	scoreOnTestSet = FALSE,
	emailForNotification = NA
) {
	list(
		produceFeatureClusteringReport = produceFeatureClusteringReport,
		produceReports = produceReports,
		scoreOnTestSet = scoreOnTestSet,
		emailForNotification = emailForNotification
	)
}

#' contextObject
#' 
#' @param data: a data frame or a path to a file containing the context data
#' @param name: an identifier for the context object to be created (optional).
#' @param keyColumns: Specify the key columns to be used by the context object (optional).
#' @param timeColumn: Specify the time column to be used by the context object (relevant in time series contexts) (optional).
contextObject = function(data, name = NULL, keyColumns = list(), timeColumn = NULL) { #TODO: help
	#if data is data frame write it, otherwise it's a path - same for learn.file
	obj = list(data = data, name=name, keyColumns = keyColumns, timeColumn=timeColumn)
	class(obj) = "contextObject"
	obj
}



#' learn
#' 
#' Runs SparkBeyond feature enrichment and learning process.
#' @param projectName: Optional string of the session name. Setting a session name is highly recommended. "temp" by default.
#' @param trainData: train data to analyze.
#' @param target String of the column name of in the training file that contains the target of the prediction.
#' @param testData: Optional. test data to validate model results. NA by default.
#' @param trainTestSplitRatio: Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param weightColumn: Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param contextDatasets: Optional. A list of paths to context datasets to be added to the learning.
#' @param featureGenerationControl: A \code{\link{featureGenerationControl}} object with specific feature generation parameters.
#' @param knowledgeControl: A \code{\link{knowledgeControl}} object with specific external knowledge parameters.
#' @param modelBuildingControl: A \code{\link{modelBuildingControl}} object with specific model building parameters.
#' @param reportingControl: A \code{\link{reportingControl}} object with specific reporting parameters.
#' @param autoSave: Optional. Automatically saves the generated session object to a file for future use. Good in cases where the connection between R and the server was interrupted or you would like to review previous models results. TRUE by default.
#' @param runBlocking: Block the R console while the session is running. FALSE by default.
#' @return Session object that encapsulates the model.
#' @examples
#' #session = learn("titanic", getData("titanic_train"), target = "survived", algorithmsWhiteList = list("RRandomForest"), runBlocking = TRUE, autoSave=FALSE)
learn <- function(
			 projectName = "temp",
			 trainData,
			 target,
			 testData = NA,
			 weightColumn = NA,
			 weightByClass = FALSE,
			 contextDatasets = NA,
			 preProcessingCtrl = preProcessingControl(),
			 featureGenerationCtrl = featureGenerationControl(),
			 knowledgeCtrl = knowledgeControl(),
			 modelBuildingCtrl = modelBuildingControl(),
			 reportingCtrl = reportingControl(),
			 verbose = FALSE,
			 autoSave = TRUE,
			 runBlocking = TRUE,
			 ...
){
	isLatestVersion()
	isLatestRpackage()
	extraParams = list(...)
	remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else FALSE
	
	if(is.null(trainData) && !is.null(extraParams$trainData)) trainData = extraParams$trainData
	trainTestSplitRatio = if(!is.null(extraParams$trainTestSplitRatio)) extraParams$trainTestSplitRatio else preProcessingCtrl$trainTestSplitRatio
	
	if (!is.na(testData) && !is.na(trainTestSplitRatio)) print ("Note: test data was provided - ignoring trainTestSplitRatio defintion.")	
	
	url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/learn")
	print(paste("Calling:", url))
	
	if (!is.na(contextDatasets)){ #writing context data to server if necessary
		if (!all(sapply(contextDatasets, function(x) class(x) == "contextObject"))) stop("Not all provided context objects are of type 'contextObject'")
		for (i in 1:length(contextDatasets)) {
			contextDatasets[[i]]$data = writeToServer(contextDatasets[[i]]$data, 
				prefix = paste0(projectName,"_context", ifelse(!is.null(contextDatasets[[i]]$name), paste0("_", contextDatasets[[i]]$name),""))
			)
		}
	}
	
	params <-list(projectName = projectName,
								trainingFilePath = writeToServer(trainData, prefix = paste0(projectName,"_train")),
								target = target,
								testFilePath= if ((class(testData) != "logical" && any(grep("data.frame", class(testData)))) || class(testData)=="character")
									writeToServer(testData, prefix = paste0(projectName,"_test")) else NA,
								trainTestSplitRatio = trainTestSplitRatio,
								weightColumn = weightColumn,
								weightByClass = weightByClass,
								contextDatasets = contextDatasets,
								
								# preprocessing control
								emptyValuePolicy = if(!is.null(extraParams$emptyValuePolicy)) extraParams$emptyValuePolicy else preProcessingCtrl$emptyValuePolicy,
								fileEncoding = if(!is.null(extraParams$fileEncoding)) extraParams$fileEncoding else preProcessingCtrl$fileEncoding,
								temporalSplitColumn = preProcessingCtrl$temporalSplitColumn,
								
								#feature search parameters
								featureSearchMode = if(!is.null(extraParams$featureSearchMode)) extraParams$featureSearchMode else featureGenerationCtrl$featureSearchMode,
								functionsWhiteList = if(!is.null(extraParams$functionsWhiteList)) extraParams$functionsWhiteList else featureGenerationCtrl$functionsWhiteList, 
								functionsBlackList = if(!is.null(extraParams$functionsBlackList)) extraParams$functionsBlackList else featureGenerationCtrl$functionsBlackList, 
								booleanNumericFeatures = if(!is.null(extraParams$booleanNumericFeatures)) extraParams$booleanNumericFeatures else featureGenerationCtrl$booleanNumericFeatures,
								numericEqualityFeatures = if(!is.null(extraParams$numericEqualityFeatures)) extraParams$numericEqualityFeatures else featureGenerationCtrl$numericEqualityFeatures,
								allowRangeFeatures = if(!is.null(extraParams$allowRangeFeatures)) extraParams$allowRangeFeatures else featureGenerationCtrl$allowRangeFeatures,																
								crossRowFeatureSearch = if(!is.null(extraParams$crossRowFeatureSearch)) extraParams$crossRowFeatureSearch else featureGenerationCtrl$crossRowFeatureSearch,
								maxFeaturesCount = if(!is.null(extraParams$maxFeaturesCount)) extraParams$maxFeaturesCount else featureGenerationCtrl$maxFeaturesCount, 
								autoColumnSubSets = if(!is.null(extraParams$autoColumnSubSets)) extraParams$autoColumnSubSets else featureGenerationCtrl$autoColumnSubSets,
								customColumnSubsets = if(!is.null(extraParams$customColumnSubsets)) extraParams$customColumnSubsets else featureGenerationCtrl$customColumnSubsets,
								maxFeatureDuration = if(!is.null(extraParams$maxFeatureDuration)) extraParams$maxFeatureDuration else featureGenerationCtrl$maxFeatureDuration, 
								overrideMaxFeatureDurationForExternalData = if(!is.null(extraParams$overrideMaxFeatureDurationForExternalData)) extraParams$overrideMaxFeatureDurationForExternalData else featureGenerationCtrl$overrideMaxFeatureDurationForExternalData,
								useCachedFeatures = if(!is.null(extraParams$useCachedFeatures)) extraParams$useCachedFeatures else featureGenerationCtrl$useCachedFeatures,
								allocatedMemoryMB = if(!is.null(extraParams$allocatedMemoryMB)) extraParams$allocatedMemoryMB else featureGenerationCtrl$allocatedMemoryMB,
								maxCollectionSize = if(!is.null(extraParams$maxCollectionSize)) extraParams$maxCollectionSize else featureGenerationCtrl$maxCollectionSize,
								maxDepth = if(!is.null(extraParams$maxDepth)) extraParams$maxDepth else featureGenerationCtrl$maxDepth,
								
								#knowledge parameters
								useGraph = if(!is.null(extraParams$useGraph)) extraParams$useGraph else knowledgeCtrl$useGraph,
								useOpenStreetMap = if(!is.null(extraParams$useOpenStreetMap)) extraParams$useOpenStreetMap else knowledgeCtrl$useOpenStreetMap,
								useCustomGraphs = if(!is.null(extraParams$useCustomGraphs)) extraParams$useCustomGraphs else knowledgeCtrl$useCustomGraphs,								
								customGraphsWhiteList = if(!is.null(extraParams$customGraphsWhiteList)) extraParams$customGraphsWhiteList else knowledgeCtrl$customGraphsWhiteList,
								customGraphsBlackList = if(!is.null(extraParams$customGraphsBlackList)) extraParams$customGraphsBlackList else knowledgeCtrl$customGraphsBlackList,
								customFunctions = if(!is.null(extraParams$customFunctions)) extraParams$customFunctions else knowledgeCtrl$customFunctions,
								
								# model building parameters
								algorithmsWhiteList = if(!is.null(extraParams$algorithmsWhiteList)) extraParams$algorithmsWhiteList else modelBuildingCtrl$algorithmsWhiteList,
								evaluationMetric = if(!is.null(extraParams$evaluationMetric)) extraParams$evaluationMetric else modelBuildingCtrl$evaluationMetric,
								crossValidation = if(!is.null(extraParams$crossValidation)) extraParams$crossValidation else modelBuildingCtrl$crossValidation,
								
								#reporting parameters
								produceFeatureClusteringReport = if(!is.null(extraParams$produceFeatureClusteringReport)) extraParams$produceFeatureClusteringReport else reportingCtrl$produceFeatureClusteringReport,
								produceReports = if(!is.null(extraParams$produceReports)) extraParams$produceReports else reportingCtrl$produceReports,
								scoreOnTestSet = if(!is.null(extraParams$scoreOnTestSet)) extraParams$scoreOnTestSet else reportingCtrl$scoreOnTestSet,
								emailForNotification = reportingCtrl$emailForNotification,
								
								externalPrefixPath = getSBserverIOfolder()
	)
	
	verifyList = function(l) {if(is.vector(l) && !is.na(l)) as.list(l) else l}
	params$algorithmsWhiteList = verifyList(params$algorithmsWhiteList)
	params$functionsWhiteList = verifyList(params$functionsWhiteList)
	params$functionsBlackList = verifyList(params$functionsBlackList)
	params$maxFeaturesCount = verifyList(params$maxFeaturesCount)
	params = params[!is.na(params)]
	
	print (paste("Training on ",params$trainingFilePath))
	
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
	session = Session(artifact_loc = res$artifactPath, 
							modelBuilt = !(length(params$algorithmsWhiteList) == 1 && 
								tolower(params$algorithmsWhiteList[[1]]) == "zeror"),
							jobId = if(is.null(res$jobId)) -1 else res$jobId
					)
	
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

	if (runBlocking)session$waitForProcess()
 
	session$modelBuilt = TRUE
	return(session)
}

#################################################################

# Depreacted 
learn.file = function(...) { 
	l = list(...)
	trainData = if(!is.null(l$trainDataFilename)) l$trainDataFilename else stop("no training data was provided")
	testData = if(!is.null(l$testDataFilename)) l$testDataFilename else NA
	session = do.call(learn,list(trainData = trainData, testData=testData, ...))
	session$modelBuilt = TRUE
	session
}

# Depreacted 
featureSearch = function(...) { 
	session = do.call(learn,list(algorithmsWhiteList = list("ZeroR"), ...))	
	session$modelBuilt = FALSE
	session
}

# Depreacted 
featureSearch.file = function(...) { 
	l = list(...)
	trainData = if(!is.null(l$trainDataFilename)) l$trainDataFilename else stop("no training data was provided")
	session = do.call(learn,list(trainData = trainData, algorithmsWhiteList = list("ZeroR"), ...))
	session$modelBuilt = FALSE
	session
}

# PUT("http://localhost:9000/api2/fileUpload/test/guy.tsv", body = "header1\theader2\n")

#' writeToServer
#' 
#' A function to write a dateframe to the server. Useful for passing a dataframe to the server for feature search / learning purposes.
#' @param data Data frame or table to export to the server.
#' @param filename: Optional. define a name to save the data to. NA by default.
#' @return A filepath to the file on the server that was created.
writeToServer = function(data, filename = NA, prefix = "data_in"){ #TODO: deal with spaces in prefix
	final_filename = if ("data.frame" %in% class(data)){ # we got a data.frame object to be written to server 
		if (is.na(filename)) { # no specific name was provided - use digest to refrain from rewriting to server
				hash = digest(data)
				new_filename = paste0(getSBserverIOfolder(), prefix, "_", hash, ".tsv") 
				new_filenamee = gsub("/+", "/", new_filename)
				if (!file.exists(new_filename)) writeToFile(data, new_filename) #due to hashing we rewrite file only if data has changed
				new_filename		
			} else {			# specific name was provided - rewrite file to server
				new_filename = paste0(getSBserverIOfolder(), filename)
				new_filename = gsub("/+", "/", new_filename)
				writeToFile(data, new_filename)
				new_filename
			}
	} else if (class(data) == "character") { # a filename was provided as data - check if exists and return it
		SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
		if (!grepl(SBdir, data)) data = paste0(getSBserverIOfolder(), data)
		if (!file.exists(data)) stop(print(paste("Provided path:", data, "does not exist")))
		data
	} else stop("No valid data.frame or filename was provided")
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