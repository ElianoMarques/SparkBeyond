# S3 functions (De facto constructors of Session)


#' problemDefinitionControl
#' 
#' @param weightColumn: Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param weightByClass: Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param trainTestSplitRatio: Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param temporalSplitColumn: Optional. A column name containing temporal information by which the data will be splitted to train and test based on trainTestSplitRatio.  
problemDefinitionControl = function(
	regressionMode = NA,
	weightColumn = NA,
	weightByClass = FALSE,
	trainTestSplitRatio = 0.8,
	temporalSplitColumn = NA
){
	list(
		trainTestSplitRatio = trainTestSplitRatio,
		temporalSplitColumn = temporalSplitColumn,
		weightByClass = weightByClass,
		weightColumn = weightColumn
	)
}

#' preProcessingControl
#' 
#' @param emptyValuePolicy: Controls how empty values in the data are being handled NA by default will replace numeric value with median value, and strings with empty strings.
#' @param fileEncoding: Optional. Options are: "ISO-8859-1", "UTF-8", "US-ASCII". NA by default will try to automatically find the best encoding.
#' @param fileEscaping: Define how escaping (e.g., \\n) should be handled when files are parsed.
preProcessingControl = function(
	emptyValuePolicy = NA,
	fileEncoding = NA,
	fileEscaping = TRUE
) {
	list(
		emptyValuePolicy = emptyValuePolicy,
		fileEncoding = fileEncoding,
		fileEscaping = fileEscaping
	)
}

#' featureGenerationControl
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
	useWeather = FALSE,
	useCustomGraphs = FALSE,	
	customGraphsWhiteList = NA,
	customGraphsBlackList = NA,
	customFunctions = NA
	) {
		list(
			useGraph = useGraph,
			useWeather = useWeather,
			useOpenStreetMap = useOpenStreetMap,
			useCustomGraphs = useCustomGraphs,			
			customGraphsWhiteList = customGraphsWhiteList,
			customGraphsBlackList = customGraphsBlackList,
			customFunctions = customFunctions
		)
}


#' modelBuildingControl
#' 
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. Uses \code{\link{algorithmsList}}
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". NA by default automatically selects AUC for classification and RMSE for regression.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
modelBuildingControl = function(
	algorithmsWhiteList = algorithmsList(), 
	evaluationMetric = NA,
	crossValidation = 5
) {
	list(
		algorithmsWhiteList = algorithmsWhiteList,
		evaluationMetric = evaluationMetric,
		crossValidation = crossValidation
	)
}

#' algorithmsList
#' 
#' @param isClassification A boolean indicator for whether to use only classification algorithms or only regression algorithms. If is NA the relevant algorithms are selected based on the target type and cardinality using the learning process. 
#' @param randomForest random forest algorithm from the randomForest package.
#' @param xgBoost eXtreme gradient boosted machine algorithm from the xgBoost package.  (currently does not support multiclass)
#' @param GBM gradient boosten machine algorithm from the xgBoost package.  
#' @param rpart decision tree algorithm from the rpart package.  
#' @param lassoGlmnet logistic regression / linear regression algorithm with alpha = 0 from the glmnet package.  
#' @param ridgeGlmnet logistic regression / linear regression algorithm with alpha = 1 from the glmnet package.  
#' @param linearRegression linear regression algorithm. lm from the base package.  
#' @param linearEnsemble A linear ensemble of a collection of GBM and rpart algorithms.
#' @param stackingEnsemble A stacking ensemble of GBM as meta-model that ensmebles a collection of GBM and rpart algorithms.
algorithmsList = function(
		isClassification = NA,
		randomForest = TRUE,
		xgBoost = FALSE,
		GBM = FALSE,
		rpart = FALSE,
		lassoGlmnet = FALSE,
		ridgeGlmnet = FALSE,
		linearRegression = FALSE,
		linearEnsemble = FALSE,
		stackingEnsemble = FALSE
	){
		algsList = vector()
		if (randomForest) algsList = c(algsList, "RRandomForest")
		if (xgBoost) algsList = c(algsList, "RXGBoost")
		if (GBM) algsList = c(algsList, "RCaretGBM")
		if (rpart) algsList = c(algsList, "RRpartDecisionTree")
		if (lassoGlmnet && is.na(isClassification)) algsList = c(algsList, "RLassoL")
		if (ridgeGlmnet && is.na(isClassification)) algsList = c(algsList, "RRidgeL")
		if (linearEnsemble) algsList = c(algsList, "RLinearEnsembleGBM_with_Rpart")
		if (stackingEnsemble) algsList = c(algsList, "RStackingEnsembleGBM_of_GBM_with_Rpart")
		
		if (!is.na(isClassification) && isClassification == TRUE) algsList = paste0(algsList, "Classifier")
		if (!is.na(isClassification) && isClassification == FALSE) algsList = paste0(algsList, "Regressor")
		
		if (linearRegression) algsList = c(algsList, "RLinearRegressionLMRegressor")
		
		if (!is.na(isClassification)){
			if (lassoGlmnet && isClassification == TRUE) algsList = c(algsList, "RLassoLogisticRegressionGlmnetClassifier")
			if (ridgeGlmnet && isClassification == TRUE) algsList = c(algsList, "RRidgeLogisticRegressionGlmnetClassifier")
			if (lassoGlmnet && isClassification == FALSE) algsList = c(algsList, "RLassoLinearRegressionGlmnetClassifier")
			if (ridgeGlmnet && isClassification == FALSE) algsList = c(algsList, "RRidgeLinearRegressionGlmnetClassifier")
		}	
		
		algsList
}

#' reportingContorl
#' 

#' @param produceFeatureClusteringReport: An indicator to produce feature cluster visualization. FALSE by default.
#' @param produceReports: "EVALUATED_FUNCTIONS".
#' @param scoreOnTestSet: Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param emailNotification: An optional email to notify when the learning is finished.
#' @param showWebView: control for whether to show a dynamic web view of the analysis in a browser.
reportingControl = function(
	produceFeatureClusteringReport = FALSE,
	produceReports = NA,
	scoreOnTestSet = FALSE,
	emailForNotification = NA,
	showWebView = FALSE
) {
	list(
		produceFeatureClusteringReport = produceFeatureClusteringReport,
		produceReports = produceReports,
		scoreOnTestSet = scoreOnTestSet,
		emailForNotification = emailForNotification,
		showWebView = showWebView
	)
}

#' contextTypesList
#' 
#' @param geoSpatial. This context allows looking for geo spatial features. All rows are indexed based on their coordinate information, allowing properties from the surrounding environment to be searched for in a KNN-fashion. A coordinate column is required for this context object to be created.
#' @param graph. This context allows looking for features on a graph or a network. This context requires defining the edges in the graph by setting the source column and target column in the contextObject.
#' @param invertedIndex. This context creates an inverted index out of text column provided in a context. The properties of each text can be search for in a KNN-fashion. A text column is required for this context object.
#' @param lookupTables. This context creates a lookup table for each key column using all other columns as properties that can be associated with the key. The key column should be unique and can be defined in the keyColumns parameter.
#' @param membershipSet. This context creates a bag of elements and supports the "contain" operation for the set. 
#' @param shapeFile. This context allows providing shape files. The structure of the input should be the location of the shapeFile, one per row.
#' @param termsMap. This context associates a string / text column with some numeric value that can be looked at for every term. 
#' @param timeSeries. This context creates a single time series per column for all the rows provided. The context is sorted by the time/data column. It is necessary that at least one time window column should appear in the data. It is optional to set the time column in the contextObject.
#' @param timeSeriesMap. This context creates multiple time series per column, group by a key. This object requires a keyed time window column to be defined in the main text. The key and time column may be set in the contextObject. 
contextTypesList = function(
		geoSpatial = FALSE,
		#geoSpatialWithPartition = FALSE,
		#geoTiff = FALSE,
		graph = FALSE,
		invertedIndex = FALSE,
		lookupTables = FALSE,
		membershipSet = FALSE, 
		shapeFile = FALSE,
		termsMap = FALSE,
		timeSeries = FALSE,
		timeSeriesMap = FALSE
	){
		contextList = vector()
		if (geoSpatial) contextList = c(contextList, "GeoSpatial")
		if (graph) contextList = c(contextList, "Graph")
		if (invertedIndex) contextList = c(contextList, "InvertedIndex")
		if (lookupTables) contextList = c(contextList, "LookupTables")
		if (membershipSet) contextList = c(contextList, "MembershipSet")
		if (shapeFile) contextList = c(contextList, "ShapeFile")
		if (termsMap) contextList = c(contextList, "TermsMap")
		if (timeSeries) contextList = c(contextList, "TimeSeries")
		if (timeSeriesMap) contextList = c(contextList, "TimeSeriesMap")
		as.list(contextList)
}

#' contextObject
#' 
#' @param data: a data frame or a path to a file containing the context data
#' @param name: an identifier for the context object to be created (optional).
#' @param keyColumns: Specify the key columns to be used by the context object (optional).
#' @param timeColumn: Specify the time column to be used by the context object (relevant in time series contexts) (optional).
contextObject = function(data, contextTypes=NULL, name = NULL, keyColumns = list(), timeColumn = NULL, graphSourceNodeColumn = NULL,graphTargetNodeColumn= NULL ) { #TODO: help
	obj = list(data = data, contextTypes = contextTypes, name=name, keyColumns = keyColumns, timeColumn=timeColumn, graphSourceNodeColumn=graphSourceNodeColumn, graphTargetNodeColumn=graphTargetNodeColumn)
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
#' @param contextDatasets: Optional. A list of paths to context datasets to be added to the learning.
#' @param problemDefinition: A \code{\link{problemDefinitionControl}} object with specific problem definition parameters.
#' @param preProcessingCtrl: A \code{\link{preProcessingControl}} object with specific preprocessing parameters.
#' @param featureGenerationCtrl: A \code{\link{featureGenerationControl}} object with specific feature generation parameters.
#' @param knowledgeCtrl: A \code{\link{knowledgeControl}} object with specific external knowledge parameters.
#' @param modelBuildingCtrl: A \code{\link{modelBuildingControl}} object with specific model building parameters.
#' @param reportingCtrl: A \code{\link{reportingControl}} object with specific reporting parameters.
#' @param runBlocking: Block the R console while the session is running. FALSE by default.
#' @return Session object that encapsulates the model.
#' @examples
#' #session = learn("titanic", getData("titanic_train"), target = "survived", algorithmsWhiteList = list("RRandomForest"), runBlocking = TRUE)

# supportThreshold
# localTopFeatureCount
# regressionDiscretizerBinsOverride
# automaticSelectionOfNumberOfFeatures
# regressionMode
# empty value policy
# linesForTypeDetection

learn <- function(
			 projectName,
			 trainData,
			 target,
			 testData = NULL,
			 contextDatasets = NULL,
			 problemDefinition = problemDefinitionControl(),
			 preProcessingCtrl = preProcessingControl(),
			 featureGenerationCtrl = featureGenerationControl(),
			 knowledgeCtrl = knowledgeControl(),
			 modelBuildingCtrl = modelBuildingControl(),
			 reportingCtrl = reportingControl(),
			 runBlocking = TRUE,
			 ...
){
	isLatestRpackage()
	extraParams = list(...)
	# TODO: verify that there are no supurious parameters, e.g. (projectname instead of projectName)
	remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
	if(remoteMode && !currentUser(FALSE)) stop("Please login before calling the learn function. Thank you.")
	
	projectName = gsub(" ", "_", projectName)
	
	if(is.null(trainData) && !is.null(extraParams$trainData)) trainData = extraParams$trainData
	trainTestSplitRatio = if(!is.null(extraParams$trainTestSplitRatio)) extraParams$trainTestSplitRatio else problemDefinition$trainTestSplitRatio
	
	if (!is.null(testData) && !is.na(trainTestSplitRatio)) print ("Note: test data was provided - ignoring trainTestSplitRatio defintion.")	
	
	url <- paste0(getSBserverDomain(),"/rapi/learn")
	print(paste("Calling:", url))
	
	if (!is.null(contextDatasets)) { #writing context data to server if necessary
		if (class(contextDatasets) != "list") {
			warning("contextDatasets should be a list")
			if (class(contextDatasets) != "contextObject") error("contextDatasets are not of class contextObject")
			contextDatasets=list(contextDatasets)
		}
		if (!all(sapply(contextDatasets, function(x) class(x) == "contextObject"))) stop("Not all provided context objects are of type 'contextObject'")
		for (i in 1:length(contextDatasets)) {
			contextName = ifelse(!is.null(contextDatasets[[i]]$name), paste0("_", contextDatasets[[i]]$name),"")
			contextDatasets[[i]]$data = 
				ifelse(!remoteMode,
					writeToServer(contextDatasets[[i]]$data, 
						prefix = paste0(projectName,"_context", contextName),
						useEscaping = preProcessingCtrl$fileEscaping
					),
					uploadToServer(data = contextDatasets[[i]]$data, projectName = projectName, name = paste0("context", contextName)
												 , useEscaping = preProcessingCtrl$fileEscaping)
			)
		}
	}
	
	params <-list(
		projectName = projectName,
		trainingFilePath = 
			ifelse (!remoteMode,
					writeToServer(trainData, prefix = paste0(projectName,"_train"), useEscaping = preProcessingCtrl$fileEscaping),
					{
						uploadedPath = uploadToServer(data = trainData,projectName = projectName, name = "train"
																					, useEscaping = preProcessingCtrl$fileEscaping)
						if(is.na(uploadedPath)) stop("failed to upload training file to server")
						uploadedPath
					}
		),
		target = target,
		testFilePath = 
			ifelse (!is.null(testData) && (any(grep("data.frame", class(testData))) || class(testData)=="character"),
				ifelse(!remoteMode,
						writeToServer(testData, prefix = paste0(projectName,"_test"), useEscaping = preProcessingCtrl$fileEscaping),
						uploadToServer(data = testData,projectName = projectName, name = "test", useEscaping = preProcessingCtrl$fileEscaping)	
				),
				NA
		),	
		
		contextDatasets = contextDatasets,
				
		#problem definition
		regressionMode = if(!is.null(extraParams$regressionMode)) extraParams$regressionMode else problemDefinition$regressionMode,
		trainTestSplitRatio = trainTestSplitRatio,
		temporalSplitColumn = if(!is.null(extraParams$temporalSplitColumn)) extraParams$temporalSplitColumn else problemDefinition$temporalSplitColumn,
		weightColumn = if(!is.null(extraParams$weightColumn)) extraParams$weightColumn else problemDefinition$weightColumn,
		weightByClass = if(!is.null(extraParams$weightByClass)) extraParams$weightByClass else problemDefinition$weightByClass,
		
		
		# preprocessing control
		emptyValuePolicy = if(!is.null(extraParams$emptyValuePolicy)) extraParams$emptyValuePolicy else preProcessingCtrl$emptyValuePolicy,
		fileEncoding = if(!is.null(extraParams$fileEncoding)) extraParams$fileEncoding else preProcessingCtrl$fileEncoding,
		fileEscaping = if(!is.null(extraParams$fileEscaping)) extraParams$fileEscaping else preProcessingCtrl$fileEscaping,
		
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
		useWeather = if(!is.null(extraParams$useWeather)) extraParams$useWeather else knowledgeCtrl$useWeather,
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
		
		externalPrefixPath = ifelse(!remoteMode, getSBserverIOfolder(), NA)
	)
	
	verifyList = function(l) {if(is.vector(l) && !is.na(l)) as.list(l) else l}
	params$algorithmsWhiteList = verifyList(params$algorithmsWhiteList)
	params$functionsWhiteList = verifyList(params$functionsWhiteList)
	params$functionsBlackList = verifyList(params$functionsBlackList)
	params$maxFeaturesCount = verifyList(params$maxFeaturesCount)
	params = params[!is.na(params)]
	
	print (paste("Training on ",params$trainingFilePath))
	
	body = rjson::toJSON(params)
	#if (verbose) print(body)
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
	
# 	if (autoSave) {
# 		tryCatch({
# 			tokens = strsplit(session$artifact_loc, "/")[[1]]
# 			varName = paste("backup", tokens[length(tokens)-1], tokens[length(tokens)], sep="_")
# 			saveFilename = paste0(getwd(),.Platform$file.sep,varName,".RData")
# 			assign(varName, session)
# 			base::save(list=varName, file=saveFilename) #auto-saving the model
# 			print (paste("auto saved Session object to a variable named '", varName,"'. To retrieve use:" ,paste0("load('",saveFilename,"').")))
# 		})
# 	}
	if (reportingCtrl$showWebView == TRUE && remoteMode == TRUE) session$webView() 
	if (runBlocking)session$waitForProcess(remoteMode=remoteMode)
 
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
