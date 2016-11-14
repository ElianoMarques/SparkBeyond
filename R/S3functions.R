# S3 functions (De facto constructors of Session)

#' timeWindowControl
#' 
#' @param dateCol The column name in training set that will be used.
#' @param window The window length (numeric)
#' @param unit The window length unit. Should be one of: "Seconds", "Minutes", "Hours", "Days", "Years"
#' @param keyCol An optional key for the sliding window (NULL as default).
#' @param relativeTime a boolean indicator for whether the time series should be coded with absolute timestamps or relative to the last point. In this case all time stamps will be negative using the defined time unit (e.g. -10 days). TRUE by default.
#' @param offset allows defining an additional time gap between that will be masked for the feature search. The entire time series will be shifted accordingly using the time window that was picked. 0 by default.
#' @param sample allows defining the maximum number of time points to be included in each time series. Random points are sampled from the time series to reduce the time series resolution in order to better capture global trends and increase runtime performance. By default set to 100.
#' @param allowHistoricalStats when true, statistics for various historical quantities of the corresponding key will be calculated over the entire history (e.g. the average historical price of an item since the beginning of time), as opposed to just within the time-window (e.g. the average price for an item within the past 3 days). TRUE by default."
#' @param naturalModality usefull if there are natural time cycle dependencies such as daily or monthly. Multi_Modal will cause the system to try them all and No_Modality to try none. Possible values: No_Modality, Hour_Of_Day, Day_Of_Week, Month_Of_Year, Multi_Modal. No_Modality by default."
#' @param applyModalityFilter a boolean indicator for whether to use only dates with the same modality as the date column, or search on both the whole window and the modal version. FALSE by default."
#' @return timeWindowControl object
timeWindowControl = function(dateCol, keyCol = NULL, window, unit = "Days", relativeTime = TRUE, sample = 100, offset = 0, allowHistoricalStats = TRUE, naturalModality = "No_Modality", applyModalityFilter = FALSE) {
	def = list(
		dateColumn = dateCol, 
		keyColumn = keyCol,
		windowSize = window, 
		timeUnit = unit, 
		relativeTime = relativeTime, 
		sampleSize = sample, 
		offsetFromTarget = offset,
		allowHistoricalStats = allowHistoricalStats,
		naturalModality = naturalModality,
		applyModalityFilter = applyModalityFilter
	)
	
	class(def) = "timeWindowControl"
	def
}

#' problemDefinitionControl
#' 
#' @param forceRegression Boolean. Force the problem to be regression problem instead of classification. Optional - NA by default will choose the best problem definition based on the target.
#' @param weightColumn Optional. String of the name of of one of the column that indicate a weighting that is assigned to each example. NA by default.
#' @param weightByClass Adds a weight column with values inverse proportional to the frequency of the class. FALSE by default.
#' @param trainTestSplitRatio Optional. Double value in [0,1] to split the train file data in order to keep some data for test. 0.8 by default. Ignored if test filename was provided.
#' @param temporalSplitColumn Optional. A column name containing temporal information by which the data will be splitted to train and test based on trainTestSplitRatio.
#' @param partitionColumn Optional. A column name containing information by which the data will be split to tain/test/validation set. Allowed values: "Train", "Validation", "Test".
#' @param timeWindowsDefinition Optional. List of \code{\link{timeWindowControl}} objects, used for creation of TimeSeries/TimeSeriesMap contexts.
problemDefinitionControl = function(
	forceRegression = NA,
	weightColumn = NA,
	weightByClass = FALSE,
	trainTestSplitRatio = 0.8,
	temporalSplitColumn = NA,
	partitionColumn = NA,
	timeWindowsDefinition = NA
){
	list(
		forceRegression = forceRegression,
		trainTestSplitRatio = trainTestSplitRatio,
		temporalSplitColumn = temporalSplitColumn,
		weightByClass = weightByClass,
		weightColumn = weightColumn,
		partitionColumn = partitionColumn,
		timeWindowsDefinition = timeWindowsDefinition
	)
}

#' preProcessingControl
#' 
#' @param fileEscaping Define how escaping (e.g., \\n) should be handled when files are parsed.
#' @param fileEncoding Optional. Options are: "ISO-8859-1", "UTF-8", "US-ASCII". NA by default will try to automatically find the best encoding.
#' @param linesForTypeDetection maximum number of lines that should be used for type detection. 10000 by default.
#' @param emptyValuePolicy Controls how empty values in numeric columns in the input data are being handled. DoubleNan is default see \code{\link{emptyValuePolicyList}}.
preProcessingControl = function(
	fileEscaping = TRUE,
	fileEncoding = NA,
	linesForTypeDetection = NA,
	emptyValuePolicy = NA
) {
	list(
		emptyValuePolicy = emptyValuePolicy,
		fileEncoding = fileEncoding,
		fileEscaping = fileEscaping
	)
}

#' emptyValuePolicyList
#' 
#' Defines how empty values in numeric columns in the original data are being handled.
#' Please note - only one of the following should be set to true
#' @param median Median value of a column
#' @param average Average value of a column
#' @param nan Not a number
#' @param negativeInfinity Negative infinity 
emptyValuePolicyList = function(
	median = FALSE,
	average = FALSE,
	nan = FALSE,
	doubleNan = FALSE,
	negativeInfinity = FALSE
) {
	policy = NA
	if (median) policy = "Median"
	if (average) policy = "Average"
	if (nan) policy = "NaN"
	if (doubleNan) policy = "DoubleNaN"
	if (negativeInfinity) policy = "NegativeInfinity"
	policy
}

#' featureSearchModeList
#' 
#' Smart presets for creating deep features and explore interactions.
#' Please note - only one of the following should be set to true
#' @param default This mode is the standard feature search mode that composes all possible function up to a predefined complexity which is based on the column type, specific functions combination, and the depth definition.
#' @param aggressiveWithPairs The genetic algorithm will perform a quick exploration of many potential pairs. 
#' @param advanced This mode will attempt to create more composite functions combination in places where a stronger signal was found - e.g., if an interim feature on time series data has shown signal in preliminary analysis, more effort will be made to exact additional signal by creating more complex expression which use or variant of that interim feature. 
#' @param advancedWithPairs This mode is similar to the one above with a focus on combining signals from pairs of interactions and producing more complex features from promising interim features pairs - please check "deep feature search with pairs example" in section G of the examples page.
#' @param digDeep This mode would follow more aggressively at looking for interactions between multiple interim features, and can attempt to create features using up to 5 fields. please check "deep feature search with many interactions" example in section G of the examples page.
featureSearchModeList = function(
	default = TRUE,
	aggressiveWithPairs = FALSE,
	advanced = FALSE,
	advancedWithPairs = FALSE,
	digDeep = FALSE
) {
	mode = "DEFAULT"
	if (advanced) mode = "ADVANCED"
	if (advancedWithPairs) mode = "ADVANCED_WITH_PAIRS"
	if (aggressiveWithPairs) mode = "AGGRESSIVE_WITH_PAIRS"
	if (digDeep) mode = "DIG_DEEP"
	mode
}

#' featureGenerationControl
#' 
#' @param maxFeaturesCount A list of integers indicating how many features should be created by the SparkBeyond engine. Optional 300 by default. Please note that if several feature cuts are defined they will be evaluated during the model building by cross validation and may result in increased running time. 
#' @param automaticSelectionOfNumberOfFeatures. A heuristic that aims to produce a one or more feature counts that will improve the cummulative RIG for the set of features. (All automatically produced cutoffs are guaranteed to be below the maximum limit defined in maxFeaturesCount). Please note that as several feature cuts can be produced and evaluated during the model building using cross validation, setting this feature to TRUE may result in increased running time. 
#' @param minSupportAbsolute Minimal support in absolute number of instances that any feature should have. 3 by default. Setting this parameter to a high value may result in less (than requested) features being produced. 
#' @param maxDepth Optional. Integer < 8 which represent the maximum number of transformations allowed during the feature search phase. Increasing this value should be considered with cautious as the feature search phase is exponential. 2 by default.
#' @param featureSearchMode Smart presets for creating deep features and explore interactions. See \code{\link{featureSearchModeList}} for details.
#' @param functionsWhiteList Optional. A list of strings that represents a set of functions that will be used to guide the feature search. NA by default.
#' @param functionsBlackList Optional. A list of strings that represents a set of function that will be excluded from the feature search. Can also include function domains including('math','arithmetics', 'collections', 'booleanOperators', 'semantics', 'nlp', 'trigonometry', 'bitwise'). NA by default.
#' @param localTopFeatureCount The maximal number of top features that should be created from a single column. 1000 by default.
#' @param regressionDiscretizerBinsOverride Define the bin boundaries that should be created for regresion problem bins. By default 6 bins will be created, with boundaries defined by equal mass (in order to not create a bias towards any of the bins). Should be a list of numbers spanning the entire target range. These bins will be used only for feature search purposes and not for model building. 
#' @param booleanNumericFeatures A boolean indicating whether to transform all features to boolean values. (i.e., when FALSE the continuous value of the feature left-hand-side will be passed to the algorithm, without taking into account the specific cutoff chosen during the feature search phase). NA by default indicating that it will be TRUE for classification problems and FALSE for regression problems.
#' @param numericEqualityFeatures A boolean indicator for whether to include features that compare numeric fields with specific values. TRUE by default.
#' @param allowRangeFeatures A boolean indicator for whether to include features that define range over a set of numeric values. TRUE by default.
#' @param useRawNumericColumns Use the original numeric columns as features when building the model. This will automatically attempt to also build a model based only on the numeric columns with applying additional transformations on the data. FALSE by default.
#' @param crossRowFeatureSearch. A booleean indicating whether to allow creating features using data collected from multiple rows together.FALSE by default.
#' @param autoColumnSubSets Optional. A list of values contain any of the following: "CONCEPT", "NUMERIC_PAIRS", "ALL_PAIRS". "CONCEPT" will aim to generate column subset from fields that are from similar non-numeric types or combination of date and non-numeric elements. "NUMERIC_PAIRS" will create all column subsets for numeric columns. "ALL_PAIRS" will create subsets of size 2 from all columns. "CONCEPT" by default.
#' @param customColumnSubsets Optional. A List of lists containing specific column subsets to examine. In order to set a certain depth to the subset, add an element in the end of the customSubSet with one digit as a string representing the requested depth. If not such element was defined the regular depth definitions will be used. NA by default.
#' @param maxFeatureDuration Optional. A numeric value representing the maximum allowed time a feature may take during search per row in milliseconds. 100 by default.
#' @param overrideMaxFeatureDurationForExternalData A boolean indicating whether the maxFeatureDuration parameter should not be used for features that use external data. TRUE by default.
#' @param allocatedMemoryMB Optional. Integer value representing how to chunk the memory during feature search . 1000MB by default.
#' @param maxCollectionSize Optional. Integer  value repsenting what is the maximum cardinality allowed for a transformation during feature search. NA by default corresponding to 200K.
#' @param useCachedFeatures Optional. A boolean indicating whether to use cached features (from previous run). TRUE by default.
featureGenerationControl = function(
	maxFeaturesCount = list(300),
	automaticSelectionOfNumberOfFeatures = FALSE,
	minSupportAbsolute = NA,
	maxDepth = 2.5,
	featureSearchMode = featureSearchModeList(), 
	functionsWhiteList = NA,
	functionsBlackList = NA,
	localTopFeatureCount = NA,
	regressionDiscretizerBinsOverride = NA, 
	booleanNumericFeatures = NA,
	numericEqualityFeatures = TRUE,
	allowRangeFeatures = TRUE,
	useRawNumericColumns = FALSE,

	crossRowFeatureSearch = FALSE,
	autoColumnSubSets = list("CONCEPT"),
	customColumnSubsets = NA,
	maxFeatureDuration = 100,
	overrideMaxFeatureDurationForExternalData = TRUE,
	allocatedMemoryMB = 1000,
	maxCollectionSize = NA,
	useCachedFeatures = TRUE
) {
	list(
		maxFeaturesCount = maxFeaturesCount,
		minSupportAbsolute = minSupportAbsolute,
		maxDepth = maxDepth,
		featureSearchMode = featureSearchMode,
		functionsWhiteList = functionsWhiteList,
		functionsBlackList = functionsBlackList,
		localTopFeatureCount = localTopFeatureCount,
		regressionDiscretizerBinsOverride = ifelse (is.na(regressionDiscretizerBinsOverride), NA, as.list(regressionDiscretizerBinsOverride)),
		booleanNumericFeatures = booleanNumericFeatures,
		numericEqualityFeatures = numericEqualityFeatures,
		allowRangeFeatures = allowRangeFeatures,
		useRawNumericColumns = useRawNumericColumns,
		
		crossRowFeatureSearch = crossRowFeatureSearch,
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
#' @param linkedDataCore Includes DBPedia, Yago2, Wordnet and OpenLibrary. DBpedia is a crowd-sourced community effort to extract structured information from Wikipedia. Matches training data by Text elements.
#' @param openStreetMap OpenStreetMap (OSM) is a collaborative project to create a free editable map of the world. Matches training data by location (coordinate).
#' @param weather Weather data from NOAA Climate.gov. Matches the training data by date and location.
#' @param usCensus Experimental: The United States Census decennial census. Matches training data by Zipcode.
#' @param news Experimental: News data from the GDELT Project that monitors the world's broadcast, print, and web news. Matches the training data based on a date, location or both.
#' @param worldBank Experimental: Historical values of the World Bank Global Development Indicators (WDI) by country. Matches the training data by a country.
#' @param twitter Experimental: information from tweets that appeared on a certain date (based on dates in the data). (Limited by the maximum allowed Twitter quota per day).
#' @param searchEngines Experimental: Use search engines to find matching datasets. Matches training data by textual elements
#' @param customDatasets Custom datasets current include include US zipcode information with 44 parameters, UK postcode information, data on the largest 1000 cities and airport information. 
knowledgeControl = function(
	linkedDataCore = FALSE,
	openStreetMap = FALSE,
	weather = FALSE,
	usCensus = FALSE,
	news = FALSE,
	worldBank = FALSE,
	twitter = FALSE, 
	searchEngines = FALSE,
	customDatasets = FALSE
	
	#useCustomGraphs = FALSE,	
	#customGraphsWhiteList = NA,
	#customGraphsBlackList = NA
) {
	# customGraphsWhiteList A list that filters which domains should be used. NA by default.
	# customGraphsBlackList A list that filters which custom graphs should be ignored. NA by default.
	# customFunctions A list of additional functions that should be incorporated to the feature search. NA by default.
	customDatasets = customDatasets ||
		weather || usCensus || news || worldBank || twitter || searchEngines
	
	list(
		linkedDataCore = linkedDataCore,
		openStreetMap = openStreetMap,
		weather = weather,
		usCensus = usCensus,
		news = news,
		worldBank = worldBank,
		twitter = twitter,
		searchEngines = searchEngines,
		customDatasets = customDatasets
	
		#customGraphsWhiteList = customGraphsWhiteList,
		#customGraphsBlackList = customGraphsBlackList,
		#customFunctions = customFunctions
	)
}


#' modelBuildingControl
#' 
#' @param algorithmsWhiteList: Optional. A list of strings that represents the set of algorithms to run. Uses \code{\link{algorithmsList}}
#' @param extraModels: Optional. A named list of algorithms to run, along with the hyperparameters to set for these algorithms.
#' @param evaluationMetric: Optional. A string representing the evaluation metric. Should be either "AUC", "PREC", or "RMSE". NA by default automatically selects AUC for classification and RMSE for regression.
#' @param crossValidation: Optional. Integer value representing how many cross validation splits should be used. 5 by default.
#' @param maxRecordsForModelBuild: The maximal number of records to use for model training. 100K by default. 
modelBuildingControl = function(
	algorithmsWhiteList = algorithmsList(),
	extraModels = list(),
	evaluationMetric = NA,
	crossValidation = 5,
	maxRecordsForModelBuild = NA
) {
	list(
		algorithmsWhiteList = algorithmsWhiteList,
    extraModels = extraModels,
		evaluationMetric = evaluationMetric,
		crossValidation = crossValidation,
		maxRecordsForModelBuild = maxRecordsForModelBuild
	)
}

#' algorithmsList
#' 
#' @param isClassification A boolean indicator for whether to use only classification algorithms or only regression algorithms. If is NA the relevant algorithms are selected based on the target type and cardinality using the learning process. 
#' @param randomForest random forest algorithm from the randomForest package.
#' @param zeroR predicts the mean (for a numeric class) or the mode (for a nominal class). This method requires almost no computation and will be the fastest to apply.
#' @param xgBoost eXtreme gradient boosted machine algorithm from the xgBoost package.  (currently does not support multiclass)
#' @param GBM gradient boosted machine algorithm from the xgBoost package.  
#' @param rpart decision tree algorithm from the rpart package.  
#' @param lassoGlmnet logistic regression / linear regression algorithm with alpha = 0 from the glmnet package.  
#' @param ridgeGlmnet logistic regression / linear regression algorithm with alpha = 1 from the glmnet package.  
#' @param linearRegression linear regression algorithm. lm from the base package.  
#' @param linearEnsemble A linear ensemble of a collection of GBM and rpart algorithms.
#' @param stackingEnsemble A stacking ensemble of GBM as meta-model that ensmebles a collection of GBM and rpart algorithms.
#' @param naiveBayes.weka Classification algorithm. A naive Bayes classifier is a simple probabilistic classifier based on applying Bayes' theorem with strong independence assumptions.
#' @param bagging.weka bagging a classifier to reduce variance Can do classification and regression.
#' @param votedPerceptron.weka Classification algorithm. The perceptron is an algorithm for supervised classification of an input into one of several possible non-binary outputs.
#' @param classificationViaClustering.weka A simple meta-classifier that uses a clusterer for classification. For cluster algorithms that use a fixed number of clusterers.
#' @param classificationViaRegression.weka Doing classification using regression methods. Class is binarized and one regression model is built for each class value.
#' @param randomSubSpace.weka Constructs a decision tree based classifier that maintains highest accuracy on training data and improves on generalization accuracy as it grows in complexity. The classifier consists of multiple trees constructed systematically by pseudorandomly selecting subsets of components of the feature vector, that is, trees constructed in randomly chosen subspaces.
#' @param bayesNet.weka A Bayes Network classifier. Provides datastructures (network structure, conditional probability distributions, etc.) and facilities common to Bayes Network learning algorithms like K2 and B.
#' @param libSVM.weka Wrapper for the libSVM library, an integrated software for support vector classification, (C-SVC, nu-SVC), regression (epsilon-SVR, nu-SVR) and distribution estimation (one-class SVM). It supports multi-class classification.
#' @param adaBoostM1.weka Algorithm for boosting a nominal class classifier using the Adaboost M1 method. Only nominal class problems can be tackled.
#' @param decisionTable.weka Algorithm for building and using a simple decision table majority classifier.
#' @param smo.weka Implements John Platt's sequential minimal optimization algorithm for training a support vector classifier.
#' @param repTree.weka Fast decision tree learner. Builds a decision/regression tree using information gain/variance and prunes it using reduced-error pruning (with backfitting).
#' @param smo.regression.weka SMOreg implements the support vector machine for regression.A regression scheme that employs any classifier on a copy of the data that has the class attribute (equal-width) discretized. 
#' @param regressionByDiscretization.weka A regression scheme that employs any classifier on a copy of the data that has the class attribute (equal-width) discretized. 
algorithmsList = function(
		isClassification = NA,
		randomForest = TRUE,
		zeroR = FALSE,
		xgBoost = FALSE,
		GBM = FALSE,
		rpart = FALSE,
		lassoGlmnet = FALSE,
		ridgeGlmnet = FALSE,
		linearRegression = FALSE,
		linearEnsemble = FALSE,
		stackingEnsemble = FALSE,
		naiveBayes.weka = FALSE,
		bagging.weka = FALSE,
		votedPerceptron.weka = FALSE,
		classificationViaClustering.weka = FALSE,
		classificationViaRegression.weka = FALSE,
		randomSubSpace.weka = FALSE,
		bayesNet.weka = FALSE,
		libSVM.weka = FALSE,
		adaBoostM1.weka = FALSE,
		decisionTable.weka = FALSE,
		smo.weka = FALSE,
		repTree.weka = FALSE,
		smo.regression.weka = FALSE,
		regressionByDiscretization.weka = FALSE
	){
		algsList = vector()
		if (randomForest) algsList = c(algsList, "RRandomForestClassifier", "RRandomForestRegressor")
		if (xgBoost) algsList = c(algsList, "RXGBoostClassifier" , "RXGBoostRegressor")
		if (GBM) algsList = c(algsList, "RCaretGBMClassifier","RCaretGBMRegressor")
		if (rpart) algsList = c(algsList, "RRpartDecisionTreeClassifier", "RRpartDecisionTreeRegressor")
		if (lassoGlmnet) algsList = c(algsList, "RLassoLogisticRegressionGlmnetClassifier", "RLassoLinearRegressionGlmnetRegressor")
		if (ridgeGlmnet) algsList = c(algsList, "RRidgeLogisticRegressionGlmnetClassifier",  "RRidgeLinearRegressionGlmnetRegressor")
		if (linearEnsemble) algsList = c(algsList, "RLinearEnsembleGBM_with_RpartClassifier", "RLinearEnsembleGBM_with_RpartRegressor")
		if (stackingEnsemble) algsList = c(algsList, "RStackingEnsembleGBM_of_GBM_with_RpartClassifier", "RStackingEnsembleGBM_of_GBM_with_RpartRegressor")

		if (linearRegression) algsList = c(algsList, "RLinearRegressionLMRegressor")
		
		toKeep = if (!is.na(isClassification)){
			if (isClassification == TRUE) which(grepl("Classifier", algsList))
			else if (isClassification == FALSE) which(grepl("Regressor", algsList))
		} else NULL
	
		if (!is.null(toKeep)) algsList = algsList[toKeep]
		
		if (naiveBayes.weka) algsList = c(algsList, "weka.classifiers.bayes.NaiveBayes")
		if (bagging.weka) algsList = c(algsList, "weka.classifiers.meta.Bagging(-I 20)")
		if (votedPerceptron.weka) algsList = c(algsList, "weka.classifiers.functions.VotedPerceptron")
		if (classificationViaClustering.weka) algsList = c(algsList, "weka.classifiers.meta.ClassificationViaClustering")
		if (classificationViaRegression.weka) algsList = c(algsList, "weka.classifiers.meta.ClassificationViaRegression")
		if (randomSubSpace.weka) algsList = c(algsList, "weka.classifiers.meta.RandomSubSpace")
		if (bayesNet.weka) algsList = c(algsList, "weka.classifiers.bayes.BayesNet")
		if (libSVM.weka) algsList = c(algsList, "weka.classifiers.functions.LibSVM")
		if (adaBoostM1.weka) algsList = c(algsList, "weka.classifiers.meta.AdaBoostM1")
		if (decisionTable.weka) algsList = c(algsList, "weka.classifiers.rules.DecisionTable")
		if (smo.weka) algsList = c(algsList, "weka.functions.SMO")
		if (repTree.weka) algsList = c(algsList, "weka.classifiers.trees.REPTree(-M 5)")
		if (smo.regression.weka) algsList = c(algsList, "weka.classifiers.functions.SMOreg")
		if (regressionByDiscretization.weka) algsList = c(algsList, "weka.classifiers.meta.RegressionByDiscretization(-B 3 -W weka.classifiers.trees.RandomForest)")
		if (zeroR) algsList = c(algsList, "ZeroR")
		algsList
}

#' reportingContorl
#' 
#' @param showWebView control for whether to show a dynamic web view of the analysis in a browser.
#' @param emailNotification An optional email to notify when the learning is finished.
#' @param scoreOnTestSet Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.
#' @param featureClustersReport Produce feature cluster visualization. FALSE by default.
#' @param featureVisualizations Show visual distribution of the feature against the target for classification problems. FALSE by default.
#' @param evaluatedFunctionsReport Creates a report with the entire list of functions that were evaluated. For contextObjects will also show for each object which functions directly used the contextObject.
reportingControl = function(
	showWebView = TRUE,
	emailForNotification = NA,
	scoreOnTestSet = FALSE,
	featureClustersReport = FALSE,
	featureVisualizations = FALSE,
	evaluatedFunctionsReport = FALSE
) {
	list(
		featureClustersReport = featureClustersReport,
		eevaluatedFunctionsReport = evaluatedFunctionsReport,
		scoreOnTestSet = scoreOnTestSet,
		featureVisualizations = featureVisualizations,
		emailForNotification = emailForNotification,
		showWebView = showWebView
	)
}

#' contextTypesList
#' 
#' @param geoSpatial This context allows looking for geo spatial features. All rows are indexed based on their coordinate information, allowing properties from the surrounding environment to be searched for in a KNN-fashion. A coordinate column is required for this context object to be created.
#' @param graph This context allows looking for features on a graph or a network. This context requires defining the edges in the graph by setting the source column and target column in the contextObject.
#' @param invertedIndex This context creates an inverted index out of text column provided in a context. The properties of each text can be search for in a KNN-fashion. A text column is required for this context object.
#' @param lookupTables This context creates a lookup table for each key column using all other columns as properties that can be associated with the key. The key column should be unique and can be defined in the keyColumns parameter.
#' @param membershipSet This context creates a bag of elements and supports the "contain" operation for the set. 
#' @param osmFile This context allows providing OSM files. The structure of the input should be the location of the osmFile, one per row.
#' @param shapeFile This context allows providing shape files. The structure of the input should be the location of the shapeFile, one per row.
#' @param termsMap This context associates a string / text column with some numeric value that can be looked at for every term. 
#' @param timeSeries This context creates a single time series per column for all the rows provided. The context is sorted by the time/data column. It is necessary that at least one time window column should appear in the data. It is optional to set the time column in the contextObject.
#' @param timeSeriesMap This context creates multiple time series per column, group by a key. This object requires a keyed time window column to be defined in the main text. The key and time column may be set in the contextObject. 
contextTypesList = function(
		geoSpatial = FALSE,
		#geoSpatialWithPartition = FALSE,
		#geoTiff = FALSE,
		graph = FALSE,
		invertedIndex = FALSE,
		lookupTables = FALSE,
		membershipSet = FALSE, 
		osmFile = FALSE,
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
		if (osmFile) contextList = c(contextList, "OSMFile")
		if (shapeFile) contextList = c(contextList, "ShapeFile")
		if (termsMap) contextList = c(contextList, "TermsMap")
		if (timeSeries) contextList = c(contextList, "TimeSeries")
		if (timeSeriesMap) contextList = c(contextList, "TimeSeriesMap")
		as.list(contextList)
}

#' contextObject
#'
#' @param data a data frame or a path to a file containing the context data
#' @param contextTypes one of the context types available in \code{\link{contextTypesList}}. 
#' @param name an identifier for the context object to be created (optional).
#' @param keyColumns allows to specify the key columns to be used by the context object (for lookupTables and timeSeriesMap contexts). (Optional).
#' @param timeColumn allows to specify the time column to be used by the context object (for timeSeries and timeSeriesMap contexts). (Optional).
#' @param graphSourceNodeColumn allows to specify the graph source node column (for graph context). (Optional)
#' @param graphTargetNodeColumn allows to specify the graph target node column (for graph context). (Optional)
contextObject = function(data, contextTypes=NULL, name = NULL, keyColumns = list(), timeColumn = NULL, graphSourceNodeColumn = NULL,graphTargetNodeColumn= NULL ) { 
	keyColumns = as.list(keyColumns)
	if (!is.null(name) && .isServerVersionOlderThan("1.8") && substr(name,1,1)==toupper(substr(name,1,1))){
		warning("Please change the context name to lower case convention for proper context matching")
	}
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
#' @param contextDatasets: Optional. A list of \code{\link{contextObject}} to be added to the learning.
#' @param problemDefinition: A \code{\link{problemDefinitionControl}} object with specific problem definition parameters.
#' @param preProcessing: A \code{\link{preProcessingControl}} object with specific preprocessing parameters.
#' @param featureGeneration: A \code{\link{featureGenerationControl}} object with specific feature generation parameters.
#' @param knowledge: A \code{\link{knowledgeControl}} object with specific external knowledge parameters.
#' @param modelBuilding: A \code{\link{modelBuildingControl}} object with specific model building parameters.
#' @param reporting: A \code{\link{reportingControl}} object with specific reporting parameters.
#' @param runBlocking: Block the R console while the session is running. FALSE by default.
#' @return \code{\link{Session}} object that encapsulates the model.
#' @examples
#' #session = learn("titanic", getData("titanic_train"), target = "survived")
learn <- function(
			 projectName,
			 trainData,
			 target,
			 testData = NULL,
			 contextDatasets = NULL,
			 problemDefinition = problemDefinitionControl(),
			 preProcessing = preProcessingControl(),
			 featureGeneration = featureGenerationControl(),
			 knowledge = knowledgeControl(),
			 modelBuilding = modelBuildingControl(),
			 reporting = reportingControl(),
			 runBlocking = TRUE,
			 ...
){
	isLatestRpackage()
	extraParams = list(...)
	if(!is.null(extraParams$problemDefinitionCtrl)) problemDefinition = extraParams$problemDefinitionCtrl
	if(!is.null(extraParams$preProcessingCtrl)) preProcessing = extraParams$preProcessingCtrl
	if(!is.null(extraParams$featureGenerationCtrl)) featureGeneration = extraParams$featureGenerationCtrl
	if(!is.null(extraParams$knowledgeCtrl)) knowledge = extraParams$knowledgeCtrl
	if(!is.null(extraParams$modelBuildingCtrl)) modelBuilding = extraParams$modelBuildingCtrl
	if(!is.null(extraParams$reportingCtrl)) reporting = extraParams$reportingCtrl
	
	uncompressedUpload = ifelse(!is.null(extraParams$uncompressedUpload), extraParams$uncompressedUpload, FALSE)
	fileUploadThreshold = ifelse(uncompressedUpload, NA, 0)

	# TODO: verify that there are no supurious parameters, e.g. (projectname instead of projectName)
	remoteMode = if(!is.null(extraParams$remoteMode)) extraParams$remoteMode else is.null(getSBserverIOfolder())
	if(remoteMode && !currentUser(FALSE)) stop("Please login before calling the learn function. Thank you.")
	
	projectName = gsub(" ", "_", projectName)
	
	if(is.null(trainData) && !is.null(extraParams$trainData)) trainData = extraParams$trainData
	trainTestSplitRatio = if(!is.null(extraParams$trainTestSplitRatio)) extraParams$trainTestSplitRatio else problemDefinition$trainTestSplitRatio
	
	if (!is.null(testData) && !is.na(trainTestSplitRatio)) message ("Note: test data was provided - ignoring trainTestSplitRatio defintion.")	
	
	if (!is.null(testData) && setequal(names(trainData),names(testData))==FALSE) {
		message("trainData:")
		message(paste0(names(trainData),","))
		message("testData:")
		message(paste0(names(testData),","))
		stop ("testData and trainData have different schemas")
	}
	
	url <- paste0(getSBserverDomain(),"/rapi/learn")
	message(paste("Calling:", url))
	
	isContextObjectDefinition = function(c) "contextObject" %in% class(c) || "contextDefinition" %in% class(c)
	if (!is.null(contextDatasets)) {
		if (class(contextDatasets) != "list") {
			warning("contextDatasets should be a list")
			if (!isContextObjectDefinition(contextDatasets)) error("contextDatasets are not of class contextObject")
			contextDatasets=list(contextDatasets)
		}
		contextDatasets = as.list(contextDatasets)
		if (!all(sapply(contextDatasets, isContextObjectDefinition))) stop("Not all provided context objects are of type 'contextObject'")
		contextDatasets = lapply(contextDatasets, function(context) {
			if("contextDefinition" %in% class(context)) {  # the new way of defining contexts
				createContextObject = function(data=NULL, contextProvider=NULL, contextTypes=NULL, name = NULL, keyColumns = list(), timeColumn = NULL, graphSourceNodeColumn = NULL,graphTargetNodeColumn= NULL) {
					context = contextObject(data, contextTypes, name, keyColumns, timeColumn, graphSourceNodeColumn ,graphTargetNodeColumn)
					if(!is.null(contextProvider)) {
						context$contextProvider = contextProvider
					}
					context
				}

				if("codeFileContextDefinition" %in% class(context)) {
					createContextObject(contextProvider = list(url = context$url, name = "Code file", jsonClass = "com.sparkbeyond.runtime.data.transform.CodeFile"),
															name = context$name)
				} else if("word2VecBasedOnDataContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = preProcessing$fileEscaping, directUploadThreshold = fileUploadThreshold),
															name = context$name,
															keyColumns = context$keyColumns,
															contextTypes = list("Word2Vec"))
				} else if("word2VecPretrainedS3ContextDefinition" %in% class(context)) {
					createContextObject(contextProvider = list(S3source = context$modelName, name = "Word2Vec", jsonClass = "com.sparkbeyond.runtime.data.transform.Word2Vec"),
															name = context$name,
															contextTypes = list("Word2VecPretrainedS3"))
				} else if("word2VecPretrainedLocalContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
					createContextObject(data=uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
																									, useEscaping = preProcessing$fileEscaping, directUploadThreshold = fileUploadThreshold),
															name = context$name,
															contextTypes = list("Word2VecPretrainedLocal"))
				} else if("featuresFromRevisionContextDefinition" %in% class(context)) {
					createContextObject(contextProvider = list(revisionId = context$revision, name = "FeaturesFromRevision", jsonClass = "com.sparkbeyond.runtime.data.transform.FeaturesFromRevision"),
															name = context$name)
				} else if("openStreetMapContextDefinition" %in% class(context)) {
					contextName = ifelse(!is.null(context$name), paste0("_", context$name), "")
					serverPath = uploadFileToServer(filePath = context$filePath, projectName = projectName, name = paste0("context", contextName))
					createContextObject(contextProvider = list(path = serverPath, jsonClass = "com.sparkbeyond.runtime.data.transform.OpenStreetMapFileInput"),
															name = context$name,
															contextTypes = contextTypesList(osmFile = TRUE))
				} else if("shapeFileContextDefinition" %in% class(context)) {
					findFilesRelatedToShapeFile = function(shapeFilePath) {
						stopifnot(tools::file_ext(shapeFilePath) == "shp")

						fileNameWithoutExtention = tools::file_path_sans_ext(shapeFilePath)
						possibleExts = c("cpg", "dbf", "prj", "shx")
						relatedFiles = paste0(fileNameWithoutExtention,".",possibleExts)
						existingRelatedFiles = relatedFiles[file.exists(relatedFiles)]
						
						requiredExts = c("dbf", "shx")
						requiredFiles = paste0(fileNameWithoutExtention,".",requiredExts)
						if(!all(requiredFiles %in% existingRelatedFiles)) {
							vec2String = function(vec) paste(vec, collapse = ", ")
							stop(paste0("Missing some of the mandatory files for shape file: ", shapeFilePath, ".\n Required: ", vec2String(requiredFiles), ".\n Found: ", vec2String(existingRelatedFiles)))
						}
						relatedFiles
					}
					
					if(! tools::file_ext(context$filePath) == "shp") {
						stop(paste("filePath in shapeFile context should point to a file with .shp extension. Instead received:", context$filePath))
					}
					
					shapeComplimentaryFiles = findFilesRelatedToShapeFile(context$filePath)
					uploadFiles = Vectorize(uploadFileToServer, c("filePath"))
					
					contextName = ifelse(!is.null(context$name), paste0("_", context$name), "")
					uploadFiles(shapeComplimentaryFiles, projectName = projectName, name = paste0("context", contextName), generateHash=FALSE)
					serverPath = uploadFileToServer(filePath = context$filePath, projectName = projectName, name = paste0("context", contextName), generateHash=FALSE)
					createContextObject(contextProvider = list(path = serverPath, jsonClass = "com.sparkbeyond.runtime.data.transform.ShapeFileInput"),
															name = context$name,
															contextTypes = list("SpatialIndexFromShapes"))
				}
			} else {
				#Handle data.frame input
				contextName = ifelse(!is.null(context$name), paste0("_", context$name),"")
				context$data =
					ifelse(!remoteMode,
						writeToServer(context$data,
							prefix = paste0(projectName,"_context", contextName),
							useEscaping = preProcessing$fileEscaping
						),
						uploadToServer(data = context$data, projectName = projectName, name = paste0("context", contextName)
													 , useEscaping = preProcessing$fileEscaping, directUploadThreshold = fileUploadThreshold)
				  )
				  context
			}
		})
	}

	algorithmsWhiteList = if(!is.null(extraParams$algorithmsWhiteList)) extraParams$algorithmsWhiteList else modelBuilding$algorithmsWhiteList
    extraModels = if(!is.null(extraParams$extraModels)) extraParams$extraModels else modelBuilding$extraModels
	params <-list(
		projectName = projectName,
		trainingFilePath = 
			ifelse (!remoteMode,
					writeToServer(trainData, prefix = paste0(projectName,"_train"), useEscaping = preProcessing$fileEscaping),
					{
						uploadedPath = uploadToServer(data = trainData,projectName = projectName, name = "train"
																					, useEscaping = preProcessing$fileEscaping, directUploadThreshold = fileUploadThreshold)
						if(is.na(uploadedPath)) stop("failed to upload training file to server")
						uploadedPath
					}
		),
		target = target,
		testFilePath = 
			ifelse (!is.null(testData) && (any(grep("data.frame", class(testData))) || class(testData)=="character"),
				ifelse(!remoteMode,
						writeToServer(testData, prefix = paste0(projectName,"_test"), useEscaping = preProcessing$fileEscaping),
						uploadToServer(data = testData,projectName = projectName, name = "test", useEscaping = preProcessing$fileEscaping, directUploadThreshold = fileUploadThreshold)
				),
				NA
		),	
		
		contextDatasets = contextDatasets,
				
		#problem definition
		regressionMode = if(!is.null(extraParams$forceRegression)) extraParams$forceRegression else problemDefinition$forceRegression,
		trainTestSplitRatio = trainTestSplitRatio,
		temporalSplitColumn = if(!is.null(extraParams$temporalSplitColumn)) extraParams$temporalSplitColumn else problemDefinition$temporalSplitColumn,
		weightColumn = if(!is.null(extraParams$weightColumn)) extraParams$weightColumn else problemDefinition$weightColumn,
		weightByClass = if(!is.null(extraParams$weightByClass)) extraParams$weightByClass else problemDefinition$weightByClass,
		partitionColumn = if(!is.null(extraParams$partitionColumn)) extraParams$partitionColumn else problemDefinition$partitionColumn,
		timeWindowsDefinition = if(!is.null(extraParams$timeWindowsDefinition)) list(extraParams$timeWindowsDefinition) else problemDefinition$timeWindowsDefinition,
		
		# preprocessing control
		fileEscaping = if(!is.null(extraParams$fileEscaping)) extraParams$fileEscaping else preProcessing$fileEscaping,
		fileEncoding = if(!is.null(extraParams$fileEncoding)) extraParams$fileEncoding else preProcessing$fileEncoding,
		emptyValuePolicy = if(!is.null(extraParams$emptyValuePolicy)) extraParams$emptyValuePolicy else preProcessing$emptyValuePolicy,
		linesForTypeDetection = if(!is.null(extraParams$linesForTypeDetection)) extraParams$linesForTypeDetection else preProcessing$linesForTypeDetection,
		
		#feature search parameters
		maxFeaturesCount = if(!is.null(extraParams$maxFeaturesCount)) extraParams$maxFeaturesCount else featureGeneration$maxFeaturesCount, 
		automaticSelectionOfNumberOfFeatures = if(!is.null(extraParams$automaticSelectionOfNumberOfFeatures)) extraParams$automaticSelectionOfNumberOfFeatures else featureGeneration$automaticSelectionOfNumberOfFeatures, 		
		supportThreshold = if(!is.null(extraParams$minSupportAbsolute)) extraParams$minSupportAbsolute else featureGeneration$minSupportAbsolute, 		
		featureSearchMode = if(!is.null(extraParams$featureSearchMode)) extraParams$featureSearchMode else featureGeneration$featureSearchMode,
		functionsWhiteList = if(!is.null(extraParams$functionsWhiteList)) extraParams$functionsWhiteList else featureGeneration$functionsWhiteList, 
		functionsBlackList = if(!is.null(extraParams$functionsBlackList)) extraParams$functionsBlackList else featureGeneration$functionsBlackList, 
		localTopFeatureCount = if(!is.null(extraParams$localTopFeatureCount)) extraParams$localTopFeatureCount else featureGeneration$localTopFeatureCount, 
		regressionDiscretizerBinsOverride = if(!is.null(extraParams$regressionDiscretizerBinsOverride)) extraParams$regressionDiscretizerBinsOverride else featureGeneration$regressionDiscretizerBinsOverride, 
		booleanNumericFeatures = if(!is.null(extraParams$booleanNumericFeatures)) extraParams$booleanNumericFeatures else featureGeneration$booleanNumericFeatures,
		numericEqualityFeatures = if(!is.null(extraParams$numericEqualityFeatures)) extraParams$numericEqualityFeatures else featureGeneration$numericEqualityFeatures,
		allowRangeFeatures = if(!is.null(extraParams$allowRangeFeatures)) extraParams$allowRangeFeatures else featureGeneration$allowRangeFeatures,																
		useRawNumericColumns = if(!is.null(extraParams$useRawNumericColumns)) extraParams$useRawNumericColumns else featureGeneration$useRawNumericColumns,																
		crossRowFeatureSearch = if(!is.null(extraParams$crossRowFeatureSearch)) extraParams$crossRowFeatureSearch else featureGeneration$crossRowFeatureSearch,
		autoColumnSubSets = if(!is.null(extraParams$autoColumnSubSets)) extraParams$autoColumnSubSets else featureGeneration$autoColumnSubSets,
		customColumnSubsets = if(!is.null(extraParams$customColumnSubsets)) extraParams$customColumnSubsets else featureGeneration$customColumnSubsets,
		maxFeatureDuration = if(!is.null(extraParams$maxFeatureDuration)) extraParams$maxFeatureDuration else featureGeneration$maxFeatureDuration, 
		overrideMaxFeatureDurationForExternalData = if(!is.null(extraParams$overrideMaxFeatureDurationForExternalData)) extraParams$overrideMaxFeatureDurationForExternalData else featureGeneration$overrideMaxFeatureDurationForExternalData,
		useCachedFeatures = if(!is.null(extraParams$useCachedFeatures)) extraParams$useCachedFeatures else featureGeneration$useCachedFeatures,
		allocatedMemoryMB = if(!is.null(extraParams$allocatedMemoryMB)) extraParams$allocatedMemoryMB else featureGeneration$allocatedMemoryMB,
		maxCollectionSize = if(!is.null(extraParams$maxCollectionSize)) extraParams$maxCollectionSize else featureGeneration$maxCollectionSize,
		maxDepth = if(!is.null(extraParams$maxDepth)) extraParams$maxDepth else featureGeneration$maxDepth,
		
		#knowledge parameters
		useGraph = if(!is.null(extraParams$linkedDataCore)) extraParams$linkedDataCore else knowledge$linkedDataCore,
		useOpenStreetMap = if(!is.null(extraParams$openStreetMap)) extraParams$openStreetMap else knowledge$openStreetMap,
		useWeather = if(!is.null(extraParams$weather)) extraParams$weather else knowledge$weather,
		usCensus = if(!is.null(extraParams$usCensus)) extraParams$usCensus else knowledge$usCensus,
		news = if(!is.null(extraParams$news)) extraParams$news else knowledge$news,
		worldBank = if(!is.null(extraParams$worldBank)) extraParams$worldBank else knowledge$worldBank,
		twitter = if(!is.null(extraParams$twitter)) extraParams$twitter else knowledge$twitter,
		searchEngines = if(!is.null(extraParams$searchEngines)) extraParams$searchEngines else knowledge$searchEngines,
		customDatasets = if(!is.null(extraParams$customDatasets)) extraParams$customDatasets else knowledge$customDatasets,								
		
		#customGraphsWhiteList = if(!is.null(extraParams$customGraphsWhiteList)) extraParams$customGraphsWhiteList else knowledge$customGraphsWhiteList,
		#customGraphsBlackList = if(!is.null(extraParams$customGraphsBlackList)) extraParams$customGraphsBlackList else knowledge$customGraphsBlackList,
		#customFunctions = if(!is.null(extraParams$customFunctions)) extraParams$customFunctions else knowledge$customFunctions,
		
		# model building parameters
		algorithmsWhiteList = .algorithmsCompatibility$adaptWhiteList(algorithmsWhiteList),
		extraModels = .algorithmsCompatibility$adaptExtraModels(extraModels),
		evaluationMetric = if(!is.null(extraParams$evaluationMetric)) extraParams$evaluationMetric else modelBuilding$evaluationMetric,
		crossValidation = if(!is.null(extraParams$crossValidation)) extraParams$crossValidation else modelBuilding$crossValidation,
		maxRecordsForModelBuild = if(!is.null(extraParams$maxRecordsForModelBuild)) extraParams$maxRecordsForModelBuild else modelBuilding$maxRecordsForModelBuild,
		
		#reporting parameters
		produceFeatureClusteringReport = if(!is.null(extraParams$featureClustersReport)) extraParams$featureClustersReport else reporting$featureClustersReport,
		produceEvaluatedFunctionsReport = if(!is.null(extraParams$evaluatedFunctionsReport)) extraParams$evaluatedFunctionsReport else reporting$evaluatedFunctionsReport,
		featureVisualizations = if(!is.null(extraParams$featureVisualizations)) extraParams$featureVisualizations else reporting$featureVisualizations,
		scoreOnTestSet = if(!is.null(extraParams$scoreOnTestSet)) extraParams$scoreOnTestSet else reporting$scoreOnTestSet,
		emailForNotification = reporting$emailForNotification,
		
		externalPrefixPath = ifelse(!remoteMode, getSBserverIOfolder(), NA)
	)

	
	verifyList = function(l) {if(is.vector(l) && length(l)>0 && !is.na(l)) as.list(l) else l}
	params$algorithmsWhiteList = verifyList(params$algorithmsWhiteList)
	params$functionsWhiteList = verifyList(params$functionsWhiteList)
	params$functionsBlackList = verifyList(params$functionsBlackList)
	params$maxFeaturesCount = verifyList(params$maxFeaturesCount)
	params = params[!is.na(params)]
	
	message (paste("Training on ",params$trainingFilePath))
	
	body = rjson::toJSON(params)
	#if (verbose) print(body)
	res = httr::POST(url, body = body, httr::content_type_json())
	res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
	if (!is.null(res$error)) {
		res = paste("Train error: ", res$error, " - terminating.")
		message(res)
		stop(res)
	}
	
	message(paste("Artifact location was created at:", res$artifactPath))
	session = Session(artifact_loc = res$artifactPath, 
							modelBuilt = !(length(params$algorithmsWhiteList) == 1 &&
								tolower(params$algorithmsWhiteList[[1]]) == "zeror" &&
								length(params$extraModels) == 0),
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
	if (reporting$showWebView == TRUE && remoteMode == TRUE) session$webView() 
	if (runBlocking)session$waitForProcess(remoteMode=remoteMode)
 
	session$modelBuilt = TRUE
	return(session)
	
}

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
