#' All supported algorithms
#'
#' Usage examples:
#' algorithms$scikit$RandomForest
#' algorithms$R$RXGBoost
algorithms = list(
	
		Weka = list(
			BayesianLogisticRegression = "weka.classifiers.bayes.BayesianLogisticRegression",
			BayesNet = "weka.classifiers.bayes.BayesNet",
			NaiveBayes = "weka.classifiers.bayes.NaiveBayes",
			LeastMedSq = "weka.classifiers.functions.LeastMedSq",
			LibLINEAR = "weka.classifiers.functions.bLINEAR",
			LibSVM = "weka.classifiers.functions.LibSVM",
			LinearRegression = "weka.classifiers.functions.LinearRegression",
			Logistic = "weka.classifiers.functions.Logistic",
			SimpleLinearRegression = "weka.classifiers.functions.SimpleLinearRegression",
			SMO = "weka.classifiers.functions.SMO",
			SMOreg = "weka.classifiers.functions.SMOreg",
			VotedPerceptron = "weka.classifiers.functions.VotedPerceptron",
			IBk = "weka.classifiers.lazy.IBk",
			AdaBoostM1 = "weka.classifiers.meta.AdaBoostM1",
			Bagging = "weka.classifiers.meta.Bagging",
			ClassificationViaClustering = "weka.classifiers.meta.ClassificationViaClustering",
			ClassificationViaRegression = "weka.classifiers.meta.ClassificationViaRegression",
			Decorate = "weka.classifiers.meta.Decorate",
			LogitBoost = "weka.classifiers.meta.LogitBoost",
			RandomSubSpace = "weka.classifiers.meta.RandomSubSpace",
			RegressionByDiscretization = "weka.classifiers.meta.RegressionByDiscretization",
			DecisionTable = "weka.classifiers.rules.DecisionTable",
			ZeroR = "weka.classifiers.rules.ZeroR",
			Id3 = "weka.classifiers.trees.Id3",
			RandomForest = "weka.classifiers.trees.RandomForest",
			REPTree = "weka.classifiers.trees.REPTree",
			J48 = "J48"
		),

		scikit = list(
			AdaBoost = "SciKitLearnAdaBoost",
			Bagging = "SciKitLearnBagging",
			DecisionTree = "SciKitLearnDecisionTree",
			ElasticNet = "SciKitLearnElasticNet",
			ElasticNetCVRegressor = "SciKitLearnElasticNetCVRegressor",
			GradientBoosting = "SciKitLearnGradientBoosting",
			Lasso = "SciKitLearnLasso",
			LassoCV = "SciKitLearnLassoCV",
			LassoLarsCV = "SciKitLearnLassoLarsCV",
			LinearRegression = "SciKitLearnLinearRegression",
			LogisticRegressionClassifier = "SciKitLearnLogisticRegressionClassifier",
			LogisticRegressionCVClassifier = "SciKitLearnLogisticRegressionCVClassifier",
			NuSVMRegressor = "SciKitLearnNuSVMRegressor",
			RandomForest = "SciKitLearnRandomForest",
			Ridge = "SciKitLearnRidge",
			SGD = "SciKitLearnSGD",
			SVM = "SciKitLearnSVM",
			XGBoost = "SciKitLearnXGBoost",
			KerasDeepLearning = "KerasDeepLearning"
		),

		R = list(
			RCaretGBM = "RCaretGBM",
			RLassoGlmNet = "RLassoGlmNet",
			RRandomForest = "RRandomForest",
			RRidgeGlmNet = "RRidgeGlmNet",
			RRpartDecisionTree = "RRpartDecisionTree",
			RStackingEnsembleGBM_of_GBM_with_Rpart = "RStackingEnsembleGBM_of_GBM_with_Rpart",
			RXGBoost = "RXGBoost"
		),

		MLlib = list(
			MLlibDecisionTree = "MLlibDecisionTree",
			MLlibGBTRegressor = "MLlibGBTRegressor",
			MLlibLinearModel = "MLlibLinearModel",
			MLlibNaiveBayes = "MLlibNaiveBayes",
			MLlibRandomForest = "MLlibRandomForest"
		),
		
		ZeroR="ZeroR"
)

