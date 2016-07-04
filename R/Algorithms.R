#' All supported algorithms
#'
#' Usage examples:
#' algorithms$scikit$classifier$RandomForest
#' algorithms$R$regressor$RXGBoost
algorithms = list(
		Weka = list(
			classifier = list(
				NaiveBayes = "weka.classifiers.bayes.NaiveBayes",
				VotedPerceptron = "weka.classifiers.functions.VotedPerceptron",
				Id3 = "weka.classifiers.trees.Id3",
				RandomSubSpace = "weka.classifiers.meta.RandomSubSpace",
				BayesianLogisticRegression = "weka.classifiers.bayes.BayesianLogisticRegression",
				ClassificationViaClustering = "weka.classifiers.meta.ClassificationViaClustering",
				ClassificationViaRegression = "weka.classifiers.meta.ClassificationViaRegression",
				LogitBoost = "weka.classifiers.meta.LogitBoost",
				BayesNet = "weka.classifiers.bayes.BayesNet",
				SMO = "weka.classifiers.functions.SMO",
				LibSVM = "weka.classifiers.functions.LibSVM",
				LibLINEAR = "weka.classifiers.functions.LibLINEAR",
				Decorate = "weka.classifiers.meta.Decorate",
				RandomForest = "weka.classifiers.trees.RandomForest",
				AdaBoostM1 = "weka.classifiers.meta.AdaBoostM1",
				Bagging = "weka.classifiers.meta.Bagging",
				REPTree = "weka.classifiers.trees.REPTree",
				IBk = "weka.classifiers.lazy.IBk",
				Logistic = "weka.classifiers.functions.Logistic",
				DecisionTable = "weka.classifiers.rules.DecisionTable"
			),
			regressor = list(
				LeastMedSq = "weka.classifiers.functions.LeastMedSq",
				LinearRegression = "weka.classifiers.functions.LinearRegression",
				SimpleLinearRegression = "weka.classifiers.functions.SimpleLinearRegression",
				SMOreg = "weka.classifiers.functions.SMOreg",
				REPTree = "weka.classifiers.trees.REPTree",
				RegressionByDiscretization = "weka.classifiers.meta.RegressionByDiscretization"
			)
		),

		scikit = list(
			classifier = list(
				KerasDeepLearning = "KerasDeepLearningClassifier",
				DecisionTree = "SciKitLearnDecisionTreeClassifier",
				DecisionTreeGini = "SciKitLearnDecisionTreeGiniClassifier",
				Bagging = "SciKitLearnBaggingClassifier",
				SVM = "SciKitLearnSVMClassifier",
				RandomForest = "SciKitLearnRandomForestClassifier",
				AdaBoost = "SciKitLearnAdaBoostClassifier",
				LogisticRegression = "SciKitLearnLogisticRegressionClassifier",
				LogisticRegressionCV = "SciKitLearnLogisticRegressionCVClassifier",
				SGD = "SciKitLearnSGDClassifier",
				GradientBoosting = "SciKitLearnGradientBoostingClassifier"
			),
			regressor = list(
				KerasDeepLearning = "KerasDeepLearningRegressor",
				LinearRegression = "SciKitLearnLinearRegression",
				DecisionTree = "SciKitLearnDecisionTreeRegressor",
				Lasso = "SciKitLearnLasso",
				Ridge = "SciKitLearnRidge",
				Bagging = "SciKitLearnBaggingRegressor",
				SVM = "SciKitLearnSVMRegressor",
				NuSVM = "SciKitLearnNuSVMRegressor",
				SGD = "SciKitLearnSGDRegressor",
				LassoLarsCV = "SciKitLearnLassoLarsCV",
				LassoCV = "SciKitLearnLassoCV",
				ElasticNet = "SciKitLearnElasticNet",
				RandomForest = "SciKitLearnRandomForestRegressor",
				AdaBoost = "SciKitLearnAdaBoostRegressor",
				GradientBoosting = "SciKitLearnGradientBoostingRegressor"
			)
		),

		R = list(
			classifier = list(
				RRpartDecisionTree = "RRpartDecisionTreeClassifier",
				RCaretGBM = "RCaretGBMClassifier",
				RStackingEnsembleGBM_of_GBM_with_Rpart = "RStackingEnsembleGBM_of_GBM_with_RpartClassifier",
				RRandomForest = "RRandomForestClassifier",
				RXGBoost = "RXGBoostClassifier",
				RLassoLogisticRegressionGlmnet = "RLassoLogisticRegressionGlmnetClassifier",
				RRidgeLogisticRegressionGlmnet = "RRidgeLogisticRegressionGlmnetClassifier"
			),
			regressor = list(
				RLinearRegressionLM = "RLinearRegressionLMRegressor",
				RRpartDecisionTree = "RRpartDecisionTreeRegressor",
				RLinearEnsembleGBM_with_Rpart = "RLinearEnsembleGBM_with_RpartRegressor",
				RStackingEnsembleGBM_of_GBM_with_Rpart = "RStackingEnsembleGBM_of_GBM_with_RpartRegressor",
				RCaretGBM = "RCaretGBMRegressor",
				RRandomForest = "RRandomForestRegressor",
				RXGBoost = "RXGBoostRegressor",
				RLassoLinearRegressionGlmnet = "RLassoLinearRegressionGlmnetRegressor",
				RRidgeLinearRegressionGlmnet = "RRidgeLinearRegressionGlmnetRegressor"
			)
		),

		MLlib = list(
			classifier = list(
				MLlibLogisticRegression = "MLlibLogisticRegression",
				MLlibDecisionTree = "MLlibDecisionTreeClassifier",
				MLlibNaiveBayes = "MLlibNaiveBayes",
				MLlibRandomForest = "MLlibRandomForestClassifier"
			),
			regressor = list(
				MLlibLinearRegression = "MLlibLinearRegression",
				MLlibDecisionTree = "MLlibDecisionTreeRegressor",
				MLlibGBT = "MLlibGBTRegressor",
				MLlibRandomForest = "MLlibRandomForestRegressor"
			)
		)
)

