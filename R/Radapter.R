# replace with an SB object that will be initialized with url and will contain the methods

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
		print(paste("Prediction sent to server... waiting... ",i))
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
  return(res)
}

#' Run SparkBeyond prediction on a result of SBlearn.
#' @param modelPath path to the model file returned by SBlearn.
#' @param dataPath  path to the file to be tested.
#' @param outputPath path to write the results of the prediction.
#' @return A data frame containing the prediction.
#' @param server_port the port to be accessed in the SparkBeyond API server. 9000 by default.
#' @examples
#' predictRes = SBpredict(modelRes$artifactPath, titanic_test_filename, "./titanic_test.tsv.gz")
SBpredict <- function(modelPath, dataPath, outputPath, server_port = 9000){

	url <- paste("http://127.0.0.1:",server_port,"/rapi/predict", sep="")
	params <-list(modelPath = modelPath,
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
}
