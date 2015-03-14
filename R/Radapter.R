# replace with an SB object that will be initialized with url and will contain the methods
#testFilePath: Option[String] = None,
#maxDepth: Option[Int],
#trainTestSplitRatio: Option[Double],
#algorithmsWhiteList: Option[List[String]],
#hints: Option[Seq[String]],
#globalFeatureIterations: Option[Int],
#evaluationMetric: Option[String],
#weightColumn: Option[String],
#scoreOnTestSet: Option[Boolean],
#crossValidation: Option[Int],
#useGraph: Option[Boolean]


#' Run SparkBeyond feature enrichment and learning process.
#'
#' @param sessionName String of the session name.
#' @param trainingFilePath path to the file to be trained on.
#' @param target column name of in the training file that conatins the target of the prediction.
#' @return model path of the prediction on \code{trainingFilePath}.
#' @examples
#' modelRes = SBlearn("titanic", titanic_train_filename, "survived")
SBlearn <- function(sessionName, trainingFilePath, target){
	require(httr)
	require(rjson)

	url <- "http://127.0.0.1:9000/rapi/learn"
	params <-list("sessionName" = sessionName,
		"trainingFilePath" = trainingFilePath,
		algorithmsWhiteList = list("J48"),
		target = target)

	body = rjson::toJSON(params)
	res = httr::POST(url, body = body, httr::content_type_json())
  res <- jsonlite::fromJSON(txt=content(res, as="text"),simplifyDataFrame=TRUE)
  if (!is.null(res$error)) {
    print(paste("Train error: ", res$error, " - terminating."))
    return(NULL)
  }

  serverResponded = FALSE
  i = 0
	repeat {
		print(paste("Prediction sent to server... waiting... ",i))
		flush.console()
		i = i+1
    if (i >= 3 && !serverResponded) {
      print(paste("Server didn't respond for 30 seconds... check that server is up... terminating."))
      flush.console()
      return (NULL)
    }
		Sys.sleep(10)
		statusFile = paste(res$artifactPath,"/json/status.json", sep="")
		if (file.exists(statusFile)){ #currently assuming that application won't crush before file is created
        serverResponded = TRUE
			  status = fromJSON(paste(readLines(statusFile, warn=FALSE), collapse=""))
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
#'
#' @param modelPath path to the model file returned by SBlearn.
#' @param dataPath  path to the file to be tested.
#' @param outputPath path to write the results of the prediction.
#' @return A data frame containing the prediction.
#' @examples
#' predictRes = SBpredict(modelRes$artifactPath, titanic_test_filename, "./titanic_test.tsv.gz")
SBpredict <- function(modelPath, dataPath, outputPath){
	require(httr)
	require(rjson)

	url <- "http://127.0.0.1:9000/rapi/predict"
	params <-list(modelPath = modelPath,
		dataPath = dataPath,
		outputPath = outputPath)

	body = rjson::toJSON(params)
	res = httr::POST(url, body = body, httr::content_type_json())
  res <- jsonlite::fromJSON(txt=content(res, as="text"),simplifyDataFrame=TRUE)

  finalRes = if (!is.null(res$error) && res$result == "OK"){
  	read.table(outputPath, header = TRUE, sep="\t")
  } else {
    print(paste("Prediction failed: ", res$error))
    NULL
  }
	return(finalRes)
}
