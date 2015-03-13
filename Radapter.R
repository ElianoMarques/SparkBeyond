# replace with an SB object that will be initialized with url and will contain the methods

testFilePath: Option[String] = None,
maxDepth: Option[Int],
trainTestSplitRatio: Option[Double],
algorithmsWhiteList: Option[List[String]],
hints: Option[Seq[String]],
globalFeatureIterations: Option[Int],
evaluationMetric: Option[String],
weightColumn: Option[String],
scoreOnTestSet: Option[Boolean],
crossValidation: Option[Int],
useGraph: Option[Boolean]

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
  	i = 0
  	repeat {
			
		print(i)
		flush.console()

  		Sys.sleep(10)
		i = i+1
		statusFile = paste(res$artifactPath,"/json/status.json", sep="")
		if (file.exists(statusFile)){ #currently assuming that application won't crush before file is created
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
    
    finalRes = if (res$result == "OK"){
    	read.table(outputPath, header = TRUE, sep="\t")
    } else "Prediction failed"
	return(finalRes)	
}

modelRes = SBlearn("titanic", "/Users/zinman/Google\ Drive/data/datasets/public/titanic/titanic_train.tsv", "survived")

predictRes = SBpredict(modelRes$artifactPath, "/Users/zinman/Google\ Drive/data/datasets/public/titanic/titanic_test.csv", "/tmp/titanic_test.tsv.gz")


