#test package

#Install package:
#require(devtools)
#install_github("zinman/SBadapter")

#load library
#library(SBadapter)

#' Run SparkBeyond titanic example.
#'
#' @param configuration Configuration ID as string. 1 - use RRandomForest, 2 - use defaultList
#' @examples
#' # run_SB_examples()
run_SB_examples <- function(configuration='1', weightByClass = FALSE) {

  runTitanicLearn <- function(configuration='1', weightByClass = FALSE, runBlocking = TRUE) {
    params = list(
      projectName = "titanic",
      trainData = getData("titanic_train"),
      target = "survived",
      weightByClass = weightByClass,
      runBlocking = runBlocking,
      useCachedFeatures = TRUE
    )
    additional_params = switch (configuration,
                                "1" = list(algorithmsWhiteList = list("RRandomForestClassifier")),
                                "2" = list(algorithmsWhiteList = NA)
    )

    model = do.call(learn,c(params, additional_params))
    return(model)
  }

  runTitanicTestEnrich <- function(model, featureCount = 10) {
    print("Enriching titanic test data")
    enrichRes = model$enrich(getData("titanic_test"), featureCount=featureCount)
    if (ncol(enrichRes) == 0) stop("Enrichment failed")
    return(enrichRes)
  }

  runTitanicTestPredict <- function(model) {
    print("Running titanic test example")
    predictRes = model$predict(getData("titanic_test"))
    if (nrow(predictRes) == 0) stop("Prediction failed")
    return(predictRes)
  }

  runTitanicFeatureSelection <- function() {
    print("Performing feature search only on Titanic train data")
    model = featureSearch(projectName = "titanic",
                          trainData = getData("titanic_train"),
                          target = "survived"
    )
    return (model)
  }


  print(paste("Running titanic train example - configuration: ",   configuration))
  write(paste("Running titanic train example - configuration: ",   configuration), stderr())

  res = tryCatch({
    model = runTitanicLearn(configuration, weightByClass)
    runTitanicTestEnrich(model)
    runTitanicTestPredict(model) #TODO: check why all the escaping in the scala output were produced, potentially return only last 3 columns and not the entire row
    return ("Success")
  }, error = function(e) {
    write (e$message, stderr())
    return (e$message)
  })
  return (res)
}


#' Auxiliary function to get public test datasets.
#'
#' @param String of the name of the dataset. Should be one of "titanic_train", "titanic_test", "flights_delay"
#' @return DataFrame containing the data.
#' @examples
#' # Do Not Run:
#' # getData("titanic_train")
#' # getData("titanic_test")
getData <- function(datasetName) {
  datasetNameLC = tolower(datasetName)

  #auxiliary functions
  getTitanicData <- function(train = TRUE) {
    titanic_filename = system.file("extdata", if (train) "titanic_train.tsv" else "titanic_test.csv", package = "SBadapter")
    sep = if (tools::file_ext(titanic_filename) == "tsv") "\t" else ","
    data = read.table(titanic_filename, header = TRUE, sep = sep, stringsAsFactors=FALSE) #inspect file content
    #str(data) #inspect file content
    return(data)
  }

  switch(datasetNameLC,
    titanic_train = getTitanicData(TRUE),
    titanic_test = getTitanicData(FALSE),
    flights_delay = {
      destName = "flights_weatherDelay.tsv.gz"
      if (! file.exists(destName)) download.file("http://s3.amazonaws.com/public-sparkbeyond/flights_2008_weatherDelay.tsv.gz", destName)
      if (file.exists(destName))
        read.table("flights_weatherDelay.tsv.gz", sep="\t", header=TRUE)
      else stop("Flight weather delay was not available")
    },
    stop(paste0("The requested dataset '",datasetName,"' does not exists in the datasets list"))
  )
}

