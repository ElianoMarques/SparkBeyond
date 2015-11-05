#test package

#Install package:
#require(devtools)
#install_github("zinman/SparkBeyond")

#load library
#library(SparkBeyond)

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
      useCachedFeatures = TRUE,
      autoSave = FALSE
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
                          target = "survived",
                          autoSave = FALSE
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
  getExtData <- function(name) {
    filename = system.file("extdata", name, package = "SparkBeyond")
    sep = if (tools::file_ext(filename) == "tsv" || (tools::file_ext(filename) == "gz" && grepl("tsv", filename))) "\t" else ","
    data = read.table(filename, header = TRUE, sep = sep, stringsAsFactors=FALSE) #inspect file content
    #str(data) #inspect file content
    return(data)
  }

  switch(datasetNameLC,
    titanic_train = getExtData("titanic_train.tsv"),
    titanic_test = getExtData("titanic_test.csv"),
    tweets_sentiment = getExtData("tweets_sentiment.tsv"),
    tweets_sentimentkaggle = getExtData("kaggleSentiment.tsv"),
    sentiment_lexicon = getExtData("SentimentLexicon_Hu_Liu_KDD2004.tsv"),
    sf_street_names = getExtData("SF_Street_Names.csv"),
    la_street_names = getExtData("LA_Street_Names.tsv"),
    test_context = getExtData("test_context.csv"),
    city_tweets = getExtData("City_tweets.csv"),
    city_geocodeddb =getExtData("City_geocodedDB.csv"),
    flights_delay = getExtData("flights_weatherDelay.tsv.gz"),
    #flights_delay = getExtData("flights_weatherDelay.tsv.gz"), #the git upload in RStudio doesn't support tsv.gz
#      flights_delay =       {
#        destName = "flights_weatherDelay.tsv.gz"
#        if (! file.exists(destName) || !file.info("flights_weatherDelay.tsv.gz")$size > 0) download.file("http://s3.amazonaws.com/public-sparkbeyond/flights_2008_weatherDelay.tsv.gz", destName)
#        if (file.exists(destName))
#          read.table("flights_weatherDelay.tsv.gz", sep="\t", header=TRUE)
#        else stop("Flight weather delay was not available")
#      },
    airports = getExtData("airports.csv.gz"),
    frequencychange = getExtData("frequencyChangeDS.csv"),
		googlestock = getExtData("googDF.tsv"),
		complexsum = getExtData("ComplexSum.tsv"),
    stop(paste0("The requested dataset '",datasetName,"' does not exists in the datasets list"))
  )
}

#' Shows an html page with various learning examples
#'
examples = function() {
  browseURL(system.file("extdata", "examples-master.html", package = "SparkBeyond"))
}

#' Shows getting started tutorial
#'
tutorial = function() {
	browseURL(system.file("extdata", "tutorial.html", package = "SparkBeyond"))
}

#' Shows various sparkbeyond resources
#'
resources = function() {
	browseURL("https://docs.google.com/document/d/1wF7EkdyEB8409blpkianOyQxvi-OWasVEGegdLk-MOI/pub")
}

#' Shows html help of this package
#'
SBhelp = function() {
	browseURL("https://s3.amazonaws.com/public-sparkbeyond/SBadapterDocs/index.html")
}


#' Shows FAQ
#'
faq = function() {
	browseURL("https://docs.google.com/document/d/1MfUduRR2jZiLaZPfc8Q68ijvWctjNKgg7smeqEoH9yk/pub")
}

#' Shows explantion of all the reports produced by the SparkBeyond enginet
#'
reportsList = function() {
	browseURL("https://docs.google.com/document/d/16ogU45DHrW0x_BVLS5TiyPMblORUGSxaScR8i_lIDI0")
}
