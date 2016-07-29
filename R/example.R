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


#' getData
#' 
#' Auxiliary function to get public test datasets.
#'
#' @param String of the name of the dataset. Options are:
#' \itemize{
#' \item titanic_train (see A. Basic modeling example)
#' \item titanic_test (see A. Basic modeling example)
#' \item tweets_sentiment (see C. Text analysis example)
#' \item sentiment_lexicon (see C. Text analysis example)
#' \item languages (see D. GeoSpatial analysis example)
#' \item frequencychange (see E. TimeSeries analysis example)
#' \item emergency_locations (see F. World knowledge example)
#' \item museums (see F. World knowledge example)
#' \item flights_delay (see F. World knowledge example)
#' \item googlestock (see G. Advanced features control example)
#' \item complexsum (see G. Advanced features control example)
#' }
#' @return DataFrame containing the data.
#' @examples
#' # Do Not Run:
#' # getData("titanic_train")
#' # getData("titanic_test")
getData <- function(datasetName) {
  datasetNameLC = tolower(datasetName)

  #auxiliary functions
  getExtData <- function(name) {
    filename = system.file("extdata/data", name, package = "SparkBeyond")
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
    languages = getExtData("languages.tsv"),
    emergency_locations = getExtData("emergency_locations.tsv"),
    museums = getExtData("museumsNextToParks.tsv"),
    countries_distance = getExtData("country_distance.tsv"),
    countries = getExtData("countries.csv"),
    spambase = getExtData("spambase.csv"),
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
    wnvdata = getExtData("wnvData.csv"),
    wnvweather = getExtData("wnvWeather.csv"),
    primenumbers = getExtData("primeNumbers.tsv"),
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
	browseURL("https://sparkbeyond.freshdesk.com/support/solutions/16000049489")
}

#' Shows html help of this package
#'
SBhelp = function() {
	browseURL("https://sparkbeyond.freshdesk.com/support/solutions/folders/16000076461")
}


#' Shows FAQ
#'
faq = function() {
	browseURL("https://sparkbeyond.freshdesk.com/support/solutions/folders/16000075995")
}

#' Shows explantion of all the reports produced by the SparkBeyond enginet
#'
reportsList = function() {
	browseURL("https://sparkbeyond.freshdesk.com/support/solutions/folders/16000075996")
}
