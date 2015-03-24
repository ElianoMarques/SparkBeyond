#test package

#Install package:
#require(devtools)
#install_github("zinman/SBadapter")

#load library
#library(SBadapter)

#' Run SparkBeyond titanic example.
#' @param configuration Configuration ID as string. 1 - use only J48, 2 - use RRandomForest, 3 - use defaultList
#' @param server_port the port to be accessed in the SparkBeyond API server. 9000 by default
#' @examples
#' run_SB_examples()
run_SB_examples <- function(configuration='1', server_port = 9000) {
  print(paste("Running titanic train example - configuration: ",   configuration))
  write(paste("Running titanic train example - configuration: ",   configuration), stderr())


  #perform learn on titanic train dataset
  titanic_train_filename = system.file("extdata", "titanic_train.tsv", package = "SBadapter")
  #titanic_train = read.table(titanic_train_filename, header = TRUE, sep="\t") #inspect file content
  #str(titanic_train) #inspect file content
  res = tryCatch({
    params = list(
      sessionName = "titanic",
      trainingFilePath = titanic_train_filename,
      target = "survived",
      server_port=server_port
    )
    additional_params = switch (configuration,
      "1" = list(algorithmsWhiteList = list("J48")),
      "2" = list(algorithmsWhiteList = list("RRandomForest")),
      "3" = list(algorithmsWhiteList = NA)
    )

    model = do.call(SBlearn,c(params, additional_params))


    #perform prediction on titanic test dataset
    print("Running titanic test example")
    titanic_test_filename = system.file("extdata", "titanic_test.csv", package = "SBadapter")
    #titanic_test = read.table(titanic_test_filename, header = TRUE, sep=",") #inspect file content
    #str(titanic_test) #inspect file content
    predictRes = model$predict(titanic_test_filename, paste(getwd(),"titanic_test.tsv.gz",sep="/"))
    if (nrow(predictRes) == 0) stop("Prediction failed")

    #perform enrichment on titanic test dataset
    print("Enriching titanic test data")
    enrichRes = model$enrich(titanic_test_filename, paste(getwd(),"titanic_test_enriched.tsv.gz",sep="/"), featureCount=NA)
    if (ncol(enrichRes) == 0) stop("Enrichment failed")

    return ("Success")

  }, error = function(e) {
    write (e$message, stderr())
    return (e$message)
  })
  return (res)
}


