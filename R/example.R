#test package

#Install package:
#require(devtools)
#install_github("zinman/SBadapter")

#load library
#library(SBadapter)

#' Run SparkBeyond titanic example.
#' @examples
#' run_SB_examples()
run_SB_examples <- function() {
  print("Running titanic train example")

  #perform learn on titanic train dataset
  titanic_train_filename = system.file("extdata", "titanic_train.tsv", package = "SBadapter")
  #titanic_train = read.table(titanic_train_filename, header = TRUE, sep="\t") #inspect file content
  #str(titanic_train) #inspect file content
  tryCatch({
    modelRes = SBlearn("titanic", titanic_train_filename, "survived")

    #perform prediction on titanic test dataset
    if (!is.null(modelRes)){
      print("Running titanic test example")
      titanic_test_filename = system.file("extdata", "titanic_test.csv", package = "SBadapter")
      #titanic_test = read.table(titanic_test_filename, header = TRUE, sep=",") #inspect file content
      #str(titanic_test) #inspect file content
      predictRes = SBpredict(modelRes$artifactPath, titanic_test_filename, "./titanic_test.tsv.gz")
      return (1)
    }
  }, error = function(e) {
    write (e$message, stderr())
    return (0)
  })
  return (0)
}
