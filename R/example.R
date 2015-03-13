#test package

#Install package:
#require(devtools)
#install_github("zinman/SBadapter")

#load library
library(SBadapter)

#perform learn on titanic train dataset
modelRes = SBlearn("titanic", "./titanic_train.tsv", "survived")

#perform prediction on titanic test dataset
predictRes = SBpredict(modelRes$artifactPath, "./titanic_test.csv", "./titanic_test.tsv.gz")

