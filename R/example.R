test package
#require(devtools)
#install_github("zinman/SBadapter")
library(SBadapter)

modelRes = SBlearn("titanic", "/Users/zinman/Google\ Drive/data/datasets/public/titanic/titanic_train.tsv", "survived")

predictRes = SBpredict(modelRes$artifactPath, "/Users/zinman/Google\ Drive/data/datasets/public/titanic/titanic_test.csv", "/tmp/titanic_test.tsv.gz")

