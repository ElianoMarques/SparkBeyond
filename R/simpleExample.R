# #' Learn
# model = SBlearn(sessionName = "titanic",
#                             trainingFilePath = getTitanicFilename(train = TRUE),
#                             target = "survived",
#                             server_port=server_port)
# #' Feature search only
# model = SBfeatureSearchOnly(sessionName = "titanic",
#                             trainingFilePath = getTitanicFilename(train = TRUE),
#                             target = "survived",
#                             server_port=server_port)
# #' Enrich
# enrichRes = model$enrich(titanic_test_filename, paste(getwd(),"titanic_test_enriched.tsv.gz",sep="/"), featureCount=NA)
#
# #' Predict
# predictRes = model$predict(getTitanicFilename(train=FALSE), paste(getwd(),"titanic_test.tsv.gz",sep="/"))
testZip = function () {
  zipData = data.frame(
    zipcode1 = c("15217","89830","98002","14905","14607","15001","01008","02186","18816","18817","18822","01238","05846","22807","34205","24243","26347","28203","29040"),
    zipcode2 = c("90210","14620","12858","94115","97423","01008","15001","18816","02186","34997","18817","05846","30655","23125","22807","26347","24243","29040","28203"),
    target = c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0), stringsAsFactors=FALSE)
  zipModel = learn("zipModel",zipData,"target", algorithmsWhiteList=list("RRandomForestClassifier"), functionsBlackList = list("numericInfo"))
}

stam = function(){
  cornellRaw=read.csv("~/Downloads/Cornell results 2015.csv")
  cornellRaw = cornellRaw[1:168,]
  rownames(cornellRaw) = cornellRaw[,1]
  cornell = cornellRaw[,9:ncol(cornellRaw)]
  cornell[is.na(cornell)] = 0
  hc = hclust(dist(cornell))
  pdf("~/Downloads/cornell-dendogram.pdf", width=40, height=15)
  plot(hc)
  dev.off()
  #res = cbind(cornellRaw[,1],kmeans(cornell, 10)$cluster, kmeans(cornell, 15)$cluster, kmeans(cornell, 20)$cluster)
  #colnames(res) = c("bac", "clusters10", "clusters15", "clusters20")
}


