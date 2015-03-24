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
