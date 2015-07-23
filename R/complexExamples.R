complexExamples = function() {
  library(data.table)
  data = getData("titanic_train")
  data = as.data.table(data)
  grouped = data[,lapply(.SD,list), by="pclass"]
  writeToFile(grouped, "/tmp/titanic_grouped.tsv", c("pclass"))

  data2 = zipCols(data, "zipped", "age", "sex")
  grouped = groupBy(data2, c("pclass"))
  writeToFile(grouped, "/tmp/time_series.tsv", c("pclass"))

  b=zipAllCols(data,"age",c("pclass"))
  writeToFile(b, "/tmp/time_series_full.tsv", c("pclass"))

  a= grouped[,.(pclass,zipped)]
  b=cols2Text(a, c("pclass"))

  #date to number
}
