pasteList = function(x) {
  x1 = unlist(x)
  content = if (typeof(x1) == "character") {
    x2 = sapply(x1, function(s) gsub("\"","\\\\\"",s)) #deal with escaping
    paste("\"", paste(x2,collapse = "\",\"",sep=""), "\"",sep="")
  } else {
    paste(x1,collapse = ",",sep="")
  }
  paste("[",content,"]",sep="")
}

writeGroupedData = function(data, groupColumns, outputFile) { #sugar for writing grouped data
  library(data.table)
  toWrite = data[,lapply(.SD,pasteList), by=groupColumns]
  write.table(toWrite, file=outputFile, sep="\t", row.names=FALSE, quote=FALSE)
}

runComplexTypeExample = function() {
  library(data.table)
  df = data.table(id = c(1,1,2), name = c("a","b","c"), num = c(1,2,3))
  #grouped = df[,.(name=list(c(name)), num=list(c(num))),by="id"] #create grouped data frame
  grouped = df[,lapply(.SD,list), by="id"]

  library(dplyr)
  mutate(grouped, id2=id*2)
  mutate(grouped, num2=lapply(num,sum))
  myTail = function(x) tail(x,n=1)
  mutate(grouped, name2=lapply(name,myTail))

  finalDF = mutate(grouped, name2=lapply(name,myPaste)) %>% select(-c(num,name))
  write.table(finalDF, file="/tmp/finalDF.tsv", sep="\t", row.names=FALSE) # works!

  write.table(df, file="/tmp/df.tsv", sep="\t", row.names=FALSE) #works
  write.table(grouped, file="/tmp/grouped.tsv", sep="\t",  row.names=FALSE) #fails
  capture.output(grouped, file="/tmp/grouped.txt") #not structured
}

runOperatorExample = function() {
  # chain operator example
  `%>>%` <- function(x,y) {sum(x,y)}

  # Info on the := operator for data.table
  # http://www.rdocumentation.org/packages/data.table/functions/assign.html
}
