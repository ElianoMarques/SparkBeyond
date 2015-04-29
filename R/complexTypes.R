#' Sugar to identify type of column
colTypeOf = function(y) {typeof(y)}
#' Sugar to identify size of a list column
colLength = function(y) {if(length(y) == 1 && is.na(y)) list(NA) else length(unlist(y))}
#' Sugar to trim a list column by a number
trimN = function(y,n) {if(is.na(n)) list(y) else if (length(y) == 1 && is.na(y)) list(NA) else list(y[1:n])}
#' Sugar to trim a column by another column variable
trimByCol = function(y,n) {if(is.na(n)) list(y) else if (length(y) == 1 && is.na(y))  list(NA) else list(list(y[1:n]))}
#' Sugar to exclude columns
excludeCols = function(data, cols) data[, (cols) := NULL] #note: parenthesis around cols are important
#' groupBy sugar
groupBy = function(data, keys)data[,lapply(.SD,list), by=keys]

#' zipCols sugar
zipCols = function(data, newName, col1, col2) {
  data[,paste0(newName):=list(mapply(cbind.data.frame, eval(parse(text=paste0(col1))), eval(parse(text=paste0(col2))), stringsAsFactors = FALSE, SIMPLIFY=F))]
}

#' zipAllCols sugar
zipAllCols = function(data, col1, by) {
  data[, lapply(.SD, function(x) list(mapply(cbind.data.frame, eval(parse(text=paste0(col1))), x, stringsAsFactors = FALSE, SIMPLIFY=F))), by=by]
}

#' sugar to convert a column to text
col2Text = function(x) {
  escapeFun = function(s) gsub("\"","\"\"",s)
  createContent = function(x1) {
    if (typeof(x1) == "character" || is.factor(x1)) {
      x2 = sapply(x1,  escapeFun) #deal with escaping
      paste0("\"", paste0(x2,collapse = "\",\""), "\"")
    } else {
      paste0(x1,collapse = ",")
    }
  }

  if (is.list(x)){
    x1 = unlist(x)
    content = createContent(x1)
    prefix = if (is.list(x[[1]])) paste0("TimeSeries[",typeof(x[[1]][[1]][[2]]),"]") else ""
    paste0(prefix,"[",content,"]")
  } else if (typeof(x) == "character" || is.factor(x)) paste0("\"",escapeFun(x),"\"") else x
}

#' sugar to convert all columns to text
cols2Text = function(data, groupColumns) {data[,lapply(.SD,col2Text), by=groupColumns]}

#' sugar to convert all columns to text and write to file
#' @param groupByColumns Optional. A vector of possible columns that were used for grouping the data. NULL by default.
writeGroupedData = function(data, outputFile, groupByColumns = NULL) { #sugar for writing grouped data
  library(data.table)
  toWrite = if (!is.null(groupByColumns)) cols2Text(data, groupByColumns) else data
  quote = if (!is.null(groupByColumns)) FALSE else TRUE
  write.table(toWrite, file=outputFile, sep="\t", row.names=FALSE, quote=quote)
}

#dt <- data.table(dt, new = paste(dt$A, dt$B, sep = ""))

#joins
joinExample = function () {
  (dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A"))
  (dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A"))
  merge(dt1, dt2) #join left # can add by = "A", allow.cartesian
  merge(dt1, dt2, all = TRUE) #join all
}

#IDate
dateTimeExample = function  () {
  (seqdates <- seq(as.IDate("2001-01-01"), as.IDate("2001-08-03"), by = "3 weeks"))
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
