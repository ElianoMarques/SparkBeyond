#' Sugar to identify type of column
colTypeOf = function(y) {typeof(y)}
#' Sugar to identify size of a list column
colLength = function(y) {if(length(y) == 1 && is.na(y)) list(NA) else length(unlist(y))}
#' Sugar to trim a list column by a number
trimN = function(y,n) {if(is.na(n)) list(y) else if (length(y) == 1 && is.na(y)) list(NA) else list(y[1:n])}
#' Sugar to trim a column by another column variable
trimByCol = function(y,n) {if(is.na(n)) list(y) else if (length(y) == 1 && is.na(y))  list(NA) else list(y[1:n])}
#' Sugar to exclude columns
excludeCols = function(data, cols) data[, (cols) := NULL] #note: parenthesis around cols are important
#' groupBy sugar
groupBy = function(data, keys){
  data = as.data.table(data)
  data[,lapply(.SD,list), by=keys]
}

setTimeColumn = function(DT, timeCol) { #assumption is can be called only if the data is grouped already, hence a data.table
  if (! timeCol %in% names(DT)) stop (paste("error:", timeCol, "does not exist"))
  if ("SB_times_col" %in% names(DT)) stop ("error: time column is already defined")
  setnames(DT, timeCol, "SB_times_col")
}

#' limitTimeSeries
# TODO - change to new format
limitTimeSeries = function(data, groupColumns, fromDate = NA, untilDate = NA){
  from = if (is.na(fromDate)) NA else unclass(as.POSIXct(fromDate)) *1000
  until = if (is.na(untilDate)) NA else unclass(as.POSIXct(untilDate)) *1000
  if (is.na(fromDate) && is.na(untilDate)) stop("Both from and until dates were not provided")

  checkDate = function(d) {
    if(is.na(from) && !is.na(until) && d <=until) TRUE
    else if (!is.na(from) && is.na(until) && d >= from) TRUE
    else if (!is.na(from) && !is.na(until) && d >= from && d<= until) TRUE
    else FALSE
  }

  data[, lapply(.SD, function(col) {
        lapply(col, function(cell) {
          if (length(unlist(cell))==1) cell
          else{
            l = unlist(lapply(cell, function(x) checkDate(x[[1]]) ))
            cell[l]
          }
        })
    }
  ), by = groupColumns]
}

#' sugar to convert a column to text
col2Text = function(x) {
  escapeFun = function(s) {
    gsub("\"","\"\"",s)
    gsub("\t","\\t",s)
  }
  createContent = function(x1) {
    if (typeof(x1) == "character" || is.factor(x1)) {
      x2 = sapply(x1,  escapeFun) #deal with escaping
      paste0("\"", paste0(x2,collapse = "\",\""), "\"")
    } else {
      paste0(x1,collapse = ",")
    }
  }

  if (is.list(x)){
    writeList = function (xi){
      content = createContent(xi)
      paste0("[",content,"]")
    }
    sapply(x, writeList)
  } else if (typeof(x) == "character" || is.factor(x)) paste0("\"",escapeFun(x),"\"") else x
}

#' sugar to convert all columns to text
cols2Text = function(data) {
    lapply(data,col2Text)
}

#' sugar to convert all columns to text and write to file
#' @param groupByColumns Optional. A vector of possible columns that were used for grouping the data. NULL by default.
writeGroupedData = function(data, outputFile) { #sugar for writing grouped data
  toWrite = cols2Text(data)
  write.table(toWrite, file=outputFile, sep="\t", row.names=FALSE, quote=FALSE)
}

# add sugar to flatten unary columns


#dt <- data.table(dt, new = paste(dt$A, dt$B, sep = ""))

#joins
# joinExample = function () {
#   (dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A"))
#   (dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A"))
#   merge(dt1, dt2) #join left # can add by = "A", allow.cartesian
#   merge(dt1, dt2, all = TRUE) #join all
# }
#
# #IDate
# dateTimeExample = function  () {
#   (seqdates <- seq(as.IDate("2001-01-01"), as.IDate("2001-08-03"), by = "3 weeks"))
# }
#
#
# runComplexTypeExample = function() {
#   library(data.table)
#   df = data.table(id = c(1,1,2), name = c("a","b","c"), num = c(1,2,3))
#   #grouped = df[,.(name=list(c(name)), num=list(c(num))),by="id"] #create grouped data frame
#   grouped = df[,lapply(.SD,list), by="id"]
#
#   library(dplyr)
#   mutate(grouped, id2=id*2)
#   mutate(grouped, num2=lapply(num,sum))
#   myTail = function(x) tail(x,n=1)
#   mutate(grouped, name2=lapply(name,myTail))
#
#   finalDF = mutate(grouped, name2=lapply(name,myPaste)) %>% select(-c(num,name))
#   write.table(finalDF, file="/tmp/finalDF.tsv", sep="\t", row.names=FALSE) # works!
#
#   write.table(df, file="/tmp/df.tsv", sep="\t", row.names=FALSE) #works
#   write.table(grouped, file="/tmp/grouped.tsv", sep="\t",  row.names=FALSE) #fails
#   capture.output(grouped, file="/tmp/grouped.txt") #not structured
# }
#
# runOperatorExample = function() {
#   # chain operator example
#   `%>>%` <- function(x,y) {sum(x,y)}
#
#   # Info on the := operator for data.table
#   # http://www.rdocumentation.org/packages/data.table/functions/assign.html
# }
