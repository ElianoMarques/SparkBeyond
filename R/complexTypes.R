#' distinctBy
#'
#' Keep only distinct rows in the input as specified by parameter "by". When multiple rows are encountered for a certain combination the first row will be selected.
#' @param data: dataframe or data.table to be grouped
#' @param by: a list of column names to distinct by
#' @return a new data.table object with the distinct data
distinctBy = function(data, by){
  grouped = groupBy(data, by)
  flattenCols(grouped, names(grouped))  #flatten all columns
  grouped
}


#' groupBy
#'
#' group the input data by certain columns. The output will be an object of type data.table in which each cell will contain a list of items.
#' @param data: dataframe or data.table to be grouped
#' @param keys: a list of column names to perform the groupBy
#' @param flatten: a boolean indicator for whether to flatten (into a vector) columns that after grouping are of size 1 for all keys. TRUE by default.
#' @return a new data.table object with the grouped data
#' @examples
#' grouped = groupBy(getData("titanic_train"), by ="pclass")
#' rownames(grouped)
#' colSizes(grouped)
#' head(grouped)
#' grouped2 = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' rownames(grouped2)
#' colSizes(grouped2)
#' head(grouped2)
groupBy = function(data, by, flatten = TRUE){
  text = paste0("list(",paste(by, collapse = ","),")")
  data = as.data.table(data)
  grouped = data[,lapply(.SD,list), by=eval(parse(text=text))]
  #set row names
  #rownames(grouped) = apply(grouped[,eval(parse(text=text))], 1, paste0,collapse = "_") #data.table does not use rownames()
  #flatten when single value
  if (flatten){
    rowsCount = nrow(grouped)
    #toFlatten = setdiff(names(which(rowsCount == apply(colSizes(grouped), 2, sum))), by)
    toFlatten = setdiff(names(which(rowsCount == apply(colSizesUnique(grouped), 2, sum) )), by)
    if (length(toFlatten) > 0) flattenCols(grouped, toFlatten)
  }
  grouped
}


#' colSizes
#'
#' Count the number of items in each column
#' @param data dataframe / data.table to be examined
#' @return a summary of items counts per row
#' @examples
#' grouped = groupBy(getData("titanic_train"), by ="pclass")
#' rownames(grouped)
#' colSizes(grouped)
#' head(grouped)
#' grouped2 = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' rownames(grouped2)
#' colSizes(grouped2)
#' head(grouped2)
colSizes = function(data) {
  colLengthFunc = function(y) {length(unlist(y))}
  sizes = sapply(data, function(x) sapply(x, function(y) colLengthFunc(y)), simplify=TRUE)
  rownames(sizes) = rownames(data)
  sizes
}

#' colSizesUnique
#'
#' Count the unique number of items in each column
#' @param data dataframe / data.table to be examined
#' @return a summary of unique items counts per row
#' @examples
#' grouped = groupBy(getData("titanic_train"), by ="pclass")
#' rownames(grouped)
#' colSizes(grouped)
#' colSizesUnique(grouped)
#' head(grouped)
#' grouped2 = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' rownames(grouped2)
#' colSizes(grouped2)
#' colSizesUnique(grouped2)
#' head(grouped2)
colSizesUnique = function(data) {
  colLengthFunc = function(y) {length(unique(unlist(y)))}
  sizes = sapply(data, function(x) sapply(x, function(y) colLengthFunc(y)), simplify=TRUE)
  rownames(sizes) = rownames(data)
  sizes
}


#' flattenCols
#'
#' Take the only the first element from each column defined in \code{cols} and set the column type into a vector
#' @param data: data.table to flatten
#' @param cols: list of columns to flatten
#' @param keepLast: Boolean. Indicates whether to keep the first or last element in the series.
#' @return nothing: operates on the input data object
#' @examples
#' grouped = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' typeof(grouped$age)
#' flattenCols(grouped, "age")
#' typeof(grouped$age) ## notice that the type of the column changed
flattenCols = function(data, cols, keepLast = FALSE) {
  flattenCol = function(data, colName) {
    if (!keepLast)
      data[,eval(as.symbol(colName)):=sapply(eval(as.symbol(colName)),`[[`,1,simplify=TRUE)]
    else
      data[,eval(as.symbol(colName)):=sapply(eval(as.symbol(colName)),last,simplify=TRUE)]
  }
  for(col in cols) {flattenCol(data,col)} # can possibly be written without the loop
}

#' typeofCols
#'
#' list the type of column for all columns in the input \code{data}
#' @param data: data.table to examine.
#' @return a vector of names for all \code{data} columns
#' @examples
#' grouped = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' typeofCols(grouped$age)
#' flattenCols(grouped, "age")
#' typeofCols(grouped)
typeofCols = function(data) {
  sapply(data, typeof)
}

#' classofCols
#'
#' list the class of column for all columns in the input \code{data}
#' @param data: data.table to examine.
#' @return a vector of names for all \code{data} columns
#' @examples
#' grouped = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' classofCols(grouped$age)
#' flattenCols(grouped, "age")
#' classofCols(grouped)
classofCols = function(data) {
	sapply(data, class)
}

#' cols2Text
#'
#' Convert all \code{data} columns into a serializable text/primitive representation. Cells that contain lists will be converted to [ , , ] representation. All strings will quoted.
#' @param data dataframe / data.table to convert to primitive represenation.
#' @param useEscaping A binary indicator noting whether a forward slash in the data needs to be escaped
#' @return a vector containing all input information in text/primitive representation
#' @examples
#' grouped = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' textGrouped = cols2Text(grouped)
#' typeofCols(textGrouped)
#' textGrouped[1,]
cols2Text = function(data, useEscaping = TRUE) {
  col2Text = function(x) {
    escapeFun = function(s) {
      s = gsub('"','""',s)
      #if (useEscaping) s = gsub("\\","\\\\",s) 
      if (TRUE) s = gsub("\\t","\\\\t",s) 
      s
    }
    
    printNonCharElement = function(e){
    	eType = class(e)
    	if (length(eType) > 1) eType = eType[1]
    	if (eType == "Date" || eType == "POSIXct") as.character(e)
    	else e
    }
    
    createContent = function(x1) {
      if (length(class(x1)) == 1 && class(x1) == "character" || is.factor(x1)) {
        x2 = sapply(x1,  escapeFun) #deal with escaping
        paste0("\"", paste0(x2,collapse = "\",\""), "\"")
      } else {      	
        paste0(printNonCharElement(x1),collapse = ",")
      }
    }

    if (is.list(x)){
      writeList = function (xi){
        content = createContent(xi)
        paste0("[",content,"]")
      }
      sapply(x, writeList)
    } else if (length(class(x)) == 1 && class(x) == "character" || is.factor(x)) paste0("\"",escapeFun(x),"\"")     	
    	else printNonCharElement(x)
  }

  sapply(data,col2Text)
}

#' writeToFile
#'
#' Convert all \code{data} columns into a serializable text/primitive representation and write them to a tab separated file.
#' @param data dataframe / data data.table to write.
#' @param useEscaping A binary indicator noting whether a forward slash in the data needs to be escaped
#' @examples
#' grouped = groupBy(getData("titanic_train"), by =list("pclass","sex"))
#' writeToFile(grouped, "titanic_grouped.tsv")
writeToFile = function(data, outputFile, useEscaping = TRUE) { #sugar for writing grouped data
  toWrite = cols2Text(data, useEscaping)
  write.table(toWrite, file=outputFile, sep="\t", row.names=FALSE, quote=FALSE)
}

#' excludeCols
#'
#' Exclude columns from a data frame. Please notice that the function return value depends on the type of input. NA will be returned if the input is of type data.table and the input object will be modified. Otherwise a  data.frame object will be returned.
#' @param data: dataframe / data data.table to modify.
#' @param cols: a list of column names to remove.
#' @return if the input \code{data} is a dataframe than a dataframe with the excluded columns will be return. If the input \code{data} is a data.table object, NA will be returned and the input object will be modified.
#' @examples
#' data = getData("titanic_train")
#'
#' ## data.table case
#' datatable = data.table(data)
#' class(datatable)
#' colnames(datatable)
#' excludeCols(datatable, list("sex", "age"))
#' colnames(datatable)
#'
#' ## dataframe case
#' class(data)
#' colnames(data)
#' excluded = excludeCols(data, list("sex", "age"))
#' colnames(excluded)
excludeCols = function(data, cols) {
  effectiveCols = intersect(cols,names(data))
  if (length(effectiveCols) > 0) {
    print (paste(paste(effectiveCols, collapse=", "), "were removed"))
    if ("data.table" %in% class(data)){
      data[, (effectiveCols) := NULL] #note: parenthesis around cols are important
      print("Note: the input object is of type data.table hence NA is returned and the input object will be modified,")
      NA
    } else {
      print("Note: the input object is of type data.frame hence, a new data.frame with the excluded columns will be returned.")
      data[ , -which(names(data) %in% cols)]
    }
  }
  else print("No columns were removed")
}

#' colsWhiteList
#'
#' get columns content from a data frame / data.table.
#' @param data: dataframe / data data.table to modify.
#' @param cols: a list of column names to get.
#' @return a dataframe / data.table with the requested columns will be returned.
colsWhiteList = function(data, cols){
  if ("data.table" %in% class(data)){
    data[,cols, with=FALSE]
  }else{
    data[,cols]
  }
}


##' Sugar to trim a list column by a number
#trimN = function(y,n) {if(is.na(n)) list(y) else if (length(y) == 1 && is.na(y)) list(NA) else list(y[1:n])}

##' Sugar to trim a column by another column variable
#trimByCol = function(y,n) {if(is.na(n)) list(y) else if (length(y) == 1 && is.na(y))  list(NA) else list(y[1:n])}


#' setTimeColumn
#'
#' Rename a requested column to "SB_times_col". When the exported data is read by the SparkBeyond engine, all other columns that contain lists that are of the same size as of the list in "SB_times_col" will be converted into time series data structures.
#' @param data: the dataframe / data.table to be modified.
#' @param timeCol: The column name in \code{data} to be renamed.
setTimeColumn = function(data, timeCol) { #assumption is can be called only if the data is grouped already, hence a data.table
  if (! timeCol %in% names(data)) stop (paste("error:", timeCol, "does not exist"))
  if ("SB_times_col" %in% names(data)) stop ("error: time column is already defined")
  setnames(data, timeCol, "SB_times_col")
}

#' limitTimeSeries
#'
#' filter rows to contain only rows that are in a certain time frame based on a from/until date inputs (inclusive). All dates are assumed to be of the same format. This function should be called before any filtering is performed.
#'
#' @param data: the dataframe / data.table to be modified.
#' @param dateCol: The column name in \code{data} that will be used for filtering. "SB_times_col" by default
#' @param fromDate: The starting date to filter from. NA by default.
#' @param untilData: The end date to filter until. NA by default.
#' @param datesFormat: the format of the from/until dates.month/day/year by default.
#' @return a new data.table with the filtered rows will be returned.
#' @examples
#' randDate <- function(N, st="2014/01/01", et="2014/12/31") {
#'  st <- as.POSIXct(as.Date(st,tz = "EST"),tz = "EST")
#'  et <- as.POSIXct(as.Date(et,tz = "EST"),tz = "EST")
#'  dt <- as.numeric(difftime(et,st,unit="secs"))
#'  ev <- sort(runif(N, 0, dt))
#'  strftime(st+ev, format="%m/%d/%Y")
#' }
#' simulateData = function(n = 100, l = 10) {
#'  data.table(ID = rep(1:n,l), value = rnorm(n*l), date = randDate(n*l))
#' }
#' tsData = simulateData()
#' head(tsData)
#' nrow(tsData)
#' nrow(limitTimeSeries(tsData, "date", fromDate ="07/01/2014"))
#' nrow(limitTimeSeries(tsData, "date", untilDate ="11/01/2014"))
#' nrow(limitTimeSeries(tsData, "date", fromDate="07/01/2014", untilDate ="11/01/2014"))
limitTimeSeries = function(data, dateCol = "SB_times_col", fromDate = NA, untilDate = NA, datesFormat = "%m/%d/%Y"){
  fromDateFormatted = strptime(fromDate, datesFormat)
  untilDateFormatted = strptime(untilDate, datesFormat)

  if(is.na(fromDate) && !is.na(untilDate)) {
    data[strptime(eval(as.symbol(dateCol)), datesFormat) <= untilDateFormatted]
  } else if (!is.na(fromDate) && is.na(untilDate)) {
    data[strptime(eval(as.symbol(dateCol)), datesFormat) >= fromDateFormatted]
  } else if (!is.na(fromDate) && !is.na(untilDate))
    data[strptime(eval(as.symbol(dateCol)), datesFormat) >= fromDateFormatted & strptime(eval(as.symbol(dateCol)), datesFormat) <= untilDateFormatted]
}

#' offsetTime
#'
#' Offsets a time-date column by a reference date to create a relative time series with respect to the reference date. Currently work only on data.table
#'
#' @param data: data.table to be modified.
#' @param dateCol: The column name in \code{data} that will be modified. "SB_times_col" by default
#' @param refDate: The reference date to use.
#' @param datesFormat: the format of the from/until dates. "\%m/\%d/\%Y" by default.
#' @param units: the time span unit to use when creating the reference. Based on \code{\link[base]{difftime}} definitions. "days" by default.
#' @return NA will be returned and the input file will be modified.
offsetTime = function(data, dateCol = "SB_times_col", refDate, datesFormat = "%m/%d/%Y", units = "days"){
  ref = strptime(refDate, datesFormat)

  offsetDateInternal = function(colDate) {
    d = strptime(colDate, datesFormat)
    difftime(d, ref, units = units)
  }

  data[,eval(as.symbol(dateCol)):=sapply(eval(as.symbol(dateCol)),offsetDateInternal)]
  NA
}



#' addTimeWindow
#'
#' Add a sliding window column to the data
#'
#' @param data: data.table to be modified.
#' @param dateCol: The column name in \code{data} that will be used.
#' @param window: The window length (numeric)
#' @param unit: The window length unit. Should be one of: "Seconds", "Minutes", "Hours", "Days", "Months", "Years", "Number"
#' @param dateFormat: provide date format for parsing. defaults to "\%m/\%d/\%Y " see strtptime for more examples i.e. "\%m/\%d/\%Y \%I:\%M:\%S \%p"
#' @param keyCol: An optional key for the sliding window (NA as default)
#' @param includeUntil: optional argument
#' @param relativeTime: optional relative time boolean flag
#' @param offset: optional offset to target. 0 by default.
#' @param sample: optional maximal possible sample value (default to maxint)
#' @return The new data
addTimeWindow = function(data, dateCol, keyCol = NA, window, unit = "Days", dateFormat ="%m/%d/%Y",includeUntil = FALSE, relativeTime = TRUE, sample = 2147483647, offset = 0) {
  unitVal = switch(unit,
  			 "Number" = 1,
         "Seconds" = 1,
         "Minutes" = 60,
         "Hours" = 60*60,
         "Days" = 24*60*60,
         "Months" = 4*7*24*60*60,
         "Years" = 12*4*7*24*60*60,
         stop("Invalid time unit. Should be one of: 'Seconds', 'Minutes', 'Hours', 'Days', 'Months', 'Years', 'Number'")
  )
  
  newCol = if (is.na(keyCol)){
  	paste0("last_", window, "_", unit)
  }else{
  	paste0("last_keyed_", window, "_", unit)
  }
  data = as.data.frame(data)
  #TODO: support data.table as well
  
  #TODO: support non-dates,  support offset calculation, Date POSix objects

  datePOSIXformatOut = "%m/%d/%Y %H:%M:%S %p %Z" #TODO: check if multiple output formats are possible
  dateColIndex = which(colnames(data)==dateCol)
  dateColData = data[[dateColIndex]]
  
  dateType = sapply(data, class)[dateColIndex] #charachter, integer, numeric, date, POSIXct
  if(length(class(dateType)) > 1) dateType = dateType[1] 
  
  generateWindow = function(dateVal, keyVal = NA) {
  	dates = if (dateType == "character") {
  		convertDateToString = function(dValue) as.character(as.POSIXct(dValue),format=datePOSIXformatOut)
  	 	dt = convertDateToString(strptime(dateVal,dateFormat,tz="EST")-offset*unitVal)
  		dt_from = convertDateToString(strptime(dateVal,dateFormat,tz="EST")-(window+offset)*unitVal) #seconds based
			c(dt_from, dt)
		} else if (dateType == "integer" || dateType == "numeric"){
			c(dateVal - window, dateVal)
		} else if (dateType == "Date") {
			dt = as.character(as.POSIXct(dateVal) - offset*unitVal,format=datePOSIXformatOut)
			dt_from = as.character(as.POSIXct(dateVal) - (window+offset)*unitVal,format=datePOSIXformatOut)
			c(dt_from, dt)			
		} else if (dateType == "POSIXct"){
			dt = as.character(dateVal - offset*unitVal)
			dt_from = as.character(dateVal -(window+offset)*unitVal)
			c(dt_from, dt)
		} else {
			stop (paste("Date column", dateCol,"type should be one of 'character', 'integer', 'numeric', 'date', 'POSIXct'."))
		}
  	paste0(keyVal,",",dates[1],",",dates[2],",",includeUntil,",",relativeTime,",",unit,",",sample)
  }
  
  if (is.na(keyCol)){
    data[,newCol] = sapply(dateColData, generateWindow)
  } else{    
    keyColData = colsWhiteList(data, keyCol)
    data[,newCol] = mapply(generateWindow,dateColData,keyColData)
  }
  data[]
}

#' join
#'
#' Joins two dataframes. Wrapper around data.table's \code{\link[data.table]{merge}}. It is required that the join will be on a unique set of keys, and that the join key columns will have the same name in both inputs.
#' @param x, y: data tables. y is coerced to a data.table if it isn't one already.
#' @param all: logical; all = TRUE is shorthand to save setting both all.x = TRUE and all.y = TRUE.
#' @param all.x: logical; if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y. These rows will have 'NA's in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output.
#' @param all.y: logical; analogous to all.x above.
#' @param suffixes: A character(2) specifying the suffixes to be used for making non-by column names unique. The suffix behavior works in a similar fashion as the \code{\link[base]{merge}} method does.
#' @return if the input \code{data} is a dataframe than a dataframe with the excluded columns will be return. If the input \code{data} is a data.table object, nothing will be returned and the input object will be modified.
#' @examples
#' data1 = data.table(id=1:2, text=c("a","b"))
#' data2 = data.table(id=rep(1:2,each=3), num=1:6)
#' grouped2 = groupBy(data2, "id")
#' join(data1,grouped2,"id")
join = function(x, y, by, all = FALSE, all.x =all, all.y=all, suffixes = c(".x", ".y")) {
  merge(x,y, by, all, all.x, all.y, suffixes)
}




# runOperatorExample = function() {
#   # chain operator example
#   `%>>%` <- function(x,y) {sum(x,y)}
#
#   # Info on the := operator for data.table
#   # http://www.rdocumentation.org/packages/data.table/functions/assign.html
# }

# #IDate
# dateTimeExample = function  () {
#   (seqdates <- seq(as.IDate("2001-01-01"), as.IDate("2001-08-03"), by = "3 weeks"))
# }
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
