# Clickstream analysis on JD data
runClickStreamExample = function() {
  library(data.table)
  library(dplyr)

  fileloc = "/Users/zinman/Downloads/2011-05-01_2011-06-01.txt"
  data = fread(fileloc, sep="\t", header=TRUE) #faster file reading
  data[,URL_FULL:=paste0(URL_BASE,URL_QUERY)
       ][,WEB_SITE_NAME:=lapply(WEB_SITE_NAME,stringr::str_trim) #remove white space from domain
         ][,DATE_AND_TIME_OF_PAGE_VIEW:=lapply(DATE_AND_TIME_OF_PAGE_VIEW,strtrim, width=19) #removing seconds from data
           ][,(c("URL_BASE","URL_QUERY","DATE_OF_PAGE_VIEW","PAGE_VIEW_SEQUENCE_NUMBER")):=NULL] #DATE_AND_TIME_OF_PAGE_VIEW is enough

  grouped = data[,lapply(.SD,list), by="WEB_SESSION_ID"] #TODO: create sugar possibly with an operator %>>%

  #option1: identify if got to 'checkout'
  matchFun = function(y) grep("checkout",y,ignore.case=TRUE, useBytes=TRUE)[1]
  grouped[,checkoutIndex:=lapply(URL_FULL, matchFun), by="WEB_SESSION_ID"
          ][, trimIndex := checkoutIndex-1
            ][, `:=` (
              DATE_AND_TIME_OF_PAGE_VIEW = mapply(trimByCol,DATE_AND_TIME_OF_PAGE_VIEW, trimIndex),
              WEB_SITE_NAME = mapply(trimByCol,WEB_SITE_NAME, trimIndex),
              URL_FULL = mapply(trimByCol,URL_FULL, trimIndex)
            ), by = WEB_SESSION_ID]
  #(grouped[!is.na(checkoutIndex), lapply(.SD, colLength),by = WEB_SESSION_ID, .SDcols = c("WEB_SITE_NAME")]) #verify that trimming is correct
  grouped[,label:=lapply(checkoutIndex,function(x) if (!is.na(x)) 1 else 0)][,(c("checkoutIndex","trimIndex")):=NULL][]
  writeGroupedData(grouped, c("WEB_SESSION_ID"), "/tmp/finalDF.tsv") #write data to file


  #option2: identify time on site (e.g., after 5 pages)
}

# using dplyr
# data = read.delim(fileloc, header=TRUE, sep="\t", quote="")
# data2 = data %>% mutate(URL_FULL = paste(URL_BASE,URL_QUERY,sep=""))
# data2 = data %>% select(c(WEB_SESSION_ID, PAGE_VIEW_SEQUENCE_NUMBER,DATE_OF_PAGE_VIEW))
#
# filteredData = data2 %>%
#   filter(PAGE_VIEW_SEQUENCE_NUMBER >= 3) %>%
#   select(WEB_SESSION_ID) %>%
#   distinct
#
# data3 = data2 %>% inner_join (filteredData, by = "WEB_SESSION_ID")
# grouped2 = grouped %>% mutate(checkoutIndex = sapply(URL_FULL, matchFun))  %>% select(-URL_FULL)



#data.table examples
#head(grouped[,.(WEB_SESSION_ID,PAGE_VIEW_SEQUENCE_NUMBER,checkoutIndex)])
#grouped[checkoutIndex>0,.(WEB_SESSION_ID,PAGE_VIEW_SEQUENCE_NUMBER,checkoutIndex)]

#a = grouped[checkoutIndex>0,.(WEB_SESSION_ID,PAGE_VIEW_SEQUENCE_NUMBER,DATE_OF_PAGE_VIEW, checkoutIndex,trimIndex)]
#a[, colLen := lapply(.SD, colLength), by = WEB_SESSION_ID, .SDcols = c("PAGE_VIEW_SEQUENCE_NUMBER")]
#b <- a[, lapply(.SD,colLength), by = WEB_SESSION_ID, .SDcols = c("PAGE_VIEW_SEQUENCE_NUMBER")] #works
#(a[, trimmed5Col := lapply(PAGE_VIEW_SEQUENCE_NUMBER, trimN, n=5), by = WEB_SESSION_ID]) #works
#(a[, `:=` (c1type=lapply(PAGE_VIEW_SEQUENCE_NUMBER, colTypeOf),
#           c2type=lapply(checkoutIndex, colTypeOf)), by = WEB_SESSION_ID]) #works
#(a[, trimmedNCol := mapply(trimByCol,PAGE_VIEW_SEQUENCE_NUMBER, checkoutIndex), by = WEB_SESSION_ID]) #works
#excludeCols(a, c("trimmedNCol","trimmedNColSize")) #works
#pasteCols(a, c("WEB_SESSION_ID")) #print columns
