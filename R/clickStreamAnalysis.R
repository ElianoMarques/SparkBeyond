# Clickstream analysis on JD data
runClickStreamExample = function() {
  library(data.table)
  library(dplyr)

  fileloc = "/Users/zinman/Downloads/2011-05-01_2011-06-01.txt"
  #data = read.delim(fileloc, header=TRUE, sep="\t", quote="")
  data = fread(fileloc, sep="\t", header=TRUE) #faster file reading
  data2 = data %>% mutate(URL_FULL = paste(URL_BASE,URL_QUERY,sep=""))

  # data2 = data %>% select(c(WEB_SESSION_ID, PAGE_VIEW_SEQUENCE_NUMBER,DATE_OF_PAGE_VIEW))
  #
  # filteredData = data2 %>%
  #   filter(PAGE_VIEW_SEQUENCE_NUMBER >= 3) %>%
  #   select(WEB_SESSION_ID) %>%
  #   distinct
  #
  # data3 = data2 %>% inner_join (filteredData, by = "WEB_SESSION_ID")

  grouped = data2[,lapply(.SD,list), by="WEB_SESSION_ID"] #TODO: create sugar possibly with an operator %>>%

  #option1: identify if got to 'checkout'
  grouped2 = grouped %>% mutate(hasCheckout = grepl("checkout",URL_FULL,ignore.case=TRUE)) %>% select(-URL_FULL)
  length(which(grouped2[,hasCheckout]))

  writeGroupedData(grouped, c("WEB_SESSION_ID"), "/tmp/finalDF.tsv")

  #option2: identify time on site (e.g., after 5 pages)
}
