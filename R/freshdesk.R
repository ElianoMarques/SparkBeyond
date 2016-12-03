category_id = 16000049969  # https://sparkbeyond.freshdesk.com/solution/categories/16000049489
folder_id = 16000076461 # https://sparkbeyond.freshdesk.com/solution/folders/16000076425

freshdeskAppKey = '' # add correct key

#uncomment below lines to run/copy to freshdesk
# 

# # folder query
# # Ask FreshDesk for a list of all solution articles
# 
# url = paste('https://sparkbeyond.freshdesk.com//solution/categories',category_id,'folders',paste0(folder_id,'.json'),sep = '/')
# cmd = paste0('curl -v -u ',freshdeskAppKey,':X -H "Content-Type: application/json" -X GET  ',url)
# res = system(cmd,intern=T) 
# 
# # parse res and extract the ids and corresponding titles
# current_articles_ids = as.numeric(stri_extract_all_regex(paste(res,collapse=" "), '(?<="id":).*?(?=,)')[[1]])
# current_articles_ids = current_articles_ids[-1] # remove the first which is the folder id
# current_titles = stri_extract_all_regex(paste(res,collapse=" "), '(?<="title":").*?(?=",)')[[1]]
# 
# 
# # if you haven't please run:
# # #   setwd("./html")
# # #   library(knitr)
# # #   knit_rd("SparkBeyond")
# # #   setwd("..")
# 
# # list the local html files and iterate over them
#  files = list.files('./html','*.html')
# 
#  for (f in files[-c(1,2)]) { # remove 00frame_toc.html and 00Index.html
#  	  title = sub(".html","",f)
#  	  #check if the file exists on freshdesk
#  	  ind = which(title == current_titles)
#  	  
#  	  # update the title and description
#  	  params = list(1)  # init params
#  	  params$solution_article$title = title
#  	  params$solution_article$description = gsub(" href=.*?>",">",readr::read_file(paste0('./html/',f)))# read in and sanitize links
# 
#  	  if (length(ind)) {
#  	  	paste0('Found ',title,' - updating the article')
#  	  	
#  	  	body = rjson::toJSON(params)
#  	  	url = paste('https://sparkbeyond.freshdesk.com/solution/categories',category_id,'folders',folder_id,'articles',paste0(current_articles_ids[ind],'.json'),sep='/')
#  	  	cmd = paste0('curl -v -u ',freshdeskAppKey,':X -H "Content-Type: application/json" -X PUT -d \'',body,'\' ',url)
#  	  } else {
#  	  	paste0('Could not find ',title,' - creating a new article')
#  	  	params$solution_article$art_type = 1
#  	  	params$solution_article$folder_id = folder_id
#  	  	
#  	  	body = rjson::toJSON(params)
#  	  	url = paste('https://sparkbeyond.freshdesk.com/solution/categories',category_id,'folders',folder_id,'articles.json',sep='/')
#  	  	cmd = paste0('curl -v -u ',freshdeskAppKey,':X -H "Content-Type: application/json" -X POST -d \'',body,'\' ',url)
#  	  }
#  	  res = system(cmd) 
#  }

 
 
 
#depricated below, examples_master should be replaced with freshdesk articles going forward

 #if (verbose) print(body)

# copy examples_master.html
# category_id = 16000049095 
# folder_id = 16000074816 
# article_id = 16000023640
# url = paste('https://sparkbeyond.freshdesk.com/solution/categories',category_id,'folders',folder_id,'articles',paste0(article_id,'.json'),sep='/')
# rm(params)
# params = list()
# params$solution_article$title = "R Package Examples()"
# #params$solution_article$description ="<div><div>For a full screen version please visit&nbsp;<span style=\"font-size: medium;\"><a href=\"https://s3.amazonaws.com/public-sparkbeyond/examples-master.html\" target=\"\">here</a>.&nbsp;</span><br><iframe src=\"https://s3.amazonaws.com/public-sparkbeyond/examples-master.html\" width=\"100%\" height=\"500\" frameborder=\"1\" allowfullscreen=\"\"></iframe></div></div>" 
# #params$solution_article$description ="@/Library/Frameworks/R.framework/Versions/3.2/Resources/library/SparkBeyond/extdata/examples-master.html"
# params$solution_article$description = readr::read_file('/Library/Frameworks/R.framework/Versions/3.2/Resources/library/SparkBeyond/extdata/examples-master.html')
# body = rjson::toJSON(params)
# cmd = paste0('curl -v -u ',freshdeskAppKey,':X -H "Content-Type: application/json" -X PUT -d \'',body,'\' ',url)
# res = system(cmd)
