category_id = 16000049489  # https://sparkbeyond.freshdesk.com/solution/categories/16000049489
folder_id = 16000076461 # https://sparkbeyond.freshdesk.com/solution/folders/16000076425
# right now folder is hard coded
# TODO add code to query folder, delete it and create a new one

url = paste('https://sparkbeyond.freshdesk.com/solution/categories',category_id,'folders',folder_id,'articles.json',sep='/')

freshdeskAppKey = '' # add correct key
#uncomment below lines to run/copy to freshdesk

# files = list.files('html','*.html')
# threshold_date = file.info(paste0('html/',files[1]))$mtime
# params = list(1)  # init params
# for (f in files[-c(1,2)]) { # remove 00frame_toc.html and 00Index.html
# 	if (as.Date(file.info(paste0('html/',f))$mtime)>=as.Date(threshold_date)) {
# 		params$solution_article$title = sub(".html","",f)
# 		params$solution_article$description = gsub(" href=.*?>",">",readr::read_file(f)) # read in and sanitize links
# 		params$solution_article$art_type = 1
# 		params$solution_article$folder_id = folder_id
# 		body = rjson::toJSON(params)
# 		cmd = paste0('curl -v -u ',freshdeskAppKey,':X -H "Content-Type: application/json" -X POST -d \'',body,'\' ',url)
# 		res = system(cmd) 
# 	}
# }

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
