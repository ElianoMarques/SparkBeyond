category_id = 16000049489
folder_id = 16000076425
url = paste('https://sparkbeyond.freshdesk.com/solution/categories',category_id,'folders',folder_id,'articles.json',sep='/')

freshdeskAppKey = '' # add correct key

files = list.files('~/SparkBeyond/html','*.html')
for (f in files[-c(1,2)]) { # remove 00frame_toc.html and 00Index.html
	params$solution_article$title = sub(".html","",f)
	params$solution_article$description = gsub(" href=.*?>",">",readr::read_file(f)) # read in and sanitize links
	params$solution_article$art_type = 1
	params$solution_article$folder_id = folder_id
	body = rjson::toJSON(params)
	cmd = paste0('curl -v -u ',freshdeskAppKey,':X -H "Content-Type: application/json" -X POST -d \'',body,'\' ',url)
	res = system(cmd)
}

#if (verbose) print(body)
