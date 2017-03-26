
.install = function(domain=NULL) {
	
	validateServerDomain = function(domain) {
		url <- paste0(domain, "/api/heartbeat")
		tryCatch ({
			response = httr::GET(url, httr::content_type_json())
			if(httr::status_code(response) != 200) {
				stop("Couldn't connect to the server, please verify that server domain ", domain, " is correct")
			}
		}, error = function(e) {
			stop("Couldn't connect to the server: ", e$message, ". Please verify that server domain ", domain, " is correct")
		})
	}
	
	downloadPackage = function(domain, localPath) {
		download_url = paste(domain, 'sdk/r', sep = '/')
		
		message(paste("Downloading the R SDK package from:", download_url))
		
		response = httr::GET(download_url, config = httr::progress(type = "down"), httr::write_disk(localPath, overwrite = TRUE))
		status = httr::status_code(response) 
		if(status == 200) {
			message("Successfully downloaded the package")
		} else {
			stop(paste0("Failed to download the package from: ", download_url, ", response status: ", status, ", error: ", httr::content(response, as = "text")))
		}
	}

	domain <- if(is.null(domain)) {
		enteredDomain = readline(prompt="Enter your server domain: ")
		if (is.null(enteredDomain)) {
			stop("No domain was entered")
		}
		enteredDomain
	} else {
		domain
	}
	
	# Remove the last slash if needed
	if(substr(domain, nchar(domain), nchar(domain)) == "/") {
		domain = substr(domain, 1, nchar(domain)-1)
	}
	
	if (!require("devtools", quietly = TRUE)) install.packages("devtools")
	
	validateServerDomain(domain)
	
	baseTempDir = tempdir()
	tempFilePath = paste(baseTempDir, "SparkBeyond.tar.gz", sep='/')
	tempExtractedFolderPath = paste(baseTempDir, "SparkBeyond", sep='/')
	
	if(file.exists(tempFilePath)) file.remove(tempFilePath)
	unlink(tempExtractedFolderPath, recursive = TRUE)
	
	downloadPackage(domain, tempFilePath)
	
	untar(tempFilePath, exdir = baseTempDir)

	message("Installing the package")
	devtools::install_local(tempExtractedFolderPath)
}
