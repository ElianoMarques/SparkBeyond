# Global variables

# Server Host
# ===========
#' A function to set the SparkBeyond server host.
#' @param host new host URL.
setSBserverHost = function(host = "http://127.0.0.1"){
  assign("SBhost", host, envir = globalenv())
  #print(paste("Setting server host to:", SBhost))
  return(SBhost)
}

#' A function to get the SparkBeyond server host.
getSBserverHost = function() {
  host = if (exists("SBhost")) SBhost else setSBserverHost()
  return(host)
}

getSBserverDomain = function() {

	port = getSBserverPort()
	if (substr(getSBserverHost(), 1, 5) == "https" || grepl(":", substr(getSBserverHost(), 6, nchar(getSBserverHost()))) || nchar(port) == 0)
		getSBserverHost()
	else		
		paste0(getSBserverHost(),":", port)
}

#' A function to print the SparkBeyond server host.
printSBserverHost = function() {
  host = getSBserverHost()
  print(paste("Server host is:", host))
}

# Server Port
# ===========
#' A function to set the SparkBeyond server port.
#' @param port new port.
setSBserverPort = function(port = "9000"){
  assign("SBport", port, envir = globalenv())
  #print(paste("Setting server port to:", SBport))
  return(SBport)
}

#' A function to get the SparkBeyond server port.
getSBserverPort = function() {
  port = if (exists("SBport")) SBport else setSBserverPort()
  return(port)
}

#' A function to print the SparkBeyond server port.
printSBserverPort = function() {
  port = getSBserverPort()
  print(paste("Server port is:", port))
}

# Server IO Folder
# ======================
#' A function to set the SparkBeyond server I/O folder.
#' @param port new port.
setSBserverIOfolder = function(folder){
  finalFolder = tryCatch ({
    if (is.null(folder) || nchar(folder) <= 2) stop("Empty folder name was provided to setSBserverIOfolder")
    prefix = if (substr(folder, 1, 2) == "\\\\") {
      curPrefix = substr(folder, 1, 2)
      folder = substr(folder, 3, nchar(folder))
      curPrefix
    } else ""
    folder = paste0(prefix,gsub("\\\\","/",folder))
    if (substr(folder, nchar(folder), nchar(folder)) != "/") folder = paste0(folder, "/")
    #if (substr(folder, nchar(folder)-1, nchar(folder)) != "//") folder = paste0(folder, "/")
    if (is.null(folder) || folder == "") {stop("folder location is empty")}
    if (!file.exists(folder)) {
      print (paste("Folder ",  folder, " does not exists - attempting to create"))
      dir.create(folder)

      #if (!file.exists(folder)) {stop(paste("failed to create folder", folder))} #fails if ends with "/"
    }
    assign("IOfolder", folder, envir = globalenv())
    print(paste("Setting server IO folder to:", IOfolder))
    return(IOfolder)
  }, error = function(e) {
    print(paste("Failed to set SBserverIOfolder. Details:",e))
    return("")
  })
  finalFolder
}

#' A function to get the SparkBeyond server IO folder.
getSBserverIOfolder = function() {
  folder = if (exists("IOfolder")) IOfolder else {
    print("Server IO folder was not defined. Please define folder using setSBserverIOfolder")
    NULL
  }
  return(folder)
}

#' A function to print the SparkBeyond server IO folder.
printSBserverIOfolder = function() {
  folder = getSBserverIOfolder()
  print(paste("Server IO folder is:", folder))
}

#' Print all current settings.
printSettings = function(){
  printSBserverIOfolder()
  printSBserverHost()
  printSBserverPort()
}

#' Save settings to the current folder.
saveSettings = function() {
  SB_IOfolder = getSBserverIOfolder()
  SB_HOST = getSBserverHost()
  SB_PORT = getSBserverPort()
  printSettings()

  save(SB_IOfolder, SB_HOST, SB_PORT, file = "settings.RData")
}

#' Load settings from the current folder.
loadSettings = function() {
  if (file.exists("settings.RData")){
    load("settings.RData")
    setSBserverIOfolder(SB_IOfolder)
    setSBserverHost(SB_HOST)
    setSBserverPort(SB_PORT)
  } else print(paste("Settings file does not exist in", getwd()))
  "Done"
}



# Server functions:

#' A function to restart server
#' @return The response from the server.
restartServer = function() {
  stop("This function has been deprecated, as now jobs are handled by a queue")  #switch to be used for killing a job in the current queue - as a method on Session?
# 	url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/die")
#   tryCatch({
# 	  res = httr::GET(url, httr::content_type_json())
# 	  i = 0
# 	  finalStatus = repeat {
# 	    i = i+1
# 	    print(paste("Waiting for server to load -" ,i))
# 	    secs = 8#min(i*2, 5)
# 	    Sys.sleep(secs)
# 	    if(isServerAlive()) {
# 	    	tryCatch({
# 		      version = serverVersion()
# 		      print(paste("Build:", version$jenkinsBuild)) #alternatively use cat to print multiline
# 		      print(paste("Time:", version$buildTime))
# 	    	})
# 	      return("Server is up.")
# 	    }
# 	    if (i > 15) return ("Server is failed to load automatically - please load manually.")
# 	  }
# 	  print(finalStatus)
#   }, error = function(e) print("Server is down - please load manually"))
}


#' A function to verify if we are using the latest server version
#' @return Boolean indicating TRUE if we are using the latest version otherwise FALSE.
isLatestVersion = function(){
  tryCatch({
     url <- paste0(getSBserverDomain(),"/isLastBuild")
     latestBuild = tryCatch({
       res = httr::GET(url, httr::content_type_json())
       res <- httr::content(res, as="text")
      res
    },
    error = function(cond) NA
    )
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    res = if (is.null(latestBuild) || is.na(latestBuild) || length(latestBuild) == 0){
              print ("Notice: latest build version was not available - if this issue continues please notify SparkBeyond.")
              FALSE
    } else {
      jenkinsBuild = serverVersion()$jenkinsBuild
      if (is.null(jenkinsBuild) || is.na(jenkinsBuild) || length(jenkinsBuild) == 0){
        print ("Notice: Jenkins build information was not available - if this issue continues please notify SparkBeyond.")
        FALSE
      } else { if (trim(latestBuild) == trim(jenkinsBuild)) TRUE else {
        print ("Notice: you are currently not using the latest engine version. Please consider running restartServer().")
        FALSE
      }
    }
    }
    res
  },error = function(e) TRUE #if there is no internet connection than we skip the check
  )
}


#General:
#' A function to update the package from github
updatePackage = function() {
  #if (! isLatestRpackage()) { #just to be on the safe side for now - not included in the if
    devtools::install_github("zinman/SparkBeyond")
  #}
  #assuming package updated successfully
  filename = "SparkBeyondLatestVersion.RData"
  url = "https://api.github.com/repos/zinman/SparkBeyond/git/refs/heads/master"
  res = httr::GET(url, httr::content_type_json())
  res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
  SparkBeyondLatestVersion = res$object$sha
  save(SparkBeyondLatestVersion, file=filename)
}

#' A function to check if the current version is the latest one
isLatestRpackage = function() {
  tryCatch({
    filename = "SparkBeyondLatestVersion.RData"
    prevVersion = ""
    if (file.exists(filename)) {
      load(filename)
      prevVersion = SparkBeyondLatestVersion
    }
    url = "https://api.github.com/repos/zinman/SparkBeyond/git/refs/heads/master"
    res = httr::GET(url, httr::content_type_json())
    res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
    SparkBeyondLatestVersion = res$object$sha

    ret = if (prevVersion == "") {
      save(SparkBeyondLatestVersion, file=filename)
      FALSE
    } else {
      if (prevVersion == SparkBeyondLatestVersion) TRUE
      else {
        print("SparkBeyond package is outdated. Please consider updatePackage()")
        FALSE
      }
    }

    rm(prevVersion)
    rm(SparkBeyondLatestVersion)
    ret
  }, error = function(e) TRUE #if there is no internet connection than we skip the check
  )
}

#' functionCatalog

#' Shows SparkBeyond current function catalog with possible
functionCatalog = function() {
  browseURL(system.file("extdata", "functionCatalog.html", package = "SparkBeyond"))
}

#' login

#' Login to SparkBeyond
login = function(username, password, domain = NA) {	
	if (!is.na(domain)){
		setSBserverHost(domain)
		setSBserverPort("")
	}
	url <- paste0(getSBserverDomain(),"/login")
	res = httr::POST(url, encode = "form", body = list(email=username, password=password))
	loggedIn = if (res$status_code == 404) {		#there is a weird redirection causing this, but this actually OK
		currentUser()
	} else {
		if (res$status_code == 400)	print ("Login failed. Please check your credentials.")
		else  print ("Login failed.")
		FALSE
	}
	loggedIn
}

#' Logout
logout = function() {
	url <- paste0(getSBserverDomain(), "/logout")
	res = httr::GET(url, encode = "form")
	ifelse(res$status_code == 200, 
	 {
	 	print ("You have been logged out")
	 	TRUE
	 }, 
	 FALSE
	)	
}

#' currentUser
#' 
#' Current user information
currentUser = function(showInfo = TRUE) {
	url <- paste0(getSBserverDomain(), "/currentUser")
	res = httr::GET(url, encode = "form")
	loggedIn = if (res$status_code == 200){
		ret = tryCatch({
			userInfo = jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
			if (!is.null(userInfo$user$fullName)) {
				if (showInfo) print (paste("Hello", userInfo$user$fullName, "!  ", paste0("(",userInfo$user$email ,")"), " on ", getSBserverDomain()))							
				TRUE
			} else	FALSE			
		}, 
		error = function(e) FALSE
		)		
		ret				
	} else FALSE
		
	if(!loggedIn)	print("You are currently not logged in - please use the login() function first.")
		
	loggedIn
}

#' showProjectsLocks

#' Shows all the active projects that acquired a lock to prevent multiple project run under the same name 
showProjectsLocks = function() {
	#if(!currentUser(FALSE)) stop("Please login")
	url <- paste0(getSBserverDomain(),"/api2/dblocks")
	res = httr::GET(url)
	jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
}

#' removeProjectLock

#' remove a lock by id (see \code{\link{showProjectsLocks}})
removeProjectLock = function(lockId) { 
	#if(!currentUser(FALSE)) stop("Please login")
	url <- paste0(getSBserverDomain(),"/api2/dblocks/",lockId,"/break")
	res = httr::POST(url)
}

#' showJobs

#' shows the status for all jobs 
#' 
#' A status of a job can be one of "queued", "running", "failed", "canceled", "done"
#' @param projectName Filter by project name.
#' @param revision Option to filter the returned list by revision number
#' @param status Filter by status. 
#' @param showAllColumns A switch for whether to show only the job ID, project name, status, and elapsed (if available). Alternatively show all columns
#' @return a data frame with the jobs
showJobs = function(projectName = NA, revision = NA, status = NA, showAllColumns = FALSE) { 
	#if(!currentUser(FALSE)) stop("Please login")
	query = {if (!is.na(projectName) && is.na(status)) paste0("?project=",projectName)
		else if (is.na(projectName) && !is.na(status)) paste0("?status=",status)
		else if (!is.na(projectName) && !is.na(status)) paste0("?project=",projectName,"&status=",status)
		else ""}
	url <- paste0(getSBserverDomain(),paste0("/api2/jobs", query))
	res = httr::GET(url)
	txt=httr::content(res, as="text")
	if (substr(txt,1,13) == "\n\n\n\n<!DOCTYPE") {  #Giving a second chance
		url <- paste0(getSBserverDomain(),paste0("/api2/jobs", query))
		res = httr::GET(url)
		txt=httr::content(res, as="text")
	}
	jobs = jsonlite::fromJSON(txt = txt, simplifyDataFrame=TRUE) 

	finalJobs = if (length(jobs) > 0) { 		
		if (showAllColumns) jobs else { 
			showCols = c("id", "project")
			if ("revision" %in% colnames(jobs)) showCols = c(showCols, "revision")
			if ("status" %in% colnames(jobs)) showCols = c(showCols, "status")
			if ("elapsed" %in% colnames(jobs)) showCols = c(showCols, "elapsed")
			jobs[,showCols]
		}
	} else {
		NULL
	}
	if ("revision" %in% colnames(finalJobs)) {
		finalJobs$revision = as.numeric(finalJobs$revision)
		if (!is.na(revision)) finalJobs = finalJobs[finalJobs$revision == revision,]
	}
	finalJobs
}

#' showJobById

#' get information for a specific job by job ID
#' @param jobId. The id of the job as defined by \code{\link{showJobs}}
showJobById = function(jobId) {
	#if(!currentUser(FALSE)) stop("Please login")
	url <- paste0(getSBserverDomain(),paste0("/api2/jobs/", jobId))
	res = httr::GET(url)
	jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
}

#' cancelJob

#' cancels a queued job by a job ID
#' @param jobId. The id of the job as defined by \code{\link{showJobs}}
#' @return TRUE if succeeded. FALSE otherwise.
cancelJob = function(jobId){
	#if(!currentUser(FALSE)) stop("Please login")
	url <- paste0(getSBserverDomain(),paste0("/api2/jobs/", jobId,"/cancel"))
	res = httr::POST(url)
	ifelse(res$status == 200, TRUE,{
		print(httr::content(res, as="text"))
		FALSE
	})
}

#' A function to clear the cache of a specific project
#' @param projectName The project name to clear (e.g., "titanic").
#' @return The response from the server.
clearCache = function(projectName) {
	#if(!currentUser(FALSE)) stop("Please login")
	url <- paste0(getSBserverDomain(),"/rapi/cleanCache/",projectName)
	res = httr::GET(url, httr::content_type_json())
	#to verify: list.files(paste0(getSBserverIOfolder(),"/",getSBserverPort(),"/artifacts/",projectName))
	if (res$status == 200) paste("Cleared:",projectName) else "Something went wrong"
}


#' A function to check whether the server is alive
#' @return The response from the server.
isServerAlive = function() {
	url <- paste0(getSBserverDomain(),"/rapi/heartbeat")
	status = tryCatch({
		res = httr::GET(url, httr::content_type_json())
		TRUE
	}, error = function(cond) FALSE
	)
	status
}


#' A function to get the server version information
#' @return The server version information.
serverVersion = function(){
	url <- paste0(getSBserverDomain(),"/buildInfo")
	status = tryCatch({
		res = httr::GET(url, httr::content_type_json())
		res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
		res
	},
	error = function(cond) NA
	)
	status
}

####################################################### 

doesFileExistOnServer = function(projectName, path){
	#if(!currentUser(FALSE)) stop("Please login")
	domain = getSBserverDomain()
	url = paste0(domain, "/api2/download/exists/",projectName,"?path=",path)
	res = httr::GET(url)
	if (res$status==200 && res$url == domain){ #not logged in the first time
		res = httr::GET(url)	#a the second time
		if (res$status==200 && res$url == domain) stop("This operation requires authentication, please log in first")
	} 
	res$status==200 && url == res$url
}

#' uploadToServer
#' 
#' @param data dataFrame to be written to the server
#' @param projectName the name of the project to save the data under
#' @param name the prefix of the file name in which the data will be saved
#' @param useEscaping A binary indicator noting whether a forward slash in the data needs to be escaped
uploadToServer = function(data, projectName, name, useEscaping = TRUE) {
	#if(!currentUser(FALSE)) stop("Please login")
	if (class(data) == "character") data
	else {
		if (! "data.frame" %in% class(data)) stop("The provided data should of type data.frame or character")
		hash = digest(data)
		filename = paste0(name, "_", hash, ".tsv")
		attempts = 2
		succeeded = doesFileExistOnServer(projectName, paste0("/uploaded/", filename))
		if (!succeeded) {  #uploading only if doesn't exist
			urlUpload = paste0(getSBserverDomain(),"/api2/fileUpload/", projectName, "/",filename)
			colHeaders = paste0(colnames(data), collapse = "\t")
			body = paste0(colHeaders, "\n", 
										paste0(apply(cols2Text(data, useEscaping),1,paste0, collapse = "\t"), collapse="\n"))
			
			while (!succeeded && attempts > 0 ) {
				httr::PUT(urlUpload, body = body)	#other options - multiPart / S3/ SSH
				attempts = attempts - 1
				succeeded = doesFileExistOnServer(projectName, paste0("/uploaded/", filename))
			}
		}
		ifelse (succeeded, paste0("/uploaded/",filename), NA)
	}
}

projectRevisions = function(projectName) {
	#if(!currentUser(FALSE)) stop("Please login")	
	url = paste0(getSBserverDomain(), "/analytics/revisions/",projectName)
	res = httr::GET(url)
	ret = if (res$status_code == 200) {
		tryCatch({
			txt =httr::content(res, as="text")
			if (nchar(txt) > 0 & txt != "[]") {
				projectInfo = jsonlite::fromJSON(txt=txt,simplifyDataFrame=TRUE)
				if(!is.null(projectInfo)) {
					projectInfo$name = as.numeric(projectInfo$name) 
				projectInfo
				}else NULL
			} else NULL
		})
	} else NULL
	if (is.null(ret)) print ("Project not found")
	excludeCols(ret, c("jsonClass"), verbose = FALSE)
}

#' writeToServer
#' 
#' A function to write a dateframe to the server. Useful for passing a dataframe to the server for feature search / learning purposes.
#' @param data Data frame or table to export to the server.
#' @param filename Optional. define a name to save the data to. NA by default.
#' @param useEscaping A binary indicator noting whether a forward slash in the data needs to be escaped
#' @return A filepath to the file on the server that was created.
writeToServer = function(data, filename = NA, prefix = "data_in", useEscaping = TRUE){ #TODO: deal with spaces in prefix
	final_filename = if ("data.frame" %in% class(data)){ # we got a data.frame object to be written to server 
		if (is.na(filename)) { # no specific name was provided - use digest to refrain from rewriting to server
			hash = digest(data)
			new_filename = paste0(getSBserverIOfolder(), prefix, "_", hash, ".tsv") 
			new_filenamee = gsub("/+", "/", new_filename)
			if (!file.exists(new_filename)) writeToFile(data, new_filename, useEscaping) #due to hashing we rewrite file only if data has changed
			new_filename		
		} else {			# specific name was provided - rewrite file to server
			new_filename = paste0(getSBserverIOfolder(), filename)
			new_filename = gsub("/+", "/", new_filename)
			writeToFile(data, new_filename, useEscaping)
			new_filename
		}
	} else if (class(data) == "character") { # a filename was provided as data - check if exists and return it
		SBdir = substr(getSBserverIOfolder(), 1, nchar(getSBserverIOfolder())-1) #removing trailing slash
		if (!grepl(SBdir, data)) data = paste0(getSBserverIOfolder(), data)
		if (!file.exists(data)) stop(print(paste("Provided path:", data, "does not exist")))
		data
	} else stop("No valid data.frame or filename was provided")
	return (final_filename)
}

.onLoad <- function(libname = find.package("SparkBeyond"), pkgname = "SparkBeyond") {
  print(paste0("Automatically trying to load settings saved in :",getwd()))
  loadSettings()
}
