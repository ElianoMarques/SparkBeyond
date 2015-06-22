# Global variables

# Server Host
# ===========
#' A function to set the SparkBeyond server host.
#' @param host new host URL.
setSBserverHost = function(host = "http://127.0.0.1"){
  assign("SBhost", host, envir = globalenv())
  print(paste("Setting server host to:", SBhost))
  return(SBhost)
}

#' A function to get the SparkBeyond server host.
getSBserverHost = function() {
  host = if (exists("SBhost")) SBhost else setSBserverHost()
  return(host)
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
  print(paste("Setting server port to:", SBport))
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
  } else print("Settings file does not exist.")
}



# Server functions:

#' A function to restart server
#' @return The response from the server.
restartServer = function() {
  url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/die")
  res = httr::GET(url, body = FALSE, httr::content_type_json())
  i = 0
  finalStatus = repeat {
    i = i+1
    print(paste("Waiting for server to load -" ,i))
    secs = 8#min(i*2, 5)
    Sys.sleep(secs)
    if(isServerAlive()) return("Server is up.")
    if (i > 15) return ("Server is failed to load automatically - please load manually.")
  }
  println(finalStatus)
}

#' A function to clear the cache of a specific project
#' @param projectName The project name to clear (e.g., "titanic").
#' @return The response from the server.
clearCache = function(projectName) {
  url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/cleanCache/",projectName)
  res = httr::GET(url, body = FALSE, httr::content_type_json())
  #to verify: list.files(paste0(getSBserverIOfolder(),"/",getSBserverPort(),"/artifacts/",projectName))
  if (res$status == 200) paste("Cleared:",projectName) else "Something went wrong"
}


#' A function to check whether the server is alive
#' @return The response from the server.
isServerAlive = function() {
  url <- paste0(getSBserverHost(),":",getSBserverPort(),"/rapi/heartbeat")
  status = tryCatch({
    res = httr::GET(url, body = FALSE, httr::content_type_json())
    TRUE
    },
    error = function(cond) FALSE
  )
  status
}

#' A function to get the server version information
#' @return The server version information.
serverVersion = function(){
  url <- paste0(getSBserverHost(),":",getSBserverPort(),"/buildInfo")
  status = tryCatch({
    res = httr::GET(url, body = body, httr::content_type_json())
    res <- jsonlite::fromJSON(txt=httr::content(res, as="text"),simplifyDataFrame=TRUE)
    res
  },
  error = function(cond) NA
  )
  status
}

#' A function to verify if we are using the latest server version
#' @return Boolean indicating TRUE if we are using the latest version otherwise FALSE.
isLatestVersion = function(){
  url <- paste0(getSBserverHost(),":",getSBserverPort(),"/isLastBuild")
  latestBuild = tryCatch({
    res = httr::GET(url, body = body, httr::content_type_json())
    res <- httr::content(res, as="text")
    res
  },
  error = function(cond) NA
  )
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  res = if (trim(latestBuild) == serverVersion()$jenkinsBuild) TRUE else {
    print ("Notice: you are currently not using the latest engine version. Please consider running restartServer().")
    FALSE
  }
  #latestBuild
}


#General:
#' A function to update the package from github
updatePackage = function() {
  devtools::install_github("zinman/SBadapter")
}

.onLoad <- function(libname = find.package("SBadapter"), pkgname = "SBadapter") {
  print("Automatically trying to load settings saved in the current directory:")
  loadSettings()
}
