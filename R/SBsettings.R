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
  if (is.null(folder) || folder == "") {stop("folder location is empty")}
  if (!file.exists(folder)) {
    print (paste("Folder ",  folder, " does not exists - attempting to create"))
    dir.create(folder)
    if (!file.exists(folder)) {stop(paste("failed to create folder", folder))}
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

#' Save settings to the current folder.
saveSettings = function() {
  SB_IOfolder = getSBserverIOfolder()
  SB_HOST = getSBserverHost()
  SB_PORT = getSBserverPort()

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
  url <- paste(getSBserverHost(),":",getSBserverPort(),"/rapi/die", sep="")
  res = httr::POST(url, body = FALSE, httr::content_type_json())
  res
}

#clean cache


#General:
#' A function to update the package from github
updatePackage = function() {
  devtools::install_github("zinman/SBadapter")
}
