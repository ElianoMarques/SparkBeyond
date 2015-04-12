# Global settings

#' A function to set the SparkBeyond server host.
#' @param host new host URL.
setSBserverHost = function(host = "http://127.0.0.1"){
  assign("SBhost", host, envir = globalenv())
  print(paste("Setting server host to:", SBhost))
  return(host)
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
