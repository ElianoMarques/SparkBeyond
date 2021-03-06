% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3functions.R
\name{reportingControl}
\alias{reportingControl}
\title{reportingContorl}
\usage{
reportingControl(showWebView = TRUE, emailForNotification = NA,
  scoreOnTestSet = FALSE, featureClustersReport = FALSE,
  featureVisualizations = FALSE, evaluatedFunctionsReport = FALSE)
}
\arguments{
\item{showWebView}{control for whether to show a dynamic web view of the analysis in a browser.}

\item{scoreOnTestSet}{Optional. A boolean representing whether scoring should be provided for the test set. FALSE by default.}

\item{featureClustersReport}{Produce feature cluster visualization. FALSE by default.}

\item{featureVisualizations}{Show visual distribution of the feature against the target for classification problems. FALSE by default.}

\item{evaluatedFunctionsReport}{Creates a report with the entire list of functions that were evaluated. For contextObjects will also show for each object which functions directly used the contextObject.}

\item{emailNotification}{An optional email to notify when the learning is finished.}
}
\description{
reportingContorl
}

