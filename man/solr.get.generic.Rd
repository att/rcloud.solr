% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request.R
\name{.solr.get.generic}
\alias{.solr.get.generic}
\title{A generic get request}
\usage{
.solr.get.generic(query = NULL, path = "select",
  solr.url = rcloud.support:::getConf("solr.url"),
  solr.auth.user = rcloud.support:::getConf("solr.auth.user"),
  solr.auth.pwd = rcloud.support:::getConf("solr.auth.pwd"))
}
\arguments{
\item{query}{optional list}

\item{path}{the route relative to the solr core}

\item{solr.url}{Usually from \code{solr.url} config. In testing can be
\code{http://solr:8983/solr/rcloudnotebooks}}

\item{solr.auth.user}{Usually from \code{solr.url} config. \code{NULL}
in testing}

\item{solr.auth.pwd}{Usually from \code{solr.url} config. \code{NULL} in
testing}
}
\value{
The response
}
\description{
A generic get request
}
