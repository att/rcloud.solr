# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url

#' Title
#'
#' @param data The body of the request.
#' @param solr.url Usually from \code{solr.url} config. In testing can be \code{http://solr:8983/solr/rcloudnotebooks}
#' @param solr.auth.user Usually from \code{solr.url} config. \code{NULL} in testing
#' @param solr.auth.pwd Usually from \code{solr.url} config. \code{NULL} in testing
#' @param isXML Logical. If TRUE the data argument directly becomes the body and \code{content_type}  is set to "text/xml"
#' @param type One of \code{c("async", "sync", "curl")} usually drawn from config file.
#' @param detach Logical. For mcparallel. Should updates be detached and forgotten about or not?
#'
#' @return The result of the httr::POST (sync). This needs to be unwrapped with a \code{parallel::mccollect} with async, and curl just returns NULL.
#' @rdname solr.post
#'
.solr.post <- function(data,
                       solr.url=rcloud.support:::getConf("solr.url"),
                       solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                       solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd"),
                       isXML=FALSE,
                       type=rcloud.support:::getConf("solr.post.method"),
                       detach = TRUE) {
  content_type <- "application/json"
  body <- rjson::toJSON(list(data))
  httpConfig <- httr::config()

  type <- match.arg(type, c("async", "sync", "curl"))

  # Check if Authentication info exists in the parameters
  if(!is.null(solr.auth.user)) httpConfig <- c(httpConfig,httr::authenticate(solr.auth.user,solr.auth.pwd))
  if(isXML){
    content_type ="text/xml"
    body=data
  }
  if(!is.null(solr.url)){
    solr.post.url <- httr::parse_url(solr.url)
    solr.post.url$path <- paste(solr.post.url$path,"update",sep="/")
    solr.post.url$query <- list(commit = "true")


    switch(type,
           async = parallel::mcparallel(httr::POST(httr::build_url(solr.post.url),
                                                   body=body,
                                                   httr::content_type(content_type),
                                                   config=httpConfig) ,detach=detach),
           sync = tryCatch(httr::POST(httr::build_url(solr.post.url),
                                      body=body,
                                      httr::content_type(content_type),
                                      config=httpConfig),
                           error = function(e) {
                             ulog::ulog("WARN: SOLR POST failed with",
                                  gsub("\n", "\\", as.character(e), fixed=TRUE))
                           }),
           curl = parallel::mcparallel(tryCatch({
             curl <- rcloud.support:::getConf("solr.curl.cmd")
             if (!isTRUE(nzchar(curl))) curl <- "curl"
             f = pipe(.cmd <- paste(curl, "-s", "-S", "-X", "POST", "--data-binary", "@-", "-H",
                                    shQuote(paste("Content-Type:", content_type)),
                                    shQuote(httr::build_url(solr.post.url)), ">/dev/null"), "wb")
             writeBin(charToRaw(body), f)
             close(f)
             parallel:::mcexit()
           }, error = function(e) {
             ulog::ulog("WARN: SOLR POST failed with", gsub("\n", "\\", as.character(e), fixed=TRUE))
           }),detach=detach)
    )
  }
}


#' A POST interface to search
#'
#' Searches are normally made with a GET request. This is a post interface.
#'
#' @inheritParams .solr.get
#'
#' @return The search response
#' @rdname solr.post.search
.solr.post.search <- function(query,
                              solr.url=rcloud.support:::getConf("solr.url"),
                              solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                              solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd")){

  content_type <- "application/json"
  httpConfig <- httr::config()

  body <-  list(q = paste0("{!parent which=doc_type:notebook}", query),
                fl = paste0("id,[child parentFilter=doc_type:notebook childFilter='", query, "']"),
                hl = "true",
                hl.fl = "text")


  # Check if Authentication info exists in the parameters
  if(!is.null(solr.auth.user)) httpConfig <- c(httpConfig,httr::authenticate(solr.auth.user,solr.auth.pwd))

  resp <- tryCatch({

    solr.post.url <- httr::parse_url(solr.url)
    solr.post.url$path <- paste(solr.post.url$path,"query",sep="/")

    httr::POST(httr::build_url(solr.post.url) ,
               body = body,
               httr::accept(content_type),
               config=httpConfig)
  },
  error = function(e) {solr.res$error$msg = e},
  warnings = function(w) {solr.res$error$msg = w}
  )

  resp
}

#' A GET interface to search solr
#'
#' The standard search method with GET.
#'
#' @inheritParams .solr.post
#' @param query Character. The search term.
#'
#' @return The search response from solr
#' @rdname solr.get
.solr.get <- function(query,
                      solr.url=rcloud.support:::getConf("solr.url"),
                      solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                      solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd")){
  solr.get.url <- httr::parse_url(solr.url)
  solr.get.url$path <- paste(solr.get.url$path,"select",sep="/")
  solr.get.url$query <- query
  # https://cwiki.apache.org/confluence/display/solr/Response+Writers
  solr.get.url$query$wt<-"json"
  httpConfig <- httr::config()
  solr.res <- list(error=list(code=solr.get.url$hostname,msg="Unknown Error"))


  if(!is.null(solr.auth.user)) {
    httpConfig <- c(httpConfig,httr::authenticate(solr.auth.user,solr.auth.pwd))
  }

  resp <- tryCatch({
    httr::GET(httr::build_url(solr.get.url),
              httr::content_type_json(),
              httr::accept_json(),
              config=httpConfig)
  },
  error = function(e) {solr.res$error$msg = e},
  warnings = function(w) {solr.res$error$msg = w}
  )

  if(!is.null(resp$message)) solr.res$error$msg <- paste0(solr.get.url$hostname," : ",resp$message)
  else if(!httr::http_error(resp)) solr.res <- rjson::fromJSON(httr::content(resp, "parsed"))
  else solr.res$error$msg <- rawToChar(resp$content)
  return(solr.res)
}
