# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url

.solr.post <- function(data,
                       solr.url=rcloud.support:::getConf("solr.url"),
                       solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                       solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd"),
                       isXML=FALSE,
                       type=rcloud.support:::getConf("solr.post.method"),
                       detach = TRUE) {
  type <- match.arg(type, c("async", "sync", "curl"))
  content_type <- "application/json"
  body <- rjson::toJSON(list(data))
  httpConfig <- httr::config()

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
          async = parallel::mcparallel(httr::POST(build_url(solr.post.url),
                                        body=body,
                                        add_headers('Content-Type'=content_type),
                                        config=httpConfig),
                             detach=detach),
          sync = tryCatch(httr::POST(build_url(solr.post.url),
                                     body=body,
                                     add_headers('Content-Type'=content_type),
                                     config=httpConfig),
                          error = function(e) {
                              ulog("WARN: SOLR POST failed with",
                                   gsub("\n", "\\", as.character(e), fixed=TRUE))
                            }),
          curl = parallel::mcparallel(tryCatch({
            curl <- rcloud.support:::getConf("solr.curl.cmd")
            if (!isTRUE(nzchar(curl))) curl <- "curl"
            f = pipe(.cmd <- paste(curl, "-s", "-S", "-X", "POST", "--data-binary", "@-", "-H",
                                   shQuote(paste("Content-Type:", content_type)),
                                   shQuote(build_url(solr.post.url)), ">/dev/null"), "wb")
            writeBin(charToRaw(body), f)
            close(f)
            parallel:::mcexit()
          }, error = function(e) {
               ulog("WARN: SOLR POST failed with", gsub("\n", "\\", as.character(e), fixed=TRUE))
             }),detach=detach)
          )
  }
}

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
