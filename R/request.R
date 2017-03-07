# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url

.solr.post <- function(data,
                       solr.url=rcloud.support::getConf("solr.url"),
                       solr.auth.user=rcloud.support::getConf("solr.auth.user"),
                       solr.auth.pwd=rcloud.support::getConf("solr.auth.pwd"),
                       isXML=FALSE) {
  content_type <- "application/json"
  body = paste("[",data,"]",sep='')
  httpConfig <- httr::config()

  # Check if Authentication info exists in the parameters
  if(!is.null(solr.auth.user)) httpConfig <- c(httpConfig,httr::authenticate(solr.auth.user,solr.auth.pwd))
  if(isXML){
    content_type ="text/xml"
    body=data
  }
  if(!is.null(solr.url)){
    solr.post.url <- httr::parse_url(solr.url)
    solr.post.url$path <- paste(solr.post.url$path,"update?commit=true",sep="/")
    mcparallel(httr::POST(build_url(solr.post.url) , body=body,add_headers('Content-Type'=content_type), config=httpConfig) ,detach=TRUE)
  }
}

.solr.get <- function(query,
                      solr.url=rcloud.support::getConf("solr.url"),
                      solr.auth.user=rcloud.support::getConf("solr.auth.user"),
                      solr.auth.pwd=rcloud.support::getConf("solr.auth.pwd")){
  solr.get.url <- httr::parse_url(solr.url)
  solr.get.url$path <- paste(solr.get.url$path,"select",sep="/")
  solr.get.url$query <- query
  # https://cwiki.apache.org/confluence/display/solr/Response+Writers
  solr.get.url$query$wt<-"json"
  httpConfig <- httr::config()
  solr.res <- list(error=list(code=solr.get.url$hostname,msg="Unknown Error"))


  if(!is.null(solr.auth.user)) httpConfig <- c(httpConfig,httr::authenticate(solr.auth.user,solr.auth.pwd))
  resp <- tryCatch({
    httr::GET(build_url(solr.get.url),content_type_json(),accept_json(),config=httpConfig)
  },
  error = function(e) {solr.res$error$msg = e},
  warnings = function(w) {solr.res$error$msg = w}
  )

  if(!is.null(resp$message)) solr.res$error$msg <- paste0(solr.get.url$hostname," : ",resp$message)
  else if(!httr::http_error(resp)) solr.res <- fromJSON(content(resp, "parsed"))
  else solr.res$error$msg <- rawToChar(resp$content)
  return(solr.res)
}
