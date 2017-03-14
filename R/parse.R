#' Parse the result from a solr search
#'
#' @param solr.res The return value from the GET request
#' @param pagesize solr pagesize
#' @param source passed through from query
#' @return The parsed search result
#' @import rjson
parse.solr.res <- function(solr.res, pagesize, source) {

  if (!is.null(solr.res$error)) paste0(solr.res$error$code,": ",solr.res$error$msg)
  response.docs <- solr.res$response$docs
  response.high <- solr.res$highlighting

  # Return error message if there is one
  if(!is.null(solr.res$error)) {
    return(c("error",solr.res$error$msg))
  }

  # Detect empty response
  if(length(response.docs) <= 0) {
    return(solr.res$response$docs)
  }

  # Process the highlighting
  response.high <- lapply(response.high, parse.response.high)

  # Build the output object
  json <- create.json.output(response.docs, response.high, solr.res, pagesize, source)

  return(json)
}

create.json.output <- function(response.docs, response.high, solr.res, pagesize, source) {

  count <- solr.res$response$numFound
  rows <- solr.res$params$rows

  json <- ""
  for(i in 1:length(response.docs)){
    time <- solr.res$responseHeader$QTime
    notebook <- response.docs[[i]]$description
    id <- response.docs[[i]]$id
    starcount <- response.docs[[i]]$starcount
    updated.at <- response.docs[[i]]$updated_at
    user <- response.docs[[i]]$user
    parts <- response.high[[i]]$content
    json[i] <- toJSON(c(QTime=time,notebook=notebook,id=id,starcount=starcount,updated_at=updated.at,user=user,numFound=count,pagesize=pagesize,parts=parts,source=as.vector(source)))
  }

  json
}
