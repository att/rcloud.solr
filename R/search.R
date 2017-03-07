#' Parse the result from a solr search
#'
#' @param solr.res The return value from the GET request
#' @param pagesize solr pagesize
#' @param source passed through from query
#' @return The parsed search result
#' @import rjson
#'
#' @export
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

parse.response.high <- function(high) {

  if(length(high) != 0) {
    if(!is.null(high$content)) {
      parts.content <- lapply(rjson::fromJSON(high$content), style.highlighting)
    } else {
      high$content <- "[{\"filename\":\"part1.R\",\"content\":[]}]"
      parts.content <- rjson::fromJSON(high$content)
    }
    if(!is.null(high$comments)) {
      final_res <-list()
      comments <- high$comments
      for(n in 1: length(comments)) {
        cmt_match <- grep("open_b_close",comments[n])
        if(as.logical(length(cmt_match))) {
          final_res[[length(final_res)+1]] <- comments[n]
        }
      }
      high$comments <- final_res
      parts.content[[length(parts.content)+1]] <- list(filename="comments", content=high$comments)
    }
    high$content <- toJSON(parts.content)
    #Handling HTML content
    high$content <- gsub("<","&lt;",high$content)
    high$content <- gsub(">","&gt;",high$content)
    high$content <- gsub("open_b_close","<b style=\\\\\"background:yellow\\\\\">",high$content)
    high$content <- gsub("open_/b_close","</b>",high$content)
  } else
    high$content <-"[{\"filename\":\"part1.R\",\"content\":[]}]"

  high
}

# This is the bit of the code that styles the highlighting
style.highlighting <- function(part.content) {

  splitted <-strsplit(part.content$content,'\n')[[1]]
  res <-list()
  for(k in 1: length(splitted)) {
    is_match <- grep("open_b_close",splitted[[k]])
    is_match_next <- NULL
    is_match_next2 <- NULL
    if(k < length(splitted)){
      is_match_next <- grep("open_b_close",splitted[[k+1]])
    }
    if(k < (length(splitted) -2)){
      is_match_next2 <- grep("open_b_close",splitted[[k+2]])
    }
    if(as.logical(length(is_match))) {
      if(!as.logical(length(is_match_next)) && !as.logical(length(is_match_next2)) ) {
        if(as.logical(length(splitted[k-1]) == "") | k ==1) {
          res[k] <- stitch.search.result(splitted,'optB',k)
        } else {
          if(as.logical(length(splitted[k-1]) != "")) {
            res[k] <- stitch.search.result(splitted,'optC',k)
          }
        }
      } else if (as.logical(length(is_match_next)) && !as.logical(length(is_match_next2)) ) {
        if(k !=1)
          res[k] <- stitch.search.result(splitted,'optD',k)
      } else {
        res[k] <- stitch.search.result(splitted,'default',k)
      }
    }
    if(k == length(splitted)) {
      res[sapply(res, is.null)] <- NULL
      part.content$content <- paste0(toString(res))
    }
  }

  part.content
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

stitch.search.result <- function(splitted, type,k) {
  #Using '|-|' as delimitter here as <br>,/n or anything else might be the content of HTML
  switch(type,
         optA = paste0(k-1,'line_no',splitted[k-1],'|-|',k,'line_no',splitted[k],'|-|',k+1,'line_no',splitted[k+1],sep='|-|'),
         optB = paste0(k,'line_no',splitted[k],'|-|',k+1,'line_no',splitted[k+1],sep='|-|'),
         optC = paste0(k-1,'line_no',splitted[k-1],'|-|',k,'line_no',splitted[k],sep='|-|'),
         optD = paste0(k-1,'line_no',splitted[k-1],sep='|-|'),
         default = paste0(k,'line_no',splitted[k],sep='|-|'))
}
