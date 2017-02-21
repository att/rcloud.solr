#' Parse the result from a solr search
#'
#' @param solr.res The return value from the GET request
#' @param pagesize solr pagesize
#' @return The parsed search result
#'
#' @export
parse.solr.res <- function(solr.res, pagesize) {

  if (!is.null(solr.res$error)) stop(paste0(solr.res$error$code,": ",solr.res$error$msg))
  response.docs <- solr.res$response$docs
  count <- solr.res$response$numFound
  rows <- solr.res$params$rows
  response.high <- solr.res$highlighting
  if(is.null(solr.res$error)) {
    if(length(response.docs) > 0) {
      for(i in 1:length(response.high)) {
        if(length(response.high[[i]]) != 0) {
          if(!is.null(response.high[[i]]$content)) {
            parts.content <- jsonlite::fromJSON(response.high[[i]]$content)
            for(j in 1:length(parts.content)) {
              splitted <-strsplit(parts.content[[j]]$content,'\n')[[1]]
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
                  parts.content[[j]]$content <- paste0(toString(res))
                }
              }
            }
          } else {
            response.high[[i]]$content <- "[{\"filename\":\"part1.R\",\"content\":[]}]"
            parts.content <- jsonlite::fromJSON(response.high[[i]]$content)
          }
          if(!is.null(response.high[[i]]$comments)) {
            final_res <-list()
            comments <- response.high[[i]]$comments
            for(n in 1: length(comments)) {
              cmt_match <- grep("open_b_close",comments[n])
              if(as.logical(length(cmt_match))) {
                final_res[[length(final_res)+1]] <- comments[n]
              }
            }
            response.high[[i]]$comments <- final_res
            parts.content[[length(parts.content)+1]] <- list(filename="comments", content=response.high[[i]]$comments)
          }
          response.high[[i]]$content <- jsonlite::toJSON(parts.content)
          #Handling HTML content
          response.high[[i]]$content <- gsub("<","&lt;",response.high[[i]]$content)
          response.high[[i]]$content <- gsub(">","&gt;",response.high[[i]]$content)
          response.high[[i]]$content <- gsub("open_b_close","<b style=\\\\\"background:yellow\\\\\">",response.high[[i]]$content)
          response.high[[i]]$content <- gsub("open_/b_close","</b>",response.high[[i]]$content)
        } else
          response.high[[i]]$content <-"[{\"filename\":\"part1.R\",\"content\":[]}]"
      }
      json <- ""
      for(i in 1:length(response.docs)){
        time <- solr.res$responseHeader$QTime
        notebook <- response.docs[[i]]$description
        id <- response.docs[[i]]$id
        starcount <- response.docs[[i]]$starcount
        updated.at <- response.docs[[i]]$updated_at
        user <- response.docs[[i]]$user
        parts <- response.high[[i]]$content
        json[i] <- jsonlite::toJSON(c(QTime=time,notebook=notebook,id=id,starcount=starcount,updated_at=updated.at,user=user,numFound=count,pagesize=pagesize,parts=parts,source=as.vector(source)))
      }
      return(json)
    } else
      return(solr.res$response$docs)
  } else
    return(c("error",solr.res$error$msg))
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
