#' Index a New Comment
#'
#' @param id Notebook ID
#' @param content The new comment content
#' @param comment.id The comment ID
#'
#' @export
solr.post.comment <- function(id, content, comment.id) {

  ## Post comments to only notebooks with visibility flag true or non encrypted notebooks
  if(rcloud.support::rcloud.is.notebook.visible(id) && !(rcloud.support:::is.notebook.encrypted(id))){


    ## query ID to see if it has existing comments
    query <- list(q=paste0("id:",id),start=0,rows=1000)
    solr.res <- .solr.get(query=query)
    comment.content <- fromJSON(content)

    # Create reponse
    res <- list()
    res$id <- id
    body <- paste(comment.id,':::',
                  comment.content,':::',
                  rcloud.support:::.session$username)
    ## pick set/add depending on the exsitng content
    if(is.null(solr.res$response$docs[[1]]$comments)) {res$comments$set <- body } else { res$comments$add <- body }

    ## send the update request
    metadata <- rjson::toJSON(res)
    .solr.post(data=metadata)
  }
}

#' Modify Index for a Comment
#'
#' @param id Notebook ID
#' @param content New comment content
#' @param cid Comment ID
#'
#' @export
solr.modify.comment <- function(id, content, cid) {

  query <- list(q=paste0("id:",id),start=0,rows=1000)
  solr.res <- .solr.get(query=query)

  cids <- trimws(unlist(lapply(solr.res$response$docs,function(o){lapply(o$comments,function(p){strsplit(p,":")[[1]][1]})})))
  index <- match(cid,cids)
  # If comment does not exist in the index create a new entry
  if(is.na(index)) {solr.post.comment(id,content,cid)} else {
    solr.res$response$docs[[1]]$comments[[index]] <- paste(cid, ':::' ,
                                                           fromJSON(content)$body, ':::' ,
                                                           rcloud.support:::.session$username)
    res <- list()
    res$id <- id
    res$comments$set <- solr.res$response$docs[[1]]$comments
    metadata <- rjson::toJSON(res)
    .solr.post(data=metadata)
  }
}

#' Delete Comment from Index
#'
#' @param id Notebook ID
#' @param cid Comment ID
#'
#' @export
solr.delete.comment <- function(id, cid) {
  query <- list(q=paste0("id:",id),start=0,rows=1000)
  solr.res <- .solr.get(query=query)
  index <- grep(cid, solr.res$response$docs[[1]]$comments)
  solr.res$response$docs[[1]]$comments <- solr.res$response$docs[[1]]$comments[-index]
  metadata <- paste0('{"id":"',id,'","comments":{"set":[\"',paste(solr.res$response$docs[[1]]$comments, collapse="\",\""),'\"]}}')
  .solr.post(data=metadata)
}

#' Delete Notebook from Index
#'
#' @param id Notebook ID
#'
#' @export
solr.delete.doc <- function(id){
  metadata <- paste0('<delete><id>',id,'</id></delete>')
  .solr.post(data=metadata, isXML=TRUE)
}
