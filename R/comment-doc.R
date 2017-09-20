#' Index a New Comment
#'
#' @param id Notebook ID
#' @param content The new comment content
#' @param comment.id The comment ID
#'
#' @export
solr.post.comment <- function(id, content, comment.id) {
  update_solr_id(id)
}

#' Modify Index for a Comment
#'
#' @param id Notebook ID
#' @param content New comment content
#' @param cid Comment ID
#'
#' @export
solr.modify.comment <- function(id, content, cid) {
  update_solr_id(id)
}

#' Delete Comment from Index
#'
#' @param id Notebook ID
#' @param cid Comment ID
#'
#' @export
solr.delete.comment <- function(id, cid) {
  update_solr_id(id)
}

#' Delete Notebook from Index
#'
#' Delete this notebook and all its child docs
#'
#' @param id Notebook ID
#'
#' @export
solr.delete.doc <- function(id) {
  metadata <-
    paste0('<delete><query>id:', id,
           ' OR notebook_id:', id,
           '</query></delete>')
  .solr.post(data = metadata, isXML = TRUE)
}


