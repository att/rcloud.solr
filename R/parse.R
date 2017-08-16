#' Parse the result from a solr search
#'
#' @param solr.res The return value from the GET request
#' @param pagesize solr pagesize
#' @param source passed through from query
#' @param start passed through from query
#' @return The parsed search result
parse.solr.res <- function(solr.res, pagesize, source, start) {

  # Return error message if there is one
  if(!is.null(solr.res$error)) {
    return(c("error",solr.res$error$msg))
  }

  matches <- solr.res$grouped$notebook_id$matches

  # Detect empty response
  if(matches <= 0) {
    response_joined <- NULL
  } else {

    response_docs <- solr.res$grouped$notebook_id$groups
    response_high <- solr.res$highlighting

    response_joined <- join_docs_high(response_docs, response_high)
  }

  # Build the output object
  response <- create_search_response(solr.res = solr.res, response_joined = response_joined, pagesize = pagesize,
                                     source = source, start = start)

  return(response)
}

#' Join the grouped documents to the highlighted documents
#'
#' @param docs The groups object from the solr response object
#' @param highlight The highlighting object from the solr response object
#'
#' @return A list similar to \code{docs} but with highlighting added in
join_docs_high <- function(docs, highlight) {

  lapply(docs, join_one_doc_high, highlight)

}

# Join a single document to the highlighting
# Supports join_docs_high
join_one_doc_high <- function(doc, highlight) {

  # Retrieve some notebook
  top_doc <- doc$doclist$docs[[1]]

  # rename groupValue to id
  out_doc <- c(list(id = doc$groupValue),
               top_doc[which(names(top_doc) %in% c("description", "updated_at", "starcount", "user"))])


  # Copy the doc list
  out_doc$doclist <- doc$doclist

  # lookup the highlighting for each doc
  out_doc$doclist$docs <- lapply(doc$doclist$docs, join_highlight, highlight)

  out_doc
}

# Lookup the highlighting for a match and add it in
join_highlight <- function(doc, highlight) {

  # Drop the notebook-specific content
  out <- doc[which(names(doc) %in% c("id", "filename", "doc_type"))]

  # Attach the highlighting (this is a linear lookup)
  out$highlighting <- highlight[[doc$id]]

  out
}

create_search_response <- function(solr.res, response_joined, pagesize, source, start) {


  response <- list(QTime = solr.res$responseHeader$QTime,
                   status = solr.res$responseHeader$status,
                   start = start,
                   pagesize = pagesize,
                   source =unname(as.vector(source)),
                   matches = solr.res$grouped$notebook_id$matches,
                   n_notebooks = solr.res$grouped$notebook_id$ngroups,
                   notebooks = response_joined)

  response
}
