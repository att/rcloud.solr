#' Parse the result from a solr search
#'
#' @param solr.res The return value from the GET request
#' @param pagesize solr pagesize
#' @param source passed through from query
#' @param start passed through from query
#' @return The parsed search result
parse.solr.res <- function(solr.res, pagesize, source, start) {
  # Return error message if there is one
  if (!is.null(solr.res$error)) {
    return(c("error", solr.res$error$msg))
  }

  matches <- solr.res$grouped$notebook_id$matches

  # Detect empty response
  if (matches <= 0) {
    response_joined <- NULL
  } else {
    response_docs <- solr.res$grouped$notebook_id$groups
    response_high <- solr.res$highlighting

    response_joined <- join_docs_high(response_docs, response_high)

    # add source to each doc
    response_joined <- lapply(response_joined, function(x) {
      x$source <- source
      x
    })
  }

  # Build the output object
  response <-
    create_search_response(
      solr.res = solr.res,
      response_joined = response_joined,
      pagesize = pagesize,
      source = source,
      start = start
    )

  return(response)
}
