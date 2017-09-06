
#' Search RCloud Notebooks
#'
#' Main search function exposed as an OCAP to the client.
#'
#' @param query Search string
#' @param all_sources Logical: Search single or multiple solr instances?
#' @param sortby Passed to solr for sorting
#' @param orderby Passed to solr for sorting
#' @param start Passed to solr
#' @param pagesize Passed to solr
#' @param group.limit Passed to solr. Controls how many cells to highlight for each notebook hit.
#' @param hl.fragsize How many charachters to return with the highlighting
#'
#' @return Search response after parsing
#' @export
#'
rcloud.search <-function(query, all_sources = FALSE, sortby = "starcount", orderby = "desc",
                         start = 0, pagesize = 10, group.limit = 4,  hl.fragsize=60) {

  # We'll be calling out to the controller

  # Keep this API constant

}

#' Search a single source
#'
#' For a single source, query solr and return results. For a single source system
#' this should be the same as \code{rcloud.search}
#'
#' @inheritParams rcloud.search
#' @param solr.url URL for the solr request
#' @param solr.auth.user Solr Authentication, username
#' @param solr.auth.pwd Solr Authentication, password
#'
#' @return Search response after parsing
#' @export
#'
ss_search <- function(query, solr.url, solr.auth.user = NULL, solr.auth.pwd = NULL,
                      sortby, orderby, start = 0, pagesize = 10,
                      group.limit = 4,  hl.fragsize=60) {

  ## FIXME: The Query comes URL encoded. From the search box? Replace all spaces with +
  ## Check if search terms are already URL encoded?
  ## DOUG Move this up to controller?
  if(nchar(query) > nchar(utils::URLdecode(query))) query <- utils::URLdecode(query)

  solr.query <- list(q=query,
                     start=start,
                     rows=pagesize,
                     indent="true",
                     group="true",
                     group.field="notebook_id",
                     group.limit=group.limit,
                     group.ngroups="true",
                     hl="true",
                     hl.preserveMulti="true",
                     hl.fragsize=hl.fragsize,
                     hl.maxAnalyzedChars=-1,
                     hl.simple.pre = "<span class=\"search-result-solr-highlight\">",
                     hl.simple.post = "</span>",
                     fl="description,id,user,updated_at,starcount,filename, doc_type",
                     hl.fl="content,comments",
                     sort=paste(sortby,orderby))

  # Make the request
  solr.res <- .solr.get(
    solr.url = solr.url,
    query = solr.query,
    solr.auth.user = solr.auth.user,
    solr.auth.pwd = solr.auth.pwd
  )

  # Parse the response
  resp <- parse.solr.res(
    solr.res,
    pagesize = pagesize,
    source = source, #
    start = start)

  resp
}
