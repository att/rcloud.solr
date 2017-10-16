

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
#' @param max_pages Sets the size of search
#' @param group.limit Passed to solr. Controls how many cells to highlight for each notebook hit.
#' @param hl.fragsize How many characters to return with the highlighting
#'
#' @return Search response after parsing
#' @export
#'
rcloud.search <-
  function(query,
           all_sources = FALSE,
           sortby = "starcount",
           orderby = "desc",
           start = 0,
           pagesize = 10,
           max_pages = 20,
           group.limit = 4,
           hl.fragsize = 60) {
    .SC$search(
      query = query,
      all_sources = all_sources,
      sortby = sortby,
      orderby = orderby,
      start = start,
      pagesize = pagesize,
      max_pages = max_pages,
      group.limit = group.limit,
      hl.fragsize = hl.fragsize
    )
  }



#' Title
#'
#' @param description
#' @param user
#' @inheritParams rcloud.search
#'
#' @return Search result direct from solr with no parsing
#' @export
#'
#' @examples
#' \dontrun{
#' rcloud.search.description("test", user = "juninho")
#' }
rcloud.search.description <- function(description, user = NULL, start = 0,
                                      pagesize = 100, sortby = "description",
                                      orderby = "desc") {


  url <- rcloud.support:::getConf("solr.url")
  if (is.null(url))
    stop("solr is not enabled")

  user <- if (!is.null(user)) paste(" AND user:", user) else " "
  query <- paste0("description:", description, "*~",
                  user,
                  " AND doc_type:notebook")

  solr.query <- list(
    q = query,
    start = start,
    rows = pagesize,
    indent = "true",
    fl = "description,id,user,updated_at,starcount",
    sort = paste(sortby, orderby)
  )
  # pass it straight back no post-processing
  .solr.get(
    solr.url = url,
    query = solr.query,
    solr.auth.user = rcloud.support:::getConf("solr.auth.user"),
    solr.auth.pwd = rcloud.support:::getConf("solr.auth.pwd")
  )
}
