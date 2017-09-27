

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
