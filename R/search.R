
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
#' @param hl.fragsize How many characters to return with the highlighting
#'
#' @return Search response after parsing
#' @export
#'
rcloud.search <-function(query, all_sources = FALSE, sortby = "starcount",
                         orderby = "desc",
                         start = 0, pagesize = 10, group.limit = 4,  hl.fragsize=60) {

  # Keep this API constant

  sources <- .SC$get_sources()

  # How can we make this prettier?
  results <- lapply(sources,
                    function(x)
                      x$search(
                        query = query,
                        sortby = sortby,
                        orderby = orderby,
                        start = start,
                        pagesize = pagesize,
                        group.limit = group.limit,
                        hl.fragsize = hl.fragsize
                      ))

  results[["main_source"]]
}

