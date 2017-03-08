
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
#'
#' @return Search response after parsing
#' @export
#'
rcloud.search <-function(query, all_sources, sortby, orderby, start, pagesize) {

  qid <- tempfile(pattern = "query", fileext = ".json") # TESTING_REMOVE
  res <- list(query = query)                            # TESTING_REMOVE

  url <- rcloud.support:::getConf("solr.url")
  if (is.null(url)) stop("solr is not enabled")

  ## FIXME: The Query comes URL encoded. From the search box? Replace all spaces with +
  ## Check if search terms are already URL encoded?
  if(nchar(query) > nchar(utils::URLdecode(query))) query <- utils::URLdecode(query)

  res$URLdecodequery <- query  # TESTING_REMOVE

  solr.query <- list(q=query,
                     start=start,
                     rows=pagesize,
                     indent="true",
                     hl="true",
                     hl.preserveMulti="true",
                     hl.fragsize=0,
                     hl.maxAnalyzedChars=-1,
                     fl="description,id,user,updated_at,starcount",
                     hl.fl="content,comments",
                     sort=paste(sortby,orderby))

  res$sol.query <- solr.query # TESTING_REMOVE
  res$pagesize <- pagesize    # TESTING_REMOVE
  res$all_sources <- all_sources  # TESTING_REMOVE

  query <- function(solr.url,source='',solr.auth.user=NULL,solr.auth.pwd=NULL) {
    solr.res <- .solr.get(solr.url=solr.url,query=solr.query,solr.auth.user=solr.auth.user,solr.auth.pwd=solr.auth.pwd)

    res$solr.res <<- solr.res  # TESTING_REMOVE

    rcloud.solr::parse.solr.res(solr.res, pagesize = pagesize, source = source)
  }
  if (isTRUE(all_sources)) {
    main <- query(url,
                  solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                  solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd"))
    l <- lapply(rcloud.support:::.session$gist.sources.conf, function(src)
      if ("solr.url" %in% names(src)) query(src['solr.url'],
                                            src['gist.source'],src['solr.auth.user'],src['solr.auth.pwd'])
      else character(0))
    resp <- unlist(c(list(main), l))
  }
  else {
    resp <- query(url,
                  solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                  solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd"))
  }
  res$response <- resp  # TESTING_REMOVE
  json <- jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE)  # TESTING_REMOVE
  writeLines(json, qid)  # TESTING_REMOVE
  resp
}
