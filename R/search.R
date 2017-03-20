
# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url


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

  url <- rcloud.support:::getConf("solr.url")
  if (is.null(url)) stop("solr is not enabled")

  # TESTING REMOVE
  q0 <- query

  ## FIXME: The Query comes URL encoded. From the search box? Replace all spaces with +
  ## Check if search terms are already URL encoded?
  if(nchar(query) > nchar(utils::URLdecode(query))) query <- utils::URLdecode(query)

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

  ################## TESTING REMOVE #################################
  if(!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Need jsonlite to output search query, response")
  }
  qid <- tempfile(pattern = "query", fileext = ".json")
  res <- list(query = q0,
              URLdecodequery = query,
              sol.query = solr.query,
              pagesize = pagesize,
              all_sources = all_sources)
  ###################################################################

  query <- function(solr.url,source='',solr.auth.user=NULL,solr.auth.pwd=NULL) {
    solr.res <- .solr.get(solr.url=solr.url,query=solr.query,solr.auth.user=solr.auth.user,solr.auth.pwd=solr.auth.pwd)

    res$solr.res <<- solr.res  ######### TESTING_REMOVE

    parse.solr.res(solr.res, pagesize = pagesize, source = source)
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

  ######################### TESTING REMOVE ###########################
  res$response <- resp
  json <- jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json, qid)
  ####################################################################

  resp
}
