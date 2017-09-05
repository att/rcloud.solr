
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

  url <- rcloud.support:::getConf("solr.url")
  if (is.null(url)) stop("solr is not enabled")


  ## FIXME: The Query comes URL encoded. From the search box? Replace all spaces with +
  ## Check if search terms are already URL encoded?
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


  query <- function(solr.url,source='',solr.auth.user=NULL,solr.auth.pwd=NULL) {
    solr.res <- .solr.get(solr.url=solr.url,query=solr.query,solr.auth.user=solr.auth.user,solr.auth.pwd=solr.auth.pwd)

    parse.solr.res(solr.res, pagesize = pagesize, source = source, start = start)
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

  resp
}
