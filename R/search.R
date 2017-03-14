
# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url

#' Update the index after a notebook change
#'
#' @param notebook The notebook object as stored by rcloud.support
#' @param starcount notebook starcount
#'
#' @export
update_solr <- function(notebook, starcount){
  #Update only notebooks which are visible
  if(rcloud.support::rcloud.is.notebook.visible(notebook$content$id) &&
     !(rcloud.support:::is.notebook.encrypted(notebook$content$id))){
  ## FIXME: gracefully handle unavailability
  content.files <- notebook$content$files
  ## Remove binary assets by removing elements with .b64 extention
  content.files <- content.files[unlist(lapply(names(content.files),function(o){utils::tail(strsplit(o,split="\\.")[[1]],1) != "b64"}))]
  fns <- as.vector(sapply(content.files, function(o) o$filename))
  ## only index cells for now ...
  ## FIXME: do we really want to exclude the scratch file?
  if (length(content.files)) {
    sizes <- as.numeric(sapply(content.files, function(o) o$size))
    size <- sum(sizes, na.rm=TRUE)
    desc <- notebook$content$description
    desc <- gsub("^\"*|\"*$", "", desc)
    desc <- gsub("^\\\\*|\\\\*$", "", desc)
    if (length(grep("\"",desc) == 1)) {
      notebook.description <- strsplit(desc,'\"')
      desc <- paste(notebook.description[[1]],collapse="\\\"")
    } else if(length(grep("\\\\",desc) == 1)){
      notebook.description <- strsplit(desc,'\\\\')
      desc <- paste(notebook.description[[1]],collapse="\\\\")
    } else {
      desc
    }
    session.content <- notebook$content
    ## FIXME: followers is not in the notebook, set to 0 for now
    metadata<-paste0('{\"id\":\"',session.content$id, '\",\"user\":\"',session.content$user$login, '\",\"created_at\":\"',session.content$created_at, '\",\"updated_at\":\"',session.content$updated_at, '\",\"description\":\"',desc, '\",\"user_url\":\"',session.content$user$url, '\",\"avatar_url\":\"',session.content$user$avatar_url, '\",\"size\":\"',size, '\",\"commited_at\":\"',session.content$updated_at, '\",\"followers\":\"',0, '\",\"public\":\"',session.content$public, '\",\"starcount\":\"',starcount, '\",\"content\":{\"set\":\"\"}}')
    metadata.list <- fromJSON(metadata)
    content.files <- unname(lapply(content.files, function(o) list('filename'=o$filename,'content'=o$content)))
    content.files <- toJSON(content.files)
    metadata.list$content$set <- content.files
    completedata <- toJSON(metadata.list)
    .solr.post(data=completedata)
    }
  }
}



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
  qid <- tempfile(pattern = "query", fileext = ".json")
  res <- list(query = q0,
              URLdecodequery = query,
              sol.query = solr.query,
              pagesize = pagesize,
              all_sources = all_sources)
  ###################################################################

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

  ######################### TESTING REMOVE ###########################
  res$response <- resp
  json <- jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json, qid)
  ####################################################################

  resp
}
