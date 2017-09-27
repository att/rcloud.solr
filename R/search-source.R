# Class functions generally redirect to internal functions beginning ss_*

#' Search Source
#'
#' An R6 class to make requests to individual src_params sources.
#'
#' @param src_params A list of solr details. Must at least contain
#'   \code{solr.url}, and the source name, \code{source}.
#'
#' @section Details:
#' \code{$new(src_name, src_params)} starts a new search source
#'
#' \code{$search(...)} search this source. Parameters are all based on
#'   \code{\link{ss_search}}.
#'
#' @name SearchSource
#' @examples
#' \dontrun{
#' SS <-
#' SearchSource$new(
#'   src_params = list(source =  "main_source",
#'                     solr.url = "http://solr:8983/solr/rcloudnotebooks")
#' )
#' }
#'
NULL

#' @export

SearchSource <- R6::R6Class(
  "SearchSource",

  public = list(
    initialize = function(src_params)
      ss_initialize(self, private, src_params),

    search = function(...) {
      ss_search(
        source = private$source,
        solr.url = private$solr.url,
        solr.auth.pwd = private$solr.auth.pwd,
        solr.auth.user = private$solr.auth.user,
        ...
      )},

    get_source = function() private$source,
    get_solr_url = function() private$solr.url
  ),

  private = list(
    source = NULL,
    solr.url = NULL,
    solr.auth.user = NULL,
    solr.auth.pwd = NULL
  )
)

ss_initialize <- function(self, private, src_params) {

  if(!exists("solr.url", src_params))
    stop("Must have solr.url in src_params")

  if(!exists("source", src_params))
    stop("Must have source in src_params")

  # Copy the bits we want into the source instance
  private$solr.url <- src_params$solr.url
  private$source <- src_params$source
  if (exists("solr.auth.user", src_params))
    private$solr.auth.user <- src_params$solr.auth.user
  if (exists("solr.auth.pwd", src_params))
    private$solr.auth.pwd <- src_params$solr.auth.pwd

  invisible(self)
}

#' Search a single source
#'
#' For a single source, query solr and return results. For a single source system
#' this should be the same as \code{rcloud.search}
#'
#' @inheritParams rcloud.search
#' @param solr.url URL for the solr request
#' @param solr.auth.user Solr authentication, username
#' @param solr.auth.pwd Solr authentication, password
#' @param source The name of the source
#'
#' @return Search response after parsing
#' @export
#'
ss_search <- function(query,
                      solr.url, solr.auth.user = NULL, solr.auth.pwd = NULL,
                      source, sortby, orderby,
                      start = 0, pagesize = 10, max_pages = 20,
                      group.limit = 4,  hl.fragsize=60) {

  ## FIXME: The Query comes URL encoded. From the search box? Replace all spaces with +
  ## Check if search terms are already URL encoded?
  ## DOUG Move this up to controller?
  if(nchar(query) > nchar(utils::URLdecode(query))) query <- utils::URLdecode(query)

  rows <- max(pagesize * max_pages, 10)

  solr.query <- list(q=query,
                     start=start,
                     rows=rows,
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

