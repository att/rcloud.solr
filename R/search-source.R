# Class functions generally redirect to internal functions beginning ss_*

#' Search Source
#'
#' An R6 class to make requests to individual src_params sources.
#'
#' @param src_name Character. Name of the source.
#' @param src_params A list of solr details. Must at least contain \code{solr.url}.
#'
#' @section Details:
#' \code{$new(src_name, src_params)} starts a new search source
#'
#' \code{$search(...)} search this source. Parameters are all based on \code{\link{ss_search}}.
#'
#' @name SearchSource
#' @examples
#' \dontrun{
#' SS <-
#' SearchSource$new(
#'   src_name = "main_source",
#'   src_params = list(solr.url = "http://solr:8983/solr/rcloudnotebooks")
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

