# Class functions generally redirect to internal functions beginning ss_*

#' Search Source
#'
#' An R6 class to make requests to individual solr sources.
#'
#' @section Details:
#' \code{$new(source = NULL)} starts a new search source
#'
#' @name SearchSource
#' @examples
#' \dontrun{
#' SS <- SearchSource$new(source list(solr.url = "http://solr:8983/solr/rcloudnotebooks"))
#' }
#'
NULL

#' @export

SearchSource <- R6::R6Class(
  "SearchSource",

  public = list(
    initialize = function(source)
      ss_initialize(self, private, source)
  ),

  private = list(
    solr.url = NULL,
    solr.auth.user = NULL,
    solr.auth.pwd = NULL
  )
)

ss_initialize <- function(self, private, source) {

  if(!exists("solr.url", source))
    stop("Must have at least solr.url in source")

  # Copy the bits we want into the source instance
  private$solr.url <- source$solr.url
  if (exists("solr.auth.user", source))
    private$solr.auth.user <- source$solr.auth.user
  if (exists("solr.auth.pwd", source))
    private$solr.auth.pwd <- source$solr.auth.pwd

  invisible(self)
}

