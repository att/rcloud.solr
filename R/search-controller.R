# Class functions generally redirect to internal functions beginning sc_*

#' Search Controller
#'
#' An R6 class to control the gathering, sorting and paging of search results.
#'
#' @param main_source a list containing at least \code{solr.url}
#' @param gist_sources a named list of lists containing at least \code{solr.url}
#'
#' @section Details:
#' \code{$new(main_source = NULL, gist_sources = NULL)} starts a new search controller
#'
#' \code{$set_sources(main_source = NULL, gist_sources = NULL)}
#'  If main_source and gist_source are provided they are concatenated.
#'  If not then they are retrieved from \code{rcloud.config} and \code{.session}
#'
#' \code{$get_sources()} return the config
#'
#' @importFrom rcloud.support rcloud.config
#'
#' @name SearchController
#' @examples
#' # Might need rcloud.support running for this to work
#' \dontrun{
#' SC <- SearchController$new(config = list(solr.url="http://solr:8983/solr/rcloudnotebooks"))
#' }
#'
NULL

#' @export

SearchController <- R6::R6Class("SearchController",

  public = list(

    initialize = function(main_source = NULL, gist_sources = NULL)
      sc_initialize(self, private, main_source, gist_sources),

    set_sources = function(main_source = NULL, gist_sources = NULL)
      sc_set_sources(self, private, main_source, gist_sources),

    get_sources = function() {
      private$sources
    }
  ),

  private = list(
    sources = NULL,
    last_search = NULL,
    results = list(),
    n_results = 0
  )
)

sc_initialize <- function(self, private, main_source, gist_sources) {

  self$set_sources(main_source, gist_sources)

  invisible(self)
}

sc_set_sources <- function(self, private, main_source, gist_sources) {

  sources <- list()

  # Get the main config from rcloud.config
  if (is.null(main_source)) {
    # TODO, can we grab all configs that begin with solr?
    main_source <- list(
      solr.url = rcloud.config("solr.url"),
      solr.auth.user = rcloud.config("solr.auth.user"),
      solr.auth.pwd = rcloud.config("solr.auth.pwd")
    )
  }

  # Get external sources from .session object
  if (is.null(gist_sources)) {
    gist_sources <-
      lapply(rcloud.support:::.session$gist.sources.conf, as.list)
  }

  # Combine and make sure that main goes first
  sources <- c(list(main_source = main_source), gist_sources)

  # TODO: Check the sources

  if(names(sources)[1] != "main_source") stop("First source must be called \"main_source\"")

  private$sources <- sources
}
