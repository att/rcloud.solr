# Class functions generally redirect to internal functions beginning sc_*

#' Search Controller
#'
#' An R6 class to control the gathering, sorting and paging of search results.
#'
#' @param sources a named list of sources. Each source must be a list containing
#' at least \code{solr.url}. The first source must be called \code{main_source}.
#'
#' @section Details:
#' \code{$new(sources = NULL)} starts a new search controller
#'
#' \code{$set_sources(sources)}
#'  If sources is provided they are concatenated they are
#'  retrieved from \code{rcloud.config} (\code{main_source})
#'  and \code{.session} (\code{gist_sources}).
#'
#' \code{$get_sources()} return the config
#'
#' @importFrom rcloud.support rcloud.config
#'
#' @name SearchController
#' @examples
#' \dontrun{
#' SC <-
#' SearchController$new(sources = list(
#'  main_source = list(solr.url = "http://solr:8983/solr/rcloudnotebooks")
#' ))
#' }
#'
NULL



#' @export

SearchController <- R6::R6Class("SearchController",

  public = list(

    initialize = function(sources = NULL)
      sc_initialize(self, private, sources),

    set_sources = function(sources = NULL)
      sc_set_sources(self, private, sources),

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

sc_initialize <- function(self, private, sources) {

  self$set_sources(sources)

  invisible(self)
}

sc_set_sources <- function(self, private, sources) {
  # Get the main config from rcloud.config
  if (is.null(sources)) {
    sources <- list()

    # TODO, can we grab all configs that begin with solr?
    main_source <- list(
      solr.url = rcloud.config("solr.url"),
      solr.auth.user = rcloud.config("solr.auth.user"),
      solr.auth.pwd = rcloud.config("solr.auth.pwd")
    )

    gist_sources <-
      lapply(rcloud.support:::.session$gist.sources.conf, as.list)

    # Combine and make sure that main goes first
    sources <- c(list(main_source = main_source), gist_sources)
  }

  # Check the sources
  if (names(sources)[1] != "main_source")
    stop("First source must be called \"main_source\"")

  has_url <- lapply(sources, function(x) "solr.url" %in% names(x))
  if(!all(has_url))
    stop("All sources must have solr.url at a minumum")test

  private$sources <- sources
}
