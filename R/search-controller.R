# Class functions generally redirect to internal functions beginning sc_*

#' Search Controller
#'
#' A description of this R6 class
#'
#' @section Details:
#' \code{$new()} starts a new search controller
#'
#' \code{$set_config()} retrieves settings from rcloud.support or allows them to be specified by the user
#'
#' \code{$set_config()} return the config
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

    initialize = function(config = NULL)
      sc_initialize(self, private, config),

    set_config = function(config = NULL)
      sc_set_config(self, private, config),

    get_config = function() {
      private$config
    }
  ),

  private = list(
    config = NULL,
    last_search = NULL,
    results = list(),
    n_results = 0
  )
)

sc_initialize <- function(self, private, config) {

  # You can initialise with a config or load one afterwards
  if(!is.null(config)) {
    self$set_config(config)
  }

  invisible(self)
}

sc_set_config <- function(self, private, config) {

  # Load what we can from rcloud.support and merge together
  # TODO: cope with failure
  rcs_config <- list(
    solr.auth.user = rcloud.support:::getConf("solr.auth.user"),
    solr.url = rcloud.support:::getConf("solr.url")
  )

  # User can rcloud.support
  if (!is.null(config)) {
    # Merge with config taking priority
    rcs_config <- utils::modifyList(rcs_config, config, keep.null = TRUE)
  }

  # TODO: Check the config

  private$config <- rcs_config
}
