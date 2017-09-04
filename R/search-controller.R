
#' Search Controller
#'
#' A description of this R6 class
#'
#' @section Details:
#' \code{$new()} starts a new search controller
#'
#' @name SearchController
#' @examples
#' # Might need rcloud.support running for this to work
#' \dontrun{
#' SC <- SearchController$new(something)
#' }
#'
NULL

#' @export

SearchController <- R6::R6Class("SearchController",
  public = list(

    initialize = function(searchConfig)
      sc_initialize(self, private, searchConfig)
  ),

  private = list(
    sources = NULL
  )
)

sc_initialize <- function(self, private, searchConfig) {

  private$sources <- searchConfig$sources

}
