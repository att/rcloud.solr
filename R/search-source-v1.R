# Class functions generally redirect to internal functions beginning ss_*

#' Search a Version 1 Source
#'
#' An R6 class to make requests to individual src_params sources. Child of \code{\link{SearchSource}}
#'
#' @name SearchSourceV1
#' @examples
#' \dontrun{
#' SS <-
#' SearchSourceV1$new(
#'   src_params = list(source =  "main_source",
#'                     solr.url = "http://solr:8983/solr/rcloudnotebooks")
#' )
#' }
#'
NULL

#' @export

SearchSourceV1 <- R6::R6Class(
  "SearchSourceV1",

  inherit = SearchSource#,

  # public = list(
  # )

)

