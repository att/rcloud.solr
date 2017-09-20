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

    search = function(all_sources, start, ...)
      sc_search(self, private, all_sources, start, ...),

    new_search = function(all_sources, ...)
      sc_new_search(self, private, all_sources, ...),

    get_sources = function() private$sources,
    get_raw_results = function() private$raw_results
  ),

  private = list(
    sources = NULL,
    last_search = NULL,
    raw_results = list(),
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
    sources <- sc_get_rcloud_sources()
  }

  # Check the sources
  if (names(sources)[1] != "main_source")
    stop("First source must be called \"main_source\"")

  has_url <- vapply(sources, function(x) "solr.url" %in% names(x), logical(1))
  if(!all(has_url))
    stop("All sources must have solr.url at a minumum")

  # Pull the names into the list
  source_named <- mapply(sources, names(sources),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)


  # Create new instances of the SearchSource class
  private$sources <- lapply(source_named, SearchSource$new)
}

#' Retrieve Solr Sources From RCloud Config
#'
sc_get_rcloud_sources <- function() {

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

  sources
}

#' Main search interface
#'
#' Called by rcloud.search
#'
#' @inheritParams rcloud.search
#' @param self pointer to this object
#' @param private private members
#' @param ... arguments to pass to \code{ss_search}
sc_search <- function(self, private, all_sources, start, ...) {

  if(start == 0) {
    # Update cached results from all sources
    self$new_search(all_sources = all_sources, start = start, ...)
  }

  # TODO
  # prepare the response
  private$raw_results[["main_source"]]
}

sc_new_search <- function(self, private, all_sources, ...) {

  sources <- if(all_sources) private$sources else private$sources[1]

  # This can be parallelised
  private$raw_results <- lapply(sources, function(src) {
    src$search(...)
  })

}

#' Merge Raw Results
#'
#' Take a list of raw results and merge into one big list of results
#'
#' @param raw_results A list of results from solr returned from the SearchSource
#'   objects.
#' @inheritParams rcloud.search
#'
#' @return A single result set, merged and sorted
sc_merge_results <- function(raw_results, sortby, orderby) {

  header <- sc_merge_header(raw_results)

  notebooks <- sc_merge_notebooks(raw_results, sortby, orderby)

  c(header, list(notebooks = notebooks))

}

sc_merge_header <- function(raw_results) {

  # drop_items and keep_items are in utils.R
  summary_fields <- c("QTime", "status", "matches", "n_notebooks")
  drop_fields <- c("notebooks", "source")
  # Combine variables that we summarise into a data.frame
  headers_summary <- lapply(raw_results, keep_items, summary_fields)
  headers_summary <- lapply(headers_summary, as.data.frame, stringsAsFactors = FALSE)
  headers_df <- do.call(rbind, headers_summary)
  # All other variables should be the same, take from the first source
  headers_pass <- drop_items(raw_results[[1]], c(summary_fields, drop_fields))

  # Combine summary variables and passthrough variables
  header <- c(list(
    QTime = max(headers_df$QTime),
    status = max(headers_df$status),
    matches = sum(headers_df$matches),
    n_notebooks = sum(headers_df$n_notebooks)
  ),
  headers_pass)

  header

}

sc_merge_notebooks <- function(raw_results, sortby, orderby) {

  raw_notebooks <- lapply(raw_results, `[[`, "notebooks")

  all_notebooks <- do.call(c, unname(raw_notebooks))

  sort_col <- vapply(all_notebooks, `[[`, sortby, FUN.VALUE = numeric(1))

  decreasing <- isTRUE(orderby == "desc")

  order_col <- order(sort_col, decreasing = decreasing)

  all_notebooks[order_col]

}
