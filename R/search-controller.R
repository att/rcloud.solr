# Class Definition --------------------------------------------------------

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

    search = function(all_sources, start, pagesize, sortby, orderby, ...)
      sc_search(self, private, all_sources, start, pagesize, sortby, orderby, ...),

    new_search = function(all_sources, sortby, orderby, ...)
      sc_new_search(self, private, all_sources, sortby, orderby, ...),

    build_response = function(start, pagesize)
      sc_build_response(self, private, start, pagesize),

    get_sources = function() private$sources,
    get_raw_results = function() private$raw_results,
    get_results = function() private$results
  ),

  private = list(
    sources = NULL,
    last_search = NULL,
    raw_results = list(),
    results = list(),
    n_results = 0
  )
)


# Methods -----------------------------------------------------------------

sc_initialize <- function(self, private, sources) {

  # This is called onload so account for situations where rcloud.support
  # doesn't have a config
  if(!is.null(rcloud.config("solr.url"))) {
    self$set_sources(sources)
  }

  invisible(self)
}

sc_set_sources <- function(self, private, sources) {
  # Get the main config from rcloud.config
  if (is.null(sources)) {
    sources <- try(sc_get_rcloud_sources())
    if("try-error" %in% class(sources)) {
      ulog::ulog("ERROR: SOLR source failed to initialise:", gsub("\n", "\\", as.character(sources)))
      return(NULL)
    }
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
  private$sources <- lapply(source_named, sc_create_source)

  # Remove bad sources
  private$sources <- private$sources[!vapply(private$sources, is.null, logical(1))]
}

#' Retrieve Solr Sources From RCloud Config
#'
sc_get_rcloud_sources <- function() {

  # TODO, can we grab all configs that begin with solr?
  main_source <- list(
    solr.url = rcloud.config("solr.url"),
    solr.auth.user = rcloud.config("solr.auth.user"),
    solr.auth.pwd = rcloud.config("solr.auth.pwd"),
    solr.api.version = rcloud.config("solr.api.version")
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
sc_search <- function(self, private, all_sources, start, pagesize, sortby, orderby, ...) {

  # If things went wrong try not to fall over
  if(length(private$sources)<1) {
    return(list(error = list (msg = "No valid sources")))
  }

  if(!all_sources && !exists("main_source", private$sources)) {
    return(list(error = list (msg = "Main source not valid")))
  }

  if (start == 0) {
    # Update cached results from all sources
    self$new_search(
      all_sources = all_sources,
      start = start,
      sortby = sortby,
      orderby = orderby,
      ...
    )
  }

  self$build_response(start, pagesize)

}

sc_new_search <- function(self, private, all_sources, sortby, orderby, ...) {

  sources <- if(all_sources) private$sources else private$sources[1]

  # This can be parallelised
  private$raw_results <- lapply(sources, function(src) {
    src$search(sortby = sortby, orderby = orderby, ...)
  })

  private$results <- sc_merge_results(private$raw_results, sortby = sortby, orderby = orderby)

  invisible(private$results)

}

sc_build_response <- function(self, private, start, pagesize) {

  nn <- private$results$header$n_notebooks

  header <- private$results$header
  header$start <- start
  header$pagesize <- pagesize

  notebooks <- NULL

  if(nn > start + pagesize) {
    notebooks <- private$results$notebooks[(start + 1):(start + pagesize)]
  } else if (nn > start) {
    notebooks <- private$results$notebooks[(start + 1):nn]
  }

  response <- c(header, list(notebooks = notebooks))

  response

}


# Internal Functions ------------------------------------------------------

sc_create_source <- function(source) {

  if(!exists("solr.auth.user", source)) {
    source$solr.auth.user <- NULL
  }
  if(!exists("solr.auth.pwd", source)) {
    source$solr.auth.pwd <- NULL
  }

  schema_version <- sc_schema_version(solr.url = source$solr.url,
                                      solr.auth.user = source$solr.auth.user,
                                      solr.auth.pwd = source$solr.auth.pwd)

  if(!is.numeric(schema_version)) {
    # something bad happened
    ulog::ulog("ERROR: Failed to retrieve schema for source")
    NULL
  }
  else if(schema_version == 1)
    SearchSourceV1$new(source)
  else
    SearchSource$new(source)
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

  list(header = header, notebooks = notebooks)

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

  if(!is.null(all_notebooks) && length(all_notebooks) > 0) {

  order_col <- switch(sortby,
                      "updated_at" = sc_order_timestamp(all_notebooks, sortby, orderby),
                      sc_order_default(all_notebooks, sortby, orderby))

  all_notebooks <- all_notebooks[order_col]

  }

  all_notebooks

}

sc_order_default <- function(all_notebooks, sortby, orderby) {

  sort_col <- sapply(all_notebooks, `[[`, sortby)
  # should be a vector. If not try to correct it
  if(is.list(sort_col)) {
    sort_col <- sc_infer_sort_col(sort_col)
  }

  decreasing <- isTRUE(orderby == "desc")

  order(sort_col, decreasing = decreasing)

}

sc_order_timestamp <- function(all_notebooks, sortby, orderby) {

  sort_col_raw <- vapply(all_notebooks, `[[`, sortby, FUN.VALUE = character(1))

  sort_col <- as.POSIXct(sort_col_raw, format = "%Y-%m-%dT%H:%M:%S")

  decreasing <- isTRUE(orderby == "desc")

  order(sort_col, decreasing = decreasing)

}

# Try to convert to a vector of sortable results
sc_infer_sort_col <- function(sort_col) {
  sort_lengths <- vapply(sort_col, length, numeric(1))

  if(any(sort_lengths > 1)) stop("Sort column returned with multiple values")

  #Replace NULLS with NA
  sort_nulls <- vapply(sort_col, is.null, logical(1))
  sort_col[sort_nulls] <- NA

  # Convert to char and us conversion (similar to read.table)
  sort_char <- vapply(sort_col, as.character, character(1))
  utils::type.convert(sort_char, as.is = TRUE)
}

#' Get the schema version
#'
#' @param path The GET route
#' @inheritParams .solr.post
#'
#' @return The version as a string
#'
sc_schema_version <- function(path = "schema/version",
                              solr.url, solr.auth.user, solr.auth.pwd) {

  resp <- .solr.get.generic(query = NULL, path = path,
                            solr.url = solr.url,
                            solr.auth.user = solr.auth.user,
                            solr.auth.pwd = solr.auth.pwd)

  if(exists("version", resp)) {
    version <- tryCatch({
      maj.version <- gsub("(^[0-9]+).*", "\\1", as.character(resp$version))
      as.numeric(maj.version)
    },
    error = function(e) {
      list(error = e)
    })
  } else {
    version <- resp
  }
  version
}
