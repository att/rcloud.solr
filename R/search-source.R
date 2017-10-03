# Class Definition --------------------------------------------------------

# Class functions generally redirect to internal functions beginning ss_*

#' Search Source
#'
#' An R6 class to make requests to individual src_params sources.
#'
#' @param src_params A list of solr details. Must at least contain
#'   \code{solr.url}, and the source name, \code{source}.
#'
#' @section Members:
#' \describe{
#'   \item{source}{The name of the source. This is \code{"main_source"} for the default, otherwise it's pulled from the gist-source.}
#'   \item{solr.url}{URL of the solr instance, including the path to the correct core. e.g. \code{http://solr:8983/solr/rcloudnotebooks}.}
#'   \item{solr.auth.user}{For authenticated instances, the user name.}
#'   \item{solr.auth.pwd}{For authenticated instances, the password.}
#'   }
#'
#' @section Public Methods:
#' \code{$new(src_name, src_params)} starts a new search source
#'
#' \code{$search(...)} search this source. Parameters are all based on
#'   \code{\link{rcloud.search}}.
#'
#'
#' \code{$get_source} get the \code{source} element
#'
#' \code{$get_solr_url} get the \code{solr.url} element
#'
#' @section Private Methods:
#'
#' \code{$parse_result(solr.res, pagesize, start)} parse the result of a search. This function can be swapped out for different versions.
#'
#' @name SearchSource
#' @examples
#' SS <-
#' SearchSource$new(
#'   src_params = list(source =  "main_source",
#'                     solr.url = "http://solr:8983/solr/rcloudnotebooks")
#' )
#'
#' SS$get_source()
NULL

#' @export

SearchSource <- R6::R6Class(
  "SearchSource",

  public = list(
    # Methods
    initialize = function(src_params)
      ss_initialize(self, private, src_params),

    search = function(...)
      ss_search(self, private, ...),

    get_source = function() private$source,
    get_solr_url = function() private$solr.url
  ),

  private = list(
    # Methods
    parse_result = function(solr.res, pagesize, start)
      ss_parse_result(self, private, solr.res, pagesize, start),

    # Members
    source = NULL,
    solr.url = NULL,
    solr.auth.user = NULL,
    solr.auth.pwd = NULL
  )
)


# Methods -----------------------------------------------------------------

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

# Search a single source
#
# This function is close to what used to be the main rcloud.search function. Now the results are
# handled by the search controller.
#
ss_search <- function(self,
                      private,
                      query,
                      sortby,
                      orderby,
                      start = 0,
                      pagesize = 10,
                      max_pages = 20,
                      group.limit = 4,
                      hl.fragsize = 60,
                      group = "true",
                      group.field = "notebook_id") {


  ## FIXME: The Query comes URL encoded. From the search box? Replace all spaces with +
  ## Check if search terms are already URL encoded?
  ## DOUG Move this up to controller?
  if(nchar(query) > nchar(utils::URLdecode(query))) query <- utils::URLdecode(query)

  rows <- max(pagesize * max_pages, 10)

  solr.query <- list(q=query,
                     start=start,
                     rows=rows,
                     indent="true",
                     group=group,
                     group.field=group.field,
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
    query = solr.query,
    solr.url = private$solr.url,
    solr.auth.user = private$solr.auth.user,
    solr.auth.pwd = private$solr.auth.pwd
  )

  # Parse the response
  resp <- private$parse_result(
    solr.res,
    pagesize = pagesize,
    start = start)

  resp
}

# Parse the result from a solr search into the format expected by controller
#
ss_parse_result <- function(self, private, solr.res, pagesize, start) {
  # Return error message if there is one
  if (!is.null(solr.res$error)) {
    return(c("error", solr.res$error$msg))
  }

  matches <- solr.res$grouped$notebook_id$matches

  # Detect empty response
  if (matches <= 0) {
    response_joined <- NULL
  } else {
    response_docs <- solr.res$grouped$notebook_id$groups
    response_high <- solr.res$highlighting

    response_joined <- ss_join_docs_high(response_docs, response_high)

    # add source to each doc
    response_joined <- lapply(response_joined, function(x) {
      x$source <- private$source
      x
    })
  }

  # Build the output object
  response <-
    ss_create_search_response(
      solr.res = solr.res,
      response_joined = response_joined,
      pagesize = pagesize,
      source = private$source,
      start = start
    )

  return(response)
}


# Internal Functions ------------------------------------------------------

#' Join the grouped documents to the highlighted documents
#'
#' @param docs The groups object from the solr response object
#' @param highlight The highlighting object from the solr response object
#'
#' @return A list similar to \code{docs} but with highlighting added in
ss_join_docs_high <- function(docs, highlight) {
  lapply(docs, ss_join_one_doc_high, highlight)

}

# Join a single document to the highlighting
# Supports join_docs_high
ss_join_one_doc_high <- function(doc, highlight) {
  # Retrieve some notebook
  top_doc <- doc$doclist$docs[[1]]

  select_fields <- c("description", "updated_at", "starcount", "user", "source")

  # rename groupValue to id and select items from top_doc
  out_doc <- c(list(id = doc$groupValue),
               top_doc[which(names(top_doc) %in% select_fields)])


  # Copy the doc list
  out_doc$doclist <- doc$doclist

  # lookup the highlighting for each doc
  out_doc$doclist$docs <-
    lapply(doc$doclist$docs, ss_join_highlight, highlight)

  out_doc
}

# Lookup the highlighting for a match and add it in
ss_join_highlight <- function(doc, highlight) {

  # Drop the notebook-specific content
  out <- doc[which(names(doc) %in% c("id", "filename", "doc_type"))]

  # Attach the highlighting (this is a linear lookup)
  out$highlighting <- highlight[[doc$id]]

  out
}

# Combine all of the results into a list with the right names, which can go back to the controller
ss_create_search_response <- function(solr.res, response_joined, pagesize, source, start) {


  response <- list(QTime = solr.res$responseHeader$QTime,
                   status = solr.res$responseHeader$status,
                   start = start,
                   pagesize = pagesize,
                   source =unname(as.vector(source)),
                   matches = solr.res$grouped$notebook_id$matches,
                   n_notebooks = solr.res$grouped$notebook_id$ngroups,
                   notebooks = response_joined)

  response
}


