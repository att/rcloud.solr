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

  inherit = SearchSource,

  public = list(
    # Methods
    search = function(...)
      ss_search(self, private, ...)
  ),

  private = list(

    parse_result = function(solr.res, pagesize, start)
      parse.solr.res(solr.res, pagesize, private$source),

    solr.options = list(
      group = "false",
      hl = "true",
      hl.preserveMulti = "true",
      hl.maxAnalyzedChars = -1,
      hl.simple.pre = "span_open_tag",
      hl.simple.post = "span_close_tag",
      hl.fragsize=0,
      fl = "description,id,user,updated_at,starcount,filename, doc_type",
      hl.fl = "content,comments"
    )

  )

)


#' Parse the result from a solr search
#'
#' @param solr.res The return value from the GET request
#' @param pagesize solr pagesize
#' @param source passed through from query
#' @return The parsed search result
#' @import rjson
parse.solr.res <- function(solr.res, pagesize, source) {

  if (!is.null(solr.res$error)) paste0(solr.res$error$code,": ",solr.res$error$msg)
  response.docs <- solr.res$response$docs
  response.high <- solr.res$highlighting

  # Return error message if there is one
  if(!is.null(solr.res$error)) {
    return(c("error",solr.res$error$msg))
  }

  # Process the highlighting
  response.high <- lapply(response.high, parse.response.high)

  # Build the output object
  json <- create.json.output(response.docs, response.high, solr.res, pagesize, source)

  return(json)
}

create.json.output <- function(response.docs, response.high, solr.res, pagesize, source) {


  json <- list(
    QTime = solr.res$responseHeader$QTime,
    status = solr.res$responseHeader$status,
    pagesize = pagesize,
    start = solr.res$response$start,
    source = as.vector(source),
    n_notebooks = solr.res$response$numFound
  )

  docs <- lapply(response.docs, function(x) {
    list(
         id = x$id,
         description = x$description,
         source = as.vector(source),
         starcount = x$starcount,
         updated_at = x$updated_at,
         user = x$user)
    })

  matches = 0
  for(i in seq_along(docs)){
    numFound <- length(response.high[[i]])
    docs[[i]]$doclist <- response.high[[i]]
    docs[[i]]$doclist$numFound <- numFound
    matches <- matches + numFound
  }
  json$matches <- matches

  json$notebooks <- docs

  json
}

parse.response.high <- function(high) {

  if(length(high) != 0) {
    if(!is.null(high$content)) {
      parts.content <- rjson::fromJSON(high$content)
    } else {
      high$content <- "[{\"filename\":\"part1.R\",\"content\":[]}]"
      parts.content <- rjson::fromJSON(high$content)
    }
    if(!is.null(high$comments)) {
      final_res <-list()
      comments <- high$comments
      for(n in 1: length(comments)) {
        cmt_match <- grep("span_open_tag",comments[n])
        if(as.logical(length(cmt_match))) {
          final_res[[length(final_res)+1]] <- comments[n]
        }
      }
      high$comments <- final_res
      parts.content[[length(parts.content)+1]] <- list(filename="comments", content=high$comments)
    }

    # Only keep files with hits
    just.contents <- vapply(parts.content, `[[`, "content", FUN.VALUE = character(1))
    parts.content <- parts.content[grepl("span_open_tag", just.contents)]

    # Easiest to loop round and swap the tags back
    for(i in seq_along(parts.content)) {
      parts.content[[i]]$content <- gsub(
        "span_open_tag",
        "<span class=\"search-result-solr-highlight\">",
        parts.content[[i]]$content
      )
      parts.content[[i]]$content <-
        gsub("span_close_tag", "</span>", parts.content[[i]]$content)

      # Now trim the results down a bit
      cont <-  parts.content[[i]]$content
      i1 <- pmax(1, regexpr("<span ", cont) - 15)
      i2 <- pmin(nchar(cont), regexpr("span>", parts.content[[i]]$content) + 20)
      parts.content[[i]]$content <- substr(cont, i1, i2)
    }


  } else
    parts.content <- list(file = "part1.R", content = "")

  parts.content
}
