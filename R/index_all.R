
#' Index all RCloud Notebooks
#'
#' Sends all notebooks from all users to solr
#'
#' @param pause time in seconds between updateseturn with the highlighting
#' @param batch_size number of documents per update
#' @param commitWithin Number of milliseconds before solr forces commit
#' @param ... Extra arguments passed to \code{.solr.post}
#'
#' @export
#'
index_all <- function(pause = 1, batch_size = 10, commitWithin = 10000, ...) {

  users <- rcloud.support::rcloud.get.users()

  notebook_ids <-
    unname(unlist(
      rcloud.support::rcloud.config.all.notebooks.multiple.users(users)
    ))

  # Turn long list into chunks that we can iterate over
  notebook_ids <- batch_notebooks(notebook_ids, batch_size = batch_size)

  for (batch in notebook_ids) {
    tryCatch({
      result <- update_batch(batch, query = list(commitWithin = commitWithin))

      if ("process" %in% class(result)) {
        result <- parallel::mccollect(result)[[1]]
      }

      if(!is.null(result)) {
        if(exists("status_code", result)) {
          if(result$status_code > 300)
            stop("response code ", result$status_code)
        } else {
          if("try-error" %in% class(result))
            stop(result)
        }
      }

      print(paste("Success", batch))

    }, warning = function(w) {
      print(paste(w, batch))
    }, error = function(e) {
      print(paste(e, batch))
    })

    Sys.sleep(pause)
  }
}


#' Allocate IDs to batches
#'
#' @param notebooks A character vector of notebook IDs
#' @param batch_size Integer with number per batch
#'
#' @return A list of batch vectors
batch_notebooks <- function(notebooks, batch_size) {
  stopifnot(is.vector(notebooks))

  n <- length(notebooks)
  if (n <= batch_size) {
    return(list(notebooks))
  }

  n_batches <- ceiling(n / batch_size)
  index <- rep(1:n_batches, each = batch_size)[1:n]

  unname(split(notebooks, index))
}
