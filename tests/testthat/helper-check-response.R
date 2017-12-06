check_response_notebooks <- function(response) {
  stopifnot(exists("matches", response))
  stopifnot(exists("pagesize", response))
  stopifnot(exists("n_notebooks", response))
  if(response$matches == 0) return(TRUE)

  exp_n_notebooks <- min(response$n_notebooks, response$pagesize)
  expect_equal(length(response$notebooks), exp_n_notebooks)

  for(notebook in response$notebooks) {
    for (x in c("id", "description", "source", "starcount", "updated_at", "user", "score")) {
      expect_true(exists(x, notebook))
    }
  }
}

check_response_docs <- function(response) {

  # The doclists should all have a docs element
  doclists <- lapply(response$notebooks, `[[`, "doclist")
  for (dl in doclists) {
    expect_true(exists("docs", dl))
  }

  # Get the docs and flatten
  docs <- do.call("c", lapply(doclists, `[[`, "docs"))
  for (doc in docs) {
    check_response_one_doc(doc)
  }
}

check_response_one_doc <- function(doc) {

  for (x in c("id", "doc_type", "filename", "highlighting")) {
    expect_true(exists(x, doc), info = paste(x, "missing from doc"))
  }
  expect_true(exists("content", doc$highlighting))
}
