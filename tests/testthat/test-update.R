context("Updating Documents")

test_that("Add a document", {

  if(!check_solr_instance("http://solr")) skip("Needs solr instance")

  nb <- readRDS("notebooks/notebook01.rds")

  response <- update_solr(nb, 1, detach = FALSE)

  on.exit(solr.delete.doc(nb$content$id)) # relies on this function working...

  # Check for the http response status
  response <- parallel::mccollect(response)[[1]]

  expect_equal(response$status_code, 200)

  # Find the solr exit code
  response_content <- rjson::fromJSON(httr::content(response))

  expect_equal(response_content$responseHeader$status, 0)
})
