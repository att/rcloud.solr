context("Updating Documents")

test_that("Add a document", {

  if(!check_solr_instance("http://solr")) skip("Needs solr instance")

  nb <- readRDS("notebooks/notebook01.rds")

  with_mock(
    `rcloud.support::rcloud.is.notebook.visible` = function(id) TRUE,
    `rcloud.support:::is.notebook.encrypted` = function(id) FALSE,
    response <- update_solr(nb, 1, detach = FALSE)
  )

  on.exit(solr.delete.doc(nb$content$id)) # relies on this function working...

  # Check for the http response status
  response <- parallel::mccollect(response)[[1]]

  expect_equal(response$status_code, 200)

  # Find the solr exit code
  response_content <- rjson::fromJSON(httr::content(response))

  expect_equal(response_content$responseHeader$status, 0)
})

test_that("Build meta data", {

  nb <- readRDS("notebooks/notebook01.rds")

  metadata <- build_update_metadata(nb, starcount = 2)

  # Lists or vectors
  # Vectors shouldn't be named
  for(i in seq_along(metadata)) {
    if(!is.list(metadata[[i]])) {
      expect_true(is.vector(metadata[[i]]), info = names(metadata)[i])

      expect_null(names(metadata[[i]]))
    }
  }

  # Check a few values
  expect_equal(metadata$id, "010b0b4451ff152e6c62")
  expect_equal(metadata$user, "rcloud")

  # Check that the content is valid when fromJSON'd
  content_files <- metadata[["_childDocuments_"]]

  expect_is(content_files, "list")

  # Also checks that any names were stripped off
  expected_list <- list(id = "010b0b4451ff152e6c62part1.R", filename = "part1.R", content = "hist(mtcars$disp)\n")
  test_list <- content_files[[1]][c("id", "filename", "content")]
  expect_equal(test_list, expected_list)

})



test_that("Recursive metadata process", {

  test_list <- list(a = 1, b = c(q=1,w=2), c = list(c=1, d=c(r="r", t="t")), d = NULL)

  exp_result <- list(a = 1, b = c(1, 2), c = list(c=1, d=c("r", "t")), d = "")

  test_result <- lapply(test_list, process_metadata_list)

  expect_equal(test_result, exp_result)

})

