context("Comments")

test_that("build comment docs", {

  nb <- readRDS("notebooks/notebook01.rds")

  notebook_info <- build_notebook_info(nb, starcount = 2)

  comments <- build_comments(notebook_info)

  # Check all four comments were found
  expect_equal(length(comments), 4)

  required_names <- c("id", "doc_type", "filename", "content", "notebook_id", "description")

  # Check the fields went in
  expect_true(all(required_names %in% names(comments[[2]])))

  # Check that they are listed as comments
  expect_equal(comments[[2]]$doc_type, "comment")

})
