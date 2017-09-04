context("SearchController")


test_that("Initialize", {
  SC <- SearchController$new(config = list(solr.url="http://example.com:8983"))

  config <- SC$get_config()

  expect_equal(config$solr.url, "http://example.com:8983")
})

test_that("Global search instance exists", {

  expect_is(.SC, "SearchController")

  expect_is(.SC, "R6")

  expect_null(.SC$get_config())

})
