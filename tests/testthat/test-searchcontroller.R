context("SearchController")


test_that("Initialize", {
  SC <- SearchController$new(main_source = list(solr.url="http://example.com:8983"))

  sources <- SC$get_sources()

  expect_equal(sources$main$solr.url, "http://example.com:8983")
})

test_that("Global search instance exists", {

  expect_is(.SC, "SearchController")

  expect_is(.SC, "R6")

})
