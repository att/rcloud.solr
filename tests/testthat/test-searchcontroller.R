context("SearchController")


test_that("Initialize", {
  SC <- SearchController$new(main_source = list(solr.url="http://example.com:8983"))

  sources <- SC$get_sources()

  expect_equal(sources$main$solr.url, "http://example.com:8983")
})

test_that("Set sources", {

  sources <- read_rcloud_conf("rc-two.conf")

  SC <- SearchController$new(main_source = sources$main_source,
                             gist_sources = sources$gist_sources)

  sources_test <- SC$get_sources()

  expect_equal(names(sources_test), c("main_source", "core-lake"))

  urls <- vapply(sources_test, `[[`, character(1), "solr.url")
  exp_urls <- c(
    "http://solr:8983/solr/rcloudnotebooks",
    "http://solr2:8983/solr/rcloudnotebooks"
  )

  expect_equivalent(urls, exp_urls)
})

test_that("Global search instance exists", {

  expect_is(.SC, "SearchController")

  expect_is(.SC, "R6")

})
