context("SearchController")


test_that("Initialize", {
  SC <- SearchController$new(sources = list(
    main_source = list(solr.url = "http://example.com:8983")
    ))

  sources <- SC$get_sources()

  expect_equal(sources$main_source$get_source(), "main_source")
})

test_that("Set two sources", {

  sources <- read_rcloud_conf("rc-two.conf")

  SC <- SearchController$new(sources = sources)

  sources_test <- SC$get_sources()

  expect_equal(names(sources_test), c("main_source", "core-lake"))

  urls <- vapply(sources_test, function(x) x$get_solr_url(), character(1))
  exp_urls <- c(
    "http://solr:8983/solr/rcloudnotebooks",
    "http://solr2:8983/solr/rcloudnotebooks"
  )

  expect_equivalent(urls, exp_urls)
})

test_that("Set one source", {

  sources <- read_rcloud_conf("rc-one.conf")

  SC <- SearchController$new(sources = sources)

  sources_test <- SC$get_sources()

  expect_equal(names(sources_test), "main_source")

  urls <- vapply(sources_test, function(x) x$get_solr_url(), character(1))
  exp_urls <- "http://solr:8983/solr/rcloudnotebooks"

  expect_equivalent(urls, exp_urls)
})


test_that("Global search instance exists", {

  expect_is(.SC, "SearchController")

  expect_is(.SC, "R6")

})
