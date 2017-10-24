context("SearchController")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

if(check_solr_instance("http://solr2")) {

  url2 <- make_solr_url("http://solr2", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url2, body = httr::upload_file("notebooks/allnotebooks2.json"))

  on.exit({
    httr::POST(url2, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  }, add = TRUE)
}

if(check_solr_instance("http://solrv1")) {

  url2 <- make_solr_url("http://solrv1", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url2, body = httr::upload_file("notebooks/allnotebooks_v1.json"))

  on.exit({
    httr::POST(url2, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  }, add = TRUE)
}


test_that("Initialize Bad", {
  SC <- SearchController$new(sources = list(
    main_source = list(solr.url = "http://example.com/")
    ))

  sources <- SC$get_sources()

  expect_null(sources$main_source)

  bad_search <- SC$search("hist")
  expect_equal(bad_search$error$msg, "No valid sources")

})

test_that("Initialize NULL", {
  SC <- SearchController$new(NULL)

  sources <- SC$get_sources()

  if (!check_solr_instance("http://solr")) {
    expect_null(sources$main_source)

    bad_search <- SC$search(all_sources = FALSE, query = "hist", start = 0, pagesize = 10,
                            sortby = "starcount", orderby = "desc")
    expect_equal(bad_search$error$msg, "No valid sources")
  }
  else {
    expect_named(sources, 'main_source')
    good_search <- SC$search(all_sources = FALSE, query = "hist", start = 0, pagesize = 10,
                            sortby = "starcount", orderby = "desc")
    expect_equal(good_search$n_notebooks, 12)
  }



})


test_that("Set two sources", {

  skip_if_not(check_solr_instance("http://solr"))
  skip_if_not(check_solr_instance("http://solr2"))

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

  skip_if_not(check_solr_instance("http://solr"))

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

test_that("New search two sources", {

  skip_if_not(check_solr_instance("http://solr"))
  skip_if_not(check_solr_instance("http://solr2"))

  sources <- read_rcloud_conf("rc-two.conf")

  SC <- SearchController$new(sources = sources)

  SC$new_search(
    "hist",
    all_sources = TRUE,
    sortby = "starcount",
    orderby = "desc",
    start = 0,
    pagesize = 10,
    group.limit = 4
  )

  # Check the raw results
  raw_results <- SC$get_raw_results()
  expect_named(raw_results, c("main_source", "core-lake"))

  matches <- unname(vapply(raw_results, `[[`, FUN.VALUE = numeric(1), "matches"))
  expect_equal(matches, c(14, 14))

  # Check merged results
  results <- SC$get_results()

  expect_named(results, c("header", "notebooks"))
  expect_equal(length(results$notebooks), 24)

})

test_that("Full search two sources", {

  skip_if_not(check_solr_instance("http://solr"))
  skip_if_not(check_solr_instance("http://solr2"))

  sources <- read_rcloud_conf("rc-two.conf")

  SC <- SearchController$new(sources = sources)

  response <- SC$search(
    "hist",
    all_sources = TRUE,
    sortby = "starcount",
    orderby = "desc",
    start = 0,
    pagesize = 10,
    max_pages = 10,
    group.limit = 4
  )

  expect_equal(response$n_notebooks, 24)
  expect_equal(length(response$notebooks), 10)
  # Check the sort order (roughly)
  expect_gte(response$notebooks[[1]]$starcount,
             response$notebooks[[2]]$starcount)
})


test_that("Two mixed sources", {

  skip_if_not(check_solr_instance("http://solr"))
  skip_if_not(check_solr_instance("http://solrv1"))

  sources <- read_rcloud_conf("rc-two-mixed.conf")

  SC <- SearchController$new(sources = sources)

  response <- SC$search(
    "hist",
    all_sources = TRUE,
    sortby = "starcount",
    orderby = "desc",
    start = 0,
    pagesize = 10,
    max_pages = 10,
    group.limit = 4
  )

  expect_equal(response$n_notebooks, 24)

})
