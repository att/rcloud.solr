context("Post request")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("default post", {
  skip_if_not(check_solr_instance("http://solr"))

  nb <- readRDS("notebooks/notebook01.rds")
  metadata.list <- build_update_metadata(nb, starcount=0)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach)

  resp <- parallel::mccollect(resp)[[1]]

  expect_equal(resp$status_code, 200)

})

test_that("sync post", {
  skip_if_not(check_solr_instance("http://solr"))

  nb <- readRDS("notebooks/notebook01.rds")
  metadata.list <- build_update_metadata(nb, starcount=1)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach, type = "sync")

  expect_equal(resp$status_code, 200)

})

test_that("async post", {
  skip_if_not(check_solr_instance("http://solr"))

  nb <- readRDS("notebooks/notebook01.rds")
  metadata.list <- build_update_metadata(nb, starcount=2)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach, type = "async")

  resp <- parallel::mccollect(resp)[[1]]

  expect_equal(resp$status_code, 200)
})

test_that("curl post", {
  skip_if_not(check_solr_instance("http://solr"))

  hasCurl <- system2("curl", "--version", stdout = FALSE)
  skip_if_not(hasCurl == 0)

  nb <- readRDS("notebooks/notebook01.rds")
  # Update with something we can check for
  nb$content$files$part1.R$content <- "testingcurltest767\n"

  metadata.list <- build_update_metadata(nb, starcount=3)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach, type = "curl")

  resp <- parallel::mccollect(resp)[[1]]

  expect_null(resp)

  # Now search to see if it went in. Should this be a lower level search?
  search_result <- rcloud.search("testingcurltest767")

  expect_equal(search_result$matches, 1)

  # Tear down this one
  httr::POST(url, httr::content_type_xml(), body = "<delete><query>id:010b0b4451ff152e6c62</query></delete>")
})
