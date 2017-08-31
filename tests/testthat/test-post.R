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

})

test_that("sync post", {
  skip_if_not(check_solr_instance("http://solr"))

  nb <- readRDS("notebooks/notebook01.rds")
  metadata.list <- build_update_metadata(nb, starcount=1)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach, type = "sync")

})

test_that("async post", {
  skip_if_not(check_solr_instance("http://solr"))

  nb <- readRDS("notebooks/notebook01.rds")
  metadata.list <- build_update_metadata(nb, starcount=2)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach, type = "async")
})

test_that("curl post", {
  skip_if_not(check_solr_instance("http://solr"))

  hasCurl <- system2("curl", "--version")
  skip_if_not(hasCurl == 0)

  nb <- readRDS("notebooks/notebook01.rds")
  metadata.list <- build_update_metadata(nb, starcount=3)
  detach = FALSE

  resp <- .solr.post(data=metadata.list, detach = detach, type = "curl")

  resp <- parallel::mccollect(resp)[[1]]

})
