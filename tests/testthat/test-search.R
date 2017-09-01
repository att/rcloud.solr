context("Search")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("rcloud.search", {

  if(!check_solr_instance("http://solr")) skip("Needs solr instance")

  res <- rcloud.search("hist", all_sources = FALSE, sortby = "starcount",
                       orderby = "desc", start = 0, pagesize = 10, group.limit = 4)

  expect_equal(res$matches, 14)
})

test_that("empty search", {

  if(!check_solr_instance("http://solr")) skip("Needs solr instance")

  res <- rcloud.search("flibble", all_sources = FALSE, sortby = "starcount",
                       orderby = "desc", start = 0, pagesize = 10, group.limit = 4)

  expect_equal(res$matches, 0)
})

