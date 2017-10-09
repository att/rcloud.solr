context("Description Search")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("Description only search", {

  resp <- rcloud.search.description("test")

  expect_equal(resp$response$numFound, 10)

  resp_ravanelli <- rcloud.search.description("test", user = "ravanelli")

  expect_equal(resp_ravanelli$response$numFound, 5)


})

