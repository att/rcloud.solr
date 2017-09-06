context("Solr Source")

test_that("Initialise source", {

  source <- list(solr.url = "http://example.com:8983/solr/rcloudnotebooks/")

  SS <- SearchSource$new(source)

})
