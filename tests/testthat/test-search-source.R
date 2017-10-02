context("Solr Source")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("Initialise source", {

  source <- read_rcloud_conf("rc-one.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSource$new(source_named[[1]])

  expect_equal(SS$get_source(), "main_source")

})

test_that("Search one source", {

  skip_if_not(check_solr_instance("http://solr"))

  source <- read_rcloud_conf("rc-one.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSource$new(source_named[[1]])

  results <- SS$search("hist",
                       sortby = "starcount",
                       orderby = "desc",
                       pagesize = 10,
                       max_pages = 10)


  expect_equal(results$n_notebooks, 12)

})

