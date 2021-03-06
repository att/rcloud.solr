context("Solr Source V1")

if(check_solr_instance("http://solrv1")) {

  url <- make_solr_url("http://solrv1", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks_v1.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("It is really version 1", {

  skip_if_not(check_solr_instance("http://solrv1"))

  solr.url <- "http://solrv1:8983/solr/rcloudnotebooks"
  solr.auth.user <- NULL
  solr.auth.pwd <- NULL

  version <-
    sc_schema_version(
      solr.url = solr.url,
      solr.auth.user = solr.auth.user,
      solr.auth.pwd = solr.auth.pwd
    )

  expect_equal(version, 1)
})

test_that("Initialise source", {

  source <- read_rcloud_conf("rc-one-old.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSourceV1$new(source_named[[1]])

  expect_equal(SS$get_source(), "main_source")

})

test_that("Search one source", {

  skip_if_not(check_solr_instance("http://solrv1"))


  source <- read_rcloud_conf("rc-one-old.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSourceV1$new(source_named[[1]])

  results <- SS$search("hist",
                       sortby = "score",
                       orderby = "desc",
                       pagesize = 10,
                       max_pages = 10)


  expect_equal(results$n_notebooks, 12)

  expect_true(exists("docs", where = results$notebooks[[1]]$doclist))

})

test_that("Empty searc", {

  skip_if_not(check_solr_instance("http://solrv1"))


  source <- read_rcloud_conf("rc-one-old.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSourceV1$new(source_named[[1]])

  results <- SS$search("thiswontmatchanything21",
                       sortby = "starcount",
                       orderby = "desc",
                       pagesize = 10,
                       max_pages = 10)


  expect_equal(results$n_notebooks, 0)
  expect_equal(results$matches, 0)

})

test_that("make a request", {

  skip_if_not(check_solr_instance("http://solrv1"))

  source <- read_rcloud_conf("rc-one-old.conf")
  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)
  SS <- SearchSourceV1$new(source_named[[1]])

  solr.query <- list(q="hist",
                     start=0,
                     rows=10,
                     indent="true",
                     group="false",
                     group.field="notebook_id",
                     group.limit=4,
                     group.ngroups="true",
                     hl="true",
                     hl.preserveMulti="true",
                     hl.fragsize=0,
                     hl.maxAnalyzedChars=-1,
                     hl.simple.pre = "span_open_tag",
                     hl.simple.post = "span_close_tag",
                     fl="description,id,user,updated_at,starcount",
                     hl.fl="content",
                     sort=paste("starcount","desc"))

  solr.res <- .solr.get(
    query = solr.query,
    solr.url = SS$get_solr_url(),
    solr.auth.user = NULL,
    solr.auth.pwd = NULL
  )

  # Just check the response looks about right
  expect_equal(names(solr.res), c("responseHeader", "response", "highlighting"))


})



