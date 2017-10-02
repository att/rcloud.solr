context("V1 Solr Source")

if(check_solr_instance("http://solrv1")) {

  url <- make_solr_url("http://solrv1", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks_v1.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("Initialise source", {

  source <- read_rcloud_conf("rc-one-old.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSource$new(source_named[[1]])

  expect_equal(SS$get_source(), "main_source")

})

test_that("Search one source", {

  skip_if_not(check_solr_instance("http://solrv1"))

  source <- read_rcloud_conf("rc-one-old.conf")

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

test_that("make a request", {

  skip_if_not(check_solr_instance("http://solrv1"))

  source <- read_rcloud_conf("rc-one-old.conf")
  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)
  SS <- SearchSourceV1$new(source_named[[1]])

  solr.query <- list(q="cars",
                     start=0,
                     rows=10,
                     indent="true",
                     group="true",
                     group.field="notebook_id",
                     group.limit=4,
                     group.ngroups="true",
                     hl="true",
                     hl.preserveMulti="true",
                     hl.fragsize=50,
                     hl.maxAnalyzedChars=-1,
                     hl.simple.pre = "<span class=\"search-result-solr-highlight\">",
                     hl.simple.post = "</span>",
                     fl="description,id,user,updated_at,starcount,filename, doc_type",
                     hl.fl="content,comments",
                     sort=paste("starcount","desc"))

  solr.res <- .solr.get(
    query = solr.query,
    solr.url = SS$get_solr_url(),
    solr.auth.user = NULL,
    solr.auth.pwd = NULL
  )

  # Just check the response looks about right
  expect_equal(names(solr.res), c("responseHeader", "grouped", "highlighting"))

  expect_equal(length(solr.res$grouped$notebook_id), 3)

})

