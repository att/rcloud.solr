context("Solr Parsing")


if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  # Uploads a bunch of notebooks
  httr::POST(url, body = httr::upload_file("notebooks/allnotebooks.json"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

#

test_that("Solr Responds", {

  skip_if_not(check_solr_instance("http://solr"))

  solr.query <- list(q="hist",
                     start=0,
                     rows=10,
                     indent="true",
                     group="true",
                     group.field="notebook_id",
                     group.limit=4,
                     group.ngroups="true",
                     hl="true",
                     hl.preserveMulti="true",
                     hl.fragsize=80,
                     hl.maxAnalyzedChars=-1,
                     hl.simple.pre = "<span class=\"search-result-solr-highlight\">",
                     hl.simple.post = "</span>",
                     fl="description,id,user,updated_at,starcount,filename, doc_type",
                     hl.fl="content,comments",
                     sort="starcount desc")

  solr.res <- .solr.get(solr.url=rcloud.support:::getConf("solr.url"),
                        query=solr.query,
                        solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                        solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd"))

  exp_names <- c("responseHeader", "grouped", "highlighting")
  expect_equal(names(solr.res), exp_names)

  # Normal parse
  resp <- ss_parse_result(self = list(), private = list(source = ""), solr.res = solr.res, pagesize = 10, start =  0)

  exp_names <- c("QTime", "status", "start", "pagesize", "source", "matches",
                 "n_notebooks", "notebooks")

  expect_equal(names(resp), exp_names)

})

test_that("Bad search", {

  skip_if_not(check_solr_instance("http://solr"))

  solr.query <- list(q="bananas@@a\\",
                     start=0,
                     rows=10,
                     indent="true",
                     group="true",
                     group.field="notebook_id",
                     group.limit=4,
                     group.ngroups="true",
                     hl="true",
                     hl.preserveMulti="true",
                     hl.fragsize=80,
                     hl.maxAnalyzedChars=-1,
                     hl.simple.pre = "<span class=\"search-result-solr-highlight\">",
                     hl.simple.post = "</span>",
                     fl="description,id,user,updated_at,starcount,filename, doc_type",
                     hl.fl="content,comments",
                     sort="starcount desc")

  solr.res <- .solr.get(solr.url=rcloud.support:::getConf("solr.url"),
                        query=solr.query,
                        solr.auth.user=rcloud.support:::getConf("solr.auth.user"),
                        solr.auth.pwd=rcloud.support:::getConf("solr.auth.pwd"))

  exp_names <- c("error")
  expect_equal(names(solr.res), exp_names)

})


