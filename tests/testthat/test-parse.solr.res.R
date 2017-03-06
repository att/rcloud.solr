context("Simple parse check")


test_that("Full output", {

  # Loop over all the recorded test cases and check it's OK
  queryfiles <- list.files(path = "queries", pattern = "\\.json$")

  # May need to sort the list order later.
  for(qfile in queryfiles) {

    tq <- rjson::fromJSON(file = file.path("queries", qfile))

    resp <- parse.solr.res(solr.res = tq$solr.res, pagesize = tq$pagesize, source = "")#tq$all_sources)

    expect_equal(resp, tq$response, info = qfile)
  }
})
