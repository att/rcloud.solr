context("Simple parse check")

sres <- readRDS("solr_res.rds")

test_that("Full output", {

  ret <- parse.solr.res(sres, pagesize = 10, source = '')

  expect_equal(ret, "{\"QTime\":\"27\",\"notebook\":\"Hist\",\"id\":\"a5332327f9b5c50f62e133b029f77e9e\",\"starcount\":\"1\",\"updated_at\":\"2017-02-21T14:11:34Z\",\"user\":\"dougmet\",\"numFound\":\"1\",\"pagesize\":\"10\",\"parts\":\"[{\\\"filename\\\":\\\"part1.R\\\",\\\"content\\\":\\\"1line_no<b style=\\\\\\\"background:yellow\\\\\\\">hist</b>(rnorm(100)) # hell|-|2line_noNA|-|\\\"},{\\\"filename\\\":\\\"part2.R\\\",\\\"content\\\":\\\"\\\"},{\\\"filename\\\":\\\"scratch.R\\\",\\\"content\\\":\\\"\\\"}]\",\"source\":\"\"}")
})
