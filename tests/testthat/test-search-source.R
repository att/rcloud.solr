context("Solr Source")

test_that("Initialise source", {

  source <- read_rcloud_conf("rc-one.conf")

  source_named <- mapply(source, names(source),
                         FUN = function(x,y) c(source=y, x),
                         SIMPLIFY = FALSE)

  SS <- SearchSource$new(source_named[[1]])

  expect_equal(SS$get_source(), "main_source")

})
