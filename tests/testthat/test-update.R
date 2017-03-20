context("Updating Documents")

test_that("Add a document", {

  if(!check_solr_instance("http://solr")) skip("Needs solr instance")

  nb <- readRDS("notebooks/notebook01.rds")

  response <- update_solr(nb, 1)

  # Is there any other way to check it worked?!
  search_response <- rcloud.search("hist", all_sources = FALSE,
                                   orderby = NULL,
                                   sortby = "starcount desc",
                                   start = 0, pagesize = 10)

  search_response
})
