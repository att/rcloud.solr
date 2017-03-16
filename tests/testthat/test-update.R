context("Updating Documents")

test_that("Add a document", {

  if(!check_solr_instance("http://solr")) skip("Needs solr instance")

  nb <- readRDS("notebooks/notebook01.rds")

  response <- update_solr(nb, 0)
  
  # TODO Check it worked!
})
