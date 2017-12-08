context("Bulk update")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test_that("Bulk update", {

  skip_if_not(check_solr_instance("http://solr"))

  with_mock(
    `rcloud.support::rcloud.config.all.notebooks.multiple.users` = mock_rcloud.config.all.notebooks.multiple.users,
    `rcloud.support::rcloud.get.notebook` = mock_rcloud.get.notebook,
    `rcloud.support::rcloud.notebook.star.count` = mock_rcloud.notebook.star.count,
    `rcloud.support::rcloud.is.notebook.visible` = function(id) TRUE,
    `rcloud.support:::is.notebook.encrypted` = function(id) FALSE,
    `rcloud.support::rcloud.get.users` = function() "dougmet",
    {
      users <- rcloud.support::rcloud.get.users()
      notebook_ids <-
        unname(unlist(
          rcloud.support::rcloud.config.all.notebooks.multiple.users(users)
        ))
      notebook_ids <- batch_notebooks(notebook_ids, batch_size = 5)
      batch <- notebook_ids[[2]]

      resp <- update_batch(batch, query = list(commitWithin = 10000))
      response <- parallel::mccollect(resp)

      expect_equal(response[[1]]$status_code, 200)
    }
  )

})


test_that("Index All", {

  skip_if_not(check_solr_instance("http://solr"))


  with_mock(
    `rcloud.support::rcloud.config.all.notebooks.multiple.users` = mock_rcloud.config.all.notebooks.multiple.users,
    `rcloud.support::rcloud.get.notebook` = mock_rcloud.get.notebook,
    `rcloud.support::rcloud.notebook.star.count` = mock_rcloud.notebook.star.count,
    `rcloud.support::rcloud.is.notebook.visible` = function(id) TRUE,
    `rcloud.support:::is.notebook.encrypted` = function(id) FALSE,
    `rcloud.support::rcloud.get.users` = function() "dougmet",
    {
      test_output <- capture_output({
        result <- index_all(pause = 1,
                            batch_size = 10,
                            commitWithin = 10000,
                            type = "sync")
      })
    }
  )

})
