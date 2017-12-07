context("Rapid updates")

if(check_solr_instance("http://solr")) {

  url <- make_solr_url("http://solr", path = "solr/rcloudnotebooks/update", query = list(commit = "true"))

  on.exit({
    httr::POST(url, httr::content_type_xml(), body = "<delete><query>*:*</query></delete>")
  })
}

test.get.comments <- function(id) {
  return(jsonlite::toJSON(list(
    list(
      body = "do comments work in solr?",
      user = list(
        login = "rcloud",
        id = 1L,
        type = "User"
      ),
      updated_at = "2017-03-08T15:33:30Z",
      created_at = "2017-03-08T15:33:30Z",
      id = 1L
    )
  ),
  auto_unbox = TRUE))
}

test_that("Rapid updates succeed", {

  skip_on_travis()
  skip_if_not(check_solr_instance("http://solr"))

  skip("This is an expected fail atm")

  nb0 <- readRDS("notebooks/notebook01.rds")


  set.seed(123)
  randomiseIDs <- function(i, nb) {
    nb$content$id <- c("rcloud1" = digest::digest(i))
    nb$content$files$scratch.R$content <-
      paste(sample(
        c(" ", letters),
        size = 1000,
        replace = TRUE,
        prob = c(0.4, rep(0.6 / 26, 26))
      ),
      collapse = "")
    nb
  }

  n <- 100

  notebooks <- lapply(1:n, randomiseIDs, nb0)

  with_mock(
    `rcloud.support::rcloud.get.comments` = test.get.comments,
    {
      resps <- lapply(notebooks,
        update_solr,
        starcount = 1,
        detach = TRUE,
        query = list(commitWithin = 1000)
      )
    }
  )

  expect_equal(rcloud.search("hist")$matches, n)

})
