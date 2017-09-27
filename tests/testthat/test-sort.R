context("Sort results")

# sources <- read_rcloud_conf("rc-two.conf")
#
# SC <- SearchController$new(sources = sources)
#
# SC$new_search(
#   "hist",
#   all_sources = TRUE,
#   sortby = "starcount",
#   orderby = "desc",
#   start = 0,
#   pagesize = 10,
#   group.limit = 4
# )
#
# saveRDS(SC$get_raw_results(), "raw_results.rds")

test_that("Sort by star count", {

  raw_results <- readRDS("raw_results.rds")

  sorted_results <- sc_merge_results(raw_results, sortby = "starcount", orderby = "desc")
  sorted_stars <- vapply(sorted_results$notebooks, `[[`, "starcount", FUN.VALUE = numeric(1))
  running_less_than <- diff(sorted_stars) <= 0

  expect_true(all(running_less_than))

  sorted_results <- sc_merge_results(raw_results, sortby = "starcount", orderby = "asc")
  sorted_stars <- vapply(sorted_results$notebooks, `[[`, "starcount", FUN.VALUE = numeric(1))
  running_greater_than <- diff(sorted_stars) >= 0

  expect_true(all(running_greater_than))

})
