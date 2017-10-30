context("Sort Results")

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


test_that("Sort by updated_at", {

  raw_results <- readRDS("raw_results.rds")

  sorted_results <- sc_merge_results(raw_results, sortby = "updated_at", orderby = "desc")
  sorted_times <- vapply(sorted_results$notebooks, `[[`, "updated_at", FUN.VALUE = character(1))

  # Cross check against lubridate
  running_less_than <- diff(lubridate::ymd_hms(sorted_times)) <= 0
  expect_true(all(running_less_than))

})

test_that("Sort by description", {

  raw_results <- readRDS("raw_results.rds")

  sorted_results <- sc_merge_results(raw_results, sortby = "description", orderby = "asc")
  sorted_descriptions <- vapply(sorted_results$notebooks, `[[`, "description", FUN.VALUE = character(1))

  # Cross check against lubridate
  expect_equal(sorted_descriptions[1], "Hist1")

})



test_that("Infer types", {

  missing_numeric <- list(1,4,3,NULL,2)
  out <- sc_infer_sort_col(missing_numeric)

  expect_equal(out, c(1,4,3,NA,2))

  mixed_types <- list(1,"A",3,NULL,2)
  out <- sc_infer_sort_col(mixed_types)

  expect_equal(out, c("1", "A", "3", NA, "2"))
})
