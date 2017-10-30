
drop_items <- function(x, items = NULL) {
  x[!(names(x) %in% items)]
}

keep_items <- function(x, items = NULL) {
  x[names(x) %in% items]
}

