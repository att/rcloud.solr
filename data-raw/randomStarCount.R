
set.seed(123)

changestar <- function(x) {

  newstar <- sample(0:30, size = 1)

  x$`_childDocuments_` <- lapply(x$`_childDocuments_`, changechildstar, newstar)

  x$starcount <- newstar

  x

}

changechildstar <- function(x, newstar) {

  x$starcount <- newstar

  x
}

allnotebooks <- jsonlite::fromJSON("notebooks/allnotebooks.json", simplifyVector = FALSE)

allnotebooks_new <- lapply(allnotebooks, changestar)

jsonlite::write_json(allnotebooks_new, "notebooks/allnotebooks.json",
                     pretty = TRUE, auto_unbox = TRUE)


set.seed(19)

allnotebooks <- jsonlite::fromJSON("notebooks/allnotebooks2.json", simplifyVector = FALSE)

allnotebooks_new <- lapply(allnotebooks, changestar)

jsonlite::write_json(allnotebooks_new, "notebooks/allnotebooks2.json",
                     pretty = TRUE, auto_unbox = TRUE)

