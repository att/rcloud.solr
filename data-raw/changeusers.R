
set.seed(123)
users <- c("juninho", "ravanelli", "emerson")

changeuser <- function(x) {

  newuser <- sample(users, size = 1)

  x$`_childDocuments_` <- lapply(x$`_childDocuments_`, changechilduser, newuser)

  x$user <- newuser

  x

}

changechilduser <- function(x, newuser) {

  x$user <- newuser

  x
}

allnotebooks <- jsonlite::fromJSON("notebooks/allnotebooks.json", simplifyVector = FALSE)

allnotebooks_new <- lapply(allnotebooks, changeuser)

jsonlite::write_json(allnotebooks_new, "notebooks/allnotebooks.json",
                     pretty = TRUE, auto_unbox = TRUE)

