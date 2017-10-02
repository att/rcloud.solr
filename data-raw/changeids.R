
changeid <- function(x) {

  newid <- strtrim(digest::digest(x$id), 20)

  x$`_childDocuments_` <- lapply(x$`_childDocuments_`, changechildid, x$id, newid)

  x$id <- newid

  x

}

changechildid <- function(x, oldid, newid) {

  x$id <- gsub(oldid, newid, x$id)
  x$notebook_id <- newid

  x
}

allnotebooks <- jsonlite::fromJSON("notebooks/allnotebooks.json", simplifyVector = FALSE)

allnotebooks_new <- lapply(allnotebooks, changeid)

jsonlite::write_json(allnotebooks_new, "notebooks/allnotebooks2.json",
                     pretty = TRUE, auto_unbox = TRUE)
