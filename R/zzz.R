.SC <- NULL

.onLoad <- function(libname, pkgname) {

  .SC <<- SearchController$new()

}
