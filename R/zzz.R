.SC <- NULL

ulog <- Rserve::ulog

.onLoad <- function(libname, pkgname) {

  .SC <<- SearchController$new(NULL)

}
