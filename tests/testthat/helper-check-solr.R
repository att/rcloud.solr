
make_solr_url <- function(hostname = "http://localhost",
                          port = 8983,
                          path = "solr/rcloudnotebooks",
                          query = NULL,
                          ...) {
  url <- httr::parse_url(hostname)
  url$port = port
  url$path <- paste(url$path, path, sep = "/")
  url$query <- query
  url <- c(url, list(...))
  class(url) <- "url"
  url
}

check_solr_instance <- function(hostname = "http://localhost",
                                port = 8983,
                                path = "solr/rcloudnotebooks/admin/ping",
                                query = NULL,
                                ...) {
  url <- make_solr_url(hostname = hostname, port = port, path = path, ...)

  response <- tryCatch(httr::GET(url, httr::timeout(1)),
                       error = function(e) list(status_code=-1))

  isTRUE(response$status_code == 200)
}
