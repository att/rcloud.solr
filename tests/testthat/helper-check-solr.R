
check_solr_instance <- function(hostname = "http://localhost",
                                port = 8983,
                                path = "solr/rcloudnotebooks/admin/ping",
                                ...) {
  url <- httr::parse_url(hostname)
  url$port = port
  url$path <- paste(url$path, path, sep = "/")
  url <- c(url, list(...))
  class(url) <- "url"

  response <- tryCatch(httr::GET(url, httr::timeout(1)),
                       error = function(e) list(status_code=-1))

  isTRUE(response$status_code == 200)
}
