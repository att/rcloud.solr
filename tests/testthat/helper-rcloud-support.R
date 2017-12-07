# rcloud support functions for mocking

mock_getConf <- function(name) {

  switch(name,
         solr.url = "http://solr:8983/solr/rcloudnotebooks",
         solr.auth.user = NULL,
         solr.auth.pwd = NULL,
         NULL)

}

mock_rcloud.config.all.notebooks.multiple.users <- function(users) {
  notebooks <- readRDS("notebooks/allnotebooks.rds")
  names(notebooks)
}

mock_rcloud.get.notebook <- function(id, raw) {
  notebooks <- readRDS("notebooks/allnotebooks.rds")
  notebooks[[id]]
}

mock_rcloud.notebook.star.count <- function(id) {
  sample(1:30, size = 1)
}
