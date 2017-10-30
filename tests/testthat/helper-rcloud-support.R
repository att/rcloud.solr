# rcloud support functions for mocking

mock_getConf <- function(name) {

  switch(name,
         solr.url = "http://solr:8983/solr/rcloudnotebooks",
         solr.auth.user = NULL,
         solr.auth.pwd = NULL,
         NULL)

}
