#' Create a new solr core
#'
#' This needs to be run on the system that runs solr. It will create a new data
#'  directory, copy the configuration in from the \code{inst/solr} package
#'  directory and make a call to the \code{CREATE} solr API.
#'
#' @param solr.url base URL of the solr instance (e.g. "http://localhost:8983")
#' @param name name of the notebook core. This should probably be left as
#'   "rcloudnotebooks" unless you know what you're doing.
#' @param overwrite Logical. If \code{FALSE} then will stop if the core already
#'   exists. If \code{TRUE} this will only overwrite the conf directory. Data
#'   will be left intact.
#' @param solr_dir Full path to the solr directory. This doesn't need to be the
#'   same as the directory where the binaries are stored.
#' @param data_dir Relative path. Name of the directory, within the core, where
#'   the data will be stored. No real need to change this.
#' @param conf_dir Relative path. Name of the directory, within the core, where
#'   the configuration files will be stored. No real need to change this.
#'
#' @export
#'
solr_core_create <- function(solr.url = "http://localhost:8983",
                             name = "rcloudnotebooks",
                             overwrite = FALSE,
                             solr_dir = "/data/rcloud/solrdata",
                             data_dir = "data",
                             conf_dir = "conf") {

  instance_dir = file.path(solr_dir, "solr", name)
  if(!overwrite) {
    if(dir.exists(instance_dir)) {
      stop(instance_dir, " exists. Set overwrite to TRUE if you want overwrite the configuration")
    }
  }

  data_dir = file.path(instance_dir, data_dir)
  conf_dir = file.path(instance_dir, conf_dir)

  # Create the instance directory
  dir.create(instance_dir, showWarnings = FALSE, recursive = TRUE)

  # Copy in the conf directory
  if(dir.exists(conf_dir)) {
    unlink(conf_dir, recursive = TRUE, force = TRUE)
  }
  file.copy(from =  system.file("solrconf", package = "rcloud.solr"),
            to = instance_dir,
            recursive = TRUE,
            overwrite = TRUE)
  file.rename(file.path(instance_dir, "solrconf"), conf_dir)

  # Create the data directory
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  # @TODO Call the API
  # ...

  # Can we do this for you?
  # #RCloud Conf
  # HOST=`hostname -f`
  # echo "#############################################################\n"
  # echo "add the below line to the rcloud conf\n"
  # echo "#------------------------------------------------------------\n"
  # echo "solr.url: http://$HOST:8983/solr/rcloudnotebooks\n"
  # echo "#------------------------------------------------------------\n"

}
