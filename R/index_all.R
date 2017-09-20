
#' Index all RCloud Notebooks
#'
#' Sends all notebooks from all users to solr
#'
#' @param pause time in seconds between updateseturn with the highlighting
#'
#' @return Search response after parsing
#' @export
#'
index_all <- function(pause = 0.5) {

  users <- rcloud.support::rcloud.get.users()

  notebooks <-
    unname(unlist(
      rcloud.support::rcloud.config.all.notebooks.multiple.users(users)
    ))

  for (i in notebooks) {
    print(paste0("Indexing ", i))
    result = tryCatch({
    x = rcloud.support::rcloud.get.notebook(i)
         rcloud.solr::update_solr(x,rcloud.support::rcloud.notebook.star.count(i))
    }, warning = function(w) {
   print(paste0(w," ",i))
    }, error = function(e) {
   print(paste0(e," ",i))
    }, finally = {
    print(paste0("Success ",i))
    })
    Sys.sleep(pause)
  }
}
