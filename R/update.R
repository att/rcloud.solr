# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url

#' Update the index after a notebook change
#'
#' @param notebook The notebook object as stored by rcloud.support
#' @param starcount notebook starcount
#'
#' @export
update_solr <- function(notebook, starcount){
  #Update only notebooks which are visible
  if(!rcloud.support::rcloud.is.notebook.visible(notebook$content$id) ||
     (rcloud.support:::is.notebook.encrypted(notebook$content$id))){
    return(NULL)
  }

  metadata.list <- build_update_metadata(notebook, starcount)
  completedata <- rjson::toJSON(metadata.list)
  .solr.post(data=completedata)

}

build_update_metadata <- function(notebook, starcount) {

  ## FIXME: gracefully handle unavailability
  content.files <- notebook$content$files
  ## Remove binary assets by removing elements with .b64 extention
  content.files <- content.files[unlist(lapply(names(content.files),function(o){utils::tail(strsplit(o,split="\\.")[[1]],1) != "b64"}))]
  ## only index cells for now ...
  ## FIXME: do we really want to exclude the scratch file?
  if (!length(content.files)) return(NULL)

  sizes <- as.numeric(sapply(content.files, function(o) o$size))
  size <- sum(sizes, na.rm=TRUE)

  desc <- build_update_description(notebook$content$description)

  session.content <- notebook$content
  ## FIXME: followers is not in the notebook, set to 0 for now
  metadata<-paste0('{\"id\":\"',session.content$id, '\",\"user\":\"',session.content$user$login, '\",\"created_at\":\"',session.content$created_at, '\",\"updated_at\":\"',session.content$updated_at, '\",\"description\":\"',desc, '\",\"user_url\":\"',session.content$user$url, '\",\"avatar_url\":\"',session.content$user$avatar_url, '\",\"size\":\"',size, '\",\"commited_at\":\"',session.content$updated_at, '\",\"followers\":\"',0, '\",\"public\":\"',session.content$public, '\",\"starcount\":\"',starcount, '\",\"content\":{\"set\":\"\"}}')
  metadata.list <- rjson::fromJSON(metadata)
  content.files <- unname(lapply(content.files, function(o) list('filename'=o$filename,'content'=o$content)))
  content.files <- rjson::toJSON(content.files)
  metadata.list$content$set <- content.files

  metadata.list
}

build_update_description <- function(desc) {

  desc  <- gsub("^\"*|\"*$", "", desc)
  desc <- gsub("^\\\\*|\\\\*$", "", desc)
  if (length(grep("\"",desc) == 1)) {
    notebook.description <- strsplit(desc,'\"')
    desc <- paste(notebook.description[[1]],collapse="\\\"")
  } else if(length(grep("\\\\",desc) == 1)){
    notebook.description <- strsplit(desc,'\\\\')
    desc <- paste(notebook.description[[1]],collapse="\\\\")
  }

  desc
}
