# Some intelligent parsing account for basics like /solr/notebook and /solr/notebook/ is essentially the same thing
# Using httr::parse_url

#' Update the index after a notebook change
#'
#' @param notebook [notebook] The notebook object as stored by rcloud.support
#' @param starcount [numeric] notebook starcount
#' @param detach [logical] Should parallel process be detached? Use FALSE for testing
#'
#' @export
update_solr <- function(notebook, starcount, detach = TRUE){
  #Update only notebooks which are visible
  if(!rcloud.support::rcloud.is.notebook.visible(notebook$content$id) ||
     (rcloud.support:::is.notebook.encrypted(notebook$content$id))){
    return(NULL)
  }

  metadata.list <- build_update_metadata(notebook, starcount)

  .solr.post(data=metadata.list, detach = detach)

}

build_update_metadata <- function(notebook, starcount) {

  ## FIXME: gracefully handle unavailability
  content.files <- notebook$content$files
  ## Remove binary assets by removing elements with .b64 extention

  content_ext <- tools::file_ext(names(content.files)) # What's more reliable? Names or filename element?
  content.files <- content.files[content_ext != "b64"]

  # Previous comment suggested scratch.R not indexed but it was. scratch.R *is* indexed.

  if (!length(content.files)) return(NULL)

  desc <- build_update_description(notebook$content$description)

  session.content <- notebook$content
  ## FIXME: followers is not in the notebook, set to 0 for now
  metadata.list <- list(
      "id"= session.content$id,
      "doc_type" = "notebook",
      "user"= session.content$user$login,
      "created_at"= session.content$created_at,
      "updated_at"= session.content$updated_at,
      "description"= desc,
      "user_url"= session.content$user$url,
      "avatar_url"= session.content$user$avatar_url,
      "commited_at"= session.content$updated_at,
      "followers"= "0",
      "public"= session.content$public,
      "starcount"= starcount
  )

  # This will handle named vectors and potentially NULL values
  metadata.list <- lapply(metadata.list, process_metadata_list)

  metadata.list$`_childDocuments_` <- build_content_files(content.files, notebook_id = metadata.list$id)

  metadata.list
}

# Refactored out. TODO - work out what this is doing
build_update_description <- function(desc) {

  desc  <- gsub("^\"*|\"*$", "", desc)                 # Remove quotes from start and end
  desc <- gsub("^\\\\*|\\\\*$", "", desc)              # Remove slashes from start and end
  if (length(grep("\"",desc) == 1)) {
    notebook.description <- strsplit(desc,'\"')
    desc <- paste(notebook.description[[1]],collapse="\\\"")
  } else if(length(grep("\\\\",desc) == 1)){
    notebook.description <- strsplit(desc,'\\\\')
    desc <- paste(notebook.description[[1]],collapse="\\\\")
  }

  desc
}

build_content_files <- function(content.files, notebook_id) {
  # Select only filename and content
  content.files <- lapply(content.files, build_one_content_file, notebook_id)
  # Remove names
  unname(content.files)

}

build_one_content_file <- function(content, notebook_id) {

  id <- paste(unname(notebook_id), content$filename, sep = "-")

  content <- c(list(id = id, doc_type = "cell"),
               content[c('filename', 'language', 'raw_url', 'size', 'content')])

  content
}

process_metadata_list <- function(li) {
  if(is.list(li)) {
    lapply(li, process_metadata_list)
  } else {
    if(is.null(li)) "" else unname(li)
  }
}
