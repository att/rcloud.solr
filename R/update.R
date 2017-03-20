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
  completedata <- rjson::toJSON(metadata.list)
  .solr.post(data=completedata, detach = detach)

}

build_update_metadata <- function(notebook, starcount) {

  ## FIXME: gracefully handle unavailability
  content.files <- notebook$content$files
  ## Remove binary assets by removing elements with .b64 extention

  content_ext <- tools::file_ext(names(content.files)) # What's more reliable? Names or filename element?
  content.files <- content.files[content_ext != "b64"]

  # Previous comment suggested scratch.R not indexed but it was. scratch.R *is* indexed.

  if (!length(content.files)) return(NULL)

  sizes <- as.numeric(sapply(content.files, function(o) o$size))
  size <- sum(sizes, na.rm=TRUE)

  desc <- build_update_description(notebook$content$description)

  session.content <- notebook$content
  ## FIXME: followers is not in the notebook, set to 0 for now
  metadata.list <- list(
      "id"= session.content$id,
      "user"= session.content$user$login,
      "created_at"= session.content$created_at,
      "updated_at"= session.content$updated_at,
      "description"= desc,
      "user_url"= session.content$user$url,
      "avatar_url"= session.content$user$avatar_url,
      "size"= size,
      "commited_at"= session.content$updated_at,
      "followers"= "0",
      "public"= session.content$public,
      "starcount"= starcount,
      "content"=list(set = "")
  )
  # Looks like for now like we do really have to encode the content as a JSON string
  # May want to look at the schema to see if this is definitely necessary

  # This will handle named vectors and potentially NULL values
  metadata.list <- lapply(metadata.list, process_metadata_list)

  metadata.list$content$set <- build_json_content_files(content.files)

  metadata.list
}

# Refactored out. TODO - work out what this is doing
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

build_json_content_files <- function(content.files) {
  # Select only filename and content
  content.files <- lapply(content.files, `[`, c('filename', 'content'))
  # Remove names
  content.files <- unname(content.files)
  # Convert to JSON (because this is what solr wants)
  content.files <- rjson::toJSON(content.files)
  content.files
}

process_metadata_list <- function(li) {
  if(is.list(li)) {
    lapply(li, process_metadata_list)
  } else {
    unname(li)
    # ifelse(is.null(li), "", unname(li))
  }
}

