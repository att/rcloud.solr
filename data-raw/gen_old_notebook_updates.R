gen_old_solr_update <- function(notebook, starcount){
  #Update only notebooks which are visible
      ## FIXME: gracefully handle unavailability
  content.files <- notebook$content$files
  ## Remove binary assets by removing elements with .b64 extention
  content.files <- content.files[unlist(lapply(names(content.files),function(o){utils::tail(strsplit(o,split="\\.")[[1]],1) != "b64"}))]
  fns <- as.vector(sapply(content.files, function(o) o$filename))
  ## only index cells for now ...
  ## FIXME: do we really want to exclude the scratch file?
  if (length(content.files)) {
    sizes <- as.numeric(sapply(content.files, function(o) o$size))
    size <- sum(sizes, na.rm=TRUE)
    desc <- notebook$content$description
    desc <- gsub("^\"*|\"*$", "", desc)
    desc <- gsub("^\\\\*|\\\\*$", "", desc)
    if (length(grep("\"",desc) == 1)) {
      notebook.description <- strsplit(desc,'\"')
      desc <- paste(notebook.description[[1]],collapse="\\\"")
    } else if(length(grep("\\\\",desc) == 1)){
      notebook.description <- strsplit(desc,'\\\\')
      desc <- paste(notebook.description[[1]],collapse="\\\\")
    } else {
      desc
    }
    session.content <- notebook$content
    ## FIXME: followers is not in the notebook, set to 0 for now
    metadata<-paste0('{\"id\":\"',session.content$id, '\",\"user\":\"',session.content$user$login, '\",\"created_at\":\"',session.content$created_at, '\",\"updated_at\":\"',session.content$updated_at, '\",\"description\":\"',desc, '\",\"user_url\":\"',session.content$user$url, '\",\"avatar_url\":\"',session.content$user$avatar_url, '\",\"size\":\"',size, '\",\"commited_at\":\"',session.content$updated_at, '\",\"followers\":\"',0, '\",\"public\":\"',session.content$public, '\",\"starcount\":\"',starcount, '\",\"content\":{\"set\":\"\"}}')
    metadata.list <- rjson::fromJSON(metadata)
    content.files <- unname(lapply(content.files, function(o) list('filename'=o$filename,'content'=o$content)))
    content.files <- rjson::toJSON(content.files)
    metadata.list$content$set <- content.files
  } else {
    stop("didn't think of that")
  }

  metadata.list
}

set.seed(17)

notebook_files <- list.files("notebooks", pattern = "^[a-f0-9]{20}\\.json$", full.names = TRUE)
notebooks_raw <- lapply(notebook_files, jsonlite::fromJSON, simplifyVector = FALSE)
starcounts <- sample(0:20, size = length(notebooks_raw), replace = TRUE)

all_notebooks <- mapply(gen_old_solr_update, notebooks_raw, starcounts, SIMPLIFY = FALSE)

writeLines(jsonlite::toJSON(all_notebooks, pretty = TRUE, auto_unbox = TRUE),
           con = "notebooks/allnotebooks_v1.json")
