
parse.response.high <- function(high) {

  if(length(high) != 0) {
    if(!is.null(high$content)) {
      parts.content <- lapply(rjson::fromJSON(high$content), style.highlighting)
    } else {
      high$content <- "[{\"filename\":\"part1.R\",\"content\":[]}]"
      parts.content <- rjson::fromJSON(high$content)
    }
    if(!is.null(high$comments)) {
      final_res <-list()
      comments <- high$comments
      for(n in 1: length(comments)) {
        cmt_match <- grep("open_b_close",comments[n])
        if(as.logical(length(cmt_match))) {
          final_res[[length(final_res)+1]] <- comments[n]
        }
      }
      high$comments <- final_res
      parts.content[[length(parts.content)+1]] <- list(filename="comments", content=high$comments)
    }
    high$content <- rjson::toJSON(parts.content)
    #Handling HTML content
    high$content <- gsub("<","&lt;",high$content)
    high$content <- gsub(">","&gt;",high$content)
    high$content <- gsub("open_b_close","<b style=\\\\\"background:yellow\\\\\">",high$content)
    high$content <- gsub("open_/b_close","</b>",high$content)
  } else
    high$content <-"[{\"filename\":\"part1.R\",\"content\":[]}]"

  high
}

# This is the bit of the code that styles the highlighting
style.highlighting <- function(part.content) {

  splitted <-strsplit(part.content$content,'\n')[[1]]
  res <-list()
  for(k in 1: length(splitted)) {
    is_match <- grep("open_b_close",splitted[[k]])
    is_match_next <- NULL
    is_match_next2 <- NULL
    if(k < length(splitted)){
      is_match_next <- grep("open_b_close",splitted[[k+1]])
    }
    if(k < (length(splitted) -2)){
      is_match_next2 <- grep("open_b_close",splitted[[k+2]])
    }
    if(as.logical(length(is_match))) {
      if(!as.logical(length(is_match_next)) && !as.logical(length(is_match_next2)) ) {
        if(as.logical(length(splitted[k-1]) == "") | k ==1) {
          res[k] <- stitch.search.result(splitted,'optB',k)
        } else {
          if(as.logical(length(splitted[k-1]) != "")) {
            res[k] <- stitch.search.result(splitted,'optC',k)
          }
        }
      } else if (as.logical(length(is_match_next)) && !as.logical(length(is_match_next2)) ) {
        if(k !=1)
          res[k] <- stitch.search.result(splitted,'optD',k)
      } else {
        res[k] <- stitch.search.result(splitted,'default',k)
      }
    }
    if(k == length(splitted)) {
      res[sapply(res, is.null)] <- NULL
      part.content$content <- paste0(toString(res))
    }
  }

  part.content
}

stitch.search.result <- function(splitted, type,k) {
  #Using '|-|' as delimitter here as <br>,/n or anything else might be the content of HTML
  switch(type,
         optA = paste0(k-1,'line_no',splitted[k-1],'|-|',k,'line_no',splitted[k],'|-|',k+1,'line_no',splitted[k+1],sep='|-|'),
         optB = paste0(k,'line_no',splitted[k],'|-|',k+1,'line_no',splitted[k+1],sep='|-|'),
         optC = paste0(k-1,'line_no',splitted[k-1],'|-|',k,'line_no',splitted[k],sep='|-|'),
         optD = paste0(k-1,'line_no',splitted[k-1],sep='|-|'),
         default = paste0(k,'line_no',splitted[k],sep='|-|'))
}
