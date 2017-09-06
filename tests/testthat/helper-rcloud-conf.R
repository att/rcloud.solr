
read_rcloud_conf <- function(rc.cf) {

  rc.gsrc <- list()
  if (isTRUE(file.exists(rc.cf))) {
    .dcf.sections.with <- function(d, sec) {
      if (!sec %in% colnames(d)) return(list())
      w <- which(!is.na(d[,sec]))
      l <- lapply(w, function(o) { e <- d[o,]; e <- e[!is.na(e)]; names(e) <- gsub("[ \t]", ".", tolower(names(e))); e })
      names(l) <- d[w,sec]
      l
    }
    #cat("Loading RCloud configuration file...\n")
    rc.all <- read.dcf(rc.cf)
    rc.c <- rc.all[1,]
    rc.c <- rc.c[!is.na(rc.c)]
    rc.gsrc <- .dcf.sections.with(rc.all, "gist.source")

    rc.c <- as.list(rc.c[grepl("solr", names(rc.c))])
  } else {
    stop(rc.cf, " not found.")
  }

  conf <- list(main_source = rc.c, gist_sources = rc.gsrc)

  conf

}
