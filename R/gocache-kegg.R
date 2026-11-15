gocache_kegg_info_url <- "https://rest.kegg.jp/info/kegg"

gocache_kegg_release <- function() {
  checked <- gocache$kegg_release_time
  if (!is.null(checked) && difftime(Sys.time(), checked, units = "hours") < 1) {
    return(gocache$kegg_release)
  }
  info <- tryCatch(
    suppressWarnings(readLines(gocache_kegg_info_url, warn = FALSE)),
    error = function(e) NULL
  )
  release <- NA_character_
  if (!is.null(info)) {
    lines <- grep("^\\s+(pathway|module|ko|genes)\\s", info, value = TRUE)
    if (length(lines) > 0) {
      release <- paste(trimws(lines), collapse = "; ")
    }
  }
  gocache$kegg_release <- release
  gocache$kegg_release_time <- Sys.time()
  release
}

gocache_kegg_key <- function(rest_url) {
  gocache_key("go-kegg", rest_url)
}

gocache_kegg_rest <- function(rest_url) {
  original <- gocache_original("clusterProfiler", "kegg_rest")
  release <- gocache_kegg_release()
  cached <- gocache_get(gocache_kegg_key(rest_url))
  if (!is.null(cached) && is.na(release)) {
    message("KEGG is unreachable; using cached KEGG data.")
    return(cached$data)
  }
  if (!is.null(cached) && identical(cached$release, release)) {
    return(cached$data)
  }
  data <- original(rest_url)
  if (!is.null(data) && !is.na(release)) {
    gocache_set(gocache_kegg_key(rest_url), list(release = release, data = data))
  }
  data
}
