gocache <- new.env(parent = emptyenv())

gocache_dir <- function() {
  compat_option("paradoxR.cache_dir", "paradoxR.go_cache_dir", tools::R_user_dir("paradoxR", which = "cache"))
}

gocache_memory_max <- function(key) {
  if (startsWith(key, "go-data-")) 3L else 100L
}

gocache_slot <- function(key) {
  if (startsWith(key, "go-data-")) "memory_data" else "memory_result"
}

gocache_memory <- function(key) {
  store <- gocache[[gocache_slot(key)]]
  if (is.null(store)) list() else store
}

gocache_load_orgdb <- function(OrgDb) {
  if (is.character(OrgDb)) {
    rlang::check_installed(OrgDb)
    OrgDb <- get(OrgDb, envir = asNamespace(OrgDb))
  }
  OrgDb
}

gocache_orgdb_id <- function(OrgDb) {
  db <- gocache_load_orgdb(OrgDb)
  path <- AnnotationDbi::dbfile(db)
  info <- file.info(path)
  list(basename(path), info$size, as.numeric(info$mtime))
}

gocache_versions <- function() {
  list(
    clusterProfiler = as.character(utils::packageVersion("clusterProfiler")),
    GO.db = as.character(utils::packageVersion("GO.db"))
  )
}

gocache_key <- function(kind, ...) {
  paste0(kind, "-", rlang::hash(list(gocache_versions(), ...)))
}

gocache_path <- function(key) {
  file.path(gocache_dir(), paste0(key, ".rds"))
}

gocache_get <- function(key) {
  store <- gocache_memory(key)
  if (key %in% names(store)) {
    gocache[[gocache_slot(key)]] <- store[c(setdiff(names(store), key), key)]
    return(store[[key]])
  }
  path <- gocache_path(key)
  drop <- function(condition) {
    try(unlink(path), silent = TRUE)
    NULL
  }
  value <- tryCatch(
    if (file.exists(path)) readRDS(path) else NULL,
    error = drop,
    warning = drop
  )
  gocache_remember(key, value)
  value
}

gocache_set <- function(key, value) {
  gocache_remember(key, value)
  dir <- gocache_dir()
  tmp <- character()
  ok <- tryCatch({
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(paste0(key, "-"), tmpdir = dir, fileext = ".tmp")
    saveRDS(value, tmp)
    gocache_move(tmp, gocache_path(key))
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    try(unlink(tmp), silent = TRUE)
  }
  invisible(value)
}

gocache_move <- function(from, to) {
  if (suppressWarnings(file.rename(from, to))) {
    return(TRUE)
  }
  if (file.exists(to)) {
    unlink(to)
    if (suppressWarnings(file.rename(from, to))) {
      return(TRUE)
    }
  }
  unlink(from)
  file.exists(to)
}

gocache_remember <- function(key, value) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  store <- gocache_memory(key)
  store <- store[setdiff(names(store), key)]
  store[[key]] <- value
  if (length(store) > gocache_memory_max(key)) {
    store <- store[-seq_len(length(store) - gocache_memory_max(key))]
  }
  gocache[[gocache_slot(key)]] <- store
  invisible(NULL)
}

#' Clear paradoxR's cache
#'
#' Deletes the GO annotation data, `enrichGO()` results and KEGG tables that
#' paradoxR keeps in memory and on disk. The cache lives in
#' `tools::R_user_dir("paradoxR", "cache")`, or in the directory set with
#' `options(paradoxR.cache_dir = )` (the older name `paradoxR.go_cache_dir`
#' still works).
#'
#' @return The number of files deleted, invisibly.
#' @export
#' @examples
#' clear_cache()
clear_cache <- function() {
  gocache$memory_data <- list()
  gocache$memory_result <- list()
  files <- list.files(gocache_dir(), pattern = "^go-.*\\.rds$", full.names = TRUE)
  partial <- list.files(gocache_dir(), pattern = "^go-.*\\.tmp$", full.names = TRUE)
  unlink(c(files, partial))
  invisible(length(files))
}
