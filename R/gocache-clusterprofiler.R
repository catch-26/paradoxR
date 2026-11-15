gocache_go_data <- function(OrgDb, ont, keytype, ...) {
  original <- gocache$original$get_GO_data
  dots <- list(...)
  if (!all(vapply(dots, is.null, logical(1)))) {
    return(original(OrgDb, ont, keytype, ...))
  }
  key <- gocache_key("go-data", gocache_orgdb_id(OrgDb), ont, keytype)
  value <- gocache_get(key)
  if (is.null(value)) {
    value <- original(OrgDb, ont, keytype, ...)
    gocache_set(key, value)
  }
  value
}

gocache_enrich_go <- function(frame) {
  original <- gocache$original$enrichGO
  arg_names <- names(formals(original))
  supplied <- arg_names[!vapply(
    arg_names,
    function(name) eval(call("missing", as.name(name)), frame),
    logical(1)
  )]
  args <- mget(supplied, envir = frame)
  key_args <- args
  key_args$OrgDb <- gocache_orgdb_id(args$OrgDb)
  key <- gocache_key("go-result", key_args)
  value <- gocache_get(key)
  if (is.null(value)) {
    value <- do.call(original, args)
    if (!is.null(value)) {
      gocache_set(key, value)
    }
  }
  value
}

gocache_make_enrich_go <- function(original) {
  wrapper <- original
  env <- new.env(parent = environment(original))
  env$gocache_enrich_go <- gocache_enrich_go
  body(wrapper) <- quote(gocache_enrich_go(environment()))
  environment(wrapper) <- env
  attr(wrapper, "paradoxR") <- TRUE
  wrapper
}

gocache_replace <- function(env, name, value) {
  if (!exists(name, envir = env, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  locked <- bindingIsLocked(name, env)
  if (locked) {
    unlockBinding(name, env)
  }
  assign(name, value, envir = env)
  if (locked) {
    lockBinding(name, env)
  }
  invisible(TRUE)
}

gocache_targets <- function() {
  ns <- asNamespace("clusterProfiler")
  envs <- list(ns)
  if ("package:clusterProfiler" %in% search()) {
    envs <- c(envs, list(as.environment("package:clusterProfiler")))
  }
  envs
}

gocache_patched <- function() {
  isNamespaceLoaded("clusterProfiler") &&
    isTRUE(attr(get("enrichGO", envir = asNamespace("clusterProfiler")), "paradoxR"))
}

#' Cache GO enrichment in clusterProfiler
#'
#' Replaces two functions inside clusterProfiler so that `clusterProfiler::enrichGO()`
#' and the functions that call it, such as `clusterProfiler::compareCluster()`,
#' reuse earlier work:
#'
#' * the GO annotation for an OrgDb, ontology and key type is built once and
#'   kept on disk, so a new R session reads it in under a second instead of
#'   querying the OrgDb again;
#' * the result of an `enrichGO()` call is kept, so the same gene list with the
#'   same settings returns at once.
#'
#' Called automatically when paradoxR is loaded, unless
#' `options(paradoxR.go_cache = FALSE)` is set beforehand. The cache is keyed
#' by the clusterProfiler and GO.db versions and the OrgDb database file, so
#' updating any of them starts a fresh cache. `reset_go_cache()` restores
#' clusterProfiler's own functions and [clear_go_cache()] deletes the cache.
#'
#' @return `TRUE` invisibly when clusterProfiler was changed, `FALSE` when it
#'   is not loaded yet; the change is then applied when it loads.
#' @export
#' @examples
#' use_go_cache()
#' reset_go_cache()
use_go_cache <- function() {
  if (!isNamespaceLoaded("clusterProfiler")) {
    if (!isTRUE(gocache$hooked)) {
      setHook(packageEvent("clusterProfiler", "onLoad"), gocache_on_load)
      gocache$hooked <- TRUE
    }
    return(invisible(FALSE))
  }
  if (gocache_patched()) {
    return(invisible(TRUE))
  }
  ns <- asNamespace("clusterProfiler")
  gocache$original <- list(
    get_GO_data = get("get_GO_data", envir = ns),
    enrichGO = get("enrichGO", envir = ns)
  )
  enrich_go <- gocache_make_enrich_go(gocache$original$enrichGO)
  gocache_replace(ns, "get_GO_data", gocache_go_data)
  for (env in gocache_targets()) {
    gocache_replace(env, "enrichGO", enrich_go)
  }
  invisible(TRUE)
}

#' @rdname use_go_cache
#' @export
reset_go_cache <- function() {
  if (!gocache_patched()) {
    return(invisible(FALSE))
  }
  ns <- asNamespace("clusterProfiler")
  gocache_replace(ns, "get_GO_data", gocache$original$get_GO_data)
  for (env in gocache_targets()) {
    gocache_replace(env, "enrichGO", gocache$original$enrichGO)
  }
  invisible(TRUE)
}

gocache_on_load <- function(...) {
  if (isTRUE(getOption("paradoxR.go_cache", TRUE))) {
    use_go_cache()
  }
}
