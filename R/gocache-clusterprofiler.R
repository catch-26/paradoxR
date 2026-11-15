gocache_go_data <- function(OrgDb, ont, keytype, ...) {
  original <- gocache_original("clusterProfiler", "get_GO_data")
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
  original <- gocache_original("clusterProfiler", "enrichGO")
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

gocache_patches <- function() {
  list(
    list(pkg = "clusterProfiler", name = "get_GO_data", make = function(original) gocache_go_data),
    list(pkg = "clusterProfiler", name = "enrichGO", make = gocache_make_enrich_go, exported = TRUE),
    list(pkg = "clusterProfiler", name = "simplify_internal", make = function(original) gocache_simplify_internal, known_body = TRUE),
    list(pkg = "GOSemSim", name = "wangMethod", make = function(original) gocache_wang, known_body = TRUE)
  )
}

gocache_original <- function(pkg, name) {
  gocache$original[[paste0(pkg, "::", name)]]
}

gocache_targets <- function(pkg, exported) {
  envs <- list(asNamespace(pkg))
  attached <- paste0("package:", pkg)
  if (isTRUE(exported) && attached %in% search()) {
    envs <- c(envs, list(as.environment(attached)))
  }
  envs
}

gocache_is_replacement <- function(fun) {
  isTRUE(attr(fun, "paradoxR"))
}

gocache_patched <- function(pkg = "clusterProfiler", name = "enrichGO") {
  isNamespaceLoaded(pkg) &&
    exists(name, envir = asNamespace(pkg), inherits = FALSE) &&
    gocache_is_replacement(get(name, envir = asNamespace(pkg)))
}

gocache_apply <- function(patch) {
  ns <- asNamespace(patch$pkg)
  if (!exists(patch$name, envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  original <- get(patch$name, envir = ns)
  if (gocache_is_replacement(original)) {
    return(invisible(TRUE))
  }
  if (isTRUE(patch$known_body) && !compat_body_is_known(patch$pkg, patch$name, original)) {
    return(invisible(FALSE))
  }
  if (is.null(gocache$original)) {
    gocache$original <- list()
  }
  gocache$original[[paste0(patch$pkg, "::", patch$name)]] <- original
  replacement <- patch$make(original)
  attr(replacement, "paradoxR") <- TRUE
  for (env in gocache_targets(patch$pkg, patch$exported)) {
    gocache_replace(env, patch$name, replacement)
  }
  invisible(TRUE)
}

gocache_revert <- function(patch) {
  if (!gocache_patched(patch$pkg, patch$name)) {
    return(invisible(FALSE))
  }
  original <- gocache_original(patch$pkg, patch$name)
  for (env in gocache_targets(patch$pkg, patch$exported)) {
    gocache_replace(env, patch$name, original)
  }
  invisible(TRUE)
}

#' Cache and speed up GO enrichment in clusterProfiler
#'
#' Replaces functions inside clusterProfiler and GOSemSim so that
#' `clusterProfiler::enrichGO()`, `clusterProfiler::simplify()` and the
#' functions that call them, such as `clusterProfiler::compareCluster()`,
#' reuse earlier work and run faster:
#'
#' * the GO annotation for an OrgDb, ontology and key type is built once and
#'   kept on disk, so a new R session reads it in under a second instead of
#'   querying the OrgDb again;
#' * the result of an `enrichGO()` call is kept, so the same gene list with the
#'   same settings returns at once;
#' * Wang semantic similarity between GO terms is computed for all term pairs
#'   at once instead of pair by pair, which speeds up `simplify()` and
#'   `enrichplot::pairwise_termsim(method = "Wang")`; results are unchanged.
#'
#' Called automatically when paradoxR is loaded, unless
#' `options(paradoxR.go_cache = FALSE)` is set beforehand. The cache is keyed
#' by the clusterProfiler and GO.db versions and the OrgDb database file, so
#' updating any of them starts a fresh cache. The `simplify()` and similarity
#' replacements apply only to the clusterProfiler and GOSemSim versions they
#' were checked against; other versions keep their own code.
#' `reset_go_cache()` restores the original functions and [clear_go_cache()]
#' deletes the cache.
#'
#' @return `TRUE` invisibly when clusterProfiler was changed, `FALSE` when it
#'   is not loaded yet; the change is then applied when it loads.
#' @export
#' @examples
#' use_go_cache()
#' reset_go_cache()
use_go_cache <- function() {
  if (is.null(gocache$hooked)) {
    gocache$hooked <- character()
  }
  for (pkg in unique(vapply(gocache_patches(), `[[`, character(1), "pkg"))) {
    if (!isNamespaceLoaded(pkg) && !pkg %in% gocache$hooked) {
      setHook(packageEvent(pkg, "onLoad"), gocache_on_load)
      gocache$hooked <- c(gocache$hooked, pkg)
    }
  }
  for (patch in gocache_patches()) {
    if (isNamespaceLoaded(patch$pkg)) {
      gocache_apply(patch)
    }
  }
  invisible(gocache_patched())
}

#' @rdname use_go_cache
#' @export
reset_go_cache <- function() {
  changed <- FALSE
  for (patch in gocache_patches()) {
    changed <- isTRUE(gocache_revert(patch)) || changed
  }
  invisible(changed)
}

gocache_on_load <- function(...) {
  if (isTRUE(getOption("paradoxR.go_cache", TRUE))) {
    use_go_cache()
  }
}
