gocache_go_data <- function(OrgDb, ont, keytype, ...) {
  original <- gocache_original("clusterProfiler", "get_GO_data")
  dots <- list(...)
  if (!all(vapply(dots, is.null, logical(1)))) {
    return(original(OrgDb, ont, keytype, ...))
  }
  key <- gocache_key("go-data", gocache_orgdb_id(OrgDb), ont, keytype)
  value <- gocache_get(key)
  if (is.null(value)) {
    value <- gocache_snapshot(gocache_fast_go_data(original)(OrgDb, ont, keytype, ...))
    gocache_set(key, value)
  }
  gocache_snapshot(value)
}

gocache_snapshot <- function(value) {
  if (!is.environment(value)) {
    return(value)
  }
  list2env(as.list(value, all.names = TRUE), envir = new.env(parent = emptyenv()))
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
  labels_only <- gocache_cutoffs_are_labels() &&
    toupper(eval(quote(ont), frame)) %in% c("BP", "MF", "CC")
  if (labels_only) {
    key_args[c("pvalueCutoff", "qvalueCutoff")] <- NULL
  }
  key <- gocache_key("go-result", key_args, gocache_result_options())
  value <- gocache_get(key)
  if (is.null(value)) {
    value <- do.call(original, args)
    if (!is.null(value)) {
      gocache_set(key, value)
    }
  }
  if (!is.null(value) && labels_only) {
    value@pvalueCutoff <- eval(quote(pvalueCutoff), frame)
    value@qvalueCutoff <- eval(quote(qvalueCutoff), frame)
  }
  value
}

gocache_result_options <- function() {
  list(enrichment_force_universe = getOption("enrichment_force_universe", FALSE))
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
    list(pkg = "clusterProfiler", name = "simplify_internal", make = function(original) gocache_simplify_internal, port = "bioc322-simplify"),
    list(pkg = "clusterProfiler", name = "simplify_internal", make = function(original) gocache323_simplify_internal, port = "bioc323-simplify"),
    list(pkg = "GOSemSim", name = "wangMethod", make = function(original) gocache_wang, port = "bioc322-wang"),
    list(pkg = "GOSemSim", name = "wangMethod", make = function(original) gocache323_wang, port = "bioc323-wang"),
    list(pkg = "GOSemSim", name = "computeIC", make = function(original) gocache_compute_ic, port = "bioc322-computeic"),
    list(pkg = "clusterProfiler", name = "kegg_rest", make = function(original) gocache_kegg_rest, fallback = FALSE),
    list(pkg = "DOSE", name = "enricher_internal", make = gocache_make_enricher, port = "bioc322-enricher"),
    list(pkg = "clusterProfiler", name = "enricher_internal", make = gocache_make_enricher, port = "bioc322-enricher")
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
    if (!is.null(gocache_original(patch$pkg, patch$name))) {
      return(invisible(TRUE))
    }
    original <- attr(original, "paradoxR_original")
    if (is.null(original)) {
      return(invisible(FALSE))
    }
  }
  if (!is.null(patch$port) && !compat_port_applies(patch$port, original)) {
    return(invisible(FALSE))
  }
  if (is.null(gocache$original)) {
    gocache$original <- list()
  }
  gocache$original[[paste0(patch$pkg, "::", patch$name)]] <- original
  replacement <- patch$make(original)
  if (!isTRUE(patch$exported) && !identical(patch$fallback, FALSE)) {
    replacement <- gocache_with_fallback(replacement, original, paste0(patch$pkg, ":::", patch$name))
  }
  attr(replacement, "paradoxR") <- TRUE
  attr(replacement, "paradoxR_original") <- original
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
  if (is.null(original)) {
    original <- attr(get(patch$name, envir = asNamespace(patch$pkg)), "paradoxR_original")
  }
  if (is.null(original)) {
    return(invisible(FALSE))
  }
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
#'   `enrichplot::pairwise_termsim(method = "Wang")`; results are unchanged;
#' * the first GO annotation for an OrgDb is read with one SQL query per
#'   ontology table instead of `AnnotationDbi::mapIds()`, and information
#'   content in `GOSemSim::godata()` is summed over all terms at once; the
#'   results are identical;
#' * every KEGG table that `enrichKEGG()`, `gseKEGG()`, `enrichMKEGG()` and
#'   `gseMKEGG()` download from the KEGG REST API is kept on disk per species,
#'   KEGG database and key type, so switching species or key type, or starting
#'   a new R session, no longer downloads it again. The cache is checked
#'   against KEGG's release dates once an hour and downloads again when KEGG
#'   has been updated; when KEGG is unreachable, the cached tables are used.
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
  if (gocache_patched()) {
    gocache_note_cutoffs()
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

gocache_with_fallback <- function(fast, original, label) {
  force(fast)
  force(original)
  function(...) {
    tryCatch(fast(...), error = function(e) {
      value <- original(...)
      gocache_warn_once(label, conditionMessage(e))
      value
    })
  }
}

gocache_warn_once <- function(label, reason) {
  if (label %in% gocache$warned) {
    return(invisible(NULL))
  }
  gocache$warned <- c(gocache$warned, label)
  warning(
    "paradoxR's replacement for ", label, " failed (", reason, "); ",
    "using the original function for this call.",
    call. = FALSE
  )
}

gocache_use_safely <- function() {
  tryCatch(use_go_cache(), error = function(e) {
    try(reset_go_cache(), silent = TRUE)
    warning(
      "paradoxR could not apply its clusterProfiler speed-ups (", conditionMessage(e), "); ",
      "the original functions stay in place.",
      call. = FALSE
    )
    invisible(FALSE)
  })
}

gocache_on_load <- function(...) {
  if (isTRUE(getOption("paradoxR.go_cache", TRUE))) {
    gocache_use_safely()
  }
}

gocache_unhook <- function() {
  for (pkg in gocache$hooked) {
    event <- packageEvent(pkg, "onLoad")
    keep <- Filter(function(hook) !identical(hook, gocache_on_load), getHook(event))
    setHook(event, keep, action = "replace")
  }
  gocache$hooked <- character()
  invisible(NULL)
}
