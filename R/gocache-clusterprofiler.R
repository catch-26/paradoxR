gocache_go_data <- function(OrgDb, ont, keytype, ...) {
  original <- gocache_original("clusterProfiler", "get_GO_data")
  dots <- list(...)
  if (!all(vapply(dots, is.null, logical(1)))) {
    return(original(OrgDb, ont, keytype, ...))
  }
  scope <- if (compat_port_applies("bioc324-getgodata-sql", original)) "ALL" else ont
  key <- gocache_key("go-data", "go2gene", gocache_orgdb_id(OrgDb), scope, keytype)
  mapids <- function(x, keys, column, keytype, ..., multiVals) {
    value <- gocache_get(key)
    if (is.list(value) && identical(names(value), keys)) {
      return(value)
    }
    value <- gocache_mapids(x, keys = keys, column = column, keytype = keytype, ..., multiVals = multiVals)
    gocache_set(key, value)
    value
  }
  gocache_fast_go_data(original, mapids)(OrgDb, ont, keytype, ...)
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
    list(pkg = "clusterProfiler", name = "simplify_internal", make = function(original) gocache_simplify_internal, port = "bioc322-simplify"),
    list(pkg = "clusterProfiler", name = "simplify_internal", make = function(original) gocache323_simplify_internal, port = "bioc323-simplify"),
    list(pkg = "GOSemSim", name = "wangMethod", make = function(original) gocache_wang, port = "bioc322-wang"),
    list(pkg = "GOSemSim", name = "wangMethod", make = function(original) gocache323_wang, port = "bioc323-wang"),
    list(pkg = "GOSemSim", name = "wangMethod", make = function(original) gocache323_wang, port = "bioc324-wang"),
    list(pkg = "GOSemSim", name = "getOffspringIdx", make = function(original) gocache323_offspring_idx, port = "bioc323-offspring-idx"),
    list(pkg = "GOSemSim", name = "computeIC", make = function(original) gocache_compute_ic, port = "bioc322-computeic"),
    list(pkg = "GOSemSim", name = "computeIC", make = function(original) gocache324_compute_ic, port = "bioc324-computeic"),
    list(pkg = "clusterProfiler", name = "kegg_rest", make = function(original) gocache_kegg_rest, fallback = FALSE),
    list(pkg = "DOSE", name = "enricher_internal", make = gocache_make_enricher, port = "bioc322-enricher"),
    list(pkg = "clusterProfiler", name = "enricher_internal", make = gocache_make_enricher, port = "bioc322-enricher"),
    list(pkg = "DESeq2", name = "trimmedCellVariance", make = speedup_make_trimmed, port = "bioc322-deseq2-trimmed-cell", option = "paradoxR.deseq2"),
    list(pkg = "DESeq2", name = "trimmedVariance", make = speedup_make_trimmed, port = "bioc322-deseq2-trimmed", option = "paradoxR.deseq2"),
    list(pkg = "sva", name = "match_quantiles", make = speedup_make_match_quantiles, port = "bioc322-sva-match-quantiles", option = "paradoxR.sva"),
    list(pkg = "Seurat", name = "FindAllMarkers", make = resultcache_make("seurat-findallmarkers"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "DiffBind", name = "dba.count", make = resultcache_make("diffbind-dba-count"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "RunPCA.default", make = resultcache_make("seurat-runpca"), s3 = TRUE, fallback = FALSE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "RunUMAP.default", make = resultcache_make("seurat-runumap"), s3 = TRUE, fallback = FALSE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "FindNeighbors.default", make = resultcache_make("seurat-findneighbors"), s3 = TRUE, fallback = FALSE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "SCTransform.default", make = resultcache_make("seurat-sctransform"), s3 = TRUE, fallback = FALSE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "CCAIntegration", make = resultcache_make("seurat-ccaintegration"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "RPCAIntegration", make = resultcache_make("seurat-rpcaintegration"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "JointPCAIntegration", make = resultcache_make("seurat-jointpcaintegration"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "Seurat", name = "HarmonyIntegration", make = resultcache_make("seurat-harmonyintegration"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "csaw", name = "windowCounts", make = resultcache_make("csaw-windowcounts"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "csaw", name = "regionCounts", make = resultcache_make("csaw-regioncounts"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "GenomicAlignments", name = ".dispatchBamFiles", make = resultcache_make("genomicalignments-summarizeoverlaps"), fallback = FALSE, option = "paradoxR.result_cache"),
    list(pkg = "ChIPseeker", name = "getTagMatrix", make = resultcache_make("chipseeker-gettagmatrix"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "EnrichedHeatmap", name = "normalizeToMatrix", make = resultcache_make("enrichedheatmap-normalizetomatrix"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "Signac", name = "FeatureMatrix", make = resultcache_make("signac-featurematrix"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "GSVA", name = "gsva,gsvaParam", make = resultcache_make("gsva-gsva"), option = "paradoxR.result_cache"),
    list(pkg = "sva", name = "ComBat_seq", make = resultcache_make("sva-combatseq"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "DESeq2", name = "lfcShrink", make = resultcache_make("deseq2-lfcshrink"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "stats", name = "hclust", make = resultcache_make("stats-hclust"), exported = TRUE, option = "paradoxR.result_cache"),
    list(pkg = "MASS", name = "kde2d", make = resultcache_make("mass-kde2d"), exported = TRUE, option = "paradoxR.result_cache")
  )
}

gocache_original <- function(pkg, name) {
  gocache$original[[paste0(pkg, "::", name)]]
}

# The environments holding a function: its namespace, the attached package
# when it is exported, and the S3 method table when it is a registered S3
# method (UseMethod() finds methods there).
gocache_targets <- function(pkg, exported, s3 = FALSE) {
  ns <- asNamespace(pkg)
  envs <- list(ns)
  attached <- paste0("package:", pkg)
  if (isTRUE(exported) && attached %in% search()) {
    envs <- c(envs, list(as.environment(attached)))
  }
  if (isTRUE(s3)) {
    envs <- c(envs, list(get(".__S3MethodsTable__.", envir = ns, inherits = FALSE)))
  }
  envs
}

gocache_is_replacement <- function(fun) {
  isTRUE(attr(fun, "paradoxR"))
}

gocache_patched <- function(pkg = "clusterProfiler", name = "get_GO_data") {
  if (grepl(",", name, fixed = TRUE)) {
    return(gocache_s4_patched(pkg, name))
  }
  isNamespaceLoaded(pkg) &&
    exists(name, envir = asNamespace(pkg), inherits = FALSE) &&
    gocache_is_replacement(get(name, envir = asNamespace(pkg)))
}

# S4 methods are named "generic,signature" in the patch registry. A method
# lives in the package's method table and in the generic's dispatch tables;
# each copy's function is replaced, so that dispatch from any caller reaches
# the replacement.
gocache_s4_signature <- function(name) {
  sub("^[^,]*,", "", name)
}

gocache_s4_tables <- function(pkg, name) {
  generic <- sub(",.*$", "", name)
  signature <- gocache_s4_signature(name)
  ns <- asNamespace(pkg)
  tables <- list()
  table <- paste0(".__T__", generic, ":", pkg)
  if (exists(table, envir = ns, inherits = FALSE)) {
    tables <- c(tables, list(get(table, envir = ns)))
  }
  fun <- get0(generic, envir = ns, inherits = FALSE)
  if (methods::is(fun, "genericFunction")) {
    env <- environment(fun)
    tables <- c(tables, Filter(Negate(is.null), list(env$.MTable, env$.AllMTable)))
  }
  Filter(function(table) exists(signature, envir = table, inherits = FALSE), tables)
}

# Assigning a method's function drops its attributes, so the replacement is
# remembered here and recognised by identity.
gocache_s4_patched <- function(pkg, name) {
  key <- paste0(pkg, "::", name)
  replacement <- gocache$s4_replacement[[key]]
  tables <- if (isNamespaceLoaded(pkg)) gocache_s4_tables(pkg, name) else list()
  !is.null(replacement) && length(tables) > 0L &&
    identical(get(gocache_s4_signature(name), envir = tables[[1L]])@.Data, replacement)
}

gocache_s4_set <- function(pkg, name, fun) {
  signature <- gocache_s4_signature(name)
  for (table in gocache_s4_tables(pkg, name)) {
    method <- get(signature, envir = table)
    method@.Data <- fun
    assign(signature, method, envir = table)
  }
  invisible(TRUE)
}

gocache_apply_s4 <- function(patch) {
  tables <- gocache_s4_tables(patch$pkg, patch$name)
  if (length(tables) == 0L) {
    return(invisible(FALSE))
  }
  if (gocache_s4_patched(patch$pkg, patch$name)) {
    return(invisible(TRUE))
  }
  original <- get(gocache_s4_signature(patch$name), envir = tables[[1L]])@.Data
  if (!is.null(patch$option) && !isTRUE(getOption(patch$option, TRUE))) {
    return(invisible(FALSE))
  }
  if (is.null(gocache$original)) {
    gocache$original <- list()
  }
  gocache$original[[paste0(patch$pkg, "::", patch$name)]] <- original
  replacement <- patch$make(original)
  attr(replacement, "paradoxR") <- TRUE
  attr(replacement, "paradoxR_original") <- original
  gocache_s4_set(patch$pkg, patch$name, replacement)
  if (is.null(gocache$s4_replacement)) {
    gocache$s4_replacement <- list()
  }
  key <- paste0(patch$pkg, "::", patch$name)
  gocache$s4_replacement[[key]] <- get(gocache_s4_signature(patch$name), envir = tables[[1L]])@.Data
  invisible(TRUE)
}

gocache_apply <- function(patch) {
  if (grepl(",", patch$name, fixed = TRUE)) {
    return(gocache_apply_s4(patch))
  }
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
  if (!is.null(patch$option) && !isTRUE(getOption(patch$option, TRUE))) {
    return(invisible(FALSE))
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
  for (env in gocache_targets(patch$pkg, patch$exported, patch$s3)) {
    gocache_replace(env, patch$name, replacement)
  }
  invisible(TRUE)
}

gocache_revert <- function(patch) {
  if (!gocache_patched(patch$pkg, patch$name)) {
    return(invisible(FALSE))
  }
  original <- gocache_original(patch$pkg, patch$name)
  if (grepl(",", patch$name, fixed = TRUE)) {
    return(if (is.null(original)) invisible(FALSE) else gocache_s4_set(patch$pkg, patch$name, original))
  }
  if (is.null(original)) {
    original <- attr(get(patch$name, envir = asNamespace(patch$pkg)), "paradoxR_original")
  }
  if (is.null(original)) {
    return(invisible(FALSE))
  }
  for (env in gocache_targets(patch$pkg, patch$exported, patch$s3)) {
    gocache_replace(env, patch$name, original)
  }
  invisible(TRUE)
}

#' Apply or remove paradoxR's speed-ups
#'
#' `use_speedups()` replaces functions inside clusterProfiler, DOSE, GOSemSim,
#' DESeq2, sva, Seurat, Signac, DiffBind, csaw, GenomicAlignments, ChIPseeker,
#' EnrichedHeatmap, GSVA, stats and MASS so that
#' `clusterProfiler::enrichGO()`, `clusterProfiler::simplify()` and the
#' functions that call them, such as `clusterProfiler::compareCluster()`,
#' reuse earlier work and run faster:
#'
#' * the GO-to-gene table for an OrgDb, ontology and key type is queried once
#'   and kept on disk, so a new R session reads it instead of querying the
#'   OrgDb again and builds the rest of the GO annotation as the original;
#' * Wang semantic similarity between GO terms is computed for all term pairs
#'   at once instead of pair by pair, which speeds up `simplify()` and
#'   `enrichplot::pairwise_termsim(method = "Wang")`; results are unchanged;
#' * the first GO annotation for an OrgDb is read with one SQL query per
#'   ontology table instead of `AnnotationDbi::mapIds()`, and information
#'   content in `GOSemSim::godata()` is summed over all terms at once; the
#'   results are identical;
#' * `DESeq2::DESeq()` computes the trimmed means behind Cook's distances
#'   with one sort per matrix instead of one `mean(trim = )` call per gene;
#'   a gene's value is used only where it provably equals the original;
#' * `sva::ComBat_seq()` moves counts to their adjusted quantiles in one
#'   vectorised `stats::pnbinom()` and `stats::qnbinom()` call instead of one
#'   call per gene and sample;
#' * every KEGG table that `enrichKEGG()`, `gseKEGG()`, `enrichMKEGG()` and
#'   `gseMKEGG()` download from the KEGG REST API is kept on disk per species,
#'   KEGG database and key type, so switching species or key type, or starting
#'   a new R session, no longer downloads it again. The cache is checked
#'   against KEGG's release dates once an hour and downloads again when KEGG
#'   has been updated; when KEGG is unreachable, the cached tables are used;
#' * the results of `Seurat::FindAllMarkers()`, `RunPCA()`, `FindNeighbors()`,
#'   `RunUMAP()`, `SCTransform()` and `IntegrateLayers()` (CCA, RPCA, joint
#'   PCA and Harmony), `Signac::FeatureMatrix()`, `DiffBind::dba.count()`,
#'   `csaw::windowCounts()`, `csaw::regionCounts()`,
#'   `GenomicAlignments::summarizeOverlaps()` on BAM files,
#'   `ChIPseeker::getTagMatrix()`, `EnrichedHeatmap::normalizeToMatrix()`,
#'   `GSVA::gsva()` on a dense matrix, `sva::ComBat_seq()`,
#'   `DESeq2::lfcShrink()`, `stats::hclust()` on 5,000 or more objects and
#'   `MASS::kde2d()` on large inputs are stored on disk, so running a script
#'   or rendering a document again returns them at once when nothing they
#'   depend on has changed. A stored result is
#'   used only when the argument values, the content of every input file
#'   (BAM files and their indexes, fragment files, peak files and annotation
#'   databases), the installed versions of the package and everything it depends on,
#'   the R build and the relevant options all match. A hit repeats what the
#'   original call did: printed output, progress text, messages and warnings
#'   in their order, the visibility of the result, option changes, loaded
#'   packages and the random number state. Only calls that took at least
#'   2 seconds (`options(paradoxR.result_cache_min_seconds = )`) and at least
#'   3 times as long as storing their result
#'   (`options(paradoxR.result_cache_min_ratio = )`) are stored, and
#'   `options(paradoxR.result_cache = FALSE)` turns this off. Nothing is stored
#'   when the cache directory is on a network drive or when the `ps` package,
#'   which measures free disk space, is not installed.
#'
#' Called automatically when paradoxR is loaded. Packages loaded later are
#' sped up as they load. The cache is keyed by the clusterProfiler and GO.db
#' versions and the OrgDb database file, so updating any of them starts a
#' fresh cache. Every cached item that has not been used for 7 days is
#' deleted automatically; see [clear_cache()]. The `simplify()`, similarity,
#' DESeq2 and sva replacements apply only to the package versions they were
#' checked against; other versions keep their own code.
#'
#' `reset_speedups()` restores the original functions, for example to compare
#' results against the original code. With `packages`, only the functions of
#' those packages are restored and the others stay fast. A restored package
#' stays original for the rest of the session, also if it is loaded later,
#' until `use_speedups()` is called for it. [clear_cache()] deletes the cache.
#'
#' To keep the original functions from the start of a session, set
#' `options(paradoxR.speedups = FALSE)` (all packages),
#' `options(paradoxR.deseq2 = FALSE)` or `options(paradoxR.sva = FALSE)`
#' before loading paradoxR.
#'
#' @param packages Names of the packages to speed up or restore, any of
#'   `"clusterProfiler"`, `"DOSE"`, `"GOSemSim"`, `"DESeq2"`, `"sva"`,
#'   `"Seurat"`, `"Signac"`, `"DiffBind"`, `"csaw"`, `"GenomicAlignments"`,
#'   `"ChIPseeker"`, `"EnrichedHeatmap"`, `"GSVA"`, `"stats"` and `"MASS"`.
#'   `NULL`, the default, means all of them.
#' @return `use_speedups()` returns `TRUE` invisibly when at least one function
#'   of `packages` is replaced, `FALSE` when none is, for example because the
#'   packages are not loaded yet; they are then sped up when they load.
#'   `reset_speedups()` returns `TRUE` invisibly when it restored at least one
#'   function.
#' @export
#' @examples
#' use_speedups()
#' reset_speedups("DESeq2")
#' use_speedups("DESeq2")
#' reset_speedups()
#' use_speedups()
use_speedups <- function(packages = NULL) {
  packages <- speedups_packages(packages)
  gocache$disabled <- setdiff(gocache$disabled, packages)
  speedups_hook()
  speedups_apply(packages)
  invisible(any(speedups_patched(packages)))
}

#' @rdname use_speedups
#' @export
reset_speedups <- function(packages = NULL) {
  packages <- speedups_packages(packages)
  gocache$disabled <- union(gocache$disabled, packages)
  invisible(speedups_revert(packages))
}

speedups_all_packages <- function() {
  unique(vapply(gocache_patches(), `[[`, character(1), "pkg"))
}

speedups_packages <- function(packages) {
  all <- speedups_all_packages()
  if (is.null(packages)) {
    return(all)
  }
  if (!is.character(packages) || anyNA(packages)) {
    rlang::abort("`packages` must be a character vector of package names.")
  }
  found <- all[match(tolower(packages), tolower(all))]
  if (anyNA(found)) {
    rlang::abort(c(
      paste0("paradoxR has no speed-ups for ", paste0("\"", packages[is.na(found)], "\"", collapse = ", "), "."),
      i = paste0("Use one of ", paste0("\"", all, "\"", collapse = ", "), ".")
    ))
  }
  unique(found)
}

speedups_select <- function(packages) {
  Filter(function(patch) patch$pkg %in% packages, gocache_patches())
}

speedups_hook <- function() {
  if (is.null(gocache$hooked)) {
    gocache$hooked <- character()
  }
  for (pkg in speedups_all_packages()) {
    if (!isNamespaceLoaded(pkg) && !pkg %in% gocache$hooked) {
      setHook(packageEvent(pkg, "onLoad"), gocache_on_load)
      gocache$hooked <- c(gocache$hooked, pkg)
    }
  }
  invisible(NULL)
}

speedups_apply <- function(packages) {
  for (patch in speedups_select(packages)) {
    if (isNamespaceLoaded(patch$pkg)) {
      gocache_apply(patch)
    }
  }
  invisible(NULL)
}

speedups_revert <- function(packages) {
  changed <- FALSE
  for (patch in speedups_select(packages)) {
    changed <- isTRUE(gocache_revert(patch)) || changed
  }
  changed
}

speedups_patched <- function(packages) {
  vapply(speedups_select(packages), function(patch) gocache_patched(patch$pkg, patch$name), logical(1))
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
  tryCatch({
    speedups_hook()
    speedups_apply(setdiff(speedups_all_packages(), gocache$disabled))
  }, error = function(e) {
    try(speedups_revert(speedups_all_packages()), silent = TRUE)
    warning(
      "paradoxR could not apply its speed-ups (", conditionMessage(e), "); ",
      "the original functions stay in place.",
      call. = FALSE
    )
    invisible(FALSE)
  })
}

gocache_on_load <- function(...) {
  if (speedups_enabled()) {
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
