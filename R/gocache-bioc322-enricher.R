gocache_intersect_each <- function(sets, keep) {
  if (!length(sets)) {
    return(lapply(sets, intersect, keep))
  }
  flat <- unlist(sets, use.names = FALSE)
  owner <- rep.int(seq_along(sets), lengths(sets))
  pos <- match(flat, keep, 0L)
  ok <- pos > 0L
  ok[ok] <- !duplicated(owner[ok] * (length(keep) + 1) + pos[ok])
  out <- split(flat[ok], factor(owner[ok], levels = seq_along(sets)))
  names(out) <- names(sets)
  out
}

gocache_enricher_swap <- function(expr) {
  if (missing(expr)) {
    return(quote(expr = ))
  }
  if (is.call(expr) && identical(expr[[1]], as.name("lapply")) && length(expr) == 4L &&
      identical(expr[[3]], as.name("intersect")) && identical(expr[[4]], as.name("extID"))) {
    return(call("gocache_intersect_each", expr[[2]], as.name("extID")))
  }
  if (is.call(expr)) {
    return(as.call(lapply(as.list(expr), gocache_enricher_swap)))
  }
  expr
}

gocache_make_enricher <- function(original) {
  fast <- original
  env <- new.env(parent = environment(original))
  env$gocache_intersect_each <- gocache_intersect_each
  body(fast) <- gocache_enricher_swap(body(original))
  environment(fast) <- env
  fast
}

gocache_cutoffs_are_labels <- function() {
  isTRUE(gocache$cutoffs_are_labels)
}

gocache_note_cutoffs <- function() {
  ns_cp <- asNamespace("clusterProfiler")
  enricher <- gocache_original("clusterProfiler", "enricher_internal")
  if (is.null(enricher)) {
    enricher <- get0("enricher_internal", envir = ns_cp, inherits = FALSE)
  }
  enrich_go <- gocache_original("clusterProfiler", "enrichGO")
  gocache$cutoffs_are_labels <- !is.null(enrich_go) &&
    compat_port_applies("bioc322-enricher", enricher) &&
    compat_port_applies("bioc322-enrichgo-cutoffs", enrich_go)
}
