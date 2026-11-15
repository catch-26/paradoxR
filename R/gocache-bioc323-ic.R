gocache323_offspring_index <- function(off, goids) {
  pos <- stats::setNames(seq_along(goids), goids)
  found <- off[goids]
  sizes <- lengths(found)
  flat <- unlist(found, use.names = FALSE)
  hit <- unname(pos[flat])
  group <- rep.int(seq_along(goids), sizes)
  keep <- !is.na(hit)
  idx <- split(as.integer(hit[keep]), factor(group[keep], levels = seq_along(goids)))
  names(idx) <- goids
  idx
}

gocache323_offspring_idx <- function(ont, goids) {
  digest <- getExportedValue("digest", "digest")
  get_cache_element <- getExportedValue("yulab.utils", "get_cache_element")
  update_cache_item <- getExportedValue("yulab.utils", "update_cache_item")
  key <- paste0("offspring_idx_", ont, "_", digest(goids))
  res <- get_cache_element("GOSemSim_offspring_idx", key)
  if (!is.null(res)) {
    return(res)
  }
  off <- get("getOffsprings", envir = asNamespace("GOSemSim"))(ont)
  idx <- gocache323_offspring_index(off, goids)
  e <- list()
  e[[key]] <- idx
  update_cache_item("GOSemSim_offspring_idx", e)
  idx
}
