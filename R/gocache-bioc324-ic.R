gocache324_compute_ic <- function(goAnno, ont) {
  gosemsim <- asNamespace("GOSemSim")
  goids <- union(names(get("getAncestors", envir = gosemsim)(ont)), goAnno$GO)
  if (anyNA(goids)) {
    return(gocache_original("GOSemSim", "computeIC")(goAnno, ont))
  }
  goterms <- goAnno$GO
  counted <- table(goterms)
  goname <- names(counted)
  missing_ids <- setdiff(goids, goname)
  zeros <- double(length(missing_ids))
  names(zeros) <- missing_ids
  gocount <- as.vector(counted)
  names(gocount) <- goname
  gocount <- c(gocount, zeros)
  offspring_idx <- get("getOffspringIdx", envir = gosemsim)(ont, goids)
  gc <- gocount[goids]
  offspring_idx <- offspring_idx[match(goids, names(offspring_idx))]
  owner <- rep.int(seq_along(goids), lengths(offspring_idx))
  desc_sum <- vapply(
    split(unname(gc[unlist(offspring_idx, use.names = FALSE)]), factor(owner, levels = seq_along(goids))),
    sum,
    numeric(1),
    na.rm = TRUE
  )
  cnt <- gc + desc_sum
  names(cnt) <- goids
  p <- cnt / sum(gocount)
  -log(p)
}
