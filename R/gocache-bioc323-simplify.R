gocache323_sv_index <- function(ont) {
  if (is.null(gocache$sv_index_323)) {
    gocache$sv_index_323 <- list()
  }
  if (!is.null(gocache$sv_index_323[[ont]])) {
    return(gocache$sv_index_323[[ont]])
  }
  rel_df <- get("ensure_gotbl_cached", envir = asNamespace("GOSemSim"))()
  rel_df <- rel_df[rel_df$Ontology == ont, ]
  if (!"relationship" %in% colnames(rel_df)) {
    rel_df$relationship <- "other"
  }
  rel_df$relationship[rel_df$relationship %in% c("isa", "is a")] <- "is_a"
  rel_df$relationship[rel_df$relationship %in% c("part of")] <- "part_of"
  rel_df$relationship[!rel_df$relationship %in% c("is_a", "part_of")] <- "other"
  rel_df <- rel_df[!is.na(rel_df$parent), ]
  terms <- unique(c(rel_df[, 1], rel_df$parent))
  child <- match(rel_df[, 1], terms)
  weight <- c(is_a = 0.8, part_of = 0.6, other = 0.7)
  index <- list(
    terms = terms,
    child = child,
    parent = match(rel_df$parent, terms),
    weight = unname(weight[rel_df$relationship]),
    rows = split(seq_len(nrow(rel_df)), factor(child, levels = seq_along(terms)))
  )
  gocache$sv_index_323[[ont]] <- index
  index
}

gocache323_sv_one <- function(id, index) {
  start <- match(id, index$terms)
  if (is.na(start)) {
    return(c(stats::setNames(1, id), all = 0))
  }
  value <- rep(NA_real_, length(index$terms))
  value[start] <- 1
  seen <- start
  rows <- index$rows[[start]]
  while (length(rows) != 0) {
    parents <- index$parent[rows]
    first <- !duplicated(parents) & is.na(value[parents])
    value[parents[first]] <- index$weight[rows[first]] * value[index$child[rows[first]]]
    seen <- c(seen, parents[first])
    rows <- sort(unlist(index$rows[unique(parents)], use.names = FALSE))
  }
  sv <- stats::setNames(value[seen], index$terms[seen])
  sv["all"] <- 0
  sv
}

gocache323_wang <- function(t1, t2, ont) {
  original <- gocache_original("GOSemSim", "wangMethod")
  terms <- unique(c(t1, t2))
  if (!ont %in% c("BP", "CC", "MF") || anyNA(terms) || "all" %in% terms) {
    return(original(t1, t2, ont))
  }
  index <- gocache323_sv_index(ont)
  svs <- lapply(terms, gocache323_sv_one, index = index)
  names(svs) <- terms
  ancestors <- unique(unlist(lapply(svs, names), use.names = FALSE))
  weights <- matrix(0, length(terms), length(ancestors), dimnames = list(terms, ancestors))
  member <- weights
  for (term in terms) {
    weights[term, names(svs[[term]])] <- svs[[term]]
    member[term, names(svs[[term]])] <- 1
  }
  w1 <- weights[t1, , drop = FALSE]
  w2 <- weights[t2, , drop = FALSE]
  sim <- (w1 %*% t(member[t2, , drop = FALSE]) + member[t1, , drop = FALSE] %*% t(w2)) /
    outer(rowSums(w1), rowSums(w2), "+")
  sim[outer(t1, t2, "==")] <- 1
  dimnames(sim) <- list(t1, t2)
  sim
}

gocache323_simplify_internal <- function(res, cutoff = 0.7, by = "p.adjust", select_fun = min,
                                         measure = "Rel", ontology, semData) {
  if (missing(semData) || is.null(semData)) {
    warn <- get("yulab_warn", envir = asNamespace("yulab.utils"))
    warn("semData is not provided. It will be calculated automatically.")
    if (measure == "Wang") {
      semData <- GOSemSim::godata(ont = ontology)
    } else {
      semData <- GOSemSim::godata(ont = ontology, computeIC = TRUE)
    }
  } else if (ontology != semData@ont) {
    stop(paste("semData is for", semData@ont, "ontology, while enrichment result is for", ontology))
  }
  sim <- GOSemSim::mgoSim(res$ID, res$ID, semData = semData, measure = measure, combine = NULL)
  sim_df <- data.frame(
    go1 = rep(rownames(sim), times = ncol(sim)),
    go2 = rep(colnames(sim), each = nrow(sim)),
    similarity = as.vector(sim)
  )
  sim_df <- sim_df[!is.na(sim_df$similarity), ]
  sim_df <- merge(sim_df, res[, c("ID", by)], by.x = "go1", by.y = "ID")
  sim_df$go2 <- as.character(sim_df$go2)
  ids <- res$ID
  ancestors <- get("getAncestors", envir = asNamespace("GOSemSim"))(ontology)
  similar <- which(sim_df$similarity > cutoff)
  rows_by_term <- split(similar, factor(sim_df$go2[similar], levels = unique(ids)))
  to_remove <- character()
  for (id in ids) {
    rows <- rows_by_term[[id]]
    if (length(rows) < 2) {
      next
    }
    subset <- sim_df[rows, ]
    best <- which(subset[, by] == select_fun(subset[, by]))
    if (length(best) > 1) {
      depth <- vapply(subset$go1[best], function(go) length(ancestors[[go]]), numeric(1))
      best <- best[which.max(depth)]
    }
    to_remove <- unique(c(to_remove, subset$go1[-best]))
  }
  res[!res$ID %in% to_remove, ]
}
