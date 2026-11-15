gocache_sv_index <- function(ont) {
  if (is.null(gocache$sv_index)) {
    gocache$sv_index <- list()
  }
  if (!is.null(gocache$sv_index[[ont]])) {
    return(gocache$sv_index[[ont]])
  }
  sem_env <- get("get_gosemsim_env", envir = asNamespace("GOSemSim"))()
  rel_df <- get("gotbl", envir = sem_env)
  rel_df <- rel_df[rel_df$Ontology == ont, ]
  if (!"relationship" %in% colnames(rel_df)) {
    rel_df$relationship <- "other"
  }
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
  gocache$sv_index[[ont]] <- index
  index
}

gocache_sv_one <- function(id, index) {
  if (identical(id, "all")) {
    return(c(all = 1))
  }
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

gocache_sv <- function(terms, ont) {
  index <- gocache_sv_index(ont)
  svs <- lapply(terms, gocache_sv_one, index = index)
  names(svs) <- terms
  svs
}

gocache_wang <- function(t1, t2, ont) {
  original <- gocache_original("GOSemSim", "wangMethod")
  if (!ont %in% c("BP", "CC", "MF")) {
    return(original(t1, t2, ont))
  }
  terms <- unique(c(t1, t2))
  if (anyNA(terms) || "all" %in% terms) {
    return(original(t1, t2, ont))
  }
  svs <- gocache_sv(terms, ont)
  unusable <- vapply(svs, function(sv) all(is.na(sv)), logical(1))
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
  sim[unusable[t1], ] <- NA
  sim[, unusable[t2]] <- NA
  sim[outer(t1, t2, "==")] <- 1
  dimnames(sim) <- list(t1, t2)
  sim
}

gocache_simplify_internal <- function(res, cutoff = 0.7, by = "p.adjust", select_fun = min,
                                      measure = "Rel", ontology, semData) {
  if (missing(semData) || is.null(semData)) {
    if (measure == "Wang") {
      semData <- GOSemSim::godata(ont = ontology)
    } else {
      stop("godata should be provided for IC-based methods...")
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
