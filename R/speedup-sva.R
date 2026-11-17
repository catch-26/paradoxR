# sva::ComBat_seq() moves each count to the same negative binomial quantile
# under the batch-free mean with match_quantiles(), one pnbinom() and one
# qnbinom() call per gene and sample in a double loop. The replacement makes
# the same two calls once over all counts above 1. Inputs on which the original
# stops with an error or warns per element (NA, empty matrices, negative or
# non-finite parameters) go to the original.

speedup_mq_ok <- function(counts_sub, old_mu, old_phi, new_mu, new_phi) {
  dims <- dim(counts_sub)
  is.matrix(counts_sub) && (is.integer(counts_sub) || is.double(counts_sub)) &&
    length(dims) == 2L && all(dims > 0L) &&
    is.numeric(old_mu) && is.numeric(new_mu) && is.numeric(old_phi) && is.numeric(new_phi) &&
    identical(dim(old_mu), dims) && identical(dim(new_mu), dims) &&
    is.null(dim(old_phi)) && is.null(dim(new_phi)) &&
    length(old_phi) == dims[1L] && length(new_phi) == dims[1L] &&
    !anyNA(counts_sub) &&
    all(is.finite(old_mu)) && all(is.finite(new_mu)) && all(is.finite(old_phi)) && all(is.finite(new_phi)) &&
    all(old_mu >= 0) && all(new_mu >= 0) && all(old_phi >= 0) && all(new_phi >= 0)
}

speedup_make_match_quantiles <- function(original) {
  force(original)
  function(counts_sub, old_mu, old_phi, new_mu, new_phi) {
    if (!speedup_mq_ok(counts_sub, old_mu, old_phi, new_mu, new_phi)) {
      return(original(counts_sub = counts_sub, old_mu = old_mu, old_phi = old_phi, new_mu = new_mu, new_phi = new_phi))
    }
    nr <- nrow(counts_sub)
    result <- matrix(as.vector(counts_sub), nr, ncol(counts_sub))
    idx <- which(counts_sub > 1)
    if (length(idx) == 0L) {
      return(result)
    }
    gene <- (idx - 1L) %% nr + 1L
    tmp_p <- stats::pnbinom(counts_sub[idx] - 1, mu = old_mu[idx], size = (1 / old_phi)[gene])
    if (anyNA(tmp_p)) {
      return(original(counts_sub = counts_sub, old_mu = old_mu, old_phi = old_phi, new_mu = new_mu, new_phi = new_phi))
    }
    move <- abs(tmp_p - 1) >= 1e-04
    if (any(move)) {
      result[idx[move]] <- 1 + stats::qnbinom(tmp_p[move], mu = new_mu[idx[move]], size = (1 / new_phi)[gene[move]])
    }
    result
  }
}
