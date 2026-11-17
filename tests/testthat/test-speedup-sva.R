sva_original <- function() {
  fun <- get("match_quantiles", envir = asNamespace("sva"))
  if (gocache_is_replacement(fun)) attr(fun, "paradoxR_original") else fun
}

skip_unless_sva_port <- function() {
  skip_if_no_bioc("sva")
  testthat::skip_if_not(
    compat_port_applies("bioc322-sva-match-quantiles", sva_original()),
    "port not verified for this sva version"
  )
}

mq_inputs <- function(nr, nc, seed, integer = TRUE) {
  set.seed(seed)
  mu <- stats::rgamma(nr, shape = 0.6, scale = 200)
  old_mu <- matrix(mu, nr, nc) * stats::runif(nr * nc, 0.5, 2)
  new_mu <- old_mu * stats::runif(nr * nc, 0.6, 1.4)
  old_phi <- stats::runif(nr, 0, 0.5)
  new_phi <- stats::runif(nr, 0, 0.5)
  counts <- matrix(stats::rnbinom(nr * nc, mu = old_mu, size = 1 / pmax(old_phi, 1e-8)), nr, nc)
  counts[1, ] <- c(5000L, rep(0L, nc - 1L))
  if (integer) storage.mode(counts) <- "integer"
  dimnames(counts) <- list(paste0("g", seq_len(nr)), paste0("s", seq_len(nc)))
  list(counts_sub = counts, old_mu = old_mu, old_phi = old_phi, new_mu = new_mu, new_phi = new_phi)
}

test_that("match_quantiles() gives identical counts", {
  skip_unless_sva_port()
  original <- sva_original()
  fast <- speedup_make_match_quantiles(original)
  for (case in list(list(400, 6, 1, TRUE), list(300, 3, 2, FALSE), list(1, 4, 3, TRUE), list(250, 1, 4, TRUE))) {
    args <- do.call(mq_inputs, case)
    expect_identical(do.call(fast, args), do.call(original, args), info = paste(case, collapse = " "))
  }
})

test_that("the result type follows the original when no count moves", {
  skip_unless_sva_port()
  original <- sva_original()
  fast <- speedup_make_match_quantiles(original)
  args <- mq_inputs(50, 4, 5)
  args$counts_sub[] <- sample(0:1, length(args$counts_sub), TRUE)
  expect_identical(do.call(fast, args), do.call(original, args))
  expect_type(do.call(fast, args), "integer")
  args$counts_sub[1, 1] <- 1e6L
  args$old_mu[1, 1] <- 1
  args$old_phi[1] <- 0.01
  expect_identical(do.call(fast, args), do.call(original, args))
})

test_that("zero dispersion gives the Poisson limit like the original", {
  skip_unless_sva_port()
  original <- sva_original()
  fast <- speedup_make_match_quantiles(original)
  args <- mq_inputs(40, 4, 6)
  args$old_phi[1:10] <- 0
  args$new_phi[5:15] <- 0
  expect_identical(do.call(fast, args), do.call(original, args))
})

test_that("inputs the original rejects go to the original", {
  skip_unless_sva_port()
  original <- sva_original()
  fast <- speedup_make_match_quantiles(original)
  args <- mq_inputs(20, 4, 7)
  na_args <- args
  na_args$counts_sub[3, 2] <- NA
  expect_identical(
    tryCatch(do.call(fast, na_args), error = conditionMessage),
    tryCatch(do.call(original, na_args), error = conditionMessage)
  )
  neg_args <- args
  neg_args$old_phi[2] <- -0.1
  expect_identical(
    suppressWarnings(tryCatch(do.call(fast, neg_args), error = conditionMessage)),
    suppressWarnings(tryCatch(do.call(original, neg_args), error = conditionMessage))
  )
  empty <- lapply(args, function(a) if (is.matrix(a)) a[0, , drop = FALSE] else a[0])
  expect_identical(
    tryCatch(do.call(fast, empty), error = conditionMessage),
    tryCatch(do.call(original, empty), error = conditionMessage)
  )
})

test_that("ComBat_seq() returns identical counts with the replacement", {
  skip_unless_sva_port()
  set.seed(9)
  n_genes <- 1500
  mu <- stats::rgamma(n_genes, shape = 0.6, scale = 300)
  size <- 1 / stats::runif(n_genes, 0.02, 0.4)
  batch3 <- rep(c("a", "b", "c"), each = 4)
  counts <- vapply(seq_along(batch3), function(j) stats::rnbinom(n_genes, mu = mu * c(a = 1, b = 1.7, c = 0.6)[[batch3[j]]], size = size), numeric(n_genes))
  counts[1:5, batch3 == "a"] <- 0
  storage.mode(counts) <- "integer"
  dimnames(counts) <- list(paste0("g", seq_len(n_genes)), paste0("s", seq_along(batch3)))
  group <- rep(c("c", "t"), 6)
  calls <- list(
    list(counts = counts, batch = batch3, group = group),
    list(counts = counts, batch = batch3),
    list(counts = counts[, 1:8], batch = batch3[1:8], group = group[1:8], covar_mod = data.frame(x = rep(c("u", "v"), each = 1, length.out = 8)))
  )
  ns <- asNamespace("sva")
  before <- get("match_quantiles", envir = ns)
  withr::defer(gocache_replace(ns, "match_quantiles", before))
  run <- function(fun) {
    gocache_replace(ns, "match_quantiles", fun)
    lapply(calls, function(args) {
      tryCatch(
        {
          utils::capture.output(value <- do.call(sva::ComBat_seq, args))
          value
        },
        error = conditionMessage
      )
    })
  }
  original <- sva_original()
  expect_identical(run(speedup_make_match_quantiles(original)), run(original))
})

test_that("options(paradoxR.sva = FALSE) keeps the original function", {
  skip_unless_sva_port()
  withr::defer({
    reset_speedups()
    use_speedups()
  })
  withr::local_options(paradoxR.sva = FALSE)
  reset_speedups()
  use_speedups()
  expect_false(gocache_patched("sva", "match_quantiles"))
  withr::local_options(paradoxR.sva = TRUE)
  use_speedups()
  expect_true(gocache_patched("sva", "match_quantiles"))
})
