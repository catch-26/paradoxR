deseq2_original <- function(name) {
  fun <- get(name, envir = asNamespace("DESeq2"))
  if (gocache_is_replacement(fun)) attr(fun, "paradoxR_original") else fun
}

skip_unless_deseq2_port <- function() {
  skip_if_no_bioc("DESeq2")
  testthat::skip_if_not(
    compat_port_applies("bioc322-deseq2-trimmed-cell", deseq2_original("trimmedCellVariance")) &&
      compat_port_applies("bioc322-deseq2-trimmed", deseq2_original("trimmedVariance")),
    "port not verified for this DESeq2 version"
  )
}

trim_for <- function(n) c(1/3, 1/4, 1/8)[as.integer(cut(n, breaks = c(0, 3.5, 23.5, Inf)))]

wide_matrix <- function(nr, n, seed) {
  set.seed(seed)
  x <- matrix(stats::rgamma(nr * n, shape = 0.6, scale = 400) / stats::runif(nr * n, 0.5, 2), nr, n)
  x[sample.int(length(x), length(x) %/% 5)] <- 0
  x[seq_len(nr) %% 7 == 0, ] <- round(x[seq_len(nr) %% 7 == 0, ])
  extreme <- matrix(stats::runif(nr * n) * 10^stats::runif(nr * n, -12, 12), nr, n)
  close <- matrix(1 + stats::runif(nr * n) * 2^-40, nr, n) * 10^stats::runif(nr, -5, 5)
  x <- rbind(x, x^2, x * 1e-9, extreme, close)
  rownames(x) <- paste0("g", seq_len(nrow(x)))
  x
}

synthetic_counts <- function(n_genes, n_samples, seed = 1) {
  set.seed(seed)
  mu <- stats::rgamma(n_genes, shape = 0.6, scale = 400)
  size <- 1 / stats::runif(n_genes, 0.02, 0.4)
  counts <- vapply(seq_len(n_samples), function(j) stats::rnbinom(n_genes, mu = mu * (1 + (j %% 2) * 0.6), size = size), numeric(n_genes))
  counts[sample.int(length(counts), 20)] <- counts[sample.int(length(counts), 20)] * 50
  storage.mode(counts) <- "integer"
  dimnames(counts) <- list(paste0("g", seq_len(n_genes)), paste0("s", seq_len(n_samples)))
  counts
}

test_that("the trimmed row mean equals mean(trim = ) wherever it returns a value", {
  for (n in 1:30) {
    for (trim in unique(c(trim_for(n), 1/3, 1/4, 1/8, 0.1))) {
      x <- wide_matrix(400, n, seed = n)
      fast <- speedup_row_trimmed_mean(x, trim)
      if (!is.null(fast)) {
        expect_identical(fast, apply(x, 1, mean, trim = trim), info = paste("n =", n, "trim =", trim))
      }
    }
  }
})

test_that("three samples per cell always take the fast path", {
  x <- wide_matrix(2000, 3, seed = 3)
  fast <- speedup_row_trimmed_mean(x, 1/3)
  expect_false(is.null(fast))
  expect_identical(fast, apply(x, 1, mean, trim = 1/3))
})

test_that("the trimmed row mean keeps row names like apply()", {
  x <- wide_matrix(10, 6, seed = 1)
  expect_identical(names(speedup_trimmed_rows(x, 1/4)), rownames(x))
  rownames(x) <- NULL
  expect_null(names(speedup_trimmed_rows(x, 1/4)))
})

test_that("inputs mean(trim = ) treats specially go to apply()", {
  x <- wide_matrix(20, 6, seed = 2)
  bad <- list(
    na = replace(x, 5, NA),
    inf = replace(x, 5, Inf),
    nan = replace(x, 5, NaN),
    integer = matrix(1:120, 20, 6),
    no_rows = x[0, , drop = FALSE],
    no_cols = x[, 0, drop = FALSE]
  )
  for (name in names(bad)) {
    expect_null(speedup_row_trimmed_mean(bad[[name]], 1/4), label = name)
    if (nrow(bad[[name]]) > 0L) {
      expect_identical(speedup_trimmed_rows(bad[[name]], 1/4), apply(bad[[name]], 1, mean, trim = 1/4), label = name)
    }
  }
  expect_null(speedup_row_trimmed_mean(x, 0))
  expect_null(speedup_row_trimmed_mean(x, 0.5))
  expect_null(speedup_row_trimmed_mean(as.vector(x), 1/4))
})

test_that("other long double formats fall back unless one value is left", {
  x <- wide_matrix(50, 6, seed = 4)
  testthat::local_mocked_bindings(speedup_longdouble_digits = function() 113L)
  expect_null(speedup_row_trimmed_mean(x, 1/4))
  expect_identical(speedup_trimmed_rows(x, 1/4), apply(x, 1, mean, trim = 1/4))
  expect_false(is.null(speedup_row_trimmed_mean(x[, 1:3], 1/3)))
})

test_that("53-digit platforms take the fast path only up to three middle values", {
  testthat::local_mocked_bindings(speedup_longdouble_digits = function() 53L)
  expect_false(is.null(speedup_row_trimmed_mean(wide_matrix(10, 4, 1), 1/4)))
  expect_false(is.null(speedup_row_trimmed_mean(wide_matrix(10, 5, 1), 1/4)))
  expect_null(speedup_row_trimmed_mean(wide_matrix(10, 6, 1), 1/4))
})

test_that("values that could overflow go to apply()", {
  x <- matrix(c(0, 1.5e308, 1.7e308, 3), 1, 4)
  expect_null(speedup_row_trimmed_mean(x, 1/4))
  expect_identical(speedup_trimmed_rows(x, 1/4), apply(x, 1, mean, trim = 1/4))
})

test_that("rows near a rounding boundary or with mixed signs equal mean(trim = )", {
  half_ulp <- matrix(c(0, 1, 1 + 2^-52, 5, 0, 3, 3 + 2^-51, 9), 2, 4, byrow = TRUE)
  expect_false(any(speedup_rounds_like_mean(t(apply(half_ulp, 1, sort))[, 2:3], rowMeans(t(apply(half_ulp, 1, sort))[, 2:3]))))
  expect_identical(speedup_trimmed_rows(half_ulp, 1/4), apply(half_ulp, 1, mean, trim = 1/4))
  set.seed(11)
  for (n in c(4, 6, 8, 12, 24, 30)) {
    x <- matrix(stats::rnorm(4000 * n) * 10^stats::runif(4000 * n, -8, 8), 4000, n)
    x[seq_len(1000), ] <- x[seq_len(1000), ] + 1e6
    x[1001:2000, ] <- round(x[1001:2000, ] * 2^20) / 2^20 + 2^-30
    expect_identical(speedup_trimmed_rows(x, trim_for(n)), apply(x, 1, mean, trim = trim_for(n)), info = paste("n =", n))
  }
})

test_that("most rows of count-like data take the fast path", {
  testthat::skip_if_not(speedup_longdouble_digits() == 64L, "needs 64-digit long double")
  x <- wide_matrix(2000, 24, seed = 9)[seq_len(2000), ]
  middle <- t(apply(x, 1, sort))[, 4:21]
  expect_gt(mean(speedup_rounds_like_mean(middle, rowMeans(middle))), 0.75)
})

test_that("the swap rewrites only apply(x, 1, mean, trim = ) calls", {
  skip_unless_deseq2_port()
  for (name in c("trimmedCellVariance", "trimmedVariance")) {
    original <- deseq2_original(name)
    fast <- speedup_make_trimmed(original)
    text <- paste(deparse(body(fast)), collapse = "\n")
    expect_false(grepl("apply\\([^)]*mean", text), info = name)
    expect_true(grepl("speedup_trimmed_rows", text), info = name)
  }
  f <- function(x) apply(x, 2, mean, trim = 0.1)
  expect_identical(speedup_make_trimmed(f), f)
})

test_that("trimmedCellVariance() and trimmedVariance() give identical results", {
  skip_unless_deseq2_port()
  cnts <- wide_matrix(3000, 24, seed = 7)
  cell_sets <- list(
    rep(1:8, each = 3),
    rep(1:4, each = 6),
    rep(1:2, each = 12),
    c(rep(1, 3), rep(2, 4), rep(3, 5), rep(4, 12))
  )
  original_cell <- deseq2_original("trimmedCellVariance")
  fast_cell <- speedup_make_trimmed(original_cell)
  for (cells in cell_sets) {
    cells <- factor(cells)
    expect_identical(fast_cell(cnts, cells), original_cell(cnts, cells))
  }
  original_all <- deseq2_original("trimmedVariance")
  fast_all <- speedup_make_trimmed(original_all)
  for (n in c(3, 8, 24)) {
    expect_identical(fast_all(cnts[, seq_len(n)]), original_all(cnts[, seq_len(n)]))
  }
})

test_that("DESeq() returns identical objects with the replacement", {
  skip_unless_deseq2_port()
  counts <- synthetic_counts(2500, 24)
  designs <- list(
    list(data = data.frame(batch = factor(rep(c("a", "b"), each = 12)), group = factor(rep(rep(c("c", "t"), each = 6), 2))), formula = ~ batch + group),
    list(data = data.frame(batch = factor(rep(letters[1:4], each = 6)), group = factor(rep(rep(c("c", "t"), each = 3), 4))), formula = ~ batch + group),
    list(data = data.frame(group = factor(rep(c("c", "t"), each = 12))), formula = ~ group),
    list(data = data.frame(x = seq_len(24) / 3), formula = ~ x)
  )
  ns <- asNamespace("DESeq2")
  run <- function(fast) {
    for (name in c("trimmedCellVariance", "trimmedVariance")) {
      original <- deseq2_original(name)
      gocache_replace(ns, name, if (fast) speedup_make_trimmed(original) else original)
    }
    lapply(designs, function(d) {
      rownames(d$data) <- colnames(counts)
      dds <- suppressMessages(DESeq2::DESeqDataSetFromMatrix(counts, d$data, d$formula))
      fit <- suppressMessages(DESeq2::DESeq(dds, quiet = TRUE))
      list(cooks = SummarizedExperiment::assays(fit)[["cooks"]], results = as.data.frame(DESeq2::results(fit)), mcols = as.data.frame(S4Vectors::mcols(fit)))
    })
  }
  before <- mget(c("trimmedCellVariance", "trimmedVariance"), envir = ns)
  withr::defer(for (name in names(before)) gocache_replace(ns, name, before[[name]]))
  expect_identical(run(TRUE), run(FALSE))
})

test_that("options(paradoxR.deseq2 = FALSE) keeps the original functions", {
  skip_unless_deseq2_port()
  withr::defer({
    reset_speedups()
    use_speedups()
  })
  withr::local_options(paradoxR.deseq2 = FALSE)
  reset_speedups()
  use_speedups()
  expect_false(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_false(gocache_patched("DESeq2", "trimmedVariance"))
  withr::local_options(paradoxR.deseq2 = TRUE)
  use_speedups()
  expect_true(gocache_patched("DESeq2", "trimmedCellVariance"))
})
