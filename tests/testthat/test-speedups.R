patched_original <- function(pkg, name) {
  fun <- get(name, envir = asNamespace(pkg))
  if (gocache_is_replacement(fun)) attr(fun, "paradoxR_original") else fun
}

skip_unless_rnaseq_ports <- function() {
  skip_if_no_bioc("DESeq2")
  skip_if_no_bioc("sva")
  testthat::skip_if_not(
    compat_port_applies("bioc322-deseq2-trimmed-cell", patched_original("DESeq2", "trimmedCellVariance")) &&
      compat_port_applies("bioc322-sva-match-quantiles", patched_original("sva", "match_quantiles")),
    "ports not verified for these DESeq2 and sva versions"
  )
}

local_all_speedups <- function(env = parent.frame()) {
  withr::defer(use_speedups(), envir = env)
  use_speedups()
}

test_that("package names are matched without regard to case", {
  expect_identical(speedups_packages(NULL), speedups_all_packages())
  expect_identical(speedups_packages(c("deseq2", "SVA", "DESeq2")), c("DESeq2", "sva"))
  expect_setequal(speedups_all_packages(), c("clusterProfiler", "GOSemSim", "DOSE", "DESeq2", "sva", "Seurat", "DiffBind"))
})

test_that("unknown package names are an error", {
  expect_error(reset_speedups("limma"), "no speed-ups for \"limma\"")
  expect_error(use_speedups(c("DESeq2", "edgeR")), "\"edgeR\"")
  expect_error(reset_speedups(1), "character vector")
  expect_error(reset_speedups(NA_character_), "character vector")
})

test_that("reset_speedups() with a package restores only that package", {
  skip_unless_rnaseq_ports()
  local_all_speedups()
  expect_true(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_true(gocache_patched("sva", "match_quantiles"))
  expect_true(reset_speedups("DESeq2"))
  expect_false(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_false(gocache_patched("DESeq2", "trimmedVariance"))
  expect_identical(
    get("trimmedCellVariance", envir = asNamespace("DESeq2")),
    gocache_original("DESeq2", "trimmedCellVariance")
  )
  expect_true(gocache_patched("sva", "match_quantiles"))
  expect_false(reset_speedups("DESeq2"))
  expect_true(use_speedups("DESeq2"))
  expect_true(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_true(gocache_patched("sva", "match_quantiles"))
})

test_that("a restored package stays original when other packages load", {
  skip_unless_rnaseq_ports()
  local_all_speedups()
  reset_speedups(c("DESeq2", "sva"))
  use_speedups("sva")
  gocache_on_load()
  expect_false(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_true(gocache_patched("sva", "match_quantiles"))
  use_speedups()
  expect_true(gocache_patched("DESeq2", "trimmedCellVariance"))
})

test_that("reset_speedups() without packages restores every package until use_speedups()", {
  skip_unless_rnaseq_ports()
  local_all_speedups()
  expect_true(reset_speedups())
  gocache_on_load()
  expect_false(any(speedups_patched(speedups_all_packages())))
  expect_false(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_true(use_speedups())
  expect_true(gocache_patched("DESeq2", "trimmedCellVariance"))
  expect_true(gocache_patched("sva", "match_quantiles"))
})

test_that("reset_speedups(\"clusterProfiler\") keeps the GOSemSim replacements", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("GOSemSim")
  loadNamespace("clusterProfiler")
  local_all_speedups()
  testthat::skip_if_not(gocache_patched("GOSemSim", "wangMethod"), "Wang port not verified for this GOSemSim version")
  reset_speedups("clusterProfiler")
  expect_false(gocache_patched())
  expect_false(gocache_patched("clusterProfiler", "kegg_rest"))
  expect_true(gocache_patched("GOSemSim", "wangMethod"))
  use_speedups("clusterProfiler")
  expect_true(gocache_patched())
})
