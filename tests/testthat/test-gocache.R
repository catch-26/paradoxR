test_that("the cache stores, reloads from disk and clears", {
  cache_dir <- tempfile("gocache")
  old <- options(paradoxR.go_cache_dir = cache_dir)
  on.exit({ options(old); unlink(cache_dir, recursive = TRUE) }, add = TRUE)
  gocache_set("go-result-a", list(x = 1))
  expect_identical(gocache_get("go-result-a"), list(x = 1))
  gocache$memory_result <- list()
  expect_identical(gocache_get("go-result-a"), list(x = 1))
  expect_null(gocache_get("go-result-b"))
  expect_identical(clear_go_cache(), 1L)
  expect_null(gocache_get("go-result-a"))
})

test_that("the in-memory store keeps the most recently used GO data", {
  old <- options(paradoxR.go_cache_dir = tempfile("gocache"))
  on.exit({ gocache$memory_data <- list(); options(old) }, add = TRUE)
  gocache$memory_data <- list()
  for (k in paste0("go-data-", 1:4)) gocache_remember(k, k)
  expect_identical(names(gocache$memory_data), paste0("go-data-", 2:4))
})

test_that("enrichGO() returns the same result with and without the cache", {
  testthat::skip_if_not_installed("clusterProfiler")
  testthat::skip_if_not_installed("org.Hs.eg.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.go_cache_dir = dir)
  on.exit({ use_go_cache(); options(old); unlink(dir, recursive = TRUE) }, add = TRUE)
  genes <- AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype = "ENTREZID")[1:300]
  run <- function() {
    clusterProfiler::enrichGO(genes, org.Hs.eg.db::org.Hs.eg.db, ont = "MF", pvalueCutoff = 1, qvalueCutoff = 1)
  }
  use_go_cache()
  expect_true(gocache_patched())
  cached <- run()
  expect_true(length(list.files(dir, pattern = "^go-data-")) == 1L)
  expect_true(length(list.files(dir, pattern = "^go-result-")) == 1L)
  expect_identical(run()@result, cached@result)
  reset_go_cache()
  expect_false(gocache_patched())
  expect_identical(run()@result, cached@result)
})
