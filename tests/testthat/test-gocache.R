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

test_that("simplify() and Wang similarity give the same result with and without the replacements", {
  testthat::skip_if_not_installed("clusterProfiler")
  testthat::skip_if_not_installed("GOSemSim")
  testthat::skip_if_not_installed("org.Hs.eg.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.go_cache_dir = dir)
  on.exit({ use_go_cache(); options(old); unlink(dir, recursive = TRUE) }, add = TRUE)
  genes <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, "GO:0006955", "ENTREZID", "GOALL", multiVals = "list")[[1]][1:150]
  use_go_cache()
  ego <- clusterProfiler::enrichGO(genes, org.Hs.eg.db::org.Hs.eg.db, ont = "MF")
  ids <- utils::head(as.data.frame(ego)$ID, 40)
  testthat::skip_if(length(ids) < 5)
  fast_sim <- GOSemSim::mgoSim(ids, ids, semData = GOSemSim::godata(ont = "MF"), measure = "Wang", combine = NULL)
  fast <- clusterProfiler::simplify(ego, cutoff = 0.5)
  expect_true(gocache_patched("GOSemSim", "wangMethod"))
  reset_go_cache()
  expect_false(gocache_patched("GOSemSim", "wangMethod"))
  expect_identical(GOSemSim::mgoSim(ids, ids, semData = GOSemSim::godata(ont = "MF"), measure = "Wang", combine = NULL), fast_sim)
  expect_identical(clusterProfiler::simplify(ego, cutoff = 0.5)@result, fast@result)
})

test_that("KEGG tables are cached per release and reused offline", {
  testthat::skip_if_not_installed("clusterProfiler")
  testthat::skip_if_not_installed("GO.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.go_cache_dir = dir)
  saved <- as.list(gocache)[c("original", "kegg_release", "kegg_release_time")]
  on.exit({
    for (name in names(saved)) gocache[[name]] <- saved[[name]]
    gocache$memory_result <- list()
    options(old)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)
  calls <- 0L
  gocache$original[["clusterProfiler::kegg_rest"]] <- function(rest_url) {
    calls <<- calls + 1L
    data.frame(from = rest_url, to = as.character(calls))
  }
  release <- function(value) {
    gocache$kegg_release <- value
    gocache$kegg_release_time <- Sys.time()
  }
  url <- "https://rest.kegg.jp/list/pathway/hsa"
  release("pathway 588 2026/11/09")
  first <- gocache_kegg_rest(url)
  gocache$memory_result <- list()
  expect_identical(gocache_kegg_rest(url), first)
  expect_identical(calls, 1L)
  release("pathway 590 2026/12/01")
  expect_identical(gocache_kegg_rest(url)$to, "2")
  release(NA_character_)
  expect_message(offline <- gocache_kegg_rest(url), "unreachable")
  expect_identical(offline$to, "2")
  expect_identical(calls, 2L)
  expect_identical(clear_go_cache(), 1L)
})

test_that("cached GO data stays separate per ontology across calls", {
  testthat::skip_if_not_installed("clusterProfiler")
  testthat::skip_if_not_installed("org.Hs.eg.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.go_cache_dir = dir)
  on.exit({ options(old); unlink(dir, recursive = TRUE) }, add = TRUE)
  use_go_cache()
  get_go_data <- get("get_GO_data", envir = asNamespace("clusterProfiler"))
  db <- org.Hs.eg.db::org.Hs.eg.db
  bp <- get_go_data(db, "BP", "ENTREZID")
  bp_terms <- names(get("PATHID2EXTID", envir = bp))
  mf <- get_go_data(db, "MF", "ENTREZID")
  bp_again <- get_go_data(db, "BP", "ENTREZID")
  expect_identical(names(get("PATHID2EXTID", envir = bp_again)), bp_terms)
  expect_false(identical(names(get("PATHID2EXTID", envir = mf)), bp_terms))
})
