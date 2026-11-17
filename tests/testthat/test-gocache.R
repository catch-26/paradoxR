test_that("the cache stores, reloads from disk and clears", {
  cache_dir <- tempfile("gocache")
  old <- options(paradoxR.cache_dir = cache_dir)
  on.exit({ options(old); unlink(cache_dir, recursive = TRUE) }, add = TRUE)
  gocache_set("go-result-a", list(x = 1))
  expect_identical(gocache_get("go-result-a"), list(x = 1))
  gocache$memory_result <- list()
  expect_identical(gocache_get("go-result-a"), list(x = 1))
  expect_null(gocache_get("go-result-b"))
  expect_identical(clear_cache(), 1L)
  expect_null(gocache_get("go-result-a"))
})

test_that("the in-memory store keeps the most recently used GO data", {
  old <- options(paradoxR.cache_dir = tempfile("gocache"))
  on.exit({ gocache$memory_data <- list(); options(old) }, add = TRUE)
  gocache$memory_data <- list()
  for (k in paste0("go-data-", 1:4)) gocache_remember(k, k)
  expect_identical(names(gocache$memory_data), paste0("go-data-", 2:4))
})

test_that("enrichGO() returns the same result with and without the cache", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("org.Hs.eg.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.cache_dir = dir)
  on.exit({ use_speedups(); options(old); unlink(dir, recursive = TRUE) }, add = TRUE)
  genes <- AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype = "ENTREZID")[1:300]
  run <- function() {
    clusterProfiler::enrichGO(genes, org.Hs.eg.db::org.Hs.eg.db, ont = "MF", pvalueCutoff = 1, qvalueCutoff = 1)
  }
  use_speedups()
  expect_true(gocache_patched())
  cached <- run()
  expect_true(length(list.files(dir, pattern = "^go-data-")) == 1L)
  expect_true(length(list.files(dir, pattern = "^go-result-")) == 1L)
  expect_identical(run()@result, cached@result)
  gocache$memory_data <- list()
  gocache$memory_result <- list()
  expect_identical(run()@result, cached@result)
  expect_length(list.files(dir, pattern = "\\.tmp$"), 0L)
  reset_speedups()
  expect_false(gocache_patched())
  expect_identical(run()@result, cached@result)
})

test_that("simplify() and Wang similarity give the same result with and without the replacements", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("GOSemSim")
  skip_if_no_bioc("org.Hs.eg.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.cache_dir = dir)
  on.exit({ use_speedups(); options(old); unlink(dir, recursive = TRUE) }, add = TRUE)
  genes <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, "GO:0006955", "ENTREZID", "GOALL", multiVals = "list")[[1]][1:150]
  use_speedups()
  ego <- clusterProfiler::enrichGO(genes, org.Hs.eg.db::org.Hs.eg.db, ont = "MF")
  ids <- utils::head(as.data.frame(ego)$ID, 40)
  testthat::skip_if(length(ids) < 5)
  fast_sim <- GOSemSim::mgoSim(ids, ids, semData = GOSemSim::godata(ont = "MF"), measure = "Wang", combine = NULL)
  fast <- suppressWarnings(clusterProfiler::simplify(ego, cutoff = 0.5))
  expect_true(gocache_patched("GOSemSim", "wangMethod"))
  reset_speedups()
  expect_false(gocache_patched("GOSemSim", "wangMethod"))
  expect_identical(GOSemSim::mgoSim(ids, ids, semData = GOSemSim::godata(ont = "MF"), measure = "Wang", combine = NULL), fast_sim)
  expect_identical(suppressWarnings(clusterProfiler::simplify(ego, cutoff = 0.5))@result, fast@result)
})

test_that("KEGG tables are cached per release and reused offline", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("GO.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.cache_dir = dir)
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
  expect_identical(clear_cache(), 1L)
})

test_that("cached GO data stays separate per ontology across calls", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("org.Hs.eg.db")
  dir <- tempfile("gocache")
  old <- options(paradoxR.cache_dir = dir)
  on.exit({ options(old); unlink(dir, recursive = TRUE) }, add = TRUE)
  use_speedups()
  get_go_data <- get("get_GO_data", envir = asNamespace("clusterProfiler"))
  db <- org.Hs.eg.db::org.Hs.eg.db
  bp <- get_go_data(db, "BP", "ENTREZID")
  testthat::skip_if_not(is.environment(bp), "this clusterProfiler returns GO data as an object, not a shared environment")
  bp_terms <- names(get("PATHID2EXTID", envir = bp))
  mf <- get_go_data(db, "MF", "ENTREZID")
  bp_again <- get_go_data(db, "BP", "ENTREZID")
  expect_identical(names(get("PATHID2EXTID", envir = bp_again)), bp_terms)
  expect_false(identical(names(get("PATHID2EXTID", envir = mf)), bp_terms))
})

test_that("gocache_intersect_each() matches lapply(intersect) exactly", {
  set.seed(1)
  universe <- c(as.character(1:50), NA)
  sets <- list(
    a = c("1", "2", "2", "60", NA),
    b = character(),
    c = as.character(c(5, 3, 3, 1)),
    d = "99"
  )
  expect_identical(gocache_intersect_each(sets, universe), lapply(sets, intersect, universe))
  expect_identical(gocache_intersect_each(sets, character()), lapply(sets, intersect, character()))
  expect_identical(gocache_intersect_each(list(), universe), lapply(list(), intersect, universe))
  big <- replicate(200, sample(as.character(1:300), sample(0:40, 1), replace = TRUE), simplify = FALSE)
  names(big) <- paste0("t", seq_along(big))
  expect_identical(gocache_intersect_each(big, as.character(1:150)), lapply(big, intersect, as.character(1:150)))
})

test_that("a failing replacement falls back to the original function", {
  gocache$warned <- character()
  original <- function(x, y = 2) x + y
  broken <- function(x, y = 2) stop("boom")
  safe <- gocache_with_fallback(broken, original, "pkg:::f")
  expect_warning(expect_identical(safe(1), 3), "pkg:::f")
  expect_identical(safe(1, y = 5), 6)
  failing_original <- function(x) stop("upstream error")
  both <- gocache_with_fallback(broken, failing_original, "pkg:::g")
  expect_error(both(1), "upstream error")
  expect_false("pkg:::g" %in% gocache$warned)
})

test_that("replacements left by an earlier paradoxR load can be reverted and re-applied", {
  skip_if_no_bioc("clusterProfiler")
  loadNamespace("clusterProfiler")
  on.exit(use_speedups(), add = TRUE)
  reset_speedups()
  stock <- get("enrichGO", envir = asNamespace("clusterProfiler"))
  use_speedups()
  expect_true(gocache_patched())
  gocache$original <- NULL
  expect_true(reset_speedups())
  expect_identical(get("enrichGO", envir = asNamespace("clusterProfiler")), stock)
  use_speedups()
  gocache$original <- NULL
  use_speedups()
  expect_true(gocache_patched())
  expect_identical(gocache_original("clusterProfiler", "enrichGO"), stock)
})

test_that("unhooking removes only paradoxR's load hooks", {
  event <- packageEvent("paradoxRhooktest", "onLoad")
  other <- function(...) NULL
  setHook(event, other)
  setHook(event, gocache_on_load)
  old <- gocache$hooked
  on.exit({ gocache$hooked <- old; setHook(event, NULL, action = "replace") }, add = TRUE)
  gocache$hooked <- "paradoxRhooktest"
  gocache_unhook()
  expect_identical(getHook(event), list(other))
})

test_that("fingerprints follow the function code, not its source text", {
  f <- function(x, y = 2) {
    x + y
  }
  g <- eval(parse(text = "function(x, y = 2) {\n\n  x +   y\n}", keep.source = TRUE)[[1]])
  h <- function(x, y = 3) {
    x + y
  }
  expect_identical(compat_fingerprint(f), compat_fingerprint(g))
  expect_false(identical(compat_fingerprint(f), compat_fingerprint(h)))
  expect_false(compat_port_applies("no-such-port", f))
  expect_false(compat_port_applies("bioc322-wang", NULL))
})

testthat::test_that("the offspring index matches a per-term lookup", {
  off <- list(a = c("b", "c", "z"), b = "c", c = NA_character_, d = c("a", "a"))
  goids <- c("a", "b", "c", "d", "e")
  pos <- stats::setNames(seq_along(goids), goids)
  expected <- lapply(goids, function(id) {
    ids <- off[[id]]
    if (is.null(ids)) integer(0) else as.integer(stats::na.omit(pos[ids]))
  })
  names(expected) <- goids
  testthat::expect_identical(gocache323_offspring_index(off, goids), expected)
})
