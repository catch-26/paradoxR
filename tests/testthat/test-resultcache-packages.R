stock <- function(pkg, code, env = parent.frame()) {
  reset_speedups(pkg)
  on.exit(use_speedups(pkg), add = TRUE)
  eval(substitute(code), env)
}

test_that("Seurat::FindAllMarkers() returns the original result from the cache", {
  skip_if_no_bioc("Seurat")
  local_result_cache()
  loadNamespace("Seurat")
  use_speedups("Seurat")
  expect_true(gocache_patched("Seurat", "FindAllMarkers"))
  object <- SeuratObject::pbmc_small
  expected <- stock("Seurat", Seurat::FindAllMarkers(object, verbose = FALSE))
  first <- Seurat::FindAllMarkers(object, verbose = FALSE)
  expect_length(stored_results(), 1L)
  second <- Seurat::FindAllMarkers(object, verbose = FALSE)
  expect_identical(first, expected)
  expect_identical(second, expected)
  expect_length(stored_results(), 1L)
})

test_that("Seurat::FindAllMarkers() reruns only when a part it reads changes", {
  skip_if_no_bioc("Seurat")
  local_result_cache()
  loadNamespace("Seurat")
  use_speedups("Seurat")
  object <- SeuratObject::pbmc_small
  runs <- function() length(stored_results())
  Seurat::FindAllMarkers(object, verbose = FALSE)
  unrelated <- object
  unrelated$note <- "x"
  suppressWarnings(unrelated[["extra"]] <- unrelated[["pca"]])
  command <- unrelated@commands[[1]]
  command@time.stamp <- Sys.time()
  unrelated@commands[[1]] <- command
  Seurat::FindAllMarkers(unrelated, verbose = FALSE)
  expect_identical(runs(), 1L)
  check <- function(changed, ...) {
    expected <- stock("Seurat", Seurat::FindAllMarkers(changed, verbose = FALSE, ...))
    expect_identical(Seurat::FindAllMarkers(changed, verbose = FALSE, ...), expected)
  }
  idents <- object
  SeuratObject::Idents(idents)[1:5] <- "0"
  check(idents)
  expect_identical(runs(), 2L)
  data <- object
  layer <- SeuratObject::LayerData(data, layer = "data")
  layer[1, 1:10] <- layer[1, 1:10] + 1
  SeuratObject::LayerData(data, layer = "data") <- layer
  check(data)
  expect_identical(runs(), 3L)
  grouped <- object
  grouped$group <- paste0("g", SeuratObject::Idents(grouped))
  check(grouped, group.by = "group")
  grouped$group[1:10] <- "g1"
  check(grouped, group.by = "group")
  expect_identical(runs(), 5L)
  normalised <- object
  command <- normalised@commands[["NormalizeData.RNA"]]
  command@params$normalization.method <- "CLR"
  normalised@commands[["NormalizeData.RNA"]] <- command
  check(normalised)
  expect_identical(runs(), 6L)
})

test_that("DiffBind::dba.count() returns the original counts and sees changed BAM files", {
  skip_if_no_bioc("DiffBind")
  skip_if_no_bioc("Rsamtools")
  local_result_cache()
  loadNamespace("DiffBind")
  use_speedups("DiffBind")
  expect_true(gocache_patched("DiffBind", "dba.count"))
  dir <- withr::local_tempdir()
  samples <- make_dba_files(dir)
  dba <- suppressMessages(DiffBind::dba(sampleSheet = samples))
  dba$config$doBlacklist <- FALSE
  dba$config$doGreylist <- FALSE
  count <- function() suppressMessages(DiffBind::dba.count(dba))
  expected <- stock("DiffBind", count())
  expect_identical(count(), expected)
  expect_identical(count(), expected)
  expect_length(stored_results(), 1L)
  Sys.sleep(2.1)
  make_dba_files(dir, seed = 2L)
  changed <- stock("DiffBind", count())
  expect_false(identical(changed, expected))
  expect_identical(count(), changed)
  expect_length(stored_results(), 2L)
})
