# Runs `code` with every patch removed, so that nested calls (dba.count()
# calling summarizeOverlaps(), say) run unpatched too.
stock <- function(pkg, code, env = parent.frame()) {
  enabled <- union(pkg, setdiff(speedups_all_packages(), gocache$disabled))
  reset_speedups()
  on.exit(use_speedups(enabled), add = TRUE)
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
  dba$config$RunParallel <- FALSE
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

seurat_scaled <- function() {
  object <- suppressMessages(suppressWarnings(SeuratObject::UpdateSeuratObject(SeuratObject::pbmc_small)))
  object <- Seurat::FindVariableFeatures(object, verbose = FALSE)
  Seurat::ScaleData(object, verbose = FALSE)
}

expect_cached <- function(pkg, code, part, env = parent.frame()) {
  force(env)
  code <- substitute(code)
  expected <- stock(pkg, eval(code, env))
  before <- length(stored_results())
  first <- eval(code, env)
  stored <- length(stored_results())
  second <- eval(code, env)
  expect_gt(stored, before)
  expect_identical(length(stored_results()), stored)
  expect_identical(part(first), part(expected))
  expect_identical(part(second), part(expected))
  invisible(first)
}

test_that("Seurat dimensional reduction, neighbours and SCTransform return the original results", {
  skip_if_no_bioc("Seurat")
  local_result_cache()
  loadNamespace("Seurat")
  use_speedups("Seurat")
  for (name in c("RunPCA.default", "RunUMAP.default", "FindNeighbors.default", "SCTransform.default")) {
    expect_true(gocache_patched("Seurat", name))
  }
  object <- seurat_scaled()
  set.seed(1)
  pca <- expect_cached("Seurat", suppressWarnings(Seurat::RunPCA(object, npcs = 10, verbose = FALSE)), function(x) x[["pca"]])
  seed <- .Random.seed
  set.seed(2)
  suppressWarnings(Seurat::RunPCA(object, npcs = 10, verbose = FALSE))
  expect_identical(.Random.seed, seed)
  expect_cached("Seurat", Seurat::FindNeighbors(pca, dims = 1:10, verbose = FALSE), function(x) x[["RNA_snn"]])
  expect_cached("Seurat", suppressWarnings(Seurat::RunUMAP(pca, dims = 1:10, verbose = FALSE)), function(x) x[["umap"]])
  expect_cached("Seurat", suppressWarnings(Seurat::SCTransform(object, verbose = FALSE)), function(x) x[["SCT"]])
})

test_that("Seurat RunPCA() reruns when the scaled data change but not for unrelated changes", {
  skip_if_no_bioc("Seurat")
  local_result_cache()
  loadNamespace("Seurat")
  use_speedups("Seurat")
  object <- seurat_scaled()
  pca <- function(x) suppressWarnings(Seurat::RunPCA(x, npcs = 10, verbose = FALSE))
  pca(object)
  unrelated <- object
  unrelated$note <- "x"
  pca(unrelated)
  expect_length(stored_results(), 1L)
  changed <- object
  scaled <- SeuratObject::LayerData(changed, layer = "scale.data")
  scaled[1, ] <- scaled[1, ] + 1
  SeuratObject::LayerData(changed, layer = "scale.data") <- scaled
  expected <- stock("Seurat", pca(changed))
  expect_identical(pca(changed)[["pca"]], expected[["pca"]])
  expect_length(stored_results(), 2L)
})

test_that("Seurat IntegrateLayers() returns the original integrated reduction", {
  skip_if_no_bioc("Seurat")
  local_result_cache()
  loadNamespace("Seurat")
  use_speedups("Seurat")
  expect_true(gocache_patched("Seurat", "CCAIntegration"))
  object <- seurat_scaled()
  object$batch <- rep(c("a", "b"), length.out = ncol(object))
  suppressWarnings(object[["RNA"]] <- split(object[["RNA"]], f = object$batch))
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object <- Seurat::FindVariableFeatures(object, verbose = FALSE)
  object <- Seurat::ScaleData(object, verbose = FALSE)
  object <- suppressWarnings(Seurat::RunPCA(object, npcs = 10, verbose = FALSE))
  expect_cached(
    "Seurat",
    Seurat::IntegrateLayers(object, method = Seurat::CCAIntegration, k.weight = 20, dims = 1:10, verbose = FALSE),
    function(x) x[["integrated.dr"]]
  )
})

test_that("csaw and summarizeOverlaps() counts match the original and see changed BAM files", {
  skip_if_no_bioc("csaw")
  skip_if_no_bioc("GenomicAlignments")
  local_result_cache()
  loadNamespace("csaw")
  loadNamespace("GenomicAlignments")
  use_speedups(c("csaw", "GenomicAlignments"))
  expect_true(gocache_patched("csaw", "windowCounts"))
  expect_true(gocache_patched("GenomicAlignments", ".dispatchBamFiles"))
  dir <- withr::local_tempdir()
  bams <- make_dba_files(dir)$bamReads
  regions <- GenomicRanges::GRanges("chr1", IRanges::IRanges(c(1700, 5700, 9700), width = 600))
  pkgs <- c("csaw", "GenomicAlignments")
  windows <- function() csaw::windowCounts(bams, width = 200, filter = 5)
  expected <- expect_cached(pkgs, windows(), identity)
  expect_cached(pkgs, csaw::regionCounts(bams, regions), identity)
  set.seed(1)
  expect_cached(pkgs, GenomicAlignments::summarizeOverlaps(regions, Rsamtools::BamFileList(bams), mode = "Union"), identity)
  expect_cached(pkgs, GenomicAlignments::summarizeOverlaps(regions, bams, mode = GenomicAlignments::IntersectionStrict), identity)
  Sys.sleep(2.1)
  make_dba_files(dir, seed = 2L)
  changed <- stock(pkgs, windows())
  expect_false(identical(changed, expected))
  expect_identical(windows(), changed)
})

test_that("ChIPseeker getTagMatrix() and EnrichedHeatmap return the original results", {
  skip_if_no_bioc("ChIPseeker")
  skip_if_no_bioc("EnrichedHeatmap")
  skip_if_no_bioc("TxDb.Hsapiens.UCSC.hg19.knownGene")
  local_result_cache()
  loadNamespace("ChIPseeker")
  loadNamespace("EnrichedHeatmap")
  pkgs <- c("ChIPseeker", "EnrichedHeatmap")
  use_speedups(pkgs)
  txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
  peaks <- GenomicRanges::GRanges("chr1", IRanges::IRanges(seq(1e6, 5e7, length.out = 300), width = 500))
  expect_false(gocache_patched("ChIPseeker", "annotatePeak"))
  promoters <- suppressMessages(ChIPseeker::getPromoters(TxDb = txdb, upstream = 1000, downstream = 1000))
  expect_cached(pkgs, ChIPseeker::getTagMatrix(peaks, windows = promoters, upstream = 1000, downstream = 1000, verbose = FALSE), identity)
  withr::local_seed(1)
  signal <- GenomicRanges::GRanges("chr1", IRanges::IRanges(seq(1e6, 5e7, length.out = 2000), width = 300), score = stats::runif(2000))
  target <- GenomicRanges::GRanges("chr1", IRanges::IRanges(seq(1e6, 5e7, length.out = 300), width = 1))
  matrix <- expect_cached(pkgs, EnrichedHeatmap::normalizeToMatrix(signal, target, value_column = "score", verbose = FALSE), identity)
  expect_identical(attr(matrix, "signal_name"), "signal")
})

test_that("Signac::FeatureMatrix() returns the original counts", {
  skip_if_no_bioc("Signac")
  skip_if_no_bioc("Rsamtools")
  local_result_cache()
  loadNamespace("Signac")
  use_speedups("Signac")
  dir <- withr::local_tempdir()
  withr::local_seed(5)
  start <- sort(sample(1e6:2e6, 5000))
  cells <- paste0("cell", 1:50)
  path <- file.path(dir, "fragments.tsv")
  utils::write.table(
    data.frame("chr1", start, start + 200L, sample(cells, 5000, TRUE), 1L),
    path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE
  )
  Rsamtools::bgzip(path, paste0(path, ".gz"))
  Rsamtools::indexTabix(paste0(path, ".gz"), format = "bed")
  fragments <- Signac::CreateFragmentObject(paste0(path, ".gz"), cells = stats::setNames(cells, cells), verbose = FALSE)
  features <- GenomicRanges::GRanges("chr1", IRanges::IRanges(seq(1e6, 2e6, length.out = 40), width = 5000))
  expect_cached("Signac", Signac::FeatureMatrix(fragments, features, verbose = FALSE), identity)
})

test_that("GSVA::gsva() on a dense matrix returns the original scores", {
  skip_if_no_bioc("GSVA")
  local_result_cache()
  loadNamespace("GSVA")
  use_speedups("GSVA")
  expect_true(gocache_patched("GSVA", "gsva,gsvaParam"))
  withr::local_seed(1)
  x <- matrix(stats::rnorm(20000), 2000, dimnames = list(paste0("g", 1:2000), paste0("s", 1:10)))
  sets <- list(a = paste0("g", 1:20), b = paste0("g", 30:80))
  expect_cached("GSVA", GSVA::gsva(GSVA::gsvaParam(x, sets), verbose = FALSE), identity)
  reset_speedups("GSVA")
  expect_false(gocache_patched("GSVA", "gsva,gsvaParam"))
})

test_that("sva::ComBat_seq() returns the original counts and repeats its printed output", {
  skip_if_no_bioc("sva")
  local_result_cache()
  loadNamespace("sva")
  use_speedups("sva")
  withr::local_seed(1)
  counts <- matrix(stats::rpois(6000, 20), 500)
  combat <- function() sva::ComBat_seq(counts, batch = rep(1:2, 6))
  expected <- stock("sva", utils::capture.output(value <- combat()))
  expect_cached("sva", combat(), identity)
  expect_identical(utils::capture.output(value <- combat()), expected)
})

test_that("DESeq2::lfcShrink() returns the original estimates across DESeq() runs", {
  skip_if_no_bioc("DESeq2")
  local_result_cache()
  loadNamespace("DESeq2")
  use_speedups("DESeq2")
  withr::local_seed(1)
  data <- DESeq2::makeExampleDESeqDataSet(n = 500, m = 6)
  dds <- DESeq2::DESeq(data, quiet = TRUE)
  shrink <- function(object) suppressMessages(DESeq2::lfcShrink(object, coef = 2, type = "normal"))
  expect_cached("DESeq2", shrink(dds), identity)
  again <- DESeq2::DESeq(data, quiet = TRUE)
  expect_identical(shrink(again), stock("DESeq2", shrink(again)))
  expect_length(stored_results(), 1L)
})

test_that("stats::hclust() keeps the caller's call and skips small inputs", {
  local_result_cache()
  use_speedups("stats")
  expect_true(gocache_patched("stats", "hclust"))
  withr::local_seed(1)
  distances <- stats::dist(matrix(stats::rnorm(12000), 6000))
  first <- expect_cached("stats", stats::hclust(distances, method = "average"), identity)
  expect_identical(first$call, quote(stats::hclust(d = distances, method = "average")))
  small <- stats::dist(matrix(stats::rnorm(200), 100))
  stats::hclust(small)
  expect_length(stored_results(), 1L)
})

test_that("MASS::kde2d() returns the original density and skips small inputs", {
  skip_if_not_installed("MASS")
  local_result_cache()
  loadNamespace("MASS")
  use_speedups("MASS")
  withr::local_seed(1)
  x <- stats::rnorm(4e5)
  y <- stats::rnorm(4e5)
  expect_cached("MASS", MASS::kde2d(x, y, n = 25), identity)
  MASS::kde2d(x[1:1000], y[1:1000])
  expect_length(stored_results(), 1L)
})
