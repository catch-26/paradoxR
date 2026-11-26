local_result_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("paradoxR-results", .local_envir = env)
  withr::local_options(
    paradoxR.cache_dir = dir,
    paradoxR.result_cache_min_seconds = 0,
    paradoxR.result_cache_min_ratio = 0,
    paradoxR.cache_min_free_pct = 0,
    paradoxR.cache_min_free_gb = 0,
    .local_envir = env
  )
  pinned <- gocache$pinned
  withr::defer(gocache$pinned <- pinned, envir = env)
  testthat::local_mocked_bindings(
    cache_fs = function(dir = gocache_dir()) list(type = "ext4", total = 1e15, free = 1e15),
    .env = env
  )
  dir
}

# A cached copy of `fun` registered under the spec `spec`, as the patch
# registry would install it.
local_cached_function <- function(fun, spec = list(), env = parent.frame()) {
  spec <- utils::modifyList(list(pkg = "paradoxRtoy", name = "toy", packages = character()), spec)
  testthat::local_mocked_bindings(
    resultcache_specs = function() list(toy = spec),
    .env = env
  )
  original <- gocache$original
  withr::defer(gocache$original <- original, envir = env)
  gocache$original[["paradoxRtoy::toy"]] <- fun
  wrapper <- resultcache_make("toy")(fun)
  attr(wrapper, "paradoxR_original") <- fun
  wrapper
}

stored_results <- function() {
  list.files(file.path(gocache_dir(), "results"))
}

# Four small single-end BAM files with five peaks on one chromosome.
make_dba_files <- function(dir, n_reads = 4000L, seed = 1L) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::local_seed(seed)
  centres <- c(2000L, 6000L, 10000L, 14000L, 18000L)
  samples <- data.frame(
    SampleID = paste0("s", 1:4), Tissue = "t", Factor = "f",
    Condition = rep(c("A", "B"), each = 2), Replicate = c(1, 2, 1, 2)
  )
  samples$bamReads <- file.path(dir, paste0(samples$SampleID, ".bam"))
  samples$Peaks <- file.path(dir, paste0(samples$SampleID, ".bed"))
  samples$PeakCaller <- "bed"
  for (i in seq_len(nrow(samples))) {
    pos <- sort(as.integer(sample(centres, n_reads, replace = TRUE) + stats::rnorm(n_reads, 0, 150 + 50 * i)))
    pos <- pmax(pos, 1L)
    sam <- tempfile(fileext = ".sam")
    writeLines(c(
      "@HD\tVN:1.6\tSO:coordinate", "@SQ\tSN:chr1\tLN:30000",
      sprintf("r%d\t%d\tchr1\t%d\t60\t50M\t*\t0\t0\t%s\t%s", seq_along(pos), ifelse(seq_along(pos) %% 2 == 0, 16L, 0L), pos,
              strrep("A", 50), strrep("I", 50))
    ), sam)
    Rsamtools::asBam(sam, sub("\\.bam$", "", samples$bamReads[i]), overwrite = TRUE, indexDestination = TRUE)
    peaks <- data.frame(chr = "chr1", start = centres - 300L, end = centres + 300L, name = paste0("p", seq_along(centres)), score = 100 - i)
    utils::write.table(peaks, samples$Peaks[i], sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  samples
}
