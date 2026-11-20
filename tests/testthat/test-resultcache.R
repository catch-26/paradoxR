test_that("a repeated call returns the stored result without running again", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x, y = 2) {
    runs <<- runs + 1
    x * y
  })
  expect_identical(toy(1:3), c(2, 4, 6))
  expect_identical(toy(1:3), c(2, 4, 6))
  expect_identical(runs, 1)
  expect_length(stored_results(), 1L)
})

test_that("any change in an argument runs the function again", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x, y = 2) {
    runs <<- runs + 1
    x * y
  })
  toy(1:3)
  expect_identical(toy(c(1, 2, 3)), c(2, 4, 6))
  expect_identical(toy(1:3, y = 3), c(3, 6, 9))
  expect_identical(toy(1:4), c(2, 4, 6, 8))
  expect_identical(runs, 4)
})

test_that("arguments in ... are part of the key", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x, ...) {
    runs <<- runs + 1
    sum(x, ...)
  })
  expect_identical(toy(1, 2), 3)
  expect_identical(toy(1, 3), 4)
  expect_identical(toy(1, 2), 3)
  expect_identical(runs, 2)
})

test_that("missing() inside the original still works", {
  local_result_cache()
  toy <- local_cached_function(function(x, peaks) if (missing(peaks)) "consensus" else peaks)
  expect_identical(toy(1), "consensus")
  expect_identical(toy(1, "given"), "given")
  expect_identical(toy(1), "consensus")
})

test_that("arguments are evaluated once", {
  local_result_cache()
  toy <- local_cached_function(function(x) x)
  n <- 0
  arg <- function() {
    n <<- n + 1
    n
  }
  toy(arg())
  expect_identical(n, 1)
})

test_that("messages, warnings and option changes are replayed on a hit", {
  local_result_cache()
  withr::local_options(paradoxRtoy.seen = NULL)
  toy <- local_cached_function(function(x) {
    message("step one")
    warning("careful")
    options(paradoxRtoy.seen = TRUE)
    x
  })
  expect_message(expect_warning(toy(1), "careful"), "step one")
  options(paradoxRtoy.seen = NULL)
  expect_message(expect_warning(value <- toy(1), "careful"), "step one")
  expect_identical(value, 1)
  expect_true(getOption("paradoxRtoy.seen"))
})

test_that("functions that draw random numbers are keyed by the random state", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(n) {
    runs <<- runs + 1
    stats::runif(n)
  })
  set.seed(1)
  a <- toy(3)
  after_a <- .Random.seed
  set.seed(2)
  b <- toy(3)
  expect_false(identical(a, b))
  set.seed(1)
  expect_identical(toy(3), a)
  expect_identical(.Random.seed, after_a)
  expect_identical(runs, 2)
})

test_that("functions that do not draw random numbers ignore the random state", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x) {
    runs <<- runs + 1
    x
  })
  set.seed(1)
  toy(1)
  set.seed(2)
  toy(1)
  expect_identical(runs, 1)
})

test_that("errors are not stored", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x) {
    runs <<- runs + 1
    stop("bad input")
  })
  expect_error(toy(1), "bad input")
  expect_error(toy(1), "bad input")
  expect_identical(runs, 2)
  expect_length(stored_results(), 0L)
})

test_that("fast calls are not stored", {
  local_result_cache()
  withr::local_options(paradoxR.result_cache_min_seconds = 60)
  toy <- local_cached_function(function(x) x)
  toy(1)
  expect_length(stored_results(), 0L)
})

test_that("the cache can be turned off with options", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x) {
    runs <<- runs + 1
    x
  })
  withr::with_options(list(paradoxR.result_cache = FALSE), {
    toy(1)
    toy(1)
  })
  withr::with_options(list(paradoxR.speedups = FALSE), toy(1))
  expect_identical(runs, 3)
  expect_length(stored_results(), 0L)
})

test_that("nothing is stored on a network file system", {
  local_result_cache()
  testthat::local_mocked_bindings(cache_fs = function(dir = gocache_dir()) list(type = "nfs4", total = 1e15, free = 1e15))
  toy <- local_cached_function(function(x) x)
  toy(1)
  expect_length(stored_results(), 0L)
})

test_that("a change in an input file's content runs the function again", {
  local_result_cache()
  path <- withr::local_tempfile(lines = "first")
  runs <- 0
  toy <- local_cached_function(
    function(file) {
      runs <<- runs + 1
      readLines(file)
    },
    list(files = function(args) args$file)
  )
  expect_identical(toy(path), "first")
  expect_identical(toy(path), "first")
  writeLines("second", path)
  expect_identical(toy(path), "second")
  writeLines("first", path)
  expect_identical(toy(path), "first")
  expect_identical(runs, 2)
})

test_that("file fingerprints are reused while size and modification time match", {
  local_result_cache()
  path <- withr::local_tempfile(lines = "content")
  Sys.setFileTime(path, Sys.time() - 60)
  first <- resultcache_file_id(path)
  expect_length(list.files(file.path(gocache_dir(), "files")), 1L)
  testthat::local_mocked_bindings(hash_file = function(path) stop("hashed again"), .package = "rlang")
  expect_identical(resultcache_file_id(path), first)
})

test_that("recently modified files are always hashed again", {
  local_result_cache()
  path <- withr::local_tempfile(lines = "content")
  resultcache_file_id(path)
  expect_length(list.files(file.path(gocache_dir(), "files")), 0L)
})

test_that("BAM index files are part of the inputs", {
  dir <- withr::local_tempdir()
  bam <- file.path(dir, "a.bam")
  file.create(bam, file.path(dir, "a.bam.bai"))
  expect_setequal(resultcache_bam_files(c(bam, NA, "")), c(bam, file.path(dir, "a.bam.bai")))
})

test_that("installed package versions and the platform are part of the key", {
  versions <- resultcache_versions("rlang")
  expect_true("rlang" %in% names(versions))
  expect_identical(versions[["rlang"]], as.character(utils::packageVersion("rlang")))
  expect_true(is.na(resultcache_versions("paradoxRnotapackage")[["paradoxRnotapackage"]]))
  expect_named(resultcache_platform(), c("version", "platform", "blas", "lapack", "lapack_version"))
})

test_that("a damaged stored result is dropped and computed again", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x) {
    runs <<- runs + 1
    x
  })
  toy(1)
  path <- file.path(gocache_dir(), "results", stored_results())
  writeBin(as.raw(1:20), path)
  expect_identical(toy(1), 1)
  expect_identical(runs, 2)
})

test_that("the cache is bypassed inside a targets pipeline", {
  testthat::skip_if_not_installed("targets")
  local_result_cache()
  testthat::local_mocked_bindings(tar_active = function() TRUE, .package = "targets")
  toy <- local_cached_function(function(x) x)
  toy(1)
  expect_length(stored_results(), 0L)
})
