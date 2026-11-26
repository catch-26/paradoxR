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
  path <- withr::local_tempfile()
  writeLines("first", path)
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

test_that("a function with its own seed argument ignores the incoming random state", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(n, seed = 42) {
    runs <<- runs + 1
    set.seed(seed)
    stats::runif(n)
  }, spec = list(seed = "seed"))
  set.seed(1)
  a <- toy(3)
  after <- .Random.seed
  set.seed(2)
  expect_identical(toy(3), a)
  expect_identical(.Random.seed, after)
  expect_identical(runs, 1)
  expect_length(stored_results(), 1L)
  toy(3, seed = 7)
  expect_identical(runs, 2)
})

test_that("a seed argument set to NULL keys by the incoming random state", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(n, seed = NULL) {
    runs <<- runs + 1
    if (!is.null(seed)) set.seed(seed)
    stats::runif(n)
  }, spec = list(seed = "seed"))
  set.seed(1)
  a <- toy(3)
  set.seed(2)
  expect_false(identical(toy(3), a))
  set.seed(1)
  expect_identical(toy(3), a)
  expect_identical(runs, 2)
})

test_that("package functions as arguments are keyed by code, package and version", {
  stable <- resultcache_stable(list(f = stats::median, g = base::sum))
  expect_identical(stable$f$package, "stats")
  expect_identical(stable$f$version, as.character(utils::packageVersion("stats")))
  expect_identical(stable$g, list(primitive = deparse(base::sum)))
  has_env <- function(x) is.environment(x) || (is.list(x) && any(vapply(x, has_env, logical(1))))
  expect_false(has_env(stable))
})

test_that("a call with a function from outside a package is not stored", {
  local_result_cache()
  runs <- 0
  toy <- local_cached_function(function(x, f) {
    runs <<- runs + 1
    f(x)
  })
  toy(1:3, function(v) sum(v))
  toy(1:3, function(v) sum(v))
  expect_identical(runs, 2)
  expect_length(stored_results(), 0L)
  toy(1:3, base::sum)
  toy(1:3, base::sum)
  expect_identical(runs, 3)
})

test_that("BAM file handles and parallel back ends are keyed by their settings", {
  skip_if_no_bioc("Rsamtools")
  skip_if_not_installed("BiocParallel")
  path <- system.file("extdata", "ex1.bam", package = "Rsamtools")
  a <- resultcache_stable(list(Rsamtools::BamFile(path), BiocParallel::SerialParam()))
  b <- resultcache_stable(list(Rsamtools::BamFile(path), BiocParallel::SerialParam()))
  expect_identical(rlang::hash(a), rlang::hash(b))
  c <- resultcache_stable(list(Rsamtools::BamFile(path, yieldSize = 10), BiocParallel::SerialParam()))
  expect_false(identical(rlang::hash(a), rlang::hash(c)))
  list_a <- resultcache_stable(Rsamtools::BamFileList(path))
  expect_identical(rlang::hash(list_a), rlang::hash(resultcache_stable(Rsamtools::BamFileList(path))))
})

test_that("restore puts back what the original takes from the caller's expressions", {
  local_result_cache()
  toy <- local_cached_function(function(x) {
    structure(x * 2, name = deparse(substitute(x)))
  }, spec = list(restore = function(value, frame, call, original) {
    attr(value, "name") <- deparse(eval(quote(substitute(x)), frame))
    value
  }))
  input <- 1:3
  expect_identical(attr(toy(input), "name"), "input")
  expect_identical(attr(toy(input), "name"), "input")
  expect_length(stored_results(), 1L)
})

test_that("calls made while a stored function runs are not stored on their own", {
  local_result_cache()
  inner_runs <- 0
  inner <- local_cached_function(function(x) {
    inner_runs <<- inner_runs + 1
    x + 1
  })
  inner(1)
  expect_length(stored_results(), 1L)
  gocache$resultcache_running <- TRUE
  withr::defer(gocache$resultcache_running <- FALSE)
  expect_identical(inner(5), 6)
  expect_length(stored_results(), 1L)
})

test_that("printed output and conditions are replayed in their original order", {
  local_result_cache()
  toy <- local_cached_function(function(x) {
    cat("first ")
    message("between")
    cat("line\n")
    print(x)
    warning("careful")
    cat("no newline")
    x
  })
  record <- function() {
    events <- character()
    out <- utils::capture.output(withCallingHandlers(
      toy(1:3),
      message = function(m) {
        events <<- c(events, "message")
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        events <<- c(events, "warning")
        invokeRestart("muffleWarning")
      }
    ))
    list(out = out, events = events)
  }
  first <- record()
  second <- record()
  expect_length(stored_results(), 1L)
  expect_identical(second, first)
  expect_identical(first$out, c("first line", "[1] 1 2 3", "no newline[1] 1 2 3"))
})

test_that("an invisible result stays invisible on a hit", {
  local_result_cache()
  toy <- local_cached_function(function(x) invisible(x * 2))
  expect_false(withVisible(toy(2))$visible)
  expect_false(withVisible(toy(2))$visible)
  expect_length(stored_results(), 1L)
  shown <- local_cached_function(function(x) x * 3)
  expect_true(withVisible(shown(2))$visible)
  expect_true(withVisible(shown(2))$visible)
})

test_that("packages loaded during the original call are loaded again on a hit", {
  local_result_cache()
  candidates <- setdiff(c("colorspace", "DBI", "S4Vectors"), loadedNamespaces())
  candidates <- candidates[nzchar(vapply(candidates, function(p) system.file(package = p), character(1)))]
  skip_if(length(candidates) == 0L, "no unloaded test package available")
  pkg <- candidates[[1L]]
  toy <- local_cached_function(function(x) {
    loadNamespace(pkg)
    x
  })
  toy(1)
  key <- sub("[.]qs2$", "",stored_results())
  expect_true(pkg %in% resultcache_read(key)$namespaces)
  unloadNamespace(pkg)
  expect_false(pkg %in% loadedNamespaces())
  toy(1)
  expect_true(pkg %in% loadedNamespaces())
})

test_that("text written straight to the console is replayed in order", {
  local_result_cache()
  toy <- local_cached_function(function(x) {
    cat("progress 50%\n", file = stderr())
    message("note")
    cat("progress 100%\n", file = stderr())
    x
  })
  run <- function() {
    file <- withr::local_tempfile()
    con <- file(file, open = "w")
    sink(con, type = "message")
    toy(1)
    sink(type = "message")
    close(con)
    readLines(file)
  }
  first <- run()
  second <- run()
  expect_length(stored_results(), 1L)
  expect_identical(first, c("progress 50%", "note", "progress 100%"))
  expect_identical(second, first)
})

test_that("an immediately printed warning is recognised and printed the same way again", {
  event <- list(type = "warning", message = "now")
  expect_identical(resultcache_printed(event, "Warning: now\nmore"), 13L)
  expect_identical(resultcache_printed(event, "Warning in f(x) :\n  now\n"), 24L)
  expect_identical(resultcache_printed(event, "progress"), 0L)
  expect_identical(resultcache_printed(list(type = "message", message = "hi\n"), "hi\nrest"), 3L)
})

test_that("package state a stock call creates is created again on a hit", {
  local_result_cache()
  made <- FALSE
  local_mocked_bindings(resultcache_states = function() list(toy = list(
    exists = function() made,
    create = function() made <<- TRUE
  )))
  toy <- local_cached_function(function(x) {
    made <<- TRUE
    x
  })
  toy(1)
  made <- FALSE
  toy(1)
  expect_true(made)
  expect_length(stored_results(), 1L)
})

test_that("a hit leaves package state alone that the stock call did not create", {
  local_result_cache()
  made <- FALSE
  local_mocked_bindings(resultcache_states = function() list(toy = list(
    exists = function() made,
    create = function() made <<- TRUE
  )))
  toy <- local_cached_function(function(x) x)
  toy(1)
  toy(1)
  expect_false(made)
})
