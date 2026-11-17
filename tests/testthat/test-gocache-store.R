local_cache_dir <- function(dir = tempfile("gocache"), env = parent.frame()) {
  old <- options(paradoxR.cache_dir = dir)
  saved <- list(data = gocache$memory_data, result = gocache$memory_result)
  restore <- function() {
    options(old)
    gocache$memory_data <- saved$data
    gocache$memory_result <- saved$result
    unlink(dir, recursive = TRUE)
  }
  do.call(on.exit, list(as.call(list(restore)), add = TRUE), envir = env)
  gocache$memory_data <- list()
  gocache$memory_result <- list()
  dir
}

test_that("the default cache directory is the platform user cache directory", {
  old <- options(paradoxR.cache_dir = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(gocache_dir(), tools::R_user_dir("paradoxR", which = "cache"))
  cache_root <- tempfile("usercache")
  old_env <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_root)
  on.exit(if (is.na(old_env)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old_env), add = TRUE)
  expect_identical(normalizePath(dirname(gocache_dir()), mustWork = FALSE), normalizePath(file.path(cache_root, "R"), mustWork = FALSE))
})

test_that("cache keys are portable file names", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("GO.db")
  key <- gocache_key("go-data", list("org.Hs.eg.sqlite", 1, 2), "BP", "ENTREZID")
  expect_match(key, "^go-data-[0-9a-f]+$")
  expect_identical(basename(gocache_path(key)), paste0(key, ".rds"))
  expect_identical(tolower(key), key)
})

test_that("the cache works in a directory with spaces and non-ASCII characters", {
  testthat::skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "session charset is not UTF-8")
  dir <- local_cache_dir(file.path(tempfile("gocache"), "café cache", "R paradox"))
  gocache_set("go-result-a", list(x = "été"))
  gocache$memory_result <- list()
  expect_identical(gocache_get("go-result-a"), list(x = "été"))
  expect_true(file.exists(gocache_path("go-result-a")))
})

test_that("writing a key again replaces the file on disk", {
  local_cache_dir()
  gocache_set("go-result-a", 1)
  gocache_set("go-result-a", 2)
  gocache$memory_result <- list()
  expect_identical(gocache_get("go-result-a"), 2)
  expect_length(list.files(gocache_dir(), pattern = "\\.tmp$"), 0L)
})

test_that("writing a key that another connection holds open neither warns nor fails", {
  local_cache_dir()
  gocache_set("go-result-a", 1)
  con <- file(gocache_path("go-result-a"), open = "rb")
  expect_silent(gocache_set("go-result-a", 2))
  close(con)
  expect_identical(gocache_get("go-result-a"), 2)
  expect_length(list.files(gocache_dir(), pattern = "\\.tmp$"), 0L)
})

test_that("a damaged cache file is dropped and rebuilt", {
  local_cache_dir()
  gocache_set("go-result-a", list(x = 1))
  writeBin(as.raw(1:20), gocache_path("go-result-a"))
  gocache$memory_result <- list()
  expect_silent(value <- gocache_get("go-result-a"))
  expect_null(value)
  expect_false(file.exists(gocache_path("go-result-a")))
  gocache_set("go-result-a", list(x = 1))
  gocache$memory_result <- list()
  expect_identical(gocache_get("go-result-a"), list(x = 1))
})

test_that("clear_cache() also removes unfinished writes", {
  local_cache_dir()
  gocache_set("go-result-a", 1)
  writeBin(as.raw(1:5), file.path(gocache_dir(), "go-result-b-1a2b.tmp"))
  writeBin(as.raw(1:5), file.path(gocache_dir(), "other.rds"))
  expect_identical(clear_cache(), 1L)
  expect_identical(list.files(gocache_dir()), "other.rds")
})

test_that("a cache directory that cannot be created leaves the result in memory", {
  blocker <- tempfile("gocache")
  writeLines("not a directory", blocker)
  on.exit(unlink(blocker), add = TRUE)
  local_cache_dir(file.path(blocker, "sub"))
  expect_silent(gocache_set("go-result-a", 1))
  expect_identical(gocache_get("go-result-a"), 1)
  gocache$memory_result <- list()
  expect_silent(value <- gocache_get("go-result-a"))
  expect_null(value)
})

test_that("a cache directory that the session cannot encode leaves the result in memory", {
  testthat::skip_if(isTRUE(l10n_info()[["UTF-8"]]), "session charset is UTF-8")
  local_cache_dir(file.path(tempfile("gocache"), "caf\u00e9"))
  expect_silent(gocache_set("go-result-a", 1))
  expect_identical(gocache_get("go-result-a"), 1)
  gocache$memory_result <- list()
  expect_silent(value <- gocache_get("go-result-a"))
  expect_null(value)
})
