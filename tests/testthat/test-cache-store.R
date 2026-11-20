local_store <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("paradoxR-cache", .local_envir = env)
  withr::local_options(paradoxR.cache_dir = dir, .local_envir = env)
  pinned <- gocache$pinned
  withr::defer(gocache$pinned <- pinned, envir = env)
  gocache$pinned <- character()
  dir
}

age_file <- function(path, days) {
  Sys.setFileTime(path, Sys.time() - days * 86400)
}

test_that("files unused for longer than the expiry are deleted", {
  dir <- local_store()
  dir.create(file.path(dir, "results"))
  old <- file.path(dir, "results", "old.qs2")
  new <- file.path(dir, "go-result-new.rds")
  writeBin(as.raw(1:3), old)
  writeBin(as.raw(1:3), new)
  age_file(old, 8)
  age_file(new, 6)
  expect_identical(cache_sweep(), 1L)
  expect_false(file.exists(old))
  expect_true(file.exists(new))
})

test_that("the expiry follows the option", {
  dir <- local_store()
  path <- file.path(dir, "go-result-a.rds")
  writeBin(as.raw(1:3), path)
  age_file(path, 2)
  withr::local_options(paradoxR.cache_max_age_days = 1)
  cache_sweep()
  expect_false(file.exists(path))
})

test_that("files used in this session are never deleted", {
  dir <- local_store()
  path <- file.path(dir, "go-result-a.rds")
  writeBin(as.raw(1:3), path)
  age_file(path, 30)
  cache_pin(path)
  cache_sweep()
  expect_true(file.exists(path))
})

test_that("unfinished writes older than an hour are deleted", {
  dir <- local_store()
  stale <- file.path(dir, "go-result-a-1.tmp")
  fresh <- file.path(dir, "go-result-b-2.tmp")
  writeBin(as.raw(1:3), stale)
  writeBin(as.raw(1:3), fresh)
  age_file(stale, 1)
  cache_sweep()
  expect_false(file.exists(stale))
  expect_true(file.exists(fresh))
})

test_that("a cache hit refreshes the file's last use", {
  dir <- local_store()
  saved <- list(data = gocache$memory_data, result = gocache$memory_result)
  withr::defer({
    gocache$memory_data <- saved$data
    gocache$memory_result <- saved$result
  })
  gocache_set("go-result-a", 1)
  path <- gocache_path("go-result-a")
  age_file(path, 6)
  gocache$memory_result <- list()
  gocache$pinned <- character()
  expect_identical(gocache_get("go-result-a"), 1)
  expect_lt(as.numeric(difftime(Sys.time(), file.mtime(path), units = "mins")), 1)
})

test_that("loading a sped-up package cleans the cache even with speed-ups off", {
  dir <- local_store()
  withr::local_options(paradoxR.speedups = FALSE)
  path <- file.path(dir, "go-result-a.rds")
  writeBin(as.raw(1:3), path)
  age_file(path, 8)
  gocache$last_sweep <- NULL
  cache_on_load()
  expect_false(file.exists(path))
})

test_that("load hooks for cleanup cover every package with speed-ups", {
  expect_true(all(c("clusterProfiler", "DESeq2") %in% cache_hook_packages()))
  for (pkg in cache_hook_packages()) {
    hooks <- getHook(packageEvent(pkg, "onLoad"))
    expect_true(any(vapply(hooks, identical, logical(1), cache_on_load)), info = pkg)
  }
})

test_that("the sweep runs again only after the interval", {
  local_store()
  withr::local_options(paradoxR.cache_sweep_seconds = 3600)
  gocache$last_sweep <- NULL
  expect_true(cache_sweep_due())
  cache_maybe_sweep()
  expect_false(cache_sweep_due())
  expect_true(cache_sweep_due(Sys.time() + 3601))
})

test_that("the free-space floor is 15% of the disk or 15 GB, whichever is larger", {
  withr::local_options(paradoxR.cache_min_free_pct = NULL, paradoxR.cache_min_free_gb = NULL)
  expect_identical(cache_min_free_bytes(1e12), 1.5e11)
  expect_identical(cache_min_free_bytes(1e11), 1.5e10)
  expect_identical(cache_min_free_bytes(5e10), 1.5e10)
  withr::local_options(paradoxR.cache_min_free_pct = 10, paradoxR.cache_min_free_gb = 1)
  expect_identical(cache_min_free_bytes(1e12), 1e11)
})

test_that("free space is read for the cache's own disk", {
  testthat::skip_if_not_installed("ps")
  dir <- local_store()
  fs <- cache_fs(file.path(dir, "not", "yet", "created"))
  expect_type(fs$type, "character")
  expect_gt(fs$total, 0)
  expect_gte(fs$free, 0)
  expect_lte(fs$free, fs$total)
})

test_that("writes that would break the floor delete the least recently used files first", {
  dir <- local_store()
  paths <- file.path(dir, paste0("go-result-", 1:3, ".rds"))
  for (i in 1:3) {
    writeBin(as.raw(rep(1, 100)), paths[i])
    age_file(paths[i], 4 - i)
  }
  free <- 1000
  testthat::local_mocked_bindings(
    cache_fs = function(dir) list(type = "ext4", total = 1e4, free = free + 100 * sum(!file.exists(paths))),
    cache_min_free_bytes = function(total) 1000
  )
  expect_true(cache_make_room(150, dir))
  expect_identical(file.exists(paths), c(FALSE, FALSE, TRUE))
})

test_that("a write that cannot fit is skipped", {
  dir <- local_store()
  testthat::local_mocked_bindings(
    cache_fs = function(dir) list(type = "ext4", total = 1e4, free = 500),
    cache_min_free_bytes = function(total) 1000
  )
  expect_false(cache_make_room(10, dir))
})

test_that("without a free-space reading, only small caches may write", {
  dir <- local_store()
  testthat::local_mocked_bindings(cache_fs = function(dir) NULL)
  expect_false(cache_make_room(10, dir))
  expect_true(cache_make_room(10, dir, require_fs = FALSE))
})

test_that("network file systems are recognised", {
  expect_true(cache_is_network(list(type = "nfs4")))
  expect_true(cache_is_network(list(type = "fuse.sshfs")))
  expect_true(cache_is_network(list(type = "cifs")))
  expect_false(cache_is_network(list(type = "ext4")))
  expect_false(cache_is_network(list(type = "apfs")))
  expect_false(cache_is_network(list(type = "ntfs")))
  expect_false(cache_is_network(NULL))
})
