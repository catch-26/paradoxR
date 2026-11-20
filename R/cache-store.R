# Shared disk housekeeping for every paradoxR cache: last-use expiry, a
# free-space floor and least-recently-used eviction. Last use is the file's
# modification time, refreshed on every hit.

cache_max_age_days <- function() {
  getOption("paradoxR.cache_max_age_days", 7)
}

cache_min_free_bytes <- function(total) {
  pct <- getOption("paradoxR.cache_min_free_pct", 15)
  gb <- getOption("paradoxR.cache_min_free_gb", 15)
  max(total * pct / 100, gb * 1e9)
}

cache_files <- function(dir = gocache_dir()) {
  if (!dir.exists(dir)) {
    return(character())
  }
  list.files(dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
}

cache_pin <- function(path) {
  gocache$pinned <- union(gocache$pinned, normalizePath(path, winslash = "/", mustWork = FALSE))
  invisible(path)
}

cache_is_pinned <- function(paths) {
  normalizePath(paths, winslash = "/", mustWork = FALSE) %in% gocache$pinned
}

cache_touch <- function(path) {
  cache_pin(path)
  try(suppressWarnings(Sys.setFileTime(path, Sys.time())), silent = TRUE)
  invisible(path)
}

cache_sweep <- function(dir = gocache_dir(), now = Sys.time()) {
  gocache$last_sweep <- now
  files <- cache_files(dir)
  if (length(files) == 0L) {
    return(invisible(0L))
  }
  mtime <- file.mtime(files)
  age <- as.numeric(difftime(now, mtime, units = "days"))
  partial <- grepl("\\.tmp$", files) & age > 1 / 24
  expired <- age > cache_max_age_days()
  drop <- files[!is.na(age) & (expired | partial) & !cache_is_pinned(files)]
  if (length(drop) > 0L) {
    suppressWarnings(unlink(drop))
  }
  invisible(length(drop))
}

cache_sweep_due <- function(now = Sys.time()) {
  last <- gocache$last_sweep
  is.null(last) || as.numeric(difftime(now, last, units = "secs")) >= cache_sweep_interval()
}

cache_sweep_interval <- function() {
  getOption("paradoxR.cache_sweep_seconds", 0)
}

cache_maybe_sweep <- function() {
  if (cache_sweep_due()) {
    try(suppressWarnings(cache_sweep()), silent = TRUE)
  }
  invisible(NULL)
}

cache_fs <- function(dir = gocache_dir()) {
  if (!rlang::is_installed("ps")) {
    return(NULL)
  }
  probe <- dir
  while (!dir.exists(probe) && !identical(dirname(probe), probe)) {
    probe <- dirname(probe)
  }
  info <- tryCatch(ps::ps_fs_info(probe), error = function(e) NULL)
  if (is.null(info) || nrow(info) != 1L) {
    return(NULL)
  }
  list(
    type = tolower(as.character(info$type)),
    total = as.numeric(info$total_data_blocks) * as.numeric(info$block_size),
    free = as.numeric(info$free_blocks_non_superuser) * as.numeric(info$block_size)
  )
}

cache_network_types <- c(
  "nfs", "nfs4", "cifs", "smbfs", "smb2", "smb3", "afpfs", "webdav", "davfs", "ncpfs",
  "9p", "fuse.sshfs", "fuse.rclone", "fuse.s3fs", "fuse.gcsfuse", "fuse.davfs2", "fuse.glusterfs",
  "lustre", "gpfs", "beegfs", "ceph"
)

cache_is_network <- function(fs) {
  !is.null(fs) && (fs$type %in% cache_network_types || startsWith(fs$type, "fuse.sshfs"))
}

# Makes room for `size` bytes under the free-space floor by deleting the least
# recently used files that this session has not touched. Returns FALSE when the
# write would still break the floor, or when free space cannot be measured and
# `require_fs` is TRUE.
cache_make_room <- function(size, dir = gocache_dir(), require_fs = TRUE) {
  fs <- cache_fs(dir)
  if (is.null(fs)) {
    return(!require_fs)
  }
  floor <- cache_min_free_bytes(fs$total)
  if (fs$free - size >= floor) {
    return(TRUE)
  }
  files <- cache_files(dir)
  files <- files[!cache_is_pinned(files)]
  if (length(files) > 0L) {
    info <- file.info(files, extra_cols = FALSE)
    files <- files[order(info$mtime)]
    sizes <- info$size[order(info$mtime)]
    need <- floor - (fs$free - size)
    take <- which(cumsum(sizes) >= need)
    drop <- if (length(take) > 0L) files[seq_len(take[1L])] else files
    suppressWarnings(unlink(drop))
    fs <- cache_fs(dir)
    if (is.null(fs)) {
      return(!require_fs)
    }
  }
  fs$free - size >= floor
}

cache_note_once <- function(id, text) {
  if (id %in% gocache$noted) {
    return(invisible(NULL))
  }
  gocache$noted <- c(gocache$noted, id)
  message(text)
  invisible(NULL)
}

cache_on_load <- function(...) {
  try(cache_maybe_sweep(), silent = TRUE)
  invisible(NULL)
}

cache_hook_packages <- function() {
  speedups_all_packages()
}

cache_hook <- function() {
  for (pkg in cache_hook_packages()) {
    if (!pkg %in% gocache$cache_hooked) {
      setHook(packageEvent(pkg, "onLoad"), cache_on_load)
      gocache$cache_hooked <- c(gocache$cache_hooked, pkg)
    }
  }
  invisible(NULL)
}

cache_unhook <- function() {
  for (pkg in gocache$cache_hooked) {
    event <- packageEvent(pkg, "onLoad")
    keep <- Filter(function(hook) !identical(hook, cache_on_load), getHook(event))
    setHook(event, keep, action = "replace")
  }
  gocache$cache_hooked <- character()
  invisible(NULL)
}
