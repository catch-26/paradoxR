# Stores the results of slow, deterministic analysis functions from other
# packages. A stored result is used only when every input matches: the
# argument values, the content of every input file the function reads, the
# installed versions of the package and everything it depends on, the R build
# and BLAS, the relevant options and, when the function draws random numbers,
# the random number state. A hit also replays the messages and warnings, the
# option changes and the random number state that the original call left.
# Functions are added as entries of resultcache_specs().

resultcache_subdir <- "results"
resultcache_files_subdir <- "files"

resultcache_enabled <- function() {
  isTRUE(getOption("paradoxR.result_cache", TRUE)) && speedups_enabled()
}

resultcache_min_seconds <- function() {
  getOption("paradoxR.result_cache_min_seconds", 10)
}

resultcache_make <- function(id) {
  force(id)
  function(original) {
    wrapper <- original
    env <- new.env(parent = environment(original))
    env$resultcache_call <- resultcache_call
    env$resultcache_id <- id
    body(wrapper) <- quote(resultcache_call(resultcache_id, environment()))
    environment(wrapper) <- env
    wrapper
  }
}

resultcache_supplied <- function(original, frame) {
  arg_names <- setdiff(names(formals(original)), "...")
  arg_names[!vapply(
    arg_names,
    function(name) eval(call("missing", as.name(name)), frame),
    logical(1)
  )]
}

resultcache_original_call <- function(original, supplied, has_dots) {
  args <- stats::setNames(lapply(supplied, as.name), supplied)
  if (has_dots) {
    args <- c(args, list(quote(...)))
  }
  as.call(c(list(original), args))
}

resultcache_call <- function(id, frame) {
  spec <- resultcache_specs()[[id]]
  original <- gocache_original(spec$pkg, spec$name)
  if (is.null(original)) {
    original <- attr(sys.function(sys.parent()), "paradoxR_original")
  }
  supplied <- resultcache_supplied(original, frame)
  has_dots <- "..." %in% names(formals(original))
  call <- resultcache_original_call(original, supplied, has_dots)
  run <- function() eval(call, frame)
  plan <- tryCatch(resultcache_plan(id, spec, original, frame, supplied, has_dots), error = function(e) NULL)
  if (is.null(plan)) {
    return(run())
  }
  entry <- tryCatch(resultcache_lookup(plan), error = function(e) NULL)
  if (!is.null(entry)) {
    return(resultcache_replay(entry))
  }
  resultcache_record(plan, run)
}

resultcache_plan <- function(id, spec, original, frame, supplied, has_dots) {
  if (!resultcache_enabled() || !rlang::is_installed("rlang", version = "1.3.0")) {
    return(NULL)
  }
  if (isNamespaceLoaded("targets") && isTRUE(tryCatch(targets::tar_active(), error = function(e) FALSE))) {
    return(NULL)
  }
  cache_maybe_sweep()
  args <- mget(supplied, envir = frame)
  if (has_dots) {
    args <- c(args, list(... = eval(quote(list(...)), frame)))
  }
  if (!is.null(spec$project)) {
    args <- spec$project(args)
  }
  if (!is.null(spec$bypass) && isTRUE(spec$bypass(args))) {
    return(NULL)
  }
  files <- if (is.null(spec$files)) character() else spec$files(args)
  key <- rlang::hash(list(
    id = id,
    code = compat_fingerprint(original),
    packages = resultcache_versions(c(spec$pkg, spec$packages)),
    r = resultcache_platform(),
    options = resultcache_options(spec$options),
    args = args,
    missing = setdiff(setdiff(names(formals(original)), "..."), supplied),
    files = resultcache_file_ids(files)
  ))
  list(id = id, key = paste0(gsub("[^a-z0-9]", "", tolower(id)), "-", key))
}

resultcache_platform <- function() {
  list(
    version = R.version$version.string,
    platform = R.version$platform,
    blas = tryCatch(extSoftVersion()[["BLAS"]], error = function(e) NA_character_),
    lapack = tryCatch(La_library(), error = function(e) NA_character_),
    lapack_version = tryCatch(La_version(), error = function(e) NA_character_)
  )
}

resultcache_options <- function(patterns) {
  if (length(patterns) == 0L) {
    return(list())
  }
  all <- options()
  keep <- Reduce(`|`, lapply(patterns, grepl, x = names(all)), FALSE)
  all <- all[keep]
  all[order(names(all))]
}

# Installed versions of the packages and everything they depend on (Depends,
# Imports, LinkingTo), read from the installed DESCRIPTION files once per
# session. Packages that are not installed count as NA, so installing one
# (for example presto for Seurat) changes the key.
resultcache_versions <- function(packages) {
  id <- paste(sort(unique(packages)), collapse = ",")
  if (is.null(gocache$versions)) {
    gocache$versions <- list()
  }
  if (!is.null(gocache$versions[[id]])) {
    return(gocache$versions[[id]])
  }
  seen <- character()
  queue <- unique(packages)
  base <- c("R", rownames(utils::installed.packages(priority = "base", noCache = FALSE)))
  while (length(queue) > 0L) {
    pkg <- queue[[1L]]
    queue <- queue[-1L]
    if (pkg %in% seen || pkg %in% base) {
      next
    }
    seen <- c(seen, pkg)
    desc <- suppressWarnings(utils::packageDescription(pkg, fields = c("Depends", "Imports", "LinkingTo")))
    if (!is.list(desc)) {
      next
    }
    deps <- unlist(lapply(desc, function(field) {
      if (is.null(field) || is.na(field)) return(character())
      trimws(sub("\\(.*", "", strsplit(field, ",")[[1L]]))
    }))
    queue <- c(queue, setdiff(deps[nzchar(deps)], seen))
  }
  seen <- sort(seen)
  versions <- stats::setNames(vapply(seen, compat_installed_version, character(1)), seen)
  gocache$versions[[id]] <- versions
  versions
}

resultcache_path <- function(key) {
  ext <- if (rlang::is_installed("qs2")) ".qs2" else ".rds"
  file.path(gocache_dir(), resultcache_subdir, paste0(key, ext))
}

resultcache_read <- function(key) {
  dir <- file.path(gocache_dir(), resultcache_subdir)
  for (path in file.path(dir, paste0(key, c(".qs2", ".rds")))) {
    if (!file.exists(path)) {
      next
    }
    drop <- function(condition) {
      try(unlink(path), silent = TRUE)
      NULL
    }
    if (endsWith(path, ".qs2") && !rlang::is_installed("qs2")) {
      next
    }
    value <- tryCatch(
      if (endsWith(path, ".qs2")) {
        qs2::qs_read(path, validate_checksum = TRUE)
      } else {
        readRDS(path)
      },
      error = drop,
      warning = drop
    )
    if (!is.null(value)) {
      cache_touch(path)
      return(value)
    }
  }
  NULL
}

resultcache_write <- function(key, value) {
  fs <- cache_fs()
  if (is.null(fs) || cache_is_network(fs)) {
    return(invisible(FALSE))
  }
  path <- resultcache_path(key)
  dir <- dirname(path)
  tmp <- character()
  ok <- tryCatch({
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!cache_make_room(as.numeric(utils::object.size(value)))) {
      cache_note_once(
        "floor",
        "paradoxR: not storing results this session; the disk is close to its free-space limit."
      )
      stop("not enough free disk space")
    }
    tmp <- tempfile("result-", tmpdir = dir, fileext = ".tmp")
    if (endsWith(path, ".qs2")) {
      qs2::qs_save(value, tmp)
    } else {
      saveRDS(value, tmp, compress = FALSE)
    }
    gocache_move(tmp, cache_pin(path))
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    try(unlink(tmp), silent = TRUE)
  }
  invisible(isTRUE(ok))
}

resultcache_rng_key <- function(key, seed) {
  paste0(key, "-", rlang::hash(seed))
}

resultcache_lookup <- function(plan) {
  entry <- resultcache_read(plan$key)
  if (!is.null(entry) && isTRUE(entry$rng_dependent)) {
    entry <- resultcache_read(resultcache_rng_key(plan$key, resultcache_seed()))
  }
  if (!is.null(entry) && !identical(entry$format, 1L)) {
    return(NULL)
  }
  entry
}

resultcache_seed <- function() {
  get0(".Random.seed", envir = globalenv(), inherits = FALSE)
}

resultcache_set_seed <- function(seed) {
  if (is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", seed, envir = globalenv())
  }
}

resultcache_condition <- function(condition, type) {
  call <- conditionCall(condition)
  if (!is.null(call) && utils::object.size(call) > 10000) {
    call <- NULL
  }
  list(type = type, message = conditionMessage(condition), class = class(condition), call = call)
}

resultcache_record <- function(plan, run) {
  seed_before <- resultcache_seed()
  options_before <- options()
  conditions <- list()
  started <- proc.time()[["elapsed"]]
  value <- withCallingHandlers(
    run(),
    message = function(m) conditions[[length(conditions) + 1L]] <<- resultcache_condition(m, "message"),
    warning = function(w) conditions[[length(conditions) + 1L]] <<- resultcache_condition(w, "warning")
  )
  elapsed <- proc.time()[["elapsed"]] - started
  if (elapsed < resultcache_min_seconds()) {
    return(value)
  }
  seed_after <- resultcache_seed()
  rng_dependent <- !identical(seed_before, seed_after)
  options_after <- options()
  changed <- union(names(options_before), names(options_after))
  changed <- changed[!vapply(changed, function(n) identical(options_before[[n]], options_after[[n]]), logical(1))]
  entry <- list(
    format = 1L,
    value = value,
    conditions = conditions,
    rng_dependent = rng_dependent,
    seed = seed_after,
    options = stats::setNames(lapply(changed, function(n) options_after[[n]]), changed),
    seconds = elapsed
  )
  try({
    if (rng_dependent) {
      resultcache_write(plan$key, list(format = 1L, rng_dependent = TRUE))
      resultcache_write(resultcache_rng_key(plan$key, seed_before), entry)
    } else {
      resultcache_write(plan$key, entry)
    }
  }, silent = TRUE)
  value
}

resultcache_replay <- function(entry) {
  if (length(entry$options) > 0L) {
    options(entry$options)
  }
  for (condition in entry$conditions) {
    signal <- structure(
      class = condition$class,
      list(message = condition$message, call = condition$call)
    )
    if (identical(condition$type, "warning")) {
      warning(signal)
    } else {
      message(signal)
    }
  }
  if (isTRUE(entry$rng_dependent)) {
    resultcache_set_seed(entry$seed)
  }
  entry$value
}

# Content fingerprints of input files. A file's content hash is reused while
# its path, size and modification time are unchanged; files modified in the
# last two seconds are always hashed again, since coarse file system
# timestamps cannot tell such edits apart.
resultcache_file_ids <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  if (length(paths) == 0L) {
    return(list())
  }
  ids <- lapply(paths, resultcache_file_id)
  ids[order(vapply(ids, `[[`, character(1), "path"))]
}

resultcache_file_id <- function(path) {
  full <- normalizePath(path, winslash = "/", mustWork = FALSE)
  info <- file.info(full, extra_cols = FALSE)
  if (is.na(info$size) || isTRUE(info$isdir)) {
    return(list(path = full, size = NA_real_, hash = NA_character_))
  }
  mtime <- as.numeric(info$mtime)
  manifest <- file.path(gocache_dir(), resultcache_files_subdir, paste0(rlang::hash(full), ".rds"))
  known <- tryCatch(if (file.exists(manifest)) readRDS(manifest) else NULL, error = function(e) NULL)
  settled <- as.numeric(Sys.time()) - mtime > 2
  if (settled && is.list(known) && identical(known$path, full) &&
      identical(known$size, info$size) && identical(known$mtime, mtime)) {
    cache_touch(manifest)
    return(list(path = full, size = info$size, hash = known$hash))
  }
  hash <- rlang::hash_file(full)
  if (settled) {
    try({
      dir.create(dirname(manifest), recursive = TRUE, showWarnings = FALSE)
      tmp <- tempfile("file-", tmpdir = dirname(manifest), fileext = ".tmp")
      saveRDS(list(path = full, size = info$size, mtime = mtime, hash = hash), tmp)
      gocache_move(tmp, cache_pin(manifest))
    }, silent = TRUE)
  }
  list(path = full, size = info$size, hash = hash)
}
