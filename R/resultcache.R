# Stores the results of slow, deterministic analysis functions from other
# packages. A stored result is used only when every input matches: the
# argument values, the content of every input file the function reads, the
# installed versions of the package and everything it depends on, the R build
# and BLAS, the relevant options and, when the function draws random numbers,
# the random number state. A hit also replays the messages and warnings, the
# option changes and the random number state that the original call left.
# Functions are added as entries of resultcache_specs(). Calls made while a
# stored function runs (dba.count() calling summarizeOverlaps(), say) are not
# stored on their own, since the outer result already covers them.

resultcache_subdir <- "results"
resultcache_files_subdir <- "files"

resultcache_enabled <- function() {
  isTRUE(getOption("paradoxR.result_cache", TRUE)) && speedups_enabled()
}

# The packages that store results are loaded with paradoxR, so that the
# first stored or reused result does not load them in the middle of a call
# (loading qs2 also registers methods), which a stock call would not do.
resultcache_load_helpers <- function() {
  if (isTRUE(getOption("paradoxR.result_cache", TRUE))) {
    for (pkg in c("ps", "qs2")) {
      try(loadNamespace(pkg), silent = TRUE)
    }
  }
  invisible(NULL)
}

resultcache_min_seconds <- function() {
  getOption("paradoxR.result_cache_min_seconds", 2)
}

# A result is kept only when the call took at least this many times as long
# as hashing its inputs plus writing the result, which is about what a later
# hit costs.
resultcache_min_ratio <- function() {
  getOption("paradoxR.result_cache_min_ratio", 5)
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
  caller <- sys.call(sys.parent())
  finish <- function(result) {
    value <- if (is.null(spec$restore)) result$value else spec$restore(result$value, frame, caller, original)
    if (result$visible) value else invisible(value)
  }
  if (isTRUE(gocache$resultcache_running)) {
    return(finish(withVisible(run())))
  }
  planned <- proc.time()[["elapsed"]]
  plan <- tryCatch(resultcache_plan(id, spec, original, frame, supplied, has_dots), error = function(e) NULL)
  if (is.null(plan)) {
    return(finish(withVisible(run())))
  }
  plan$hash_seconds <- proc.time()[["elapsed"]] - planned
  entry <- tryCatch(resultcache_lookup(plan), error = function(e) NULL)
  if (!is.null(entry)) {
    return(finish(withVisible(resultcache_replay(entry))))
  }
  gocache$resultcache_running <- TRUE
  on.exit(gocache$resultcache_running <- FALSE, add = TRUE)
  finish(withVisible(resultcache_record(plan, run)))
}

resultcache_plan <- function(id, spec, original, frame, supplied, has_dots) {
  if (!resultcache_enabled() || !rlang::is_installed("rlang", version = "1.3.0")) {
    return(NULL)
  }
  if (isNamespaceLoaded("targets") && isTRUE(tryCatch(targets::tar_active(), error = function(e) FALSE))) {
    return(NULL)
  }
  args <- mget(supplied, envir = frame)
  if (has_dots) {
    args <- c(args, list(... = eval(quote(list(...)), frame)))
  }
  if (!is.null(spec$bypass) && isTRUE(spec$bypass(args))) {
    return(NULL)
  }
  cache_maybe_sweep()
  if (!is.null(spec$project)) {
    args <- spec$project(args)
  }
  files <- if (is.null(spec$files)) character() else spec$files(args)
  args <- resultcache_stable(args)
  seeded <- !is.null(spec$seed) && resultcache_seeded(get(spec$seed, envir = frame))
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
  list(id = id, key = paste0(gsub("[^a-z0-9]", "", tolower(id)), "-", key), seeded = seeded)
}

# A function with its own seed argument sets the random number state itself,
# so its result does not depend on the state it was called with.
resultcache_seeded <- function(seed) {
  is.numeric(seed) && length(seed) == 1L && !is.na(seed)
}

# Replaces values that differ between sessions although they mean the same:
# package functions by their code, package and version, Bioconductor file
# handles by their paths and settings, annotation databases by their file
# content, and parallel back ends by their class (the functions stored give
# the same result on any back end). Other functions can call code that is not
# part of the key, so a call with one is not stored.
resultcache_stable <- function(x) {
  if (is.function(x)) {
    if (is.primitive(x)) {
      return(list(primitive = deparse(x)))
    }
    env <- environment(x)
    if (!isNamespace(env)) {
      stop("function argument outside a package")
    }
    pkg <- unname(getNamespaceName(env))
    x <- utils::removeSource(x)
    return(list(fun = list(formals(x), body(x)), package = pkg, version = compat_installed_version(pkg)))
  }
  if (methods::is(x, "BiocParallelParam")) {
    return(list(class = class(x)))
  }
  if (methods::is(x, "RsamtoolsFile")) {
    fields <- setdiff(names(methods::getRefClass(class(x))$fields()), ".extptr")
    return(c(list(class = class(x)), lapply(stats::setNames(fields, fields), function(field) x[[field]])))
  }
  if (methods::is(x, "RsamtoolsFileList")) {
    return(list(class = class(x), files = lapply(as.list(x), resultcache_stable), names = names(x)))
  }
  if (methods::is(x, "AnnotationDb")) {
    return(list(
      class = class(x),
      file = resultcache_file_ids(AnnotationDbi::dbfile(x)),
      metadata = AnnotationDbi::metadata(x)
    ))
  }
  if (is.list(x) && !is.object(x)) {
    x[] <- lapply(x, resultcache_stable)
  }
  x
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
  file.path(gocache_dir(), resultcache_subdir, paste0(key, ".qs2"))
}

resultcache_read <- function(key) {
  path <- resultcache_path(key)
  if (!file.exists(path)) {
    return(NULL)
  }
  drop <- function(condition) {
    try(unlink(path), silent = TRUE)
    NULL
  }
  value <- tryCatch(
    qs2::qs_read(path, validate_checksum = TRUE),
    error = drop,
    warning = drop
  )
  if (!is.null(value)) {
    cache_touch(path)
  }
  value
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
    qs2::qs_save(value, tmp)
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
  if (!is.null(entry) && !identical(entry$format, 2L)) {
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

# Runs the original and records everything a hit has to repeat: printed
# output, text written to the console's error stream (progress bars) and
# conditions in the order they happened, whether the value was visible, the
# option changes, the packages the call loaded or attached, and the random
# number state.
resultcache_record <- function(plan, run) {
  recorded <- resultcache_run_recorded(run)
  result <- recorded$result
  value <- result$value
  if (recorded$seconds < resultcache_min_seconds()) {
    return(if (result$visible) value else invisible(value))
  }
  rng_dependent <- !isTRUE(plan$seeded) && !identical(recorded$seed_before, recorded$seed_after)
  seed_before <- recorded$seed_before
  entry <- resultcache_entry(recorded, rng_dependent = rng_dependent, seeded = isTRUE(plan$seeded))
  try({
    key <- if (rng_dependent) resultcache_rng_key(plan$key, seed_before) else plan$key
    started <- proc.time()[["elapsed"]]
    written <- resultcache_write(key, entry)
    cost <- (proc.time()[["elapsed"]] - started) + plan$hash_seconds
    if (isTRUE(written)) {
      if (recorded$seconds < resultcache_min_ratio() * cost) {
        unlink(resultcache_path(key))
      } else if (rng_dependent) {
        resultcache_write(plan$key, list(format = 2L, rng_dependent = TRUE))
      }
    }
  }, silent = TRUE)
  if (result$visible) value else invisible(value)
}

# Lazily created package state that a stock call can leave behind and that
# a hit has to create the same way: each entry says whether the state exists
# and creates it through the package's own functions.
resultcache_states <- function() {
  list(
    "biocparallel-registry" = list(
      exists = function() {
        isNamespaceLoaded("BiocParallel") &&
          length(get(".registry", envir = asNamespace("BiocParallel"))$bpparams) > 0L
      },
      create = function() invisible(BiocParallel::registered())
    ),
    "tidyselect-vars" = list(
      exists = function() {
        isNamespaceLoaded("tidyselect") &&
          exists("selected", envir = get("vars_env", envir = asNamespace("tidyselect")), inherits = FALSE)
      },
      create = function() invisible(tidyselect::eval_select(quote(NULL), data.frame()))
    )
  )
}

resultcache_state_check <- function() {
  vapply(resultcache_states(), function(state) isTRUE(tryCatch(state$exists(), error = function(e) FALSE)), logical(1))
}

resultcache_run_recorded <- function(run) {
  states_before <- resultcache_state_check()
  seed_before <- resultcache_seed()
  options_before <- options()
  namespaces_before <- loadedNamespaces()
  search_before <- search()
  capture <- resultcache_capture()
  on.exit(capture$close(), add = TRUE)
  started <- proc.time()[["elapsed"]]
  result <- withCallingHandlers(
    withVisible(run()),
    message = function(m) capture$condition(m, "message"),
    warning = function(w) capture$condition(w, "warning")
  )
  events <- capture$finish()
  options_after <- options()
  changed <- union(names(options_before), names(options_after))
  changed <- changed[!vapply(changed, function(n) identical(options_before[[n]], options_after[[n]]), logical(1))]
  list(
    result = result,
    events = events,
    namespaces = setdiff(loadedNamespaces(), namespaces_before),
    attached = setdiff(search(), search_before),
    options = stats::setNames(lapply(changed, function(n) options_after[[n]]), changed),
    seed_before = seed_before,
    seed_after = resultcache_seed(),
    seconds = proc.time()[["elapsed"]] - started,
    states = names(which(resultcache_state_check() & !states_before))
  )
}

resultcache_entry <- function(recorded, rng_dependent, seeded = FALSE) {
  list(
    format = 2L,
    value = recorded$result$value,
    visible = recorded$result$visible,
    events = recorded$events,
    namespaces = recorded$namespaces,
    attached = recorded$attached,
    rng_dependent = rng_dependent,
    seeded = seeded,
    seed = recorded$seed_after,
    options = recorded$options,
    states = recorded$states,
    seconds = recorded$seconds
  )
}

# Copies what the original call prints while it still reaches the console:
# standard output through a split sink, the error stream through a message
# sink whose text is passed on at every condition and at the end. A
# condition's own printed text is recognised and dropped, since replaying
# the condition prints it again.
resultcache_capture <- function() {
  paths <- c(output = tempfile("paradoxR-output-"), console = tempfile("paradoxR-console-"))
  output <- file(paths[["output"]], open = "w")
  console <- file(paths[["console"]], open = "w")
  previous <- sink.number(type = "message")
  target <- if (previous == 2L) stderr() else getConnection(previous)
  sink(output, split = TRUE)
  sink(console, type = "message")
  open <- TRUE
  offsets <- c(output = 0, console = 0)
  events <- list()
  read_new <- function(which, con) {
    flush(con)
    size <- file.size(paths[[which]])
    if (is.na(size) || size <= offsets[[which]]) {
      return("")
    }
    bytes <- readBin(paths[[which]], "raw", size)
    text <- rawToChar(bytes[(offsets[[which]] + 1):size])
    offsets[[which]] <<- size
    if (.Platform$OS.type == "windows") {
      text <- gsub("\r\n", "\n", text, fixed = TRUE)
    }
    text
  }
  add <- function(event) events[[length(events) + 1L]] <<- event
  pass_on <- function(text) {
    if (previous == 2L) {
      sink(type = "message")
      on.exit(sink(console, type = "message"))
      cat(text, file = stderr())
    } else {
      cat(text, file = target)
    }
  }
  take <- function() {
    text <- read_new("output", output)
    if (nzchar(text)) {
      add(list(type = "output", text = text))
    }
    text <- read_new("console", console)
    if (!nzchar(text)) {
      return(invisible())
    }
    pass_on(text)
    last <- if (length(events) > 0L) events[[length(events)]] else NULL
    if (!is.null(last) && last$type %in% c("message", "warning") && !isTRUE(last$printed)) {
      printed <- resultcache_printed(last, text)
      if (printed > 0L) {
        events[[length(events)]]$printed <<- TRUE
        events[[length(events)]]$immediate <<- identical(last$type, "warning")
        text <- substring(text, printed + 1L)
      }
    }
    if (nzchar(text)) {
      add(list(type = "console", text = text))
    }
    invisible()
  }
  close_all <- function() {
    if (!open) {
      return(invisible())
    }
    try(take(), silent = TRUE)
    open <<- FALSE
    sink()
    if (previous == 2L) sink(type = "message") else sink(target, type = "message")
    close(output)
    close(console)
    unlink(paths)
    invisible()
  }
  list(
    condition = function(condition, type) {
      take()
      add(resultcache_condition(condition, type))
    },
    finish = function() {
      close_all()
      events
    },
    close = close_all
  )
}

# The number of characters at the start of `text` that R printed for the
# condition itself: the message, or an immediately printed warning.
resultcache_printed <- function(event, text) {
  if (identical(event$type, "message")) {
    return(if (startsWith(text, event$message)) nchar(event$message) else 0L)
  }
  if (!startsWith(text, "Warning")) {
    return(0L)
  }
  at <- regexpr(event$message, text, fixed = TRUE)
  if (at < 0L) {
    return(0L)
  }
  end <- as.integer(at) + nchar(event$message) - 1L
  if (identical(substr(text, end + 1L, end + 1L), "\n")) end + 1L else end
}

resultcache_replay <- function(entry) {
  for (ns in entry$namespaces) {
    try(loadNamespace(ns), silent = TRUE)
  }
  for (pkg in rev(sub("^package:", "", entry$attached[startsWith(entry$attached, "package:")]))) {
    try(suppressPackageStartupMessages(library(pkg, character.only = TRUE)), silent = TRUE)
  }
  states <- resultcache_states()
  for (name in intersect(entry$states, names(states))) {
    if (!isTRUE(tryCatch(states[[name]]$exists(), error = function(e) TRUE))) {
      try(states[[name]]$create(), silent = TRUE)
    }
  }
  if (length(entry$options) > 0L) {
    options(entry$options)
  }
  for (event in entry$events) {
    if (identical(event$type, "output")) {
      cat(event$text)
    } else if (identical(event$type, "console")) {
      cat(event$text, file = stderr())
    } else if (identical(event$type, "warning")) {
      signal <- structure(class = event$class, list(message = event$message, call = event$call))
      if (isTRUE(event$immediate)) {
        old <- options(warn = max(1L, getOption("warn", 0L)))
        on.exit(options(old), add = TRUE)
        warning(signal)
        options(old)
      } else {
        warning(signal)
      }
    } else {
      message(structure(class = event$class, list(message = event$message, call = event$call)))
    }
  }
  if (isTRUE(entry$rng_dependent) || isTRUE(entry$seeded)) {
    resultcache_set_seed(entry$seed)
  }
  if (isFALSE(entry$visible)) invisible(entry$value) else entry$value
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
