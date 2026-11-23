.onLoad <- function(libname, pkgname) {
  try(cache_hook(), silent = TRUE)
  try(cache_maybe_sweep(), silent = TRUE)
  if (speedups_enabled()) {
    resultcache_load_helpers()
    gocache_use_safely()
  }
}

.onUnload <- function(libpath) {
  try(speedups_revert(speedups_all_packages()), silent = TRUE)
  try(gocache_unhook(), silent = TRUE)
  try(cache_unhook(), silent = TRUE)
}
