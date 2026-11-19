.onLoad <- function(libname, pkgname) {
  if (speedups_enabled()) {
    gocache_use_safely()
  }
}

.onUnload <- function(libpath) {
  try(speedups_revert(speedups_all_packages()), silent = TRUE)
  try(gocache_unhook(), silent = TRUE)
}
