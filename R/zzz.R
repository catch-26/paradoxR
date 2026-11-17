.onLoad <- function(libname, pkgname) {
  if (speedups_enabled()) {
    gocache_use_safely()
  }
}

.onUnload <- function(libpath) {
  try(reset_speedups(), silent = TRUE)
  try(gocache_unhook(), silent = TRUE)
}
