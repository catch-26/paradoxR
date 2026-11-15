.onLoad <- function(libname, pkgname) {
  if (isTRUE(getOption("paradoxR.go_cache", TRUE))) {
    gocache_use_safely()
  }
}

.onUnload <- function(libpath) {
  try(reset_go_cache(), silent = TRUE)
  try(gocache_unhook(), silent = TRUE)
}
