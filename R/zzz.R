.onLoad <- function(libname, pkgname) {
  if (isTRUE(getOption("paradoxR.go_cache", TRUE))) {
    gocache_use_safely()
  }
}
