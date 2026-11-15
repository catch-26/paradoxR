.onLoad <- function(libname, pkgname) {
  if (isTRUE(getOption("paradoxR.go_cache", TRUE))) {
    use_go_cache()
  }
}
