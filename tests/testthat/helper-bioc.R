skip_if_no_bioc <- function(pkg) {
  if (identical(Sys.getenv("PARADOXR_REQUIRE_BIOC"), "true")) {
    loadNamespace(pkg)
    return(invisible(TRUE))
  }
  testthat::skip_if_not_installed(pkg)
}
