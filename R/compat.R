# Every version switch for upstream packages lives here.

ggplot2_has_no_scale_name <- function() {
  rlang::is_installed("ggplot2", version = "3.5.0")
}

compat_discrete_scale <- function(aesthetics, palette, ..., scale_name = "paradoxR") {
  if (ggplot2_has_no_scale_name()) {
    ggplot2::discrete_scale(aesthetics = aesthetics, palette = palette, ...)
  } else {
    ggplot2::discrete_scale(aesthetics = aesthetics, scale_name = scale_name, palette = palette, ...)
  }
}

# Replacements that copy or edit an upstream function's code ("ports") only
# apply where they were verified: the upstream function's fingerprint (its
# arguments and body, source references removed) must match, and the installed
# versions of every package the port relies on must match one verified set.
# Anything else keeps the original. Add verified versions; never remove them.

compat_fingerprint <- function(fun) {
  fun <- utils::removeSource(fun)
  rlang::hash(list(formals(fun), body(fun)))
}

compat_ports <- list(
  "bioc322-simplify" = list(
    fingerprint = "867bca878d6252679a0dbd948eae1c4e",
    versions = list(c(clusterProfiler = "4.18.4", GOSemSim = "2.36.0"))
  ),
  "bioc322-wang" = list(
    fingerprint = "ea47d9e4340fe166026b85cb3869c88b",
    versions = list(c(GOSemSim = "2.36.0"))
  ),
  "bioc322-computeic" = list(
    fingerprint = "77e11a73ae426235ad7da381348ec718",
    versions = list(c(GOSemSim = "2.36.0"))
  ),
  "bioc322-enricher" = list(
    fingerprint = "bd70cb4f6914f2dcf5800239b729a8b6",
    versions = list(c(DOSE = "4.4.0", clusterProfiler = "4.18.4"))
  ),
  "bioc322-enrichgo-cutoffs" = list(
    fingerprint = "022d8f63fe6579cbe54d9719fa69d68d",
    versions = list(c(clusterProfiler = "4.18.4", DOSE = "4.4.0"))
  ),
  "bioc323-simplify" = list(
    fingerprint = "a733138bef9b59a68e580664856e5b3e",
    versions = list(c(clusterProfiler = "4.20.3", GOSemSim = "2.38.3"))
  ),
  "bioc323-wang" = list(
    fingerprint = "a18168a3dd8ba96dd40c0c9c7ef19c18",
    versions = list(c(GOSemSim = "2.38.3"))
  ),
  "bioc323-offspring-idx" = list(
    fingerprint = "c5af08e67d404358c0314b7525055a38",
    versions = list(c(GOSemSim = "2.38.3"))
  ),
  "bioc322-getgodata-sql" = list(
    fingerprint = "6e8f2f19066acebe25a238d44533ce74",
    versions = list(c(clusterProfiler = "4.18.4", AnnotationDbi = "1.72.0"))
  )
)

compat_installed_version <- function(pkg) {
  tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
}

compat_port_applies <- function(port, fun) {
  entry <- compat_ports[[port]]
  if (is.null(entry) || !is.function(fun) || !identical(compat_fingerprint(fun), entry$fingerprint)) {
    return(FALSE)
  }
  any(vapply(entry$versions, function(set) {
    installed <- vapply(names(set), compat_installed_version, character(1))
    identical(unname(installed), unname(set))
  }, logical(1)))
}
