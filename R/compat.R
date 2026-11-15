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

# Replacements that copy an upstream function's logic only apply to the exact
# upstream body they were written against; anything else keeps the original.

compat_body_hash <- function(fun) {
  rlang::hash(deparse(body(fun)))
}

compat_known_bodies <- list(
  "clusterProfiler::simplify_internal" = "3d981b3983d28991df0727112ad06298",
  "GOSemSim::wangMethod" = "10ef1581c9a8090b8eecdf118d3ef0ec",
  "GOSemSim::computeIC" = "9c38a4afc249a0cf0abc85f678d1a9e1",
  "DOSE::enricher_internal" = "241b30008e06a197d73d16d5c60e8265",
  "clusterProfiler::enricher_internal" = "241b30008e06a197d73d16d5c60e8265",
  "clusterProfiler::enrichGO" = "d4d3877be48ece3a33582550cb2704b2"
)

compat_body_is_known <- function(pkg, name, fun) {
  identical(compat_known_bodies[[paste0(pkg, "::", name)]], compat_body_hash(fun))
}
