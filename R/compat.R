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
  "GOSemSim::wangMethod" = "10ef1581c9a8090b8eecdf118d3ef0ec"
)

compat_body_is_known <- function(pkg, name, fun) {
  identical(compat_known_bodies[[paste0(pkg, "::", name)]], compat_body_hash(fun))
}
