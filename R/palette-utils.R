# Shared plumbing for every palette in the palette family.

palette_ggplot2_options <- c(
  "ggplot2.discrete.colour", "ggplot2.discrete.fill",
  "ggplot2.continuous.colour", "ggplot2.continuous.fill",
  "ggplot2.binned.colour", "ggplot2.binned.fill"
)

palette_scale_d <- function(aesthetics, palette, ...) {
  compat_discrete_scale(aesthetics, palette, ...)
}

palette_scale_c <- function(aesthetics, colours, ...) {
  ggplot2::scale_colour_gradientn(colours = colours, ..., aesthetics = aesthetics)
}

palette_scale_b <- function(aesthetics, colours, ...) {
  ggplot2::scale_colour_stepsn(colours = colours, ..., aesthetics = aesthetics)
}

palette_scale_div <- function(aesthetics, colours, ..., midpoint = 0) {
  ggplot2::scale_colour_gradientn(
    colours = colours,
    ...,
    rescaler = palette_mid_rescaler(midpoint),
    aesthetics = aesthetics
  )
}

palette_mid_rescaler <- function(midpoint) {
  force(midpoint)
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to = to, from = from, mid = midpoint)
  }
}

palette_lookup <- function(palette, sources, call = rlang::caller_env()) {
  choices <- unique(unlist(lapply(sources, names), use.names = FALSE))
  rlang::arg_match0(palette, choices, arg_nm = "palette", error_call = call)
  source <- Find(\(s) palette %in% names(s), sources)
  unname(source[[palette]])
}

palette_set_options <- function(colour_d, fill_d, colour_c, fill_c, colour_b, fill_b) {
  new <- rlang::set_names(
    list(colour_d, fill_d, colour_c, fill_c, colour_b, fill_b),
    palette_ggplot2_options
  )
  invisible(options(new))
}

#' Restore ggplot2's default colour scales
#'
#' Clears the `ggplot2.discrete.*`, `ggplot2.continuous.*` and
#' `ggplot2.binned.*` options set by `use_<palette>_palette()`, such as
#' [use_sienna_palette()].
#'
#' @return The previous option values, invisibly.
#' @export
#' @examples
#' use_sienna_palette()
#' reset_ggplot2_palette()
reset_ggplot2_palette <- function() {
  new <- rlang::rep_named(palette_ggplot2_options, list(NULL))
  invisible(options(new))
}
