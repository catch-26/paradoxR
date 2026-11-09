# Regenerates R/palette-ember-data.R from the 24 ember swatches.
# Run from the package root: source("data-raw/palette-ember.R")
# Needs colorspace; the package itself does not.

ember_colours <- c(
  raspberry  = "#C05476",
  rose       = "#D85675",
  salmon     = "#D6837A",
  red        = "#DA5234",
  vermilion  = "#E3683E",
  orange     = "#DD7835",
  amber      = "#E0963C",
  yellow     = "#E7BA51",
  mustard    = "#D8BE5E",
  lime       = "#BCC256",
  leaf       = "#85AD59",
  forest     = "#489160",
  mint       = "#55B080",
  teal       = "#429A8E",
  sky        = "#4B99D2",
  blue       = "#668BE1",
  periwinkle = "#828BC2",
  indigo     = "#6E72C3",
  lavender   = "#AE9CCE",
  mauve      = "#A479B1",
  purple     = "#A75ABA",
  brown      = "#957367",
  grey       = "#7C7C7C",
  taupe      = "#A5998C"
)

ember_l <- 63
ember_c <- 60
ember_harmony <- 0.35
ember_neutral_names <- c("brown", "grey", "taupe")
ember_wheel_names <- c(
  "raspberry", "red", "orange", "amber", "yellow", "lime", "leaf",
  "mint", "teal", "sky", "blue", "lavender", "purple"
)

hex_to_hcl <- function(x) {
  colorspace::hex2RGB(x) |>
    methods::as("polarLUV") |>
    colorspace::coords()
}

hcl_to_hex <- function(h, c, l) {
  n <- max(length(h), length(c), length(l))
  h <- rep_len(h, n) %% 360
  c <- pmax(rep_len(c, n), 0)
  l <- pmin(pmax(rep_len(l, n), 0), 100)
  out <- colorspace::hex(colorspace::polarLUV(L = l, C = c, H = h), fixup = FALSE)
  while (anyNA(out)) {
    bad <- is.na(out)
    c[bad] <- pmax(c[bad] - 0.5, 0)
    out[bad] <- colorspace::hex(colorspace::polarLUV(L = l[bad], C = c[bad], H = h[bad]), fixup = c[bad] == 0)
  }
  out
}

max_chroma <- function(h, l) {
  cs <- seq(0, 150, by = 0.5)
  ok <- !is.na(colorspace::hex(colorspace::polarLUV(L = l, C = cs, H = h %% 360), fixup = FALSE))
  max(cs[ok])
}

harmonise <- function(x, l = ember_l, c = ember_c, strength = ember_harmony) {
  hcl <- hex_to_hcl(x)
  hcl_to_hex(hcl[, "H"], c + strength * (hcl[, "C"] - c), l + strength * (hcl[, "L"] - l)) |>
    rlang::set_names(names(x))
}

unwrap_hue <- function(h) {
  if (length(h) < 2) {
    return(h)
  }
  out <- h
  for (i in 2:length(h)) {
    out[i] <- out[i - 1] + ((h[i] - out[i - 1] + 180) %% 360 - 180)
  }
  out
}

seq_colours <- function(hues, l = c(93, 28), chroma = c(10, ember_c, 32), peak = 0.6, n = 9) {
  t <- seq(0, 1, length.out = n)
  h <- unwrap_hue(unname(hues))
  hh <- if (length(h) == 1) rep(h, n) else stats::approx(seq(0, 1, length.out = length(h)), h, t)$y
  ll <- l[1] + (l[2] - l[1]) * t
  cc <- dplyr::if_else(
    t <= peak,
    chroma[1] + (chroma[2] - chroma[1]) * t / peak,
    chroma[2] + (chroma[3] - chroma[2]) * (t - peak) / (1 - peak)
  )
  hcl_to_hex(hh, cc, ll)
}

div_colours <- function(h_low, h_high, l = c(45, 96), chroma = ember_c, c_mid = 5, h_mid = ember_h[["taupe"]], n = 5) {
  c_end <- min(chroma, max_chroma(h_low, l[1]), max_chroma(h_high, l[1]))
  t <- seq(1, 0, length.out = n)[-n]
  ll <- l[2] - (l[2] - l[1]) * t^1.3
  cc <- c_mid + (c_end - c_mid) * t
  c(hcl_to_hex(h_low, cc, ll), hcl_to_hex(h_mid, c_mid, l[2]), rev(hcl_to_hex(h_high, cc, ll)))
}

ember_h <- hex_to_hcl(ember_colours)[, "H"] |>
  rlang::set_names(names(ember_colours))

ember_colours_harmonised <- c(
  harmonise(ember_colours[setdiff(names(ember_colours), ember_neutral_names)]),
  brown = hcl_to_hex(ember_h[["brown"]], 22, 50),
  grey  = hcl_to_hex(0, 0, ember_l),
  taupe = hcl_to_hex(ember_h[["taupe"]], 12, 74)
)[names(ember_colours)]

ember_wheel <- ember_h[ember_wheel_names]

ember_gradients <- list(
  sunset   = seq_colours(ember_h[c("yellow", "amber", "orange", "red", "raspberry")]),
  cinder   = seq_colours(ember_h[c("mustard", "orange", "vermilion", "red")]),
  forest   = seq_colours(ember_h[c("lime", "leaf", "mint", "teal")]),
  ocean    = seq_colours(ember_h[c("mint", "teal", "sky", "blue", "indigo")]),
  dusk     = seq_colours(ember_h[c("blue", "lavender", "purple", "raspberry")]),
  berry    = seq_colours(ember_h[c("salmon", "rose", "raspberry", "purple")]),
  spectral = hcl_to_hex(seq(ember_h[["red"]], ember_h[["purple"]], length.out = 9), ember_c, ember_l),
  earth    = seq_colours(ember_h[c("taupe", "brown")], l = c(88, 30), chroma = c(6, 22, 8))
)

ember_diverging <- list(
  red_blue     = div_colours(ember_h[["red"]], ember_h[["blue"]]),
  orange_teal  = div_colours(ember_h[["orange"]], ember_h[["teal"]]),
  purple_green = div_colours(ember_h[["purple"]], ember_h[["leaf"]]),
  rose_sky     = div_colours(ember_h[["rose"]], ember_h[["sky"]])
)

ember_ramps <- names(ember_colours) |>
  rlang::set_names() |>
  purrr::map(\(colour) {
    if (colour == "grey") {
      seq_colours(0, chroma = c(0, 0, 0))
    } else if (colour %in% ember_neutral_names) {
      seq_colours(ember_h[[colour]], chroma = c(4, 18, 8))
    } else {
      seq_colours(ember_h[[colour]])
    }
  })

format_value <- function(x) {
  if (is.character(x)) {
    paste0("\"", x, "\"")
  } else {
    format(x, digits = 17)
  }
}

format_vector <- function(name, x) {
  keys <- format(names(x))
  body <- paste0("  ", keys, " = ", format_value(x), c(rep(",", length(x) - 1), ""))
  c(paste(name, "<- c("), body, ")", "")
}

format_list <- function(name, x) {
  keys <- format(names(x))
  body <- purrr::map_chr(x, \(stops) paste0("c(", paste(format_value(unname(stops)), collapse = ", "), ")"))
  body <- paste0("  ", keys, " = ", body, c(rep(",", length(x) - 1), ""))
  c(paste(name, "<- list("), body, ")", "")
}

c(
  "# Generated by data-raw/palette-ember.R. Do not edit by hand.",
  "",
  format_vector("ember_colours", ember_colours),
  format_vector("ember_colours_harmonised", ember_colours_harmonised),
  format_vector("ember_wheel_hues", ember_wheel),
  format_list("ember_gradients", ember_gradients),
  format_list("ember_diverging", ember_diverging),
  format_list("ember_ramps", ember_ramps)
) |>
  utils::head(-1) |>
  writeLines("R/palette-ember-data.R")
