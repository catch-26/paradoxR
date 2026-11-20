# Regenerates R/palette-sienna-data.R from the 24 sienna swatches.
# Run from the package root: source("data-raw/palette-sienna.R")
# Needs colorspace; the package itself does not.

sienna_colours <- c(
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

sienna_l <- 63
sienna_c <- 60
sienna_harmony <- 0.35
sienna_neutral_names <- c("brown", "grey", "taupe")
sienna_wheel_names <- c(
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

harmonise <- function(x, l = sienna_l, c = sienna_c, strength = sienna_harmony) {
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

seq_colours <- function(hues, l = c(97, 20), chroma = c(22, 72, 38), peak = 0.5, n = 9) {
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

div_colours <- function(h_low, h_high, l = c(32, 97), chroma = 80, c_mid = 5, h_mid = sienna_h[["taupe"]], n = 5) {
  t <- seq(1, 0, length.out = n)[-n]
  ll <- l[2] - (l[2] - l[1]) * t^1.3
  cc <- c_mid + (chroma - c_mid) * t^0.8
  c(hcl_to_hex(h_low, cc, ll), hcl_to_hex(h_mid, c_mid, l[2]), rev(hcl_to_hex(h_high, cc, ll)))
}

sienna_h <- hex_to_hcl(sienna_colours)[, "H"] |>
  rlang::set_names(names(sienna_colours))

sienna_colours_harmonised <- c(
  harmonise(sienna_colours[setdiff(names(sienna_colours), sienna_neutral_names)]),
  brown = hcl_to_hex(sienna_h[["brown"]], 22, 50),
  grey  = hcl_to_hex(0, 0, sienna_l),
  taupe = hcl_to_hex(sienna_h[["taupe"]], 12, 74)
)[names(sienna_colours)]

sienna_wheel <- sienna_h[sienna_wheel_names]

sienna_gradients <- list(
  sunset = seq_colours(sienna_h[c("yellow", "amber", "orange", "red", "raspberry")], l = c(96, 24), chroma = c(45, 90, 45), peak = 0.45),
  ember  = seq_colours(sienna_h[c("mustard", "orange", "vermilion", "red")], chroma = c(18, 55, 30)),
  forest = seq_colours(sienna_h[c("lime", "leaf", "mint", "teal")]),
  ocean  = seq_colours(sienna_h[c("mint", "teal", "sky", "blue", "indigo")], chroma = c(20, 58, 38)),
  dusk   = seq_colours(sienna_h[c("blue", "lavender", "purple", "raspberry")]),
  berry  = seq_colours(sienna_h[c("salmon", "rose", "raspberry", "purple")]),
  earth  = seq_colours(sienna_h[c("taupe", "brown")], l = c(92, 22), chroma = c(10, 28, 10))
)

sienna_diverging <- list(
  red_blue     = div_colours(sienna_h[["red"]], sienna_h[["blue"]]),
  orange_teal  = div_colours(sienna_h[["amber"]], mean(sienna_h[c("teal", "sky")]), l = c(35, 97)),
  purple_green = div_colours(sienna_h[["purple"]], sienna_h[["leaf"]]),
  rose_sky     = div_colours(sienna_h[["rose"]], sienna_h[["sky"]])
)

sienna_ramps <- names(sienna_colours) |>
  rlang::set_names() |>
  purrr::map(\(colour) {
    if (colour == "grey") {
      seq_colours(0, chroma = c(0, 0, 0))
    } else if (colour %in% sienna_neutral_names) {
      seq_colours(sienna_h[[colour]], chroma = c(4, 18, 8))
    } else {
      seq_colours(sienna_h[[colour]])
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
  "# Generated by data-raw/palette-sienna.R. Do not edit by hand.",
  "",
  format_vector("sienna_colours", sienna_colours),
  format_vector("sienna_colours_harmonised", sienna_colours_harmonised),
  format_vector("sienna_wheel_hues", sienna_wheel),
  format_list("sienna_gradients", sienna_gradients),
  format_list("sienna_diverging", sienna_diverging),
  format_list("sienna_ramps", sienna_ramps)
) |>
  utils::head(-1) |>
  writeLines("R/palette-sienna-data.R")
