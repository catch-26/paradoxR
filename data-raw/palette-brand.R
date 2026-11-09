# Regenerates R/palette-brand-data.R from the 24 brand swatches.
# Run from the package root: source("data-raw/palette-brand.R")
# Needs colorspace; the package itself does not.

brand_colours <- c(
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

brand_l <- 63
brand_c <- 60
brand_harmony <- 0.35
brand_neutral_names <- c("brown", "grey", "taupe")
brand_wheel_names <- c(
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

harmonise <- function(x, l = brand_l, c = brand_c, strength = brand_harmony) {
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

seq_colours <- function(hues, l = c(93, 28), chroma = c(10, brand_c, 32), peak = 0.6, n = 9) {
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

div_colours <- function(h_low, h_high, l = c(45, 96), chroma = brand_c, c_mid = 5, h_mid = brand_h[["taupe"]], n = 5) {
  c_end <- min(chroma, max_chroma(h_low, l[1]), max_chroma(h_high, l[1]))
  t <- seq(1, 0, length.out = n)[-n]
  ll <- l[2] - (l[2] - l[1]) * t^1.3
  cc <- c_mid + (c_end - c_mid) * t
  c(hcl_to_hex(h_low, cc, ll), hcl_to_hex(h_mid, c_mid, l[2]), rev(hcl_to_hex(h_high, cc, ll)))
}

brand_h <- hex_to_hcl(brand_colours)[, "H"] |>
  rlang::set_names(names(brand_colours))

brand_colours_harmonised <- c(
  harmonise(brand_colours[setdiff(names(brand_colours), brand_neutral_names)]),
  brown = hcl_to_hex(brand_h[["brown"]], 22, 50),
  grey  = hcl_to_hex(0, 0, brand_l),
  taupe = hcl_to_hex(brand_h[["taupe"]], 12, 74)
)[names(brand_colours)]

brand_wheel <- brand_h[brand_wheel_names]

brand_gradients <- list(
  sunset   = seq_colours(brand_h[c("yellow", "amber", "orange", "red", "raspberry")]),
  ember    = seq_colours(brand_h[c("mustard", "orange", "vermilion", "red")]),
  forest   = seq_colours(brand_h[c("lime", "leaf", "mint", "teal")]),
  ocean    = seq_colours(brand_h[c("mint", "teal", "sky", "blue", "indigo")]),
  dusk     = seq_colours(brand_h[c("blue", "lavender", "purple", "raspberry")]),
  berry    = seq_colours(brand_h[c("salmon", "rose", "raspberry", "purple")]),
  spectral = hcl_to_hex(seq(brand_h[["red"]], brand_h[["purple"]], length.out = 9), brand_c, brand_l),
  earth    = seq_colours(brand_h[c("taupe", "brown")], l = c(88, 30), chroma = c(6, 22, 8))
)

brand_diverging <- list(
  red_blue     = div_colours(brand_h[["red"]], brand_h[["blue"]]),
  orange_teal  = div_colours(brand_h[["orange"]], brand_h[["teal"]]),
  purple_green = div_colours(brand_h[["purple"]], brand_h[["leaf"]]),
  rose_sky     = div_colours(brand_h[["rose"]], brand_h[["sky"]])
)

brand_ramps <- names(brand_colours) |>
  rlang::set_names() |>
  purrr::map(\(colour) {
    if (colour == "grey") {
      seq_colours(0, chroma = c(0, 0, 0))
    } else if (colour %in% brand_neutral_names) {
      seq_colours(brand_h[[colour]], chroma = c(4, 18, 8))
    } else {
      seq_colours(brand_h[[colour]])
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
  "# Generated by data-raw/palette-brand.R. Do not edit by hand.",
  "",
  format_vector("brand_colours", brand_colours),
  format_vector("brand_colours_harmonised", brand_colours_harmonised),
  format_vector("brand_wheel_hues", brand_wheel),
  format_list("brand_gradients", brand_gradients),
  format_list("brand_diverging", brand_diverging),
  format_list("brand_ramps", brand_ramps)
) |>
  utils::head(-1) |>
  writeLines("R/palette-brand-data.R")
