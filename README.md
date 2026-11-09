<h3 align="center">paradoxR</h3>

<p align="center">Mods for existing R tools, so a specific job takes one call instead of a page of glue code.</p>

## Install

```r
pak::pak("catch-26/paradoxR")
```

Requires R 4.1 or newer.

## Mods

Each mod is either a **drop-in replacement** (same call shape as the tool it replaces) or a **thin wrapper** (data in, one specific plot out).

| Mod | Kind | Files |
|---|---|---|
| **Palette family: brand** | Drop-in replacement | `R/palette-brand.R`, `R/palette-brand-data.R` |

### Brand palette

**One call swaps every ggplot2 default.** `use_brand_palette()` sets ggplot2's `ggplot2.discrete.*`, `ggplot2.continuous.*` and `ggplot2.binned.*` options; `reset_ggplot2_palette()` switches back.

```r
library(paradoxR)
use_brand_palette(continuous = "ocean")
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) + ggplot2::geom_point()
ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) + ggplot2::geom_raster() + scale_fill_brand_c("teal", reverse = TRUE)
reset_ggplot2_palette()
```

| ggplot2 default | brand replacement |
|---|---|
| `scales::hue_pal()(n)` | `brand_pal()(n)` |
| `scales::hue_pal(h.start, direction)` | `brand_pal(h_start = 18, direction = 1, harmonise = TRUE)` |
| `ggplot2::scale_colour_discrete()` | `scale_colour_brand_d()` |
| `ggplot2::scale_fill_continuous()` | `scale_fill_brand_c("ocean")` |
| `ggplot2::scale_fill_binned()` | `scale_fill_brand_b("ocean")` |
| `ggplot2::scale_fill_gradient2(midpoint = m)` | `scale_fill_brand_div("red_blue", midpoint = m)` |

**Palette names:** sequential `sunset`, `ember`, `forest`, `ocean`, `dusk`, `berry`, `earth`; hue `spectral`; diverging `red_blue`, `orange_teal`, `purple_green`, `rose_sky`; any swatch name in `brand_colours` gives a single-hue ramp.

**Sequential gradients run light (low) to dark (high)**, the opposite of ggplot2's default; `reverse = TRUE` matches ggplot2.

**Regenerating the colours:** the hex codes are stored as literals in `R/palette-brand-data.R`. `source("data-raw/palette-brand.R")` rebuilds them from the 24 swatches with colorspace, dplyr and purrr.

## Adding a mod

1. **Name the kind:** drop-in replacement or thin wrapper.
2. **Create the files:** `usethis::use_r("<mod>-<part>")`.
3. **Declare dependencies:** `usethis::use_package("<pkg>", "Suggests")`, then `rlang::check_installed()` at the top of each function that uses it.
4. **Route version quirks through `R/compat.R`**, never inline in the mod.
5. **Test it:** `usethis::use_test("<mod>-<part>")`, with `testthat::skip_if_not_installed("<pkg>")` for Suggests packages.
6. **Pass `devtools::check()`** with no new notes, and add one example to this README.

## License

Apache 2.0, see [LICENSE](LICENSE).
