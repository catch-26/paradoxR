<h3 align="center">paradoxR</h3>

<p align="center">Drop-in colour palettes and scales for ggplot2.</p>

## Install

```r
pak::pak("catch-26/paradoxR")
```

Requires R 4.1 or newer.

## Palettes

### Sienna

**One call swaps every ggplot2 default.** `use_sienna_palette()` sets ggplot2's `ggplot2.discrete.*`, `ggplot2.continuous.*` and `ggplot2.binned.*` options; `reset_ggplot2_palette()` switches back.

```r
library(paradoxR)
use_sienna_palette(continuous = "ocean")
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) + ggplot2::geom_point()
ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) + ggplot2::geom_raster() + scale_fill_sienna_c("teal", reverse = TRUE)
reset_ggplot2_palette()
```

| ggplot2 default | sienna replacement |
|---|---|
| `scales::hue_pal()(n)` | `sienna_pal()(n)` |
| `scales::hue_pal(h.start, direction)` | `sienna_pal(h_start = 18, direction = 1, harmonise = TRUE)` |
| `ggplot2::scale_colour_discrete()` | `scale_colour_sienna_d()` |
| `ggplot2::scale_fill_continuous()` | `scale_fill_sienna_c("ocean")` |
| `ggplot2::scale_fill_binned()` | `scale_fill_sienna_b("ocean")` |
| `ggplot2::scale_fill_gradient2(midpoint = m)` | `scale_fill_sienna_div("red_blue", midpoint = m)` |

**Palette names:** sequential `sunset`, `ember`, `forest`, `ocean`, `dusk`, `berry`, `earth`; hue `spectral`; diverging `red_blue`, `orange_teal`, `purple_green`, `rose_sky`; any swatch name in `sienna_colours` gives a single-hue ramp.

**Sequential gradients run light (low) to dark (high)**, the opposite of ggplot2's default; `reverse = TRUE` matches ggplot2.

**Regenerating the colours:** the hex codes are stored as literals in `R/palette-sienna-data.R`. `source("data-raw/palette-sienna.R")` rebuilds them from the 24 swatches with colorspace, dplyr and purrr.

## License

Apache 2.0, see [LICENSE](LICENSE).
