<h3 align="center">paradoxR</h3>

<p align="center">Drop-in colour palettes for ggplot2 and faster GO and KEGG enrichment for clusterProfiler.</p>

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

## Enrichment

**Loading paradoxR makes clusterProfiler, DOSE and GOSemSim faster, with results identical to the original functions.** You do not change your code.

- **GO annotation cache:** `clusterProfiler::enrichGO()`, `clusterProfiler::gseGO()` and `clusterProfiler::groupGO()` build the GO annotation for each OrgDb, ontology and key type one time and keep it on disk. paradoxR reads the OrgDb with SQL, so this first build is 3 to 5 times faster.
- **Result reuse:** paradoxR keeps each `clusterProfiler::enrichGO()` result on disk. A repeated call, also in a new R session, returns in under a second instead of 10 to 30 seconds. A call with other p-value or q-value cutoffs returns at once.
- **simplify and Wang similarity:** `clusterProfiler::simplify()`, `GOSemSim::mgoSim()` and `enrichplot::pairwise_termsim(method = "Wang")` calculate all term pairs at one time, about 10 times faster.
- **Information content:** `GOSemSim::godata()` calculates information content for BP in about 5 seconds instead of 24.
- **KEGG download cache:** `clusterProfiler::enrichKEGG()`, `clusterProfiler::gseKEGG()`, `clusterProfiler::enrichMKEGG()` and `clusterProfiler::gseMKEGG()` download each KEGG table one time and keep it on disk for all species, key types and sessions. paradoxR compares the cache with the KEGG release dates at most one time each hour and downloads again when KEGG changes. When KEGG is not available, paradoxR uses the cached tables.

**Fallback to the original functions:** paradoxR replaces an upstream function only when the installed code is a version that paradoxR was verified against. For all other versions, the original function runs. When a replacement fails, the original function runs for that call and paradoxR shows one warning.

**Bioconductor 3.23:** the disk caches, `simplify()` and Wang similarity are sped up; information content and cutoff reuse currently use the original functions.

| Control | Effect |
|---|---|
| `reset_go_cache()` | Restores the original functions. |
| `use_go_cache()` | Applies the faster functions again. |
| `clear_go_cache()` | Deletes the GO and KEGG cache. |
| `options(paradoxR.go_cache = FALSE)` | Set before loading paradoxR to keep the original functions. |
| `options(paradoxR.go_cache_dir = )` | Sets the cache directory. The default is `tools::R_user_dir("paradoxR", "cache")`. |

```r
library(paradoxR)
if (requireNamespace("clusterProfiler", quietly = TRUE) && requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
  genes <- AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype = "ENTREZID")[1:300]
  clusterProfiler::enrichGO(genes, org.Hs.eg.db::org.Hs.eg.db, ont = "MF")
}
```

```r
library(paradoxR)
if (requireNamespace("clusterProfiler", quietly = TRUE) && requireNamespace("DOSE", quietly = TRUE)) {
  data(geneList, package = "DOSE")
  genes <- names(geneList)[abs(geneList) > 2]
  clusterProfiler::enrichKEGG(genes, organism = "hsa")
}
```

## License

Apache 2.0, see [LICENSE](LICENSE).
