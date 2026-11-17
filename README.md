<h3 align="center">paradoxR</h3>

<p align="center">Drop-in colour palettes for ggplot2, faster GO and KEGG enrichment for clusterProfiler, and faster DESeq2 and sva.</p>

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

- **GO annotation:** `clusterProfiler::enrichGO()`, `clusterProfiler::gseGO()` and `clusterProfiler::groupGO()` build the GO annotation for each OrgDb, ontology and key type one time and keep it on disk. paradoxR reads the OrgDb with SQL, so this build is faster.
- **Result reuse:** paradoxR keeps each `clusterProfiler::enrichGO()` result on disk. A call with other p-value or q-value cutoffs returns at once.
- **simplify and Wang similarity:** `clusterProfiler::simplify()`, `GOSemSim::mgoSim()` and `enrichplot::pairwise_termsim(method = "Wang")` calculate all term pairs at one time.
- **Information content:** `GOSemSim::godata()` calculates information content in one vectorised step.
- **KEGG download cache:** `clusterProfiler::enrichKEGG()`, `clusterProfiler::gseKEGG()`, `clusterProfiler::enrichMKEGG()` and `clusterProfiler::gseMKEGG()` download each KEGG table one time and keep it on disk for all species, key types and sessions. paradoxR compares the cache with the KEGG release dates at most one time each hour and downloads again when KEGG changes. When KEGG is not available, paradoxR uses the cached tables.

**First-call times** in a new R session with an empty cache, original function against paradoxR. The results are identical. Setup: Bioconductor 3.23, R 4.6.2, clusterProfiler 4.20.3, GOSemSim 2.38.3, org.Hs.eg.db, 315 Entrez IDs, 732 BP terms in the `enrichGO()` result. `DESeq2::DESeq()`: DESeq2 1.52.0, 30,000 simulated genes, 24 samples in 8 groups of 3.

| Call | Original | paradoxR | Speed-up |
|---|---|---|---|
| `clusterProfiler::enrichGO(ont = "MF")` | 7.97 s | 2.64 s | 3x |
| `clusterProfiler::enrichGO(ont = "BP")` | 19.49 s | 6.02 s | 3.2x |
| `clusterProfiler::enrichGO(ont = "ALL")` | 25.52 s | 8.65 s | 3x |
| `clusterProfiler::enrichGO(ont = "BP")`, second gene list | 2.03 s | 0.46 s | 4.4x |
| `clusterProfiler::simplify()` | 51.24 s | 2.11 s | 24x |
| `GOSemSim::mgoSim(measure = "Wang")` | 29.99 s | 0.69 s | 43x |
| `enrichplot::pairwise_termsim(method = "Wang")` | 1.97 s | 0.13 s | 15x |
| `GOSemSim::godata(ont = "BP", computeIC = TRUE)` | 25.25 s | 5.80 s | 4.4x |
| `DESeq2::DESeq()`, design `~ batch + group` | 26.81 s | 12.81 s | 2.1x |

**Fallback to the original functions:** paradoxR replaces an upstream function only when the installed code is a version that paradoxR was verified against. For all other versions, the original function runs. When a replacement fails, the original function runs for that call and paradoxR shows one warning.

| Control | Effect |
|---|---|
| `reset_go_cache()` | Restores the original functions. |
| `use_go_cache()` | Applies the faster functions again. |
| `clear_go_cache()` | Deletes the GO and KEGG cache. |
| `options(paradoxR.go_cache = FALSE)` | Set before loading paradoxR to keep the original functions. |
| `options(paradoxR.go_cache_dir = )` | Sets the cache directory. The default is `tools::R_user_dir("paradoxR", "cache")`. |

```r
library(paradoxR)
genes <- AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype = "ENTREZID")[1:300]
ego <- clusterProfiler::enrichGO(genes, org.Hs.eg.db::org.Hs.eg.db, ont = "BP")
ego_simple <- clusterProfiler::simplify(ego)
```

```r
library(paradoxR)
data(geneList, package = "DOSE")
genes <- names(geneList)[abs(geneList) > 2]
kk <- clusterProfiler::enrichKEGG(genes, organism = "hsa")
```

## RNA-seq

**Loading paradoxR also makes DESeq2 and sva faster, with results identical to the original functions.** You do not change your code.

- **DESeq2:** `DESeq2::DESeq()` calculates the trimmed means for Cook's distances with one sort for each matrix instead of one call for each gene. The speed-up is largest with few samples in each group of the design. paradoxR uses a gene's value only where it is provably equal to the original; for all other genes, the original calculation runs.
- **sva:** `sva::ComBat_seq()` adjusts all counts in one vectorised step instead of one call for each count, about 1.5 to 1.8 times faster.

| Control | Effect |
|---|---|
| `options(paradoxR.deseq2 = FALSE)` | Set before loading DESeq2 to keep the original DESeq2 functions. |
| `options(paradoxR.sva = FALSE)` | Set before loading sva to keep the original sva function. |

`reset_go_cache()` and `use_go_cache()` also restore and apply these replacements.

## License

Apache 2.0, see [LICENSE](LICENSE).
