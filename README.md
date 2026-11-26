<h3 align="center">paradoxR</h3>

<p align="center">Drop-in colour palettes for ggplot2, faster GO and KEGG enrichment with clusterProfiler, and faster DESeq2 and sva.</p>

## Install

```r
pak::repo_add(catch26 = "https://catch-26.r-universe.dev")
pak::pak("paradoxR")
```

Windows and macOS get a prebuilt binary, no Rtools or Xcode needed. The development version from GitHub needs Rtools on Windows:

```r
pak::pak("catch-26/paradoxR")
```

Requires R 4.1 or newer.

## Palettes

### Sienna

**One call swaps every ggplot2 default.** `use_sienna_palette()` sets ggplot2's `ggplot2.discrete.*`, `ggplot2.continuous.*`, `ggplot2.binned.*` and `ggplot2.ordinal.*` options; `reset_ggplot2_palette()` switches back.

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
| `ggplot2::scale_fill_continuous()`, `ggplot2::scale_fill_gradient()` | `scale_fill_sienna_c("ocean")` |
| `ggplot2::scale_fill_binned()` | `scale_fill_sienna_b("ocean")` |
| `ggplot2::scale_fill_ordinal()` | `scale_fill_sienna_o("ocean")` |
| `ggplot2::scale_fill_gradient2(midpoint = m)` | `scale_fill_sienna_div("red_blue", midpoint = m)` |

**Palette names:** sequential `sunset`, `ember`, `forest`, `ocean`, `dusk`, `berry`, `earth`; diverging `red_blue`, `orange_teal`, `purple_green`, `rose_sky`; any swatch name in `sienna_colours` gives a single-hue ramp.

**Sequential gradients run light (low) to dark (high)**, the opposite of ggplot2's default; `reverse = TRUE` matches ggplot2.

**Points and lines skip the palest colours.** Colour and ordinal scales use the gradient from `begin = 0.2`, so low values stay visible on white and grey panels; fill scales use the whole gradient. `begin` and `end` pick any part of a gradient.

**Regenerating the colours:** the hex codes are stored as literals in `R/palette-sienna-data.R`. `source("data-raw/palette-sienna.R")` rebuilds them from the 24 swatches with colorspace, dplyr and purrr.

## Enrichment

**Loading paradoxR makes clusterProfiler, DOSE and GOSemSim faster, with results identical to the original functions.** You do not change your code.

- **GO annotation:** `clusterProfiler::enrichGO()`, `clusterProfiler::gseGO()` and `clusterProfiler::groupGO()` query the GO-to-gene table for each OrgDb, ontology and key type one time and keep it on disk. paradoxR reads the OrgDb with SQL, so this query is faster. A new R session reads the table from disk and builds the rest of the GO annotation as the original does.
- **simplify and Wang similarity:** `clusterProfiler::simplify()`, `GOSemSim::mgoSim()` and `enrichplot::pairwise_termsim(method = "Wang")` calculate all term pairs at one time.
- **Information content:** `GOSemSim::godata()` calculates information content in one vectorised step.
- **KEGG download cache:** `clusterProfiler::enrichKEGG()`, `clusterProfiler::gseKEGG()`, `clusterProfiler::enrichMKEGG()` and `clusterProfiler::gseMKEGG()` download each KEGG table one time and keep it on disk for all species, key types and sessions. paradoxR compares the cache with the KEGG release dates at most one time each hour and downloads again when KEGG changes. When KEGG is not available, paradoxR uses the cached tables.

**First-call times** in a new R session with an empty cache, original function against paradoxR. The results are identical. Median of 3 runs. Setup: Bioconductor 3.23, R 4.6.2, clusterProfiler 4.20.3, GOSemSim 2.38.3, org.Hs.eg.db, 315 Entrez IDs, 732 BP terms in the `enrichGO()` result. Mouse row: org.Mm.eg.db, 398 Ensembl IDs, 905 BP terms.

| Call | Original | paradoxR | Speed-up |
|---|---|---|---|
| `clusterProfiler::enrichGO(ont = "MF")` | 8.88 s | 3.01 s | 3x |
| `clusterProfiler::enrichGO(ont = "BP")` | 20.74 s | 7.00 s | 3x |
| `clusterProfiler::enrichGO(ont = "ALL")` | 32.76 s | 9.87 s | 3.3x |
| `clusterProfiler::enrichGO(ont = "BP", keyType = "ENSEMBL", readable = TRUE)`, org.Mm.eg.db | 28.60 s | 9.75 s | 2.9x |
| `clusterProfiler::simplify()` | 62.44 s | 3.81 s | 16x |
| `GOSemSim::mgoSim(measure = "Wang")` | 34.91 s | 1.46 s | 24x |
| `enrichplot::pairwise_termsim(method = "Wang")` | 2.52 s | 0.18 s | 14x |
| `GOSemSim::godata(ont = "BP", computeIC = TRUE)` | 32.52 s | 7.05 s | 4.6x |

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

**Loading paradoxR makes DESeq2 and sva faster, with results identical to the original functions.** You do not change your code.

- **DESeq2:** `DESeq2::DESeq()` calculates the trimmed means for Cook's distances with one sort for each matrix instead of one call for each gene. The speed-up is largest with few samples in each group of the design. paradoxR uses a gene's value only where it is provably equal to the original; for all other genes, the original calculation runs.
- **sva:** `sva::ComBat_seq()` adjusts all counts in one vectorised step instead of one call for each count.

**First-call times** in a new R session, original function against paradoxR, median of 5 runs. The results are identical. Setup: Bioconductor 3.23, R 4.6.2, DESeq2 1.52.0, sva 3.60.0, 30,000 simulated genes; 24 samples for `DESeq()`, 12 samples in 2 batches for `ComBat_seq()`.

| Call | Original | paradoxR | Speed-up |
|---|---|---|---|
| `DESeq2::DESeq()`, `~ batch + group`, 8 groups of 3 samples | 16.54 s | 8.47 s | 2x |
| `DESeq2::DESeq()`, `~ batch + group`, 4 groups of 6 samples | 10.03 s | 6.15 s | 1.6x |
| `DESeq2::DESeq()`, `~ group`, 2 groups of 12 samples | 7.90 s | 5.89 s | 1.3x |
| `DESeq2::DESeq()`, `~ x`, continuous covariate | 6.42 s | 5.49 s | 1.2x |
| `sva::ComBat_seq(group = )` | 4.48 s | 2.50 s | 1.8x |
| `sva::ComBat_seq()`, no group | 3.62 s | 2.01 s | 1.8x |
| `sva::ComBat_seq(group = , covar_mod = )`, 3 batches | 8.62 s | 7.67 s | 1.1x |

## Stored results

**Loading paradoxR stores the results of slow analysis steps, so running a script or rendering a Quarto document again skips them when nothing they depend on has changed.** You do not change your code.

- **Covered calls:**
  - single-cell: `Seurat::FindAllMarkers()`, `RunPCA()`, `FindNeighbors()`, `RunUMAP()`, `SCTransform()` and `IntegrateLayers()` with `CCAIntegration`, `RPCAIntegration`, `JointPCAIntegration` or `HarmonyIntegration`, and `Signac::FeatureMatrix()`;
  - ChIP-seq and ATAC-seq: `DiffBind::dba.count()`, `csaw::windowCounts()`, `csaw::regionCounts()`, `GenomicAlignments::summarizeOverlaps()` on BAM files, `ChIPseeker::getTagMatrix()` and `EnrichedHeatmap::normalizeToMatrix()`;
  - bulk RNA-seq and general statistics: `GSVA::gsva()` on a dense matrix, `sva::ComBat_seq()`, `DESeq2::lfcShrink()`, `stats::hclust()` on 5,000 or more objects and `MASS::kde2d()` on large inputs.
- **When a stored result is used:** only when the argument values, the content of every input file (BAM files and their indexes, fragment files, peak files and annotation databases), the installed versions of the package and everything it depends on, the R build and the relevant options all match. Otherwise the original function runs. Seurat functions are keyed only by the data they read, so adding metadata or another reduction to an object does not count as a change. Functions with their own seed argument, such as `RunUMAP(seed.use = )`, reuse a result whatever the random state before the call. A stored result is replayed exactly as the original call behaved: printed output, progress text, messages and warnings in their original order, whether the result was printed, option changes, the packages the call loaded, and the random number state. Functions whose stock calls leave other traces, such as `ChIPseeker::annotatePeak()` filling its own annotation cache, are not stored.
- **What is stored:** calls that took at least 2 seconds and at least 5 times as long as storing their result. Nothing is stored when the cache directory is on a network drive.

## Speed-up settings

**Speed-ups apply on load.** `library(paradoxR)` replaces functions in clusterProfiler, DOSE, GOSemSim, DESeq2, sva, Seurat, Signac, DiffBind, csaw, GenomicAlignments, ChIPseeker, EnrichedHeatmap, GSVA, stats and MASS, and packages loaded later are sped up as they load.

**Fallback to the original functions:** paradoxR replaces an upstream function only when the installed code is a version that paradoxR was verified against. For all other versions, the original function runs. When a replacement fails, the original function runs for that call and paradoxR shows one warning.

**Compare against the original code:** `reset_speedups()` restores the original functions of one package, or of all packages when called without a name. The package stays original for the rest of the session until `use_speedups()` applies the speed-ups again.

```r
library(paradoxR)
set.seed(1)
dds <- DESeq2::makeExampleDESeqDataSet(n = 2000, m = 6)
fast <- DESeq2::results(DESeq2::DESeq(dds, quiet = TRUE))
reset_speedups("DESeq2")
original <- DESeq2::results(DESeq2::DESeq(dds, quiet = TRUE))
identical(fast, original)
use_speedups("DESeq2")
```

| Call | Effect |
|---|---|
| `reset_speedups("DESeq2")` | Restores the original functions of one package: `"clusterProfiler"`, `"DOSE"`, `"GOSemSim"`, `"DESeq2"`, `"sva"`, `"Seurat"`, `"Signac"`, `"DiffBind"`, `"csaw"`, `"GenomicAlignments"`, `"ChIPseeker"`, `"EnrichedHeatmap"`, `"GSVA"`, `"stats"` or `"MASS"`. |
| `reset_speedups()` | Restores the original functions of all packages. |
| `use_speedups("DESeq2")`, `use_speedups()` | Applies the speed-ups again, for one package or all packages. |
| `clear_cache()` | Deletes the GO and KEGG cache and the stored results. |
| `options(paradoxR.cache_dir = )` | Sets the cache directory, see below. The default is `tools::R_user_dir("paradoxR", "cache")`, the user's local cache folder. |
| `options(paradoxR.result_cache = FALSE)` | Stops storing and reusing results of slow analysis steps. |
| `options(paradoxR.result_cache_min_seconds = )` | Stores only calls that took at least this long. The default is 2 seconds. |
| `options(paradoxR.result_cache_min_ratio = )` | Stores only calls that took at least this many times as long as storing their result. The default is 5. |
| `options(paradoxR.cache_max_age_days = )` | Deletes cached items unused for this many days. The default is 7. Cleanup runs whenever paradoxR or a package it speeds up is loaded or uses the cache, also with the speed-ups off. |
| `options(paradoxR.cache_min_free_pct = , paradoxR.cache_min_free_gb = )` | Free disk space the cache always leaves: 15% of the disk or 15 GB, whichever is more. The least recently used items are deleted first; if there is still not enough room, the result is returned without being stored. |

To start a session with the original functions, see `?use_speedups`.

**Cache location:** to keep the cache in another folder in every session, set `paradoxR.cache_dir` in `~/.Rprofile`. Use an absolute path; `~` is expanded. The setting applies to the GO and KEGG cache and the stored results, to the automatic cleanup and to `clear_cache()`, on Linux, macOS and Windows. paradoxR reads it at each cache access, so it can also be changed during a session.

```r
options(paradoxR.cache_dir = "~/paradoxR-cache")
```

Without this setting, the cache follows R's own user cache folder, which the environment variable `R_USER_CACHE_DIR` (for example in `~/.Renviron`) moves for every package that uses it.

## Update at startup

This command adds one line to `~/.Rprofile`. In each interactive R session, the line compares the installed paradoxR with the version on r-universe, installs the new version if there is one, and then loads paradoxR. When r-universe cannot be reached within 5 seconds, R starts with the installed version. Other lines in `~/.Rprofile` stay as they are, and running the command again replaces only its own line.

macOS and Linux (zsh or bash):

```sh
echo 'f <- "~/.Rprofile"; x <- if (file.exists(f)) readLines(f, warn = FALSE); writeLines(c(x[!grepl("catch-26/paradoxR|catch-26.r-universe.dev", x)], r"---(if (interactive()) local({ dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE); .libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths())); r <- "https://catch-26.r-universe.dev"; o <- options(timeout = 5); a <- suppressWarnings(utils::available.packages(utils::contrib.url(r, .Platform$pkgType))); options(o); v <- a[a[, "Package"] == "paradoxR", "Version"]; if (length(v) && (!nzchar(system.file(package = "paradoxR")) || utils::packageVersion("paradoxR") < v)) { if (!requireNamespace("pak", quietly = TRUE)) utils::install.packages("pak", repos = "https://cloud.r-project.org"); pak::repo_add(catch26 = r); pak::meta_update(); pak::pak("paradoxR", ask = FALSE) }; if (nzchar(system.file(package = "paradoxR"))) library(paradoxR) }))---"), f)' | Rscript --no-init-file -
```

Windows (PowerShell):

```powershell
'f <- "~/.Rprofile"; x <- if (file.exists(f)) readLines(f, warn = FALSE); writeLines(c(x[!grepl("catch-26/paradoxR|catch-26.r-universe.dev", x)], r"---(if (interactive()) local({ dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE); .libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths())); r <- "https://catch-26.r-universe.dev"; o <- options(timeout = 5); a <- suppressWarnings(utils::available.packages(utils::contrib.url(r, .Platform$pkgType))); options(o); v <- a[a[, "Package"] == "paradoxR", "Version"]; if (length(v) && (!nzchar(system.file(package = "paradoxR")) || utils::packageVersion("paradoxR") < v)) { if (!requireNamespace("pak", quietly = TRUE)) utils::install.packages("pak", repos = "https://cloud.r-project.org"); pak::repo_add(catch26 = r); pak::meta_update(); pak::pak("paradoxR", ask = FALSE) }; if (nzchar(system.file(package = "paradoxR"))) library(paradoxR) }))---"), f)' | & (Get-ChildItem "$env:ProgramFiles\R\R-*\bin\Rscript.exe" | Select-Object -Last 1).FullName --no-init-file -
```

## License

Apache 2.0, see [LICENSE](LICENSE).
