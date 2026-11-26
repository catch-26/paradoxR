# v26.11.5 (2026-11-26)

- Stored results: calls that take 2 seconds or more are stored when they take at least 5 times as long as storing the result (was 10 seconds), so `RunPCA()` and `RunUMAP()` on typical single-cell datasets are reused. Set the limits with `options(paradoxR.result_cache_min_seconds = )` and `options(paradoxR.result_cache_min_ratio = )`.

# v26.11.4 (2026-11-26)

- Stored results: qs2 is installed with paradoxR, so results are always stored and read in the faster qs2 format.

# v26.11.3 (2026-11-24)

- Stored results: installing paradoxR also updates ps to 1.8.0 or later when an older ps is installed, since older versions cannot measure free disk space and nothing was stored.

# v26.11.2 (2026-11-23)

- Install: a one-line command for zsh, bash or PowerShell adds a line to `~/.Rprofile` that updates paradoxR from r-universe when an R session starts and then loads it.
- Stored results now work after a plain install: ps and rlang 1.3.0 or later, which storing results needs, are installed with paradoxR instead of being optional.

# v26.11.1 (2026-11-23)

- Each change to paradoxR now raises its version number, so `pak::pak()`, `update.packages()` and r-universe install it as an update instead of rebuilding the same version.

# v26.11.0 (2026-11-09)

- New package paradoxR, installed with `pak::pak("catch-26/paradoxR")`.
- Palette family: the sienna palette replaces ggplot2's default colours. `use_sienna_palette()` switches every plot, and `reset_ggplot2_palette()` switches back.
- Palette family: `scale_colour_sienna_d()`, `_c()`, `_b()` and `_div()` and their fill forms replace ggplot2's discrete, continuous, binned and diverging colour scales.
- Enrichment: loading paradoxR makes `clusterProfiler::enrichGO()`, `gseGO()` and `groupGO()` keep the OrgDb's GO-to-gene table on disk, so a new R session reads it instead of querying the OrgDb again. `reset_speedups()` switches back and `clear_cache()` deletes the cache.
- Enrichment: with paradoxR loaded, `clusterProfiler::simplify()` and Wang similarity in GOSemSim and enrichplot run about 10 times faster with identical results.
- Enrichment: the first `enrichGO()`, `gseGO()` or `groupGO()` call for an OrgDb builds its GO annotation 3 to 5 times faster, with identical results.
- Enrichment: `GOSemSim::godata()` computes information content for BP in about 5 s instead of 24 s, with identical results.
- Enrichment: with paradoxR loaded, `clusterProfiler::enrichKEGG()`, `gseKEGG()`, `enrichMKEGG()` and `gseMKEGG()` keep their KEGG downloads on disk and refresh them only when KEGG releases an update, so later calls, other key types and new R sessions skip the download.
- Enrichment: if a sped-up function fails, the original runs instead.
- Enrichment: unloading paradoxR restores the original clusterProfiler, DOSE and GOSemSim functions, and reloading it in the same session no longer breaks `reset_speedups()`.
- Enrichment: the faster `simplify()` and Wang similarity also work with Bioconductor 3.23 (clusterProfiler 4.20, GOSemSim 2.38).
- Enrichment: `GOSemSim::godata(computeIC = TRUE)` on Bioconductor 3.23 calculates information content for BP in about 7 seconds instead of 30.
- Enrichment: on Bioconductor 3.23, the first `clusterProfiler::enrichGO()` call for an OrgDb builds the GO annotation with SQL, about 8 times faster for BP.
- Enrichment: the GO and KEGG cache works the same on Linux, macOS and Windows: a file another R session is reading no longer causes a warning, a damaged cache file is rebuilt, and `clear_cache()` also removes unfinished writes.
- RNA-seq: with paradoxR loaded, `DESeq2::DESeq()` computes the trimmed means behind Cook's distances with one sort per matrix instead of one call per gene, with identical results. `options(paradoxR.deseq2 = FALSE)` keeps the original.
- RNA-seq: with paradoxR loaded, `sva::ComBat_seq()` adjusts all counts in one vectorised step instead of one call per count, with identical results. `options(paradoxR.sva = FALSE)` keeps the original.
- Settings: `options(paradoxR.speedups = FALSE)` turns off every speed-up and `options(paradoxR.cache_dir = )` sets the cache directory. The older names `paradoxR.go_cache` and `paradoxR.go_cache_dir` still work.
- Settings: `use_speedups()` and `reset_speedups()` apply and remove every speed-up, and `clear_cache()` deletes the GO and KEGG cache.
- Settings: `reset_speedups("DESeq2")` restores the original functions of one package, for example to compare results with the original code, and `use_speedups("DESeq2")` applies its speed-ups again. A restored package stays original for the rest of the session, also when it is loaded later.
- Stored results: with paradoxR loaded, `Seurat::FindAllMarkers()`, `RunPCA()`, `FindNeighbors()`, `RunUMAP()`, `SCTransform()` and `IntegrateLayers()`, `Signac::FeatureMatrix()`, `DiffBind::dba.count()`, `csaw::windowCounts()` and `regionCounts()`, `GenomicAlignments::summarizeOverlaps()` on BAM files, `ChIPseeker::getTagMatrix()`, `EnrichedHeatmap::normalizeToMatrix()`, `GSVA::gsva()`, `sva::ComBat_seq()`, `DESeq2::lfcShrink()`, `stats::hclust()` and `MASS::kde2d()` keep their results on disk, so running a script or rendering a document again returns them at once when the data, input files, arguments and package versions are unchanged. A stored result repeats the original call's printed output, messages, warnings, option changes, loaded packages and random number state. `options(paradoxR.result_cache = FALSE)` turns this off.
- Settings: cached items unused for 7 days are deleted automatically, also when the speed-ups are turned off, and the cache always leaves 15% of the disk or 15 GB free. `options(paradoxR.cache_max_age_days = )`, `paradoxR.cache_min_free_pct` and `paradoxR.cache_min_free_gb` change these limits; `clear_cache()` also deletes stored results.
- Palette family: sequential and diverging sienna gradients span a wider range from light to dark with stronger colour, `sunset` is a bright yellow-to-red gradient distinct from `ember`, and `orange_teal` stays distinguishable for colour-blind readers.
- Palette family: `use_sienna_palette()` also switches ordered factors, and `scale_colour_sienna_o()` and `scale_fill_sienna_o()` set them by hand.
- Palette family: colour scales leave out the palest part of a gradient so low-value points and lines stay visible; `begin` and `end` choose the part of a gradient a scale uses.
- Palette family: the `spectral` gradient is removed.
- Settings: `options(paradoxR.cache_dir = )` in `~/.Rprofile` keeps the cache in another folder in every session; a leading `~` is expanded, and an empty or invalid value falls back to the default folder.
