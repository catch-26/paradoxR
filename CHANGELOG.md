# v26.11.0 (2026-11-09)

- New package paradoxR, installed with `pak::pak("catch-26/paradoxR")`.
- Palette family: the sienna palette replaces ggplot2's default colours. `use_sienna_palette()` switches every plot, and `reset_ggplot2_palette()` switches back.
- Palette family: `scale_colour_sienna_d()`, `_c()`, `_b()` and `_div()` and their fill forms replace ggplot2's discrete, continuous, binned and diverging colour scales.
- Enrichment: loading paradoxR makes `clusterProfiler::enrichGO()` keep its GO annotation on disk and remember each result, so repeated and later-session calls return in under a second. `reset_go_cache()` switches back and `clear_go_cache()` deletes the cache.
- Enrichment: with paradoxR loaded, `clusterProfiler::simplify()` and Wang similarity in GOSemSim and enrichplot run about 10 times faster with identical results.
- Enrichment: the first `enrichGO()`, `gseGO()` or `groupGO()` call for an OrgDb builds its GO annotation 3 to 5 times faster, with identical results.
- Enrichment: `GOSemSim::godata()` computes information content for BP in about 5 s instead of 24 s, with identical results.
- Enrichment: with paradoxR loaded, `clusterProfiler::enrichKEGG()`, `gseKEGG()`, `enrichMKEGG()` and `gseMKEGG()` keep their KEGG downloads on disk and refresh them only when KEGG releases an update, so later calls, other key types and new R sessions skip the download.
- Enrichment: rerunning `clusterProfiler::enrichGO()` on the same genes with other p-value or q-value cutoffs returns at once, and other gene set sizes or adjustment methods take under a second. If a sped-up function fails, the original runs instead.
- Enrichment: unloading paradoxR restores the original clusterProfiler, DOSE and GOSemSim functions, and reloading it in the same session no longer breaks `reset_go_cache()`.
- Enrichment: the faster `simplify()` and Wang similarity also work with Bioconductor 3.23 (clusterProfiler 4.20, GOSemSim 2.38).
