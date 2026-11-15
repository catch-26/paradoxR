# v26.11.0 (2026-11-09)

- New package paradoxR, installed with `pak::pak("catch-26/paradoxR")`.
- Palette family: the sienna palette replaces ggplot2's default colours. `use_sienna_palette()` switches every plot, and `reset_ggplot2_palette()` switches back.
- Palette family: `scale_colour_sienna_d()`, `_c()`, `_b()` and `_div()` and their fill forms replace ggplot2's discrete, continuous, binned and diverging colour scales.
- Enrichment: loading paradoxR makes `clusterProfiler::enrichGO()` keep its GO annotation on disk and remember each result, so repeated and later-session calls return in under a second. `reset_go_cache()` switches back and `clear_go_cache()` deletes the cache.
