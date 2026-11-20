withr::local_options(
  paradoxR.cache_min_free_pct = 0,
  paradoxR.cache_min_free_gb = 0,
  .local_envir = testthat::teardown_env()
)
