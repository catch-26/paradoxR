test_that("compat_discrete_scale() builds a discrete scale on this ggplot2", {
  s <- compat_discrete_scale("colour", sienna_pal())
  expect_s3_class(s, "ScaleDiscrete")
  expect_identical(s$aesthetics, "colour")
})

test_that("the general option names win and the older names still work", {
  withr::local_options(paradoxR.speedups = NULL, paradoxR.go_cache = NULL, paradoxR.cache_dir = NULL, paradoxR.go_cache_dir = NULL)
  expect_true(speedups_enabled())
  withr::local_options(paradoxR.go_cache = FALSE)
  expect_false(speedups_enabled())
  withr::local_options(paradoxR.speedups = TRUE)
  expect_true(speedups_enabled())
  withr::local_options(paradoxR.go_cache_dir = "old")
  expect_identical(gocache_dir(), "old")
  withr::local_options(paradoxR.cache_dir = "new")
  expect_identical(gocache_dir(), "new")
})
