test_that("compat_discrete_scale() builds a discrete scale on this ggplot2", {
  s <- compat_discrete_scale("colour", ember_pal())
  expect_s3_class(s, "ScaleDiscrete")
  expect_identical(s$aesthetics, "colour")
})
