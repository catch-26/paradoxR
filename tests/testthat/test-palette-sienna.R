test_that("sienna_pal() matches the reference values for 1 to 8 groups", {
  expected <- list(
    "#D87264",
    c("#D87264", "#04A697"),
    c("#D87264", "#40AE77", "#7793DC"),
    c("#D87264", "#7EA84E", "#04A697", "#AA91D4"),
    c("#D87264", "#A5AB42", "#04A697", "#569DD3", "#B674C8"),
    c("#D87264", "#A5AB42", "#40AE77", "#04A697", "#7793DC", "#B674C8"),
    c("#D87264", "#C8A147", "#7EA84E", "#04A697", "#569DD3", "#AA91D4", "#D16E8B"),
    c("#D87264", "#C8A147", "#7EA84E", "#40AE77", "#04A697", "#569DD3", "#AA91D4", "#D16E8B")
  )
  for (n in seq_along(expected)) {
    expect_identical(sienna_pal()(n), expected[[n]])
  }
})

test_that("sienna_pal() never repeats a colour and never errors on n", {
  for (n in 1:30) {
    cols <- sienna_pal()(n)
    expect_length(cols, n)
    expect_false(anyNA(cols))
    expect_false(anyDuplicated(cols) > 0)
  }
})

test_that("sienna_pal() falls back to sienna_hcl() above 13 groups", {
  targets <- (18 + 360 * (0:13) / 14) %% 360
  expect_identical(sienna_pal()(14), sienna_hcl(targets, 60, 63))
})

test_that("harmonise = FALSE returns the original swatches for the same picks", {
  key <- match(sienna_pal()(5), sienna_colours_harmonised)
  expect_identical(sienna_pal(harmonise = FALSE)(5), unname(sienna_colours[key]))
})

test_that("direction = -1 walks the wheel the other way", {
  expect_identical(sienna_pal(direction = -1)(3), c("#D87264", "#7793DC", "#40AE77"))
})

test_that("gradients match the reference values", {
  expect_identical(
    sienna_gradient("ocean"),
    c("#DEFEEA", "#AFE7D3", "#7BCFC2", "#48B4C5", "#4F93C6", "#4C76AB", "#455B91", "#364377", "#272B60")
  )
  expect_identical(
    sienna_gradient("red_blue"),
    c("#8B2909", "#B9655A", "#E09C94", "#FBCDC9", "#FAF6F1", "#CDD7F9", "#9AABE1", "#5E7BC2", "#01499B")
  )
  expect_identical(
    sienna_ramp("teal"),
    c("#D4FFF8", "#A1E8DD", "#65D2C3", "#02BAA9", "#079E8F", "#028376", "#00685E", "#024F47", "#003731")
  )
  expect_identical(sienna_gradient("sunset", reverse = TRUE), rev(sienna_gradients$sunset))
})

test_that("every palette has 9 valid stops", {
  stops <- c(sienna_gradients, sienna_diverging, sienna_ramps)
  for (cols in stops) {
    expect_length(cols, 9)
    expect_match(cols, "^#[0-9A-F]{6}$")
  }
})

test_that("unknown palette names give a helpful error", {
  expect_error(sienna_gradient("nope"), "palette")
  expect_snapshot(sienna_gradient("ocaen"), error = TRUE)
})

test_that("sienna_hcl() never returns NA", {
  cols <- sienna_hcl(0:359, 150, 50)
  expect_false(anyNA(cols))
})

test_that("sienna_hcl() matches grDevices::hcl() inside the gamut", {
  expect_identical(sienna_hcl(c(0, 120, 240), 40, 70), grDevices::hcl(c(0, 120, 240), 40, 70))
})

test_that("sienna_harmonise() reproduces the stored harmonised swatches", {
  testthat::skip_if_not_installed("colorspace")
  wheel <- setdiff(names(sienna_colours), c("brown", "grey", "taupe"))
  expect_identical(sienna_harmonise(sienna_colours[wheel]), sienna_colours_harmonised[wheel])
})

test_that("sienna_grad_pal() returns a palette function", {
  pal <- sienna_grad_pal("ocean")
  expect_identical(pal(c(0, 1)), c("#DEFEEA", "#272B60"))
})
