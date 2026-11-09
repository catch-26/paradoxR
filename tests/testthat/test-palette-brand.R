test_that("brand_pal() matches the manifesto for 1 to 8 groups", {
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
    expect_identical(brand_pal()(n), expected[[n]])
  }
})

test_that("brand_pal() never repeats a colour and never errors on n", {
  for (n in 1:30) {
    cols <- brand_pal()(n)
    expect_length(cols, n)
    expect_false(anyNA(cols))
    expect_false(anyDuplicated(cols) > 0)
  }
})

test_that("brand_pal() falls back to brand_hcl() above 13 groups", {
  targets <- (18 + 360 * (0:13) / 14) %% 360
  expect_identical(brand_pal()(14), brand_hcl(targets, 60, 63))
})

test_that("harmonise = FALSE returns the original swatches for the same picks", {
  key <- match(brand_pal()(5), brand_colours_harmonised)
  expect_identical(brand_pal(harmonise = FALSE)(5), unname(brand_colours[key]))
})

test_that("direction = -1 walks the wheel the other way", {
  expect_identical(brand_pal(direction = -1)(3), c("#D87264", "#7793DC", "#40AE77"))
})

test_that("gradients match the manifesto", {
  expect_identical(
    brand_gradient("ocean"),
    c("#DFEFE5", "#B7DCCE", "#8AC9BF", "#61B3C2", "#6097C6", "#507FB9", "#52679E", "#485282", "#3D3E67")
  )
  expect_identical(
    brand_gradient("red_blue"),
    c("#A25449", "#C3827B", "#DEAEA9", "#F3D5D2", "#F7F3EE", "#D5DBF1", "#AEB9DE", "#8092C5", "#4C69AB")
  )
  expect_identical(
    brand_ramp("teal"),
    c("#DCEFEB", "#B4DCD5", "#8AC9BF", "#57B7AA", "#06A494", "#038D7F", "#00766B", "#026057", "#004B43")
  )
  expect_identical(brand_gradient("sunset", reverse = TRUE), rev(brand_gradients$sunset))
})

test_that("every palette has 9 valid stops", {
  stops <- c(brand_gradients, brand_diverging, brand_ramps)
  for (cols in stops) {
    expect_length(cols, 9)
    expect_match(cols, "^#[0-9A-F]{6}$")
  }
})

test_that("unknown palette names give a helpful error", {
  expect_error(brand_gradient("nope"), "palette")
  expect_snapshot(brand_gradient("ocaen"), error = TRUE)
})

test_that("brand_hcl() never returns NA", {
  cols <- brand_hcl(0:359, 150, 50)
  expect_false(anyNA(cols))
})

test_that("brand_hcl() matches grDevices::hcl() inside the gamut", {
  expect_identical(brand_hcl(c(0, 120, 240), 40, 70), grDevices::hcl(c(0, 120, 240), 40, 70))
})

test_that("brand_harmonise() reproduces the stored harmonised swatches", {
  testthat::skip_if_not_installed("colorspace")
  wheel <- setdiff(names(brand_colours), c("brown", "grey", "taupe"))
  expect_identical(brand_harmonise(brand_colours[wheel]), brand_colours_harmonised[wheel])
})

test_that("brand_grad_pal() returns a palette function", {
  pal <- brand_grad_pal("ocean")
  expect_identical(pal(c(0, 1)), c("#DFEFE5", "#3D3E67"))
})
