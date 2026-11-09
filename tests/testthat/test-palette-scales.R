build_colours <- function(p) {
  ggplot2::layer_data(p)$colour
}

build_fills <- function(p) {
  ggplot2::layer_data(p)$fill
}

test_that("discrete scales use ember_pal()", {
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = drv)) +
    ggplot2::geom_point() +
    scale_colour_ember_d()
  expect_setequal(unique(build_colours(p)), ember_pal()(3))

  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(class, fill = drv)) +
    ggplot2::geom_bar() +
    scale_fill_ember_d()
  expect_setequal(unique(build_fills(p)), ember_pal()(3))
})

test_that("color spellings are the same functions", {
  expect_identical(scale_color_ember_d, scale_colour_ember_d)
  expect_identical(scale_color_ember_c, scale_colour_ember_c)
  expect_identical(scale_color_ember_b, scale_colour_ember_b)
  expect_identical(scale_color_ember_div, scale_colour_ember_div)
})

test_that("continuous scale ends at the first and last stop", {
  df <- data.frame(x = 1:2, y = 1, z = c(0, 1))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_ember_c("ocean")
  expect_identical(build_fills(p), c("#DFEFE5", "#3D3E67"))
})

test_that("binned scale builds", {
  p <- ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) +
    ggplot2::geom_raster() +
    scale_fill_ember_b("forest")
  expect_false(anyNA(build_fills(p)))
})

test_that("diverging scale maps the midpoint to the neutral", {
  df <- data.frame(x = 1:3, y = 1, z = c(-5, 0, 10))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_ember_div("red_blue", midpoint = 0)
  expect_identical(build_fills(p)[2], "#F7F3EE")
})

test_that("extra arguments pass through to ggplot2", {
  s <- scale_colour_ember_d(name = "Drive")
  expect_identical(s$name, "Drive")
  s <- scale_fill_ember_c("teal", limits = c(0, 10))
  expect_identical(s$get_limits(), c(0, 10))
})

test_that("use_ember_palette() switches every default and reset switches back", {
  on.exit(reset_ggplot2_palette())

  use_ember_palette(continuous = "ocean")
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = drv)) +
    ggplot2::geom_point()
  expect_setequal(unique(build_colours(p)), ember_pal()(3))

  df <- data.frame(x = 1:2, y = 1, z = c(0, 1))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile()
  expect_identical(build_fills(p), c("#DFEFE5", "#3D3E67"))

  reset_ggplot2_palette()
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = drv)) +
    ggplot2::geom_point()
  expect_setequal(unique(build_colours(p)), scales::hue_pal()(3))
})

test_that("use_ember_palette() rejects unknown palettes before touching options", {
  before <- getOption("ggplot2.continuous.fill")
  expect_error(use_ember_palette(continuous = "nope"))
  expect_identical(getOption("ggplot2.continuous.fill"), before)
})
