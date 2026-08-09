test_that("the suite runs with ggplot2 loaded but not attached", {
  expect_false("package:ggplot2" %in% search())
})

test_that("position adjustments resolve ggplot2 internals from the namespace", {
  dat <- data.frame(
    x = rep(1, 50),
    y = rep(1, 50),
    size = seq(0.1, 1, length.out = 50),
    g = rep(c("A", "B"), 25)
  )

  plain <- ggplot2::ggplot(dat, ggplot2::aes(x, y))
  grouped <- ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g))
  sized <- ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size, colour = g))

  expect_no_error(ggplot2::layer_data(plain + ggplot2::geom_point(position = position_jitter_ellipse(0.5, 0.5))))
  expect_no_error(ggplot2::layer_data(plain + ggplot2::geom_point(position = position_jitter_ellipse())))
  expect_no_error(ggplot2::layer_data(plain + ggplot2::geom_point(position = position_sunflower())))
  expect_no_error(ggplot2::layer_data(plain + ggplot2::geom_point(position = position_honeycomb())))
  expect_no_error(ggplot2::layer_data(plain + ggplot2::geom_point(position = position_bluenoise(0.5, 0.5))))
  expect_no_error(ggplot2::layer_data(plain + ggplot2::geom_point(position = position_bluenoise())))
  expect_no_error(ggplot2::layer_data(sized + ggplot2::geom_point(position = position_circlepack())))
  expect_no_error(ggplot2::layer_data(grouped + ggplot2::geom_point(position = position_jitterdodge_ellipse())))
  expect_no_error(ggplot2::layer_data(grouped + ggplot2::geom_point(position = position_sunflowerdodge())))
  expect_no_error(ggplot2::layer_data(grouped + ggplot2::geom_point(position = position_honeycombdodge())))
  expect_no_error(ggplot2::layer_data(grouped + ggplot2::geom_point(position = position_bluenoisedodge())))
  expect_no_error(ggplot2::layer_data(sized + ggplot2::geom_point(position = position_circlepackdodge())))
})
