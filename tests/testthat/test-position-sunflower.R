# sunflower() ----

test_that("sunflower requires x or y", {
  expect_error(sunflower(density = 1, aspect_ratio = 1), "requires either x or y")
})

test_that("a point with no company stays where it is", {
  expect_identical(sunflower(x = 3, density = 1, aspect_ratio = 1), 3)
  expect_identical(sunflower(y = 3, density = 1, aspect_ratio = 1), 3)
})

test_that("co-located points are spread to distinct positions", {
  x <- sunflower(x = rep(1, 50), density = 1, aspect_ratio = 1)
  y <- sunflower(y = rep(1, 50), density = 1, aspect_ratio = 1)

  expect_length(x, 50)
  expect_identical(anyDuplicated(paste(x, y)), 0L)
})

test_that("the pattern is centred on the input point", {
  x <- sunflower(x = rep(4, 200), density = 1, aspect_ratio = 1)
  y <- sunflower(y = rep(9, 200), density = 1, aspect_ratio = 1)

  expect_equal(mean(x), 4, tolerance = 0.05)
  expect_equal(mean(y), 9, tolerance = 0.05)
})

test_that("higher density packs the pattern more tightly", {
  loose <- sunflower(x = rep(0, 100), density = 1, aspect_ratio = 1)
  tight <- sunflower(x = rep(0, 100), density = 4, aspect_ratio = 1)

  # width scales with 1 / sqrt(density)
  expect_equal(max(abs(tight)) * 2, max(abs(loose)), tolerance = 1e-8)
})

test_that("aspect_ratio compresses the vertical extent and leaves the horizontal alone", {
  y_square <- sunflower(y = rep(0, 100), density = 1, aspect_ratio = 1)
  y_wide <- sunflower(y = rep(0, 100), density = 1, aspect_ratio = 2)
  expect_equal(y_square / 2, y_wide, tolerance = 1e-8)

  x_square <- sunflower(x = rep(0, 100), density = 1, aspect_ratio = 1)
  x_wide <- sunflower(x = rep(0, 100), density = 1, aspect_ratio = 2)
  expect_identical(x_square, x_wide)
})

test_that("sunflower is deterministic", {
  expect_identical(
    sunflower(x = rep(1, 30), density = 1, aspect_ratio = 1),
    sunflower(x = rep(1, 30), density = 1, aspect_ratio = 1)
  )
})

# position_sunflower ----

test_that("each over-plotted cell is spread around its own centre", {
  dat <- data.frame(x = rep(c(1, 5), each = 100), y = rep(c(1, 5), each = 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_sunflower(density = 1, aspect_ratio = 1))
  )

  centres <- tapply(ld$x, dat$x, mean)
  expect_equal(as.vector(centres), c(1, 5), tolerance = 0.05)
  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
})

test_that("a cell holding a single point is left alone", {
  dat <- data.frame(x = c(1, 5), y = c(1, 5))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_sunflower())
  )

  expect_identical(ld$x, dat$x)
  expect_identical(ld$y, dat$y)
})

test_that("position_sunflower preserves row order", {
  dat <- data.frame(x = c(1, 2, 1, 2, 1), y = rep(1, 5), id = letters[1:5])

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id)) +
      ggplot2::geom_text(position = position_sunflower())
  )

  expect_identical(ld$label, letters[1:5])
})

# position_sunflowerdodge ----

test_that("sunflowerdodge separates the groups by the dodge width", {
  dat <- data.frame(x = rep(1, 200), y = rep(1, 200), g = rep(c("A", "B"), 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_sunflowerdodge(width = 1, density = 4, aspect_ratio = 1))
  )

  centres <- tapply(ld$x, ld$group, mean)
  expect_length(centres, 2)
  expect_equal(unname(diff(sort(centres))), 0.5, tolerance = 0.05)
})

test_that("sunflowerdodge spreads points within each group", {
  dat <- data.frame(x = rep(1, 200), y = rep(1, 200), g = rep(c("A", "B"), 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_sunflowerdodge(width = 1, density = 4, aspect_ratio = 1))
  )

  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
})

test_that("position_sunflowerdodge preserves row order", {
  dat <- data.frame(x = rep(1, 6), y = rep(1, 6), id = letters[1:6], g = rep(c("A", "B"), 3))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id, colour = g)) +
      ggplot2::geom_text(position = position_sunflowerdodge())
  )

  expect_identical(ld$label, letters[1:6])
})
