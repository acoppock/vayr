# position_circlepack ----

test_that("co-located points are packed to distinct positions around their centre", {
  dat <- data.frame(x = rep(2, 100), y = rep(3, 100), size = seq(0.1, 1, length.out = 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size)) +
      ggplot2::geom_point(position = position_circlepack(density = 1, aspect_ratio = 1))
  )

  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
  expect_equal(mean(ld$x), 2, tolerance = 0.05)
  expect_equal(mean(ld$y), 3, tolerance = 0.05)
})

test_that("each over-plotted cell is packed around its own centre", {
  dat <- data.frame(
    x = rep(c(1, 5), each = 50),
    y = rep(c(1, 5), each = 50),
    size = rep(seq(0.1, 1, length.out = 50), 2)
  )

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size)) +
      ggplot2::geom_point(position = position_circlepack())
  )

  centres <- tapply(ld$x, dat$x, mean)
  expect_equal(as.vector(centres), c(1, 5), tolerance = 0.05)
})

test_that("a cell holding a single point is left alone", {
  # circleProgressiveLayout puts a lone circle at (-radius, 0), so this only
  # holds because the layout is re-centred.
  dat <- data.frame(x = c(1, 5), y = c(1, 5), size = c(1, 2))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size)) +
      ggplot2::geom_point(position = position_circlepack())
  )

  expect_identical(ld$x, dat$x)
  expect_identical(ld$y, dat$y)
})

test_that("small packs are centred on the point they stand for", {
  # The drift is worst at three and five, and gone by ten.
  for (n in c(1, 2, 3, 5, 10, 25)) {
    layout <- pack_circles(rep(1e-3, n))
    expect_equal(mean(layout[, 1]), 0, tolerance = 1e-9)
    expect_equal(mean(layout[, 2]), 0, tolerance = 1e-9)
  }
})

test_that("a constant size aesthetic is handled", {
  dat <- data.frame(x = rep(1, 50), y = rep(1, 50), size = rep(2, 50))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size)) +
      ggplot2::geom_point(position = position_circlepack())
  )

  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
})

test_that("a missing size aesthetic is handled", {
  dat <- data.frame(x = rep(1, 50), y = rep(1, 50))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_text(position = position_circlepack(), label = "a")
  )

  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
})

test_that("higher density packs the circles more tightly", {
  dat <- data.frame(x = rep(1, 100), y = rep(1, 100), size = seq(0.1, 1, length.out = 100))

  spread_at <- function(density) {
    ld <- ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size)) +
        ggplot2::geom_point(position = position_circlepack(density = density))
    )
    max(abs(ld$x - 1))
  }

  expect_lt(spread_at(4), spread_at(1))
})

test_that("aspect_ratio compresses the vertical extent only", {
  dat <- data.frame(x = rep(1, 100), y = rep(1, 100), size = seq(0.1, 1, length.out = 100))

  packed <- function(aspect_ratio) {
    ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size)) +
        ggplot2::geom_point(position = position_circlepack(aspect_ratio = aspect_ratio))
    )
  }

  square <- packed(1)
  wide <- packed(2)

  expect_equal((square$y - 1) / 2, wide$y - 1, tolerance = 1e-8)
  expect_identical(square$x, wide$x)
})

test_that("position_circlepack preserves row order", {
  dat <- data.frame(x = c(1, 2, 1, 2, 1), y = rep(1, 5), id = letters[1:5])

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id)) +
      ggplot2::geom_text(position = position_circlepack())
  )

  expect_identical(ld$label, letters[1:5])
})

# position_circlepackdodge ----

test_that("circlepackdodge separates the groups by the dodge width", {
  dat <- data.frame(
    x = rep(1, 200),
    y = rep(1, 200),
    size = seq(0.1, 1, length.out = 200),
    g = rep(c("A", "B"), 100)
  )

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size, colour = g)) +
      ggplot2::geom_point(position = position_circlepackdodge(width = 1, density = 4))
  )

  centres <- tapply(ld$x, ld$group, mean)
  expect_length(centres, 2)
  expect_equal(unname(diff(sort(centres))), 0.5, tolerance = 0.05)
})

test_that("circlepackdodge packs points within each group", {
  dat <- data.frame(
    x = rep(1, 200),
    y = rep(1, 200),
    size = seq(0.1, 1, length.out = 200),
    g = rep(c("A", "B"), 100)
  )

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, size = size, colour = g)) +
      ggplot2::geom_point(position = position_circlepackdodge(width = 1, density = 4))
  )

  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
})

test_that("position_circlepackdodge preserves row order", {
  dat <- data.frame(x = rep(1, 6), y = rep(1, 6), id = letters[1:6], g = rep(c("A", "B"), 3))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id, colour = g)) +
      ggplot2::geom_text(position = position_circlepackdodge())
  )

  expect_identical(ld$label, letters[1:6])
})
