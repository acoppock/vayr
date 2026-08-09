# honeycomb ----

test_that("a lone point stays at its own coordinates", {
  expect_identical(honeycomb(1, density = 1), matrix(0, nrow = 1, ncol = 2))
})

test_that("the lattice is exact, so every point is equidistant from its nearest neighbour", {
  expect_equal(nearest_neighbour_cv(honeycomb(200, density = 1)), 0, tolerance = 1e-8)
})

test_that("sites are distinct and there are exactly n of them", {
  sites <- honeycomb(150, density = 1)

  expect_identical(nrow(sites), 150L)
  expect_identical(anyDuplicated(paste(sites[, 1], sites[, 2])), 0L)
})

test_that("spacing depends on density alone and not on how many points there are", {
  spacing_at <- function(n) min(nearest_neighbour_distances(honeycomb(n, density = 1)))

  expect_equal(spacing_at(50), spacing_at(200), tolerance = 1e-8)
})

test_that("higher density packs the lattice more tightly", {
  loose <- min(nearest_neighbour_distances(honeycomb(100, density = 1)))
  tight <- min(nearest_neighbour_distances(honeycomb(100, density = 4)))

  # spacing scales with 1 / sqrt(density)
  expect_equal(tight * 2, loose, tolerance = 1e-8)
})

test_that("the lattice covers the same footprint as the sunflower at the same density", {
  n <- 300
  comb <- honeycomb(n, density = 1)
  comb_radius <- max(sqrt(comb[, 1] ^ 2 + comb[, 2] ^ 2))

  flower_radius <- max(sqrt(
    sunflower(x = rep(0, n), density = 1, aspect_ratio = 1) ^ 2 +
      sunflower(y = rep(0, n), density = 1, aspect_ratio = 1) ^ 2
  ))

  expect_equal(comb_radius, flower_radius, tolerance = 0.1)
})

test_that("the lattice is centred on the origin", {
  sites <- honeycomb(200, density = 1)

  expect_equal(mean(sites[, 1]), 0, tolerance = 0.05)
  expect_equal(mean(sites[, 2]), 0, tolerance = 0.05)
})

test_that("honeycomb is deterministic", {
  expect_identical(honeycomb(60, density = 1), honeycomb(60, density = 1))
})

# position_honeycomb ----

test_that("each over-plotted cell is packed around its own centre", {
  dat <- data.frame(x = rep(c(1, 5), each = 100), y = rep(c(1, 5), each = 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_honeycomb(density = 100))
  )

  centres <- tapply(ld$x, dat$x, mean)
  expect_equal(as.vector(centres), c(1, 5), tolerance = 0.05)
  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
})

test_that("a cell holding a single point is left alone", {
  dat <- data.frame(x = c(1, 5), y = c(1, 5))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_honeycomb())
  )

  expect_identical(ld$x, dat$x)
  expect_identical(ld$y, dat$y)
})

test_that("aspect_ratio compresses the vertical extent and leaves the horizontal alone", {
  dat <- data.frame(x = rep(1, 100), y = rep(1, 100))

  packed <- function(aspect_ratio) {
    ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
        ggplot2::geom_point(position = position_honeycomb(density = 100, aspect_ratio = aspect_ratio))
    )
  }

  square <- packed(1)
  wide <- packed(2)

  expect_equal((square$y - 1) / 2, wide$y - 1, tolerance = 1e-8)
  expect_identical(square$x, wide$x)
})

test_that("position_honeycomb preserves row order", {
  dat <- data.frame(x = c(1, 2, 1, 2, 1), y = rep(1, 5), id = letters[1:5])

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id)) +
      ggplot2::geom_text(position = position_honeycomb())
  )

  expect_identical(ld$label, letters[1:5])
})

# position_honeycombdodge ----

test_that("honeycombdodge separates the groups by the dodge width", {
  dat <- data.frame(x = rep(1, 200), y = rep(1, 200), g = rep(c("A", "B"), 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_honeycombdodge(width = 1, density = 200))
  )

  centres <- tapply(ld$x, ld$group, mean)
  expect_length(centres, 2)
  expect_equal(unname(diff(sort(centres))), 0.5, tolerance = 0.05)
})

test_that("honeycombdodge lays each group on its own lattice", {
  dat <- data.frame(x = rep(1, 200), y = rep(1, 200), g = rep(c("A", "B"), 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_honeycombdodge(width = 1, density = 200))
  )

  expect_identical(anyDuplicated(paste(ld$x, ld$y)), 0L)
  for (group in split(ld, ld$group)) {
    expect_equal(nearest_neighbour_cv(cbind(group$x, group$y)), 0, tolerance = 1e-8)
  }
})

test_that("honeycombdodge preserves row order", {
  dat <- data.frame(x = rep(1, 6), y = rep(1, 6), id = letters[1:6], g = rep(c("A", "B"), 3))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id, colour = g)) +
      ggplot2::geom_text(position = position_honeycombdodge())
  )

  expect_identical(ld$label, letters[1:6])
})
