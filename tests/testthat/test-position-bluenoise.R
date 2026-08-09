# best_candidate_disc ----

test_that("a lone point stays at its own coordinates", {
  expect_identical(best_candidate_disc(1), matrix(0, nrow = 1, ncol = 2))
})

test_that("every point lands inside the unit disc", {
  points <- best_candidate_disc(200)
  expect_lte(max(sqrt(points[, 1] ^ 2 + points[, 2] ^ 2)), 1)
})

test_that("more candidates space the points more evenly", {
  set.seed(1)
  few <- nearest_neighbour_cv(best_candidate_disc(150, candidates = 1))
  set.seed(1)
  many <- nearest_neighbour_cv(best_candidate_disc(150, candidates = 20))

  expect_lt(many, few)
})

# position_bluenoise ----

test_that("scattered points land inside the ellipse", {
  dat <- data.frame(x = rep(1, 300), y = rep(2, 300))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_bluenoise(width = 0.5, height = 0.25, seed = 1))
  )

  radius <- sqrt(((ld$x - 1) / 0.5) ^ 2 + ((ld$y - 2) / 0.25) ^ 2)
  expect_lte(max(radius), 1)
  expect_gt(max(radius), 0.9)
})

test_that("the scatter is far more even than a uniform jitter", {
  dat <- data.frame(x = rep(1, 250), y = rep(1, 250))

  spread_of <- function(position) {
    ld <- ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y)) + ggplot2::geom_point(position = position)
    )
    nearest_neighbour_cv(cbind(ld$x, ld$y))
  }

  uniform <- spread_of(position_jitter_ellipse(0.5, 0.5, seed = 1))
  even <- spread_of(position_bluenoise(0.5, 0.5, seed = 1))

  # Uniform jitter sits near 0.5, blue noise well below it.
  expect_gt(uniform, 0.4)
  expect_lt(even, 0.25)
  expect_lt(even, uniform / 2)
})

test_that("no pair ends up far closer together than the rest", {
  dat <- data.frame(x = rep(1, 250), y = rep(1, 250))

  closest_ratio <- function(position) {
    ld <- ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y)) + ggplot2::geom_point(position = position)
    )
    neighbours <- nearest_neighbour_distances(cbind(ld$x, ld$y))
    min(neighbours) / median(neighbours)
  }

  expect_lt(closest_ratio(position_jitter_ellipse(0.5, 0.5, seed = 1)), 0.2)
  expect_gt(closest_ratio(position_bluenoise(0.5, 0.5, seed = 1)), 0.4)
})

test_that("each over-plotted position is scattered around its own centre", {
  dat <- data.frame(x = rep(c(1, 5), each = 100), y = rep(c(1, 5), each = 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_bluenoise(0.4, 0.4, seed = 1))
  )

  centres <- tapply(ld$x, dat$x, mean)
  expect_equal(as.vector(centres), c(1, 5), tolerance = 0.1)
})

test_that("a lone point in a cell is left alone", {
  dat <- data.frame(x = c(1, 5), y = c(1, 5))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_bluenoise(0.5, 0.5, seed = 1))
  )

  expect_identical(ld$x, dat$x)
  expect_identical(ld$y, dat$y)
})

test_that("a fixed seed reproduces the scatter and a different seed changes it", {
  dat <- data.frame(x = rep(1, 100), y = rep(1, 100))

  scatter_with <- function(seed) {
    ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
        ggplot2::geom_point(position = position_bluenoise(0.5, 0.5, seed = seed))
    )$x
  }

  expect_identical(scatter_with(42), scatter_with(42))
  expect_false(isTRUE(all.equal(scatter_with(42), scatter_with(43))))
})

test_that("scattering leaves the ambient RNG state untouched", {
  dat <- data.frame(x = rep(1, 50), y = rep(1, 50))

  set.seed(123)
  expected <- runif(1)

  set.seed(123)
  invisible(ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_bluenoise(0.5, 0.5, seed = 7))
  ))

  expect_identical(runif(1), expected)
})

test_that("row order is preserved", {
  dat <- data.frame(x = rep(1, 5), y = rep(1, 5), id = letters[1:5])

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id)) +
      ggplot2::geom_text(position = position_bluenoise(0.5, 0.5, seed = 1))
  )

  expect_identical(ld$label, letters[1:5])
})

test_that("default width and height fall back to the data resolution", {
  dat <- data.frame(x = rep(c(1, 3), each = 100), y = rep(1, 200))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_bluenoise(seed = 2))
  )

  expect_lte(max(abs(ld$x - dat$x)), 0.8)
})

# position_bluenoisedodge ----

test_that("bluenoisedodge separates the groups by the dodge width", {
  dat <- data.frame(x = rep(1, 400), y = rep(1, 400), g = rep(c("A", "B"), 200))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_bluenoisedodge(
        scatter.width = 0.1, scatter.height = 0.1, dodge.width = 1, seed = 1
      ))
  )

  centres <- tapply(ld$x, ld$group, mean)
  expect_length(centres, 2)
  expect_equal(unname(diff(sort(centres))), 0.5, tolerance = 0.02)
})

test_that("bluenoisedodge spaces each group evenly and independently", {
  dat <- data.frame(x = rep(1, 400), y = rep(1, 400), g = rep(c("A", "B"), 200))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_bluenoisedodge(
        scatter.width = 0.2, scatter.height = 0.2, dodge.width = 1, seed = 1
      ))
  )

  by_group <- split(ld, ld$group)
  for (group in by_group) {
    expect_lt(nearest_neighbour_cv(cbind(group$x, group$y)), 0.25)
  }

  offset_a <- by_group[[1]]$x - mean(by_group[[1]]$x)
  offset_b <- by_group[[2]]$x - mean(by_group[[2]]$x)
  expect_false(isTRUE(all.equal(offset_a, offset_b)))
})

test_that("bluenoisedodge preserves row order", {
  dat <- data.frame(x = rep(1, 6), y = rep(1, 6), id = letters[1:6], g = rep(c("A", "B"), 3))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id, colour = g)) +
      ggplot2::geom_text(position = position_bluenoisedodge(0.2, 0.2, 1, seed = 1))
  )

  expect_identical(ld$label, letters[1:6])
})
