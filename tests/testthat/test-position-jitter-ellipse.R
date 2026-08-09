# position_jitter_ellipse ----

test_that("jittered points land inside the ellipse", {
  dat <- data.frame(x = rep(1, 500), y = rep(2, 500))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.25, seed = 1))
  )

  radius <- sqrt(((ld$x - 1) / 0.5) ^ 2 + ((ld$y - 2) / 0.25) ^ 2)
  expect_lte(max(radius), 1)
  expect_gt(max(radius), 0.9)
})

test_that("jittered points are uniform over the ellipse rather than bunched at the centre", {
  dat <- data.frame(x = rep(0, 2000), y = rep(0, 2000))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_jitter_ellipse(width = 1, height = 1, seed = 4))
  )

  # A uniform draw on a disc has squared normalised radius uniform on [0, 1].
  radius <- sqrt(ld$x ^ 2 + ld$y ^ 2)
  expect_gt(stats::ks.test(radius ^ 2, "punif")$p.value, 0.01)
})

test_that("the ellipse holds when seed is NULL", {
  dat <- data.frame(x = rep(1, 500), y = rep(2, 500))
  set.seed(9)

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.25, seed = NULL))
  )

  radius <- sqrt(((ld$x - 1) / 0.5) ^ 2 + ((ld$y - 2) / 0.25) ^ 2)
  expect_lte(max(radius), 1)
})

test_that("a fixed seed reproduces the jitter and a different seed changes it", {
  dat <- data.frame(x = rep(1, 200), y = rep(1, 200))

  jitter_with <- function(seed) {
    ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
        ggplot2::geom_point(position = position_jitter_ellipse(0.5, 0.5, seed = seed))
    )$x
  }

  expect_identical(jitter_with(42), jitter_with(42))
  expect_false(isTRUE(all.equal(jitter_with(42), jitter_with(43))))
})

test_that("jittering leaves the ambient RNG state untouched", {
  dat <- data.frame(x = rep(1, 100), y = rep(1, 100))

  set.seed(123)
  expected <- runif(1)

  set.seed(123)
  invisible(ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_jitter_ellipse(0.5, 0.5, seed = 7))
  ))

  expect_identical(runif(1), expected)
})

test_that("default width and height fall back to the data resolution", {
  dat <- data.frame(x = rep(c(1, 3), each = 250), y = rep(1, 500))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = position_jitter_ellipse(seed = 2))
  )

  # resolution() is 2 here, and the default half-width is 0.4 of that.
  expect_lte(max(abs(ld$x - dat$x)), 0.8)
  expect_gt(max(abs(ld$x - dat$x)), 0.6)
})

test_that("row order is preserved", {
  dat <- data.frame(x = rep(1, 5), y = rep(1, 5), id = letters[1:5])

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id)) +
      ggplot2::geom_text(position = position_jitter_ellipse(0.5, 0.5, seed = 1))
  )

  expect_identical(ld$label, letters[1:5])
})

# position_jitterdodge_ellipse ----

test_that("dodging separates the groups by the dodge width", {
  dat <- data.frame(x = rep(1, 400), y = rep(1, 400), g = rep(c("A", "B"), 200))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_jitterdodge_ellipse(
        jitter.width = 0.05, jitter.height = 0.05, dodge.width = 1, seed = 1
      ))
  )

  centres <- tapply(ld$x, ld$group, mean)
  expect_length(centres, 2)
  expect_equal(unname(diff(sort(centres))), 0.5, tolerance = 0.02)
})

test_that("each group is jittered independently", {
  dat <- data.frame(x = rep(1, 400), y = rep(1, 400), g = rep(c("A", "B"), 200))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_jitterdodge_ellipse(
        jitter.width = 0.2, jitter.height = 0.2, dodge.width = 1, seed = 1
      ))
  )

  by_group <- split(ld, ld$group)
  offset_a <- by_group[[1]]$x - mean(by_group[[1]]$x)
  offset_b <- by_group[[2]]$x - mean(by_group[[2]]$x)

  expect_false(isTRUE(all.equal(offset_a, offset_b)))
})

test_that("dodged points land inside the ellipse around their dodged centre", {
  dat <- data.frame(x = rep(1, 400), y = rep(1, 400), g = rep(c("A", "B"), 200))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position_jitterdodge_ellipse(
        jitter.width = 0.1, jitter.height = 0.2, dodge.width = 1, seed = 3
      ))
  )

  centres <- tapply(ld$x, ld$group, mean)
  radius <- sqrt(((ld$x - centres[as.character(ld$group)]) / 0.1) ^ 2 + ((ld$y - 1) / 0.2) ^ 2)
  expect_lte(max(radius), 1.05)
})

test_that("jitterdodge preserves row order", {
  dat <- data.frame(
    x = rep(1, 6),
    y = rep(1, 6),
    id = letters[1:6],
    g = rep(c("A", "B"), 3)
  )

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id, colour = g)) +
      ggplot2::geom_text(position = position_jitterdodge_ellipse(0.2, 0.2, 1, seed = 1))
  )

  expect_identical(ld$label, letters[1:6])
})
