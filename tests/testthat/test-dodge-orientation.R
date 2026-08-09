# Every dodged position adjustment inherits from ggplot2::PositionDodge, so each
# one should honour the same `orientation` argument and separate groups up and
# down as readily as side to side.

dodged_positions <- list(
  position_sunflowerdodge = function(orientation) {
    position_sunflowerdodge(width = 1, density = 100, aspect_ratio = 1, orientation = orientation)
  },
  position_honeycombdodge = function(orientation) {
    position_honeycombdodge(width = 1, density = 100, aspect_ratio = 1, orientation = orientation)
  },
  position_circlepackdodge = function(orientation) {
    position_circlepackdodge(width = 1, density = 1, aspect_ratio = 1, orientation = orientation)
  },
  position_bluenoisedodge = function(orientation) {
    position_bluenoisedodge(0.1, 0.1, dodge.width = 1, seed = 1, orientation = orientation)
  },
  position_jitterdodge_ellipse = function(orientation) {
    position_jitterdodge_ellipse(0.1, 0.1, dodge.width = 1, seed = 1, orientation = orientation)
  }
)

group_separation <- function(position) {
  dat <- data.frame(x = rep(1, 200), y = rep(1, 200), g = rep(c("A", "B"), 100))

  ld <- ggplot2::layer_data(
    ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(position = position)
  )

  c(
    x = diff(range(tapply(ld$x, ld$group, mean))),
    y = diff(range(tapply(ld$y, ld$group, mean)))
  )
}

test_that("groups dodge side to side by default", {
  for (name in names(dodged_positions)) {
    separation <- group_separation(dodged_positions[[name]]("x"))

    expect_equal(unname(separation[["x"]]), 0.5, tolerance = 0.05, info = name)
    expect_lt(separation[["y"]], 0.05)
  }
})

test_that("orientation = 'y' dodges groups up and down instead", {
  for (name in names(dodged_positions)) {
    separation <- group_separation(dodged_positions[[name]]("y"))

    expect_equal(unname(separation[["y"]]), 0.5, tolerance = 0.05, info = name)
    expect_lt(separation[["x"]], 0.05)
  }
})

test_that("the arrangement within a group survives the flip", {
  # Turning the dodge on its side should move the clusters, not scramble them.
  for (orientation in c("x", "y")) {
    dat <- data.frame(x = rep(1, 6), y = rep(1, 6), id = letters[1:6], g = rep(c("A", "B"), 3))

    ld <- ggplot2::layer_data(
      ggplot2::ggplot(dat, ggplot2::aes(x, y, label = id, colour = g)) +
        ggplot2::geom_text(position = position_honeycombdodge(orientation = orientation))
    )

    expect_identical(ld$label, letters[1:6])
  }
})
