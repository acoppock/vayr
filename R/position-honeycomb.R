#' Arrange over-plotted points on a honeycomb lattice
#'
#' This function arranges perfectly over-plotted points on a hexagonal lattice,
#' filling outward from the centre in the order of the data. The hexagonal
#' lattice is the densest packing of equal circles in the plane, so the cluster
#' is as compact as it can be for a given spacing.
#'
#' It is the crystalline sibling of [position_sunflower()], which fills the same
#' footprint at the same `density` with a spiral rather than a lattice. Choose
#' between them on looks: the lattice reads as countable and orderly, the spiral
#' as organic and without a preferred direction.
#'
#' `position_beeswarm()` in the 'ggbeeswarm' package also offers a hexagonal
#' method, and does a different job. A beeswarm spreads points along one axis to
#' show the shape of a distribution, so perfectly over-plotted points come out as
#' a line rather than a cluster, and its hexagonal and square methods move points
#' off their true value on the data axis. Reach for a beeswarm to show a
#' distribution, and for this to show a count.
#'
#' @family Functions
#'
#' @inheritParams position_sunflower
#'
#' @returns A `ggproto` object of class `PositionHoneycomb`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(
#'     x = rep(1:3, times = 60),
#'     y = rep(1:3, times = 60)
#'   )
#'
#'   ggplot(dat, aes(x, y)) +
#'     geom_point(size = 1, position = position_honeycomb(density = 4)) +
#'     coord_equal()
#'
position_honeycomb <- function(density = 1, aspect_ratio = 1) {
  ggplot2::ggproto(NULL, PositionHoneycomb, density = density, aspect_ratio = aspect_ratio)
}

PositionHoneycomb <-
  ggplot2::ggproto(
    "PositionHoneycomb",
    ggplot2::Position,
    compute_panel = function(self, data, params, scales) {
      cells <- split(seq_len(nrow(data)), interaction(data$x, data$y, drop = TRUE))

      for (rows in cells) {
        offsets <- honeycomb(length(rows), self$density)
        data$x[rows] <- data$x[rows] + offsets[, 1]
        data$y[rows] <- data$y[rows] + offsets[, 2] / self$aspect_ratio
      }

      data
    }
  )

#' Arrange over-plotted points on a honeycomb lattice and dodge groups side-to-side
#'
#' This function applies the honeycomb position adjustment alongside the dodge
#' position adjustment, arranging overlapping points per x, y, and group on a
#' hexagonal lattice. See the [position_honeycomb()] documentation for more
#' information.
#'
#' @family Functions
#'
#' @inheritParams position_sunflower
#'
#' @param width The dodging width, which defaults to 1.
#' @param orientation The axis along which groups are separated, either
#' `"x"` (the default, side-to-side) or `"y"` (up and down). Matches the
#' argument of the same name in [ggplot2::position_dodge()].
#'
#' @returns A `ggproto` object of class `PositionHoneycombDodge`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(
#'     x = rep(1, 300),
#'     y = rep(1, 300),
#'     type = factor(sample(LETTERS[1:2], 300, replace = TRUE))
#'   )
#'
#'   ggplot(dat, aes(x, y, color = type, shape = type)) +
#'     geom_point(position = position_honeycombdodge(width = 1, density = 30)) +
#'     coord_equal()
#'
position_honeycombdodge <- function(width = 1, density = 1, aspect_ratio = 1, orientation = "x") {
  ggplot2::ggproto(NULL, PositionHoneycombDodge, width = width, density = density,
                   aspect_ratio = aspect_ratio, orientation = orientation)
}

PositionHoneycombDodge <-
  ggplot2::ggproto(
    "PositionHoneycombDodge",
    ggplot2::PositionDodge,
    setup_params = function(self, data) {
      ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)
    },
    compute_panel = function(self, data, params, scales) {
      data <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_panel(data, params, scales)

      cells <- split(seq_len(nrow(data)), interaction(data$x, data$y, data$group, drop = TRUE))

      for (rows in cells) {
        offsets <- honeycomb(length(rows), self$density)
        data$x[rows] <- data$x[rows] + offsets[, 1]
        data$y[rows] <- data$y[rows] + offsets[, 2] / self$aspect_ratio
      }

      data
    }
  )

# The n lattice sites closest to the origin, so the cluster grows outward as a
# rough disc. Spacing depends on density alone and not on n, which is what keeps
# the pattern constant while the cluster grows, and it is chosen so that n points
# cover the same area as position_sunflower() at the same density.
honeycomb <- function(n, density) {
  if (n == 1) {
    return(matrix(0, nrow = 1, ncol = 2))
  }

  spacing <- sqrt(2 * pi / (100 * sqrt(3) * density))

  # A lattice this wide always holds at least n sites.
  reach <- ceiling(sqrt(n)) + 1
  grid <- expand.grid(column = -reach:reach, row = -reach:reach)

  x <- (grid$column + grid$row / 2) * spacing
  y <- grid$row * (sqrt(3) / 2) * spacing

  # Ties are broken by angle so that equidistant sites fill in a stable order
  # rather than in whatever order expand.grid happened to produce.
  ordering <- order(x ^ 2 + y ^ 2, atan2(y, x))
  chosen <- ordering[seq_len(n)]

  cbind(x[chosen], y[chosen])
}
