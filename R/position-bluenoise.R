#' Scatter over-plotted points evenly at random
#'
#' This function spreads perfectly over-plotted points across an elliptical
#' field, like [position_jitter_ellipse()], but places them so that no two land
#' much closer together than the rest. Sampling uniformly at random, which is
#' what jittering does, leaves visible knots and voids: in a draw of 200 points
#' the closest pair typically sits about a fifteenth of the median spacing
#' apart. A reader cannot tell those knots from real structure. The arrangement
#' here has the even spacing of [position_sunflower()] while still looking
#' unstructured, so no reader mistakes a spiral arm for a finding.
#'
#' The pattern is the one the eye's own photoreceptors are laid out in, known as
#' blue noise or a Poisson-disc distribution. It is produced by Mitchell's
#' best-candidate algorithm: each point is the best of `candidates` random draws,
#' where best means farthest from every point already placed.
#'
#' @family Functions
#'
#' @param width,height The dimensions of the elliptical field the points are
#' spread across.
#' @param candidates The number of random draws considered for each point. Larger
#' values space the points more evenly and take longer. The default of 10 is
#' enough to remove essentially all of the clumping.
#' @param seed A random seed for reproducibility.
#'
#' @returns A `ggproto` object of class `PositionBlueNoise`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(x = rep(1, 400), y = rep(1, 400))
#'
#'   # Evenly scattered.
#'   ggplot(dat, aes(x, y)) +
#'     geom_point(position = position_bluenoise(width = 0.5, height = 0.5)) +
#'     coord_equal(xlim = c(0, 2), ylim = c(0, 2))
#'
#'   # Uniformly jittered, for comparison. Note the knots and the gaps.
#'   ggplot(dat, aes(x, y)) +
#'     geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
#'     coord_equal(xlim = c(0, 2), ylim = c(0, 2))
#'
position_bluenoise <- function(width = NULL, height = NULL, candidates = 10, seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  ggplot2::ggproto(NULL, PositionBlueNoise,
                   width = width, height = height,
                   candidates = candidates, seed = seed)
}

PositionBlueNoise <-
  ggplot2::ggproto(
    "PositionBlueNoise",
    ggplot2::Position,
    required_aes = c("x", "y"),
    setup_params = function(self, data) {
      list(
        width = self$width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4),
        height = self$height %||% (ggplot2::resolution(data$y, zero = FALSE) * 0.4),
        candidates = self$candidates,
        seed = self$seed
      )
    },
    compute_layer = function(self, data, params, layout) {
      cells <- split(seq_len(nrow(data)), interaction(data$x, data$y, drop = TRUE))
      with_seed_null(
        params$seed,
        scatter_bluenoise(data, cells, params$width, params$height, params$candidates)
      )
    }
  )

# Points that share a position have to be separated from each other, so the
# scatter is computed per over-plotted position rather than across the layer.
# The caller decides what counts as a position, since the dodged version has to
# treat each group separately.
scatter_bluenoise <- function(data, cells, width, height, candidates) {
  for (rows in cells) {
    offsets <- best_candidate_disc(length(rows), candidates)
    data$x[rows] <- data$x[rows] + offsets[, 1] * width
    data$y[rows] <- data$y[rows] + offsets[, 2] * height
  }

  data
}

#' Scatter over-plotted points evenly at random and dodge groups side-to-side
#'
#' This function dodges groups of points side-to-side and then scatters the
#' points that share a position across an elliptical field, spacing them evenly.
#' See the [position_bluenoise()] documentation for more information.
#'
#' @family Functions
#'
#' @param scatter.width,scatter.height The dimensions of the elliptical field the
#' points are spread across.
#' @param dodge.width The dodging width, which defaults to 1.
#' @param orientation The axis along which groups are separated, either
#' `"x"` (the default, side-to-side) or `"y"` (up and down). Matches the
#' argument of the same name in [ggplot2::position_dodge()].
#' @param candidates The number of random draws considered for each point.
#' @param seed A random seed for reproducibility.
#'
#' @returns A `ggproto` object of class `PositionBlueNoiseDodge`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(x = rep(1, 400), y = rep(1, 400),
#'                     group = sample(LETTERS[1:2], 400, replace = TRUE))
#'
#'   ggplot(dat, aes(x, y, shape = group, color = group)) +
#'     geom_point(position = position_bluenoisedodge(scatter.width = 0.4,
#'                                                   scatter.height = 0.4,
#'                                                   dodge.width = 1)) +
#'     coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
#'
position_bluenoisedodge <- function(scatter.width = NULL, scatter.height = NULL,
                                    dodge.width = 1, candidates = 10, seed = NA,
                                    orientation = "x") {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  ggplot2::ggproto(NULL, PositionBlueNoiseDodge,
                   scatter.width = scatter.width, scatter.height = scatter.height,
                   dodge.width = dodge.width, candidates = candidates, seed = seed,
                   orientation = orientation)
}

PositionBlueNoiseDodge <-
  ggplot2::ggproto(
    "PositionBlueNoiseDodge",
    ggplot2::PositionDodge,
    required_aes = c("x", "y"),
    setup_params = function(self, data) {
      params <- list(
        scatter.width = self$scatter.width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4),
        scatter.height = self$scatter.height %||% (ggplot2::resolution(data$y, zero = FALSE) * 0.4),
        candidates = self$candidates,
        seed = self$seed
      )

      self$width <- self$dodge.width
      dodge_params <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)

      modifyList(dodge_params, params)
    },
    compute_layer = function(self, data, params, layout) {
      data <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_panel(data, params, scales = NULL)

      cells <- split(seq_len(nrow(data)), interaction(data$x, data$y, data$group, drop = TRUE))

      with_seed_null(
        params$seed,
        scatter_bluenoise(data, cells, params$scatter.width, params$scatter.height, params$candidates)
      )
    }
  )

# Mitchell's best-candidate algorithm over the unit disc. Each point is the best
# of `candidates` uniform draws, where best means farthest from the points
# already placed. Unlike dart throwing, it always returns exactly n points, which
# a position adjustment has to do.
best_candidate_disc <- function(n, candidates = 10) {
  points <- matrix(0, nrow = n, ncol = 2)
  if (n == 1) {
    return(points)
  }

  draw <- function(k) {
    radius <- sqrt(stats::runif(k))
    angle <- stats::runif(k, 0, 2 * pi)
    cbind(radius * cos(angle), radius * sin(angle))
  }

  points[1, ] <- draw(1)

  for (i in 2:n) {
    proposals <- draw(candidates)
    placed <- points[seq_len(i - 1), , drop = FALSE]

    distances <- sqrt(
      outer(proposals[, 1], placed[, 1], "-") ^ 2 +
        outer(proposals[, 2], placed[, 2], "-") ^ 2
    )
    nearest <- do.call(pmin, as.data.frame(distances))

    points[i, ] <- proposals[which.max(nearest), ]
  }

  points
}
