#' Jitter points on an ellipse to avoid over-plotting
#'
#' This function adds elliptical random noise to perfectly over-plotted points,
#' offering a pleasing way to visualize many points that represent the same position.
#' In contrast to the position_jitter() function which samples from a rectangular field,
#' the position_jitter_ellipse() function samples from an elliptical field.
#' This function takes algorithmic inspiration from https://stackoverflow.com/questions/5529148/algorithm-calculate-pseudo-random-point-inside-an-ellipse
#' and https://stats.stackexchange.com/questions/120527/simulate-a-uniform-distribution-on-a-disc.
#'
#' @family Functions
#'
#' @param width,height The dimensions of the elliptical field,
#' from which over-plotted points are sampled.
#' @param seed A random seed for reproducibility.
#'
#' @returns A `ggproto` object of class `PositionJitterEllipse`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(x = rep(1, 500), y = rep(1, 500))
#'
#'   # Jitter on an ellipse.
#'   ggplot(dat, aes(x, y)) +
#'     geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
#'     coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
#'
#'   # Jitter on a rectangle, for comparison.
#'   ggplot(dat, aes(x, y)) +
#'     geom_point(position = position_jitter(width = 0.5, height = 0.5)) +
#'     coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
#'
position_jitter_ellipse <- function(width = NULL, height = NULL, seed = NA) {
    if (!is.null(seed) && is.na(seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    }
    ggplot2::ggproto(NULL, PositionJitterEllipse, width = width, height = height, seed = seed)
  }

PositionJitterEllipse <-
  ggplot2::ggproto(
    "PositionJitterEllipse",
    ggplot2::Position,
    required_aes = c("x", "y"),
    setup_params = function(self, data) {
      list(
        width = self$width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4),
        height = self$height %||% (ggplot2::resolution(data$y, zero = FALSE) * 0.4),
        seed = self$seed
      )
    },
    compute_layer = function(self, data, params, layout) {
      with_seed_null(params$seed, jitter_ellipse(data, params$width, params$height))
    }
  )

#' Jitter points on an ellipse and dodge groups side-to-side
#'
#' This function dodges groups of points side-to-side and adds elliptical random noise
#' to perfectly over-plotted points. See the position_jitter_ellipse() documentation for more information.
#'
#' @family Functions
#'
#' @param jitter.width,jitter.height The dimensions of the elliptical field,
#' from which over-plotted points are sampled.
#' @param dodge.width The dodging width, which defaults to 1.
#' @param seed A random seed for reproducibility.
#'
#' @returns A `ggproto` object of class `PositionJitterdodgeEllipse`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(x = rep(1, 500), y = rep(1, 500),
#'                     group = sample(LETTERS[1:2], 500, replace = TRUE))
#'
#'   ggplot(dat, aes(x, y, shape = group, color = group)) +
#'     geom_point(position = position_jitterdodge_ellipse(jitter.width  = 0.5,
#'                                                        jitter.height =  0.5,
#'                                                        dodge.width = 1)) +
#'     coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
#'
position_jitterdodge_ellipse <- function(jitter.width = NULL, jitter.height = NULL, dodge.width = 1, seed = NA) {
    if (!is.null(seed) && is.na(seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    }
    ggplot2::ggproto(NULL, PositionJitterDodgeEllipse, jitter.width = jitter.width, jitter.height = jitter.height, dodge.width = dodge.width, seed = seed)
  }

PositionJitterDodgeEllipse <-
  ggplot2::ggproto(
    "PositionJitterDodgeEllipse",
    ggplot2::PositionDodge,
    required_aes = c("x", "y"),
    setup_params = function(self, data) {
      params <- list(
        jitter.width = self$jitter.width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4),
        jitter.height = self$jitter.height %||% (ggplot2::resolution(data$y, zero = FALSE) * 0.4),
        seed = self$seed
      )

      self$width <- self$dodge.width
      dodge_params <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)

      modifyList(dodge_params, params)
    },
    compute_layer = function(self, data, params, layout) {
      data <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_panel(data, params, scales = NULL)

      # Jitter the dodged layer in one pass. Jittering group by group would hand
      # every group the same draw, making one group a copy of the next.
      with_seed_null(params$seed, jitter_ellipse(data, params$jitter.width, params$jitter.height))
    }
  )
