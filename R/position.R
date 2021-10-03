#' Jitter points (on an ellipse) to avoid overplotting
#'
#' Adding ellipical random noise to points that are perfectly overplotted gives a pleasing way to visualize many plotted points that represent the same position. In contrast to position_jitter which samples on a rectangle, position_jitter_ellipse samples on an ellipse.
#'
#' See: https://stackoverflow.com/questions/5529148/algorithm-calculate-pseudo-random-point-inside-an-ellipse and https://stats.stackexchange.com/questions/120527/simulate-a-uniform-distribution-on-a-disc
#'
#' @inheritParams ggplot2::position_jitter
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' dat <- data.frame(x = rep(1, 500), y = rep(1, 500))
#'
#' ggplot(dat, aes(x, y)) +
#' geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
#' coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
#'
#' # for comparision:
#'
#' ggplot(dat, aes(x, y)) +
#' geom_point(position = position_jitter(width = 0.5, height = 0.5)) +
#' coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
#'
#'
position_jitter_ellipse <- function(width = NULL,
                                    height = NULL,
                                    seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(
    NULL,
    PositionJitterEllipse,
    width = width,
    height = height,
    seed = seed
  )
}

#' @rdname position_jitter_ellipse
#' @format NULL
#' @usage NULL
#' @export
PositionJitterEllipse <-
  ggproto(
    "PositionJitterEllipse",
    Position,
    required_aes = c("x", "y"),

    setup_params = function(self, data) {
      list(
        width = self$width %||% (resolution(data$x, zero = FALSE) * 0.4),
        height = self$height %||% (resolution(data$y, zero = FALSE) * 0.4),
        seed = self$seed
      )
    },
    # https://stackoverflow.com/questions/5529148/algorithm-calculate-pseudo-random-point-inside-an-ellipse
    # https://stats.stackexchange.com/questions/120527/simulate-a-uniform-distribution-on-a-disc
    compute_layer = function(self, data, params, layout) {
      trans_x <-
        function(x) {
          set.seed(params$seed)
          n <- length(x)
          rho <- sqrt(runif(n)) * params$width
          theta <- runif(n, 0, 2 * pi)
          x + rho * cos(theta)
        }
      trans_y <-
        function(x) {
          set.seed(params$seed)
          n <- length(x)
          rho <- sqrt(runif(n)) * params$height
          theta <- runif(n, 0, 2 * pi)
          x + rho * sin(theta)
        }

      with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
    }
  )


#' Set points (on an ellipse) in a sunflower seed arrangement to avoid overplotting
#'
#'
#' See: https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle
#'
#' @inheritParams ggplot2::position_jitter
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' dat <- data.frame(x = rep(1, 500), y = rep(1, 500))
#'
#' ggplot(dat, aes(x, y)) +
#'   geom_point(position = position_sunflower(width = 0.5, height = 0.5)) +
#'   coord_equal(xlim = c(0, 2), ylim = c(0, 2))
#'
#' ggplot(dat, aes(x, y)) +
#'   geom_point(position = position_sunflower(width = 1, height = 0.5)) +
#'   coord_equal(xlim = c(0, 2), ylim = c(0, 2))

position_sunflower <- function(width = NULL,
                                    height = NULL,
                               nudge_x = 0,
                               nudge_y = 0,
                                    seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(
    NULL,
    PositionSunflower,
    width = width,
    height = height,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    seed = seed
  )
}



#' @rdname position_sunflower
#' @format NULL
#' @usage NULL
#' @export
PositionSunflower <-
  ggproto(
    "PositionSunflower",
    Position,
    required_aes = c("x", "y"),

    setup_params = function(self, data) {
      list(
        width = self$width %||% (resolution(data$x, zero = FALSE) * 0.4),
        height = self$height %||% (resolution(data$y, zero = FALSE) * 0.4),
        nudge_x = self$nudge_x %||% (resolution(data$x, zero = FALSE) * 0.4),
        nudge_y = self$nudge_y %||% (resolution(data$y, zero = FALSE) * 0.4),
        seed = self$seed
      )
    },
    # https://stackoverflow.com/questions/5529148/algorithm-calculate-pseudo-random-point-inside-an-ellipse
    # https://stats.stackexchange.com/questions/120527/simulate-a-uniform-distribution-on-a-disc
    compute_layer = function(self, data, params, layout) {

      trans_x <-
        function(x) {
          # https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle

          alpha = 2
          n <- length(x)
          b <- round(alpha * sqrt(n))  # number of boundary points
          phi <- (sqrt(5) + 1) / 2  # golden ratio

          radius <- function(k, n, b) {
            ifelse(k > n - b,
                   1,
                   sqrt(k - 1 / 2) / sqrt(n - (b + 1) / 2))
          }

          r <- radius(1:n, n, b)

          theta <- 1:n * (2 * pi / phi ^ 2)
          x + r * params$width * cos(theta) + params$nudge_x
        }

      trans_y <-
        function(y) {

          alpha = 2
          n <- length(y)
          b <- round(alpha * sqrt(n))  # number of boundary points
          phi <- (sqrt(5) + 1) / 2  # golden ratio

          radius <- function(k, n, b) {
            ifelse(k > n - b,
                   1,
                   sqrt(k - 1 / 2) / sqrt(n - (b + 1) / 2))
          }
          r <- radius(1:n, n, b)

          theta <- 1:n * (2 * pi / phi ^ 2)
          y + r * params$height * sin(theta) + params$nudge_y
        }

      with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
    }
  )







#' Jitter and dodge points (on an ellipse) to avoid overplotting
#'
#'
#' @inheritParams ggplot2::position_jitterdodge
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
# dat <- data.frame(x = rep(1, 500),
#                   y = rep(1, 500),
#                   group = sample(LETTERS[1:2], 500, replace = TRUE))
#
# ggplot(dat, aes(x, y, shape = group)) +
#   geom_point(position = position_jitterdodge_ellipse(jitter.width  = 0.5,
#                                                      jitter.height =  0.5,
#                                                      dodge.width = 1)) +
#   coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

position_jitterdodge_ellipse <-
  function(jitter.width = NULL,
           jitter.height = 0,
           dodge.width = 0.75,
           seed = NA)
  {
    if (!is.null(seed) && is.na(seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    }
    ggproto(NULL, PositionJitterdodgeEllipse,
            jitter.width = jitter.width,
            jitter.height = jitter.height,
            dodge.width = dodge.width,
            seed = seed)
  }

