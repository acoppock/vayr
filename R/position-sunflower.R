#' Distribute points using a sunflower seed algorithm
#'
#' This function distributes points in a ellipse via a sunflower seed algorithm as a solution for over-plotting.
#' To implement the algorithm, this function adapts the code from https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle.
#'
#' @family Functions
#'
#' @param x,y The identical coordinates of multiple over-plotted points, as vectors, which will
#' be arranged using a sunflower seed algorithm.
#' @param density The pattern density.
#' @param aspect_ratio An aspect ratio adjustment to compensate for distortion of the circular arrangement,
#' which might occur when plotting if coord_equal() is not used. A wide aspect ratio (eg. 2)
#' would adjust for vertical stretching, whereas a tall aspect ratio (eg. 0.5) would adjust for
#' horizontal stretching. An aspect ratio of 1 is appropriate when no adjustment is required.
#'
#' @returns A numeric vector of adjusted `x` or `y` positions, computed using a sunflower seed algorithm.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'   library(dplyr)
#'
#'   # Manually adjust position of N points,
#'   # arranging them per the sunflower algorithm and then dodging groups
#'   N <- 300
#'
#'   dat <- data.frame(
#'     x = sample(1:2, size = N, replace = TRUE),
#'     y = sample(1:7, size = N, replace = TRUE),
#'     type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#'   ) |>
#'     group_by(x, y, type) |>
#'     mutate(
#'       x = sunflower(x = x, density = 1, aspect_ratio = 1),
#'       y = sunflower(y = y, density = 1, aspect_ratio = 1),
#'       x = if_else(type == "A", x - (1 / 8), x + (1 / 8))
#'     )
#'
#'   ggplot(dat, aes(x, y, color = type, shape = type)) +
#'     geom_point() + coord_equal()
#'
sunflower <- function(x = NULL, y = NULL, density, aspect_ratio) {
    if (!is.null(x)) {
      n <- length(x)
    } else if (!is.null(y)) {
      n <- length(y)
    } else {
      stop("requires either x or y")
    }

    radius = 0.5 # constant radius

    alpha = 2
    b <- round(alpha * sqrt(n))  # number of boundary points
    phi <- (sqrt(5) + 1) / 2  # golden ratio

    width <- radius / sqrt((100 * density) / n)
    height <- (radius / sqrt((100 * density) / n)) / aspect_ratio

    radius <-
      function(k, n, b) {
        ifelse(k > n - b, 1, sqrt(k - 1 / 2) / sqrt(n - (b + 1) / 2))
      }

    r <- radius(1:n, n, b)
    theta <- 1:n * (2 * pi / phi ^ 2)

    if (!is.null(x)) {
      return(x + width * r * cos(theta))
    } else{
      return(y + height * r * sin(theta))
    }

  }

#' Arrange over-plotted points in a sunflower pattern
#'
#' This function applies the sunflower algorithm, as executed by the sunflower function, as a position adjustment,
#' arranging overlapping points at any given x and y into a sunflower pattern. See the 'sunflower()' documentation for
#' more information.
#'
#' @family Functions
#'
#' @param density The pattern density, which defaults to 1 but will have to be adjusted in most cases.
#' The desirable density will depend on both the ranges of the axes and the dimensions of the image.
#' @param aspect_ratio An aspect ratio adjustment to compensate for distortion of the circular arrangement,
#' which might occur when plotting if coord_equal() is not used. A wide aspect ratio (eg. 2)
#' would adjust for vertical stretching, whereas a tall aspect ratio (eg. 0.5) would adjust for
#' horizontal stretching. The default aspect ratio of 1 is appropriate when no adjustment is required.
#'
#'
#' @returns A `ggproto` object of class `PositionSunflower`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   # Use the sunflower position function to arrange N points
#'   N <- 100
#'
#'   dat <- data.frame(
#'     x = rep(1:4, times = N),
#'     y = rep(1:4, times = N)
#'   )
#'
#'   ggplot(dat, aes(x = x, y = y)) +
#'     geom_point(size = 1, position = position_sunflower(density = 1, aspect_ratio = 1)) +
#'     xlim(0, 5) +
#'     ylim(0, 5) +
#'     coord_equal()
#'
position_sunflower <- function(density = 1, aspect_ratio = 1) {
  ggplot2::ggproto(NULL, PositionSunflower, density = density, aspect_ratio = aspect_ratio)
}

PositionSunflower <-
  ggplot2::ggproto(
    "PositionSunflower",
    ggplot2::Position,
    compute_panel = function(self, data, params, scales) {
      flowers <- split(data, interaction(data$x, data$y, drop = TRUE))

      data <- do.call(rbind, lapply(flowers, function(flower) {
        flower$x = sunflower(
          x = flower$x,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        flower$y = sunflower(
          y = flower$y,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        return(flower)
      }))
      return(data)
    }
  )

#' Arrange over-plotted points in a sunflower pattern and dodge groups side-to-side
#'
#' This function applies the sunflower position adjustment alongside the dodge position adjustment,
#' arranging overlapping points per x, y, and group into a sunflower pattern. See the 'sunflower()' documentation for
#' more information.
#'
#' @family Functions
#'
#' @param width The dodging width, which defaults to 1.
#' @param density The pattern density, which defaults to 1 but will have to be adjusted in most cases.
#' The desirable density will depend on both the ranges of the axes and the dimensions of the image.
#' @param aspect_ratio An aspect ratio adjustment to compensate for distortion of the circular arrangement,
#' which might occur when plotting if coord_equal() is not used. A wide aspect ratio (eg. 2)
#' would adjust for vertical stretching, whereas a tall aspect ratio (eg. 0.5) would adjust for
#' horizontal stretching. The default aspect ratio of 1 is appropriate when no adjustment is required.
#'
#' @returns A `ggproto` object of class `PositionSunflowerDodge`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   # Use the sunflower dodge position function to arrange and dodge N points.
#'   N <- 300
#'
#'   dat <- data.frame(
#'     x = sample(1:2, size = N, replace = TRUE),
#'     y = sample(1:7, size = N, replace = TRUE),
#'     type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#'   )
#'
#'   # With coord_equal
#'   ggplot(dat, aes(x, y, color = type, shape = type)) +
#'     geom_point(position = position_sunflowerdodge(width = 0.5, density = 2, aspect_ratio = 1)) +
#'     coord_equal()
#'
#'   # Without coord_equal, might want to play with aspect ratio to get a pleasing plot
#'   ggplot(dat, aes(x, y, color = type, shape = type)) +
#'     geom_point(position = position_sunflowerdodge(width = 0.5, density = 10, aspect_ratio = 1/4))
#'
position_sunflowerdodge <- function(width = 1, density = 1, aspect_ratio = 1) {
  ggplot2::ggproto(NULL, PositionSunflowerDodge, width = width, density = density, aspect_ratio = aspect_ratio)
}

PositionSunflowerDodge <-
  ggplot2::ggproto(
    "PositionSunflowerDodge",
    ggplot2::PositionDodge,
    setup_params = function(self, data) {
      params <- ggplot2::ggproto_parent(PositionDodge, self)$setup_params(data)
      return(params)
    },
    compute_panel = function(self, data, params, scales) {
      data <- ggplot2::ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)

      flowers <- split(data, interaction(data$x, data$y, data$group, drop = TRUE))

      data <- do.call(rbind, lapply(flowers, function(flower) {
        flower$x = sunflower(
          x = flower$x,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        flower$y = sunflower(
          y = flower$y,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        return(flower)
      }))

      return(data)
    }
  )
