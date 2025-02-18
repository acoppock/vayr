#' Distribute points using a "sunflower seed" algorithm
#'
#' This function distributes points in a ellipse via the sunflower seed algorithm, as a solution for over-plotting.
#' To implement the algorithm, this function adapts the code from https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle.
#'
#' @param x x position
#' @param y y position
#' @param density seed density
#' @param aspect_ratio aspect ratio adjustment
#'
#' @returns A numeric vector of adjusted `x` or `y` positions, computed using a sunflower seed distribution.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'   library(dplyr)
#'
#'   # Manually adjust position of N points,
#'   # arranging points per the sunflower algorithm and then dodging groups
#'   N <- 300
#'
#'   dat <- data.frame(
#'     x = sample(1:2, size = N, replace = TRUE),
#'     y = sample(1:7, size = N, replace = TRUE),
#'     type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#'   ) %>%
#'     group_by(x, y, type) %>%
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
#' arranging overlapping points at any given x and y into a sunflower pattern.
#'
#' @param density seed density
#' @param aspect_ratio aspect ratio adjustment
#'
#' @returns A `ggproto` object of class `PositionSunflower`
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

#' Arrange over-plotted points in a sunflower pattern and dodges groups side-to-side
#'
#' This function applies the sunflower position adjustment alongside the dodge position adjustment,
#' arranging overlapping points per x, y, AND group into a sunflower pattern.
#'
#' @param width dodging width
#' @param density seed density
#' @param aspect_ratio aspect ratio adjustment
#'
#' @returns A `ggproto` object of class `PositionSunflowerDodge`
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
#'     geom_point(position = position_sunflowerdodge(width = 0.5, density = 10, aspect_ratio = 0.25))
#'
#'   # As applied to the Patriot Act experiment
#'   ggplot(patriot_act, aes(T1_content, PA_support, color = pid_3, group = pid_3)) +
#'     geom_point(size = 0.25, position = position_sunflowerdodge(width = 0.5,
#'                                                               density = 10,
#'                                                               aspect_ratio = 6/7)) +
#'     scale_color_manual(values = c("blue", "red")) +
#'     facet_wrap(~sample_label) +
#'     stat_smooth(position = position_dodge(width = 0.5))
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
