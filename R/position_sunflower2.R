#' Distributes points using a "sunflower seed" algorithm.
#' 
#' This function distributes points in a ellipse via the sunflower seed algorithm, as a solution for over-plotting.
#' To implement the algorithm, this function adapts the code from https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle.
#'
#' @param x x position
#' @param y y position
#' @param density seed density
#' @param aspect_ratio aspect ratio adjustment
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' # Adjust position manually, arranging points per the sunflower algorithm and then dodging groups
#' N <- 300
#' dat <- tibble(
#'   x = sample(1:2, size = N, replace = TRUE),
#'   y = sample(1:7, size = N, replace = TRUE),
#'   type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#' ) %>%
#'   group_by(x, y, type) %>%
#'   mutate(
#'     x = sunflower2(x = x, density = 1, aspect_ratio = 1),
#'     y = sunflower2(y = y, density = 1, aspect_ratio = 1),
#'     x = if_else(type == "A", x - (1 / 8), x + (1 / 8))
#'   )
#'
#' ggplot(dat, aes(x, y, color = type, shape = type)) +
#'   geom_point() + coord_equal()
#'
sunflower2 <- function(x = NULL, y = NULL, density, aspect_ratio) {
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

#' Arranges over-plotted points in a sunflower pattern.
#' 
#' This function applies the sunflower algorithm, as executed by the sunflower function, as a position adjustment,
#' arranging overlapping points at any given x and y into a sunflower pattern.
#'
#' @param density seed density
#' @param aspect_ratio aspect ratio adjustment 
#'
#' @export
#'
#' @examples
#' 
#' library(tidyverse)
#' # Use the sunflower position function to arrange points
#' N <- 100
#' dat <- tibble(
#'   x = rep(1:4, times = N),
#'   y = rep(1:4, times = N)
#' )
#'
#' ggplot(dat, aes(x = x, y = y)) +
#'   geom_point(size = 1, position = position_sunflower2(density = 1, aspect_ratio = 1)) +
#'   xlim(0, 5) +
#'   ylim(0, 5) +
#'   coord_equal()
#'
position_sunflower2 <- function(density = 1, aspect_ratio = 1) {
  ggplot2::ggproto(NULL, PositionSunflower2, density = density, aspect_ratio = aspect_ratio)
}

PositionSunflower2 <-
  ggplot2::ggproto(
    "PositionSunflower2",
    ggplot2::Position,
    compute_panel = function(self, data, params, scales) {
      flowers <- split(data, interaction(data$x, data$y, drop = TRUE))
      
      data <- do.call(rbind, lapply(flowers, function(flower) {
        flower$x = sunflower2(
          x = flower$x,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        flower$y = sunflower2(
          y = flower$y,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        return(flower)
      }))
      return(data)
    }
  )

#' Arranges over-plotted points in a sunflower pattern and dodges groups side-to-side.
#' 
#' This function applies the sunflower position adjustment alongside the dodge position adjustment,
#' arranging overlapping points per x, y, AND group into a sunflower pattern.
#'
#' @param width dodging width
#' @param density seed density
#' @param aspect_ratio aspect ratio adjustment 
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' 
#' # Use the sunflower dodge position function to arrange and dodge points.
#' N <- 300
#' dat <- tibble(
#'   x = sample(1:2, size = N, replace = TRUE),
#'   y = sample(1:7, size = N, replace = TRUE),
#'   type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#' )
#'
#' ggplot(dat, aes(x, y, color = type, shape = type)) +
#'   geom_point(position = position_sunflowerdodge2(width = 1, density = 1, aspect_ratio = 1)) +
#'   coord_equal()
#'
position_sunflowerdodge2 <- function(width = 1, density = 1, aspect_ratio = 1) {
  ggplot2::ggproto(NULL, PositionSunflowerDodge2, width = width, density = density, aspect_ratio = aspect_ratio)
}

PositionSunflowerDodge2 <-
  ggplot2::ggproto(
    "PositionSunflowerDodge2",
    ggplot2::PositionDodge,
    setup_params = function(self, data) {
      params <- ggproto_parent(PositionDodge, self)$setup_params(data)
      return(params)
    },
    compute_panel = function(self, data, params, scales) {
      data <- ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
      
      flowers <- split(data, interaction(data$x, data$y, data$group, drop = TRUE))
      
      data <- do.call(rbind, lapply(flowers, function(flower) {
        flower$x = sunflower2(
          x = flower$x,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        flower$y = sunflower2(
          y = flower$y,
          density = self$density,
          aspect_ratio = self$aspect_ratio
        )
        return(flower)
      }))
      
      return(data)
    }
  )
