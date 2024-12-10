#' Distribute points in a ellipse by the sunflower seed algorithm
#'
#' This function adapts the code here # https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle for implementing the sunflower algorithm
#'
#' @param x xposition
#' @param y yposition
#' @param width radius 1
#' @param height radius 2
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' N <- 300
#' dat <- tibble(
#'   x = sample(1:2, size = N, replace = TRUE),
#'   y = sample(1:7, size = N, replace = TRUE),
#'   type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#' ) %>%
#'   group_by(x, y, type) %>%
#'   mutate(
#'     x_s = sunflower(x = x, width = 0.05, height = 0.3),
#'     y_s = sunflower(y = y, width = 0.05, height = 0.3),
#'     x_s = if_else(type == "A", x_s - (1 / 8), x_s + (1 / 8))
#'   )
#'
#' ggplot(dat, aes(x_s, y_s, color = type, shape = type)) +
#'   geom_point()
#'
sunflower <-
  function(x = NULL, y = NULL, width, height) {
    # https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle
    if (!is.null(x)) {
      n <- length(x)
    } else if (!is.null(y)) {
      n <- length(y)
    } else {
      stop("gotta provide either x or y")
    }

    alpha = 2
    b <- round(alpha * sqrt(n))  # number of boundary points
    phi <- (sqrt(5) + 1) / 2  # golden ratio

    # this adjusts to the implied density of 100 points within the given width/height dimensions
    width <- width / sqrt(100 / n)
    height <- height / sqrt(100 / n)

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



PositionSunflower <-
  ggplot2::ggproto(
    "PositionSunflower",
    ggplot2::Position,
    compute_panel = function(self, data, params, scales) {
      # I could not find a good way to split into groups using base R, so here I use dplyr
      flowers <- data |>
        dplyr::group_by(x, y, group) |>
        dplyr::group_split()

      data <- do.call(rbind, lapply(flowers, function(flower) {
        flower$x = sunflower(
          x = flower$x,
          width = self$flower_width,
          height = self$flower_height
        )
        flower$y = sunflower(
          y = flower$y,
          width = self$flower_width,
          height = self$flower_height
        )
        return(flower)
      }))
      return(data)
    }
  )


#' Sunflower position adjustment
#'
#' @param flower_width width of flower
#' @param flower_height height of flower
#'
#' @export
#'
#' @examples
#'
#' dat <- tibble(
#' x = rep(1:4, times = 100),
#' y = rep(1:4, times = 100)
#' )
#'
#' ggplot(dat, aes(x = x, y = y)) +
#'   geom_point(size = 1, position = position_sunflower(flower_width = 0.5, flower_height = 0.5)) +
#'   xlim(0, 5) +
#'   ylim(0, 5)
#'
#'
#'
position_sunflower <-
  function(flower_width = 1,
           flower_height = 1) {
    ggplot2::ggproto(NULL,
                     PositionSunflower,
                     flower_width = flower_width,
                     flower_height = flower_height)
  }


PositionSunflowerDodge <-
  ggplot2::ggproto(
    "PositionSunflowerDodge",
    ggplot2::PositionDodge,
    setup_params = function(self, data) {
      params <- ggproto_parent(PositionDodge, self)$setup_params(data)
      return(params)
    },
    compute_panel = function(self, data, params, scales) {
      data <- ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)

      # I could not find a good way to split into groups using base R, so here I use dplyr
      flowers <- data |>
        dplyr::group_by(x, y, group) |>
        dplyr::group_split()

      data <- do.call(rbind, lapply(flowers, function(flower) {
        flower$x = sunflower(
          x = flower$x,
          width = self$flower_width,
          height = self$flower_height
        )
        flower$y = sunflower(
          y = flower$y,
          width = self$flower_width,
          height = self$flower_height
        )
        return(flower)
      }))

      return(data)
    }
  )

#' Sunflower position adjustment (dodged)
#'
#' @param width width of dodge
#' @param flower_width width of flower
#' @param flower_height height of flower
#'
#' @export
#'
#' @examples
#'
#' N <- 300
#'
#' dat <- tibble(
#'   x = sample(1:2, size = N, replace = TRUE),
#'   y = sample(1:7, size = N, replace = TRUE),
#'   type = factor(sample(LETTERS[1:2], N, replace = TRUE))
#' )
#'
#' ggplot(dat, aes(x, y, color = type, shape = type)) +
#'   geom_point(position = position_sunflowerdodge(width = 1, flower_width = 0.1, flower_height = 0.3))
#'
position_sunflowerdodge <-
  function(width = NULL,
           flower_width = 1,
           flower_height = 1) {
    ggplot2::ggproto(
      NULL,
      PositionSunflowerDodge,
      width = width,
      flower_width = flower_width,
      flower_height = flower_height
    )
  }
