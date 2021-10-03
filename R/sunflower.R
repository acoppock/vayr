#' Distribute points in a ellipse by the sunflower seed algorithm
#'
#'
#'
#' @param x xposition
#' @param y yposition
#' @param width radius 1
#' @param height radius 2
#'
#' @return
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
    if(!is.null(x)){
      n <- length(x)
    } else if (!is.null(y)){
      n <- length(y)
    } else {
      stop("gotta provide either x or y")
    }

    alpha = 2
    b <- round(alpha*sqrt(n))  # number of boundary points
    phi <- (sqrt(5)+1)/2  # golden ratio


    radius <- function(k, n, b) {
      ifelse(k > n - b,
             1,
             sqrt(k - 1 / 2) / sqrt(n - (b + 1) / 2))
    }

    r <- radius(1:n,n,b)
    theta <- 1:n * (2*pi/phi^2)

    if(!is.null(x)){
      return(x + width*r*cos(theta))
    } else{
      return(y + height*r*sin(theta))
    }

  }
