#' Arrange over-plotted points with a circle-packing algorithm
#'
#' This function uses a circle packing algorithm from the 'packcircles' package
#' to arrange perfectly over-plotted points of varying sizes into an elliptical area.
#'
#' @family Functions
#'
#' @inheritParams position_sunflower
#'
#' @param density The density of the circle pack, which defaults to 1 but will
#' have to be adjusted in most cases. The desirable density will depend on both
#' the ranges of the axes and the dimensions of the image. It will also depend
#' on the size scale.
#'
#' @returns A `ggproto` object of class `PositionCirclePack`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'   library(dplyr)
#'   library(randomizr)
#'   library(tibble)
#'
#'   dat <- data.frame(
#'     X = c(rep(0, 200)),
#'     Y = rep(0, 200),
#'     size = runif(200, 0, 1)
#'   )
#'
#'   ggplot(dat, aes(x = X, y = Y, size = size)) +
#'     geom_point(position = position_circlepack(density = 0.25, aspect_ratio = 1),
#'               alpha = 0.25) +
#'     coord_equal(xlim = c(-1, 1), ylim = c(-1, 1), expand = TRUE) +
#'     theme(legend.position = "none")
#'
#'   # Applied to a mock experiment with weighted groups
#'
#'   dat <-
#'     tibble(
#'       age_group = rep(c("young", "middle", "old"), c(100, 200, 300)),
#'       treatment = block_ra(age_group, block_m = c(50, 50, 50)),
#'       latent_outcome =
#'         case_when(age_group == "young" & treatment == 0 ~ 0.10,
#'                   age_group == "young" & treatment == 1 ~ 0.20,
#'                   age_group == "middle" & treatment == 0 ~ 0.40,
#'                   age_group == "middle" & treatment == 1 ~ 0.45,
#'                   age_group == "old" & treatment == 0 ~ 0.70,
#'                   age_group == "old" & treatment == 1 ~ 0.90),
#'       outcome = rbinom(600, size = 1,
#'                        prob = latent_outcome)
#'   )
#'
#'   dat <-
#'     dat |>
#'     mutate(
#'       treatment_prob =
#'         case_when(age_group == "young" ~ 50/100,
#'                   age_group == "middle" ~ 50/200,
#'                   age_group == "old" ~ 50/300),
#'       weights = 1/case_when(treatment == 1 ~ treatment_prob,
#'                             treatment == 0 ~ 1 - treatment_prob)
#'     )
#'
#'   ggplot(dat, aes(treatment, outcome, size = weights, color = age_group)) +
#'     geom_point(alpha = 0.5, position = position_circlepack(density = 0.5))
#'
position_circlepack <-function(density = 1, aspect_ratio = 1) {
    ggplot2::ggproto(NULL, PositionCirclePack, density = density, aspect_ratio = aspect_ratio)
  }

PositionCirclePack <-
  ggplot2::ggproto(
    "PositionCirclePack",
    ggplot2::Position,
    compute_panel = function(self, data, params, scales) {
      areas <- normalize_size(data) * (10 ^ -3) / self$density

      # Split row indices rather than the data, so rows come back in their
      # original order.
      pairs <- split(seq_len(nrow(data)), interaction(data$x, data$y, drop = TRUE))

      for (rows in pairs) {
        circle_layout <- packcircles::circleProgressiveLayout(areas[rows])

        data$x[rows] <- data$x[rows] + circle_layout$x
        data$y[rows] <- data$y[rows] + circle_layout$y / self$aspect_ratio
      }

      data
    }
  )

#' Arrange over-plotted points with a circle-packing algorithm and dodge groups side-to-side
#'
#' This function dodges groups and uses a circle packing algorithm from the 'packcircles' package
#' to arrange perfectly over-plotted points of varying sizes into an elliptical area.
#'
#' @family Functions
#'
#' @inheritParams position_circlepack
#'
#' @param width The dodging width, which defaults to 1.
#' @param orientation The axis along which groups are separated, either
#' `"x"` (the default, side-to-side) or `"y"` (up and down). Matches the
#' argument of the same name in [ggplot2::position_dodge()].
#'
#' @returns A `ggproto` object of class `PositionCirclePackDodge`.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(
#'     X = c(rep(0, 200)),
#'     Y = rep(0, 200),
#'     size = runif(200, 0, 1),
#'     id = (rep(c("A", "B"), 100))
#'   )
#'
#'   ggplot(dat, aes(x = X, y = Y, size = size, color = id)) +
#'     geom_point(position = position_circlepackdodge(width = 1, density = 1, aspect_ratio = 1),
#'               alpha = 0.25) +
#'     coord_equal(xlim = c(-1, 1), ylim = c(-1, 1), expand = TRUE) +
#'     scale_size_continuous(range = c(1, 3)) +
#'     theme(legend.position = "none")
#'
position_circlepackdodge <- function(width = 1, density = 1, aspect_ratio = 1, orientation = "x") {
    ggplot2::ggproto(NULL, PositionCirclePackDodge, width = width, density = density,
                     aspect_ratio = aspect_ratio, orientation = orientation)
  }

PositionCirclePackDodge <-
  ggplot2::ggproto(
    "PositionCirclePackDodge",
    ggplot2::PositionDodge,
    setup_params = function(self, data) {
      ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)
    },
    compute_panel = function(self, data, params, scales) {
      data <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_panel(data, params, scales)

      areas <- normalize_size(data) * (10 ^ -3) / self$density

      pairs <- split(seq_len(nrow(data)), interaction(data$x, data$y, data$group, drop = TRUE))

      for (rows in pairs) {
        circle_layout <- packcircles::circleProgressiveLayout(areas[rows])

        data$x[rows] <- data$x[rows] + circle_layout$x
        data$y[rows] <- data$y[rows] + circle_layout$y / self$aspect_ratio
      }

      data
    }
  )

