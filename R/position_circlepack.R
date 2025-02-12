#' Use circle packing to avoid over-plotting
#'  
#' This function uses circle packing algorithms from the "packcircles" package
#' to arrange perfectly over-plotted points of varying sizes into a elliptical area.
#' The density and the aspect ratio can be adjusted, and the circle pack can be 
#' random or ordered by size (ascending or descending). Because the function is a
#' position adjustment, the correct density will be a factor of both the image size
#' and the limits of the x and y axes.
#'
#' @param density density of the circle pack
#' @param aspect_ratio aspect_ratio adjustment of the elliptical area
#' @param arrange order by which the circles are packed
#' 
#' @importFrom ggplot2 ggproto Position
#' @importFrom packcircles circleProgressiveLayout
#' 
#' @export
#'
#' @examples
#' # ADD EXAMPLE HERE
#'
position_circlepack <-function(density = 1, aspect_ratio = 1, arrange = "random") {
    ggplot2::ggproto(NULL, PositionCirclePack, density = density, aspect_ratio = aspect_ratio, arrange = arrange)
  }

PositionCirclePack <-
  ggplot2::ggproto(
    "PositionCirclePack",
    ggplot2::Position,
    compute_panel = function(self, data, params, scales) {
      if (self$arrange == "ascending") {
        data <- data |> arrange(size)
      } else if (self$arrange == "descending") {
        data <- data |> arrange(desc(size))
      }
      
      data$size_normalized <- if(max(data$size) == min(data$size)) {
        rep(1, length(data$size))
      } else {
        .1 + (data$size - min(data$size)) * (1 - .1) / (max(data$size) - min(data$size))
      }
      
      pairs <- split(data, interaction(data$x, data$y, drop = TRUE))
      
      data <- do.call(rbind, lapply(pairs, function(pair) {
        density <- (10 ^ - 3) * (self$density)
        circle_layout <- packcircles::circleProgressiveLayout(pair$size_normalized * density)
        
        pair$x <- circle_layout$x + pair$x 
        pair$y <- (circle_layout$y / self$aspect_ratio) + pair$y # either make denser/less dense or both denser or both less dense
        
        return(pair)
      }))
      
      return(data)
    }
  )

#' Arrange over-plotted points with a circle-packing algorithm and dodge groups side-to-side
#'  
#' This function dodges groups and arranges the over-plotted points (of various sizes)
#' using algorithms from the "packcirles" package. The algorithms are applied per group.
#' 
#' @param width dodging width
#' @param density density of the circle pack
#' @param aspect_ratio aspect_ratio adjustment of the elliptical area
#' @param arrange order by which the circles are packed
#'
#' @importFrom ggplot2 ggproto ggproto_parent PositionDodge
#' @importFrom packcircles circleProgressiveLayout
#' 
#' @export
#'
#' @examples
#' 
#' df2 <- data.frame(
#'   X = c(rep(0, 200)),
#'   Y = rep(0, 200),
#'   size = runif(200, 0, 1),
#'   id = (rep(c("A", "B"), 100))
#' )
#' 
#' ggplot(df2, aes(x = X, y = Y, size = size, color = id)) +
#'   geom_point(position = position_circlepackdodge(width = 1, density = 1, aspect_ratio = 1),
#'              alpha = 0.25) +
#'   coord_equal(xlim = c(-2, 2), ylim = c(-2, 2), expand = TRUE) +
#'  scale_size_continuous(range = c(1, 3)) + 
#'  theme(legend.position = "none")
#'  
position_circlepackdodge <- function(width = NULL, density = 1, aspect_ratio = 1, arrange = "random") { # Width being null makes width required arg, could change though
    ggplot2::ggproto(NULL, PositionCirclePackDodge, width = width, density = density, aspect_ratio = aspect_ratio, arrange = arrange)
  }

PositionCirclePackDodge <-
  ggplot2::ggproto(
    "PositionCirclePackDodge",
    ggplot2::PositionDodge,
    setup_params = function(self, data) {
      params <- ggplot2::ggproto_parent(PositionDodge, self)$setup_params(data)
      return(params)
    },
    compute_panel = function(self, data, params, scales) {
      data <- ggplot2::ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
      
      if (self$arrange == "ascending") {
        data <- data |> arrange(size)
      } else if (self$arrange == "descending") {
        data <- data |> arrange(desc(size))
      }
      
      data$size_normalized <- if(max(data$size) == min(data$size)) {
        rep(1, length(data$size))
      } else {
        .1 + (data$size - min(data$size)) * (1 - .1) / (max(data$size) - min(data$size))
      }
      
      pairs <- split(data, interaction(data$x, data$y, data$group, drop = TRUE))
      
      data <- do.call(rbind, lapply(pairs, function(pair) {
        density <- (10 ^ - 3) * (self$density)
        circle_layout <- packcircles::circleProgressiveLayout(pair$size_normalized * density)
        
        pair$x <- circle_layout$x + pair$x 
        pair$y <- (circle_layout$y / self$aspect_ratio) + pair$y
        
        return(pair)
      }))
      
      return(data)
    }
  )

