% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/position-sunflower.R
\name{position_sunflowerdodge}
\alias{position_sunflowerdodge}
\title{Arrange over-plotted points in a sunflower pattern and dodge groups side-to-side}
\usage{
position_sunflowerdodge(width = 1, density = 1, aspect_ratio = 1)
}
\arguments{
\item{width}{The dodging width, which defaults to 1.}

\item{density}{The pattern density, which defaults to 1 but will have to be adjusted in most cases.
The desirable density will depend on both the ranges of the axes and the dimensions of the image.}

\item{aspect_ratio}{An aspect ratio adjustment to compensate for distortion of the circular arrangement,
which might occur when plotting if coord_equal() is not used. A wide aspect ratio (eg. 2)
would adjust for vertical stretching, whereas a tall aspect ratio (eg. 0.5) would adjust for
horizontal stretching. The default aspect ratio of 1 is appropriate when no adjustment is required.}
}
\value{
A \code{ggproto} object of class \code{PositionSunflowerDodge}.
}
\description{
This function applies the sunflower position adjustment alongside the dodge position adjustment,
arranging overlapping points per x, y, and group into a sunflower pattern. See the sunflower() documentation for
more information.
}
\examples{
  library(ggplot2)

  # Use the sunflower dodge position function to arrange and dodge N points.
  N <- 300

  dat <- data.frame(
    x = sample(1:2, size = N, replace = TRUE),
    y = sample(1:7, size = N, replace = TRUE),
    type = factor(sample(LETTERS[1:2], N, replace = TRUE))
  )

  # With coord_equal
  ggplot(dat, aes(x, y, color = type, shape = type)) +
    geom_point(position = position_sunflowerdodge(width = 0.5, density = 2, aspect_ratio = 1)) +
    coord_equal()

  # Without coord_equal, might want to play with aspect ratio to get a pleasing plot
  ggplot(dat, aes(x, y, color = type, shape = type)) +
    geom_point(position = position_sunflowerdodge(width = 0.5, density = 10, aspect_ratio = 1/4))

}
\seealso{
Other Functions: 
\code{\link{position_circlepack}()},
\code{\link{position_circlepackdodge}()},
\code{\link{position_jitter_ellipse}()},
\code{\link{position_jitterdodge_ellipse}()},
\code{\link{position_sunflower}()},
\code{\link{sunflower}()}
}
\concept{Functions}
