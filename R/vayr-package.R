#' Extensions for 'ggplot2' to Visualize as You Randomize
#'
#' @family Package
#'
#' @name vayr
#'
#' @description
#' Position adjustments for 'ggplot2' to implement "visualize as you randomize" principles,
#' which can be especially useful when plotting experimental data.
#'
#' @details
#' The 'vayr' package provides 'ggplot2' extensions that foster "visualize as you randomize" principles.
#' These principles should guide the visualization of experimental data.
#' Thus far, the package includes position adjustments to avoid over-plotting, facilitating plotting in "data-space."
#' The 'vayr' paper is here: https://alexandercoppock.com/coppock_2020.pdf.
#'
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 ggproto ggproto_parent Position PositionDodge resolution
#' @importFrom ggplot2 transform_position
#' @importFrom packcircles circleProgressiveLayout
#' @importFrom stats runif
## usethis namespace: end
NULL
