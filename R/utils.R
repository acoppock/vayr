with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# The x and y shifts share one radius/angle draw. Drawing them twice would place
# the point at (rho1 cos theta1, rho2 sin theta2), which is not on the ellipse.
jitter_ellipse <- function(data, width, height) {
  rho <- sqrt(stats::runif(nrow(data)))
  theta <- stats::runif(nrow(data), 0, 2 * pi)

  ggplot2::transform_position(
    data,
    function(x) x + rho * cos(theta) * width,
    function(y) y + rho * sin(theta) * height
  )
}

# Circle areas are rescaled to [0.1, 1] so that the smallest point still packs a
# visible circle. A layer with no size aesthetic, or one constant size, packs
# equal circles.
normalize_size <- function(data) {
  if (!"size" %in% names(data) || max(data$size) == min(data$size)) {
    rep(1, nrow(data))
  } else {
    0.1 + (data$size - min(data$size)) * 0.9 / (max(data$size) - min(data$size))
  }
}
