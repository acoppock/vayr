# How evenly a set of points is spaced. The coefficient of variation of the
# nearest-neighbour distances is near 0.5 for a uniform random draw, which clumps,
# and near 0 for a lattice.
nearest_neighbour_distances <- function(points) {
  distances <- as.matrix(dist(points))
  diag(distances) <- Inf
  apply(distances, 1, min)
}

nearest_neighbour_cv <- function(points) {
  neighbours <- nearest_neighbour_distances(points)
  stats::sd(neighbours) / mean(neighbours)
}
