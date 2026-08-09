# Distribute points using a sunflower seed algorithm

This function distributes points in an ellipse via a sunflower seed
algorithm as a solution for over-plotting. To implement the algorithm,
this function adapts the code from
https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle.

## Usage

``` r
sunflower(x = NULL, y = NULL, density, aspect_ratio)
```

## Arguments

- x, y:

  The identical coordinates of multiple over-plotted points, as vectors,
  which will be arranged using a sunflower seed algorithm. A vector of
  length one is returned unchanged, since a point with nothing
  over-plotting it belongs at its own coordinates.

- density:

  The pattern density.

- aspect_ratio:

  An aspect ratio adjustment to compensate for distortion of the
  circular arrangement, which might occur when plotting if coord_equal()
  is not used. A wide aspect ratio (e.g., 2) would adjust for vertical
  stretching, whereas a tall aspect ratio (e.g., 0.5) would adjust for
  horizontal stretching. An aspect ratio of 1 is appropriate when no
  adjustment is required.

## Value

A numeric vector of adjusted `x` or `y` positions, computed using a
sunflower seed algorithm.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md)

## Examples

``` r
  library(ggplot2)
  library(dplyr)

  # Manually adjust position of N points,
  # arranging them per the sunflower algorithm and then dodging groups
  N <- 300

  dat <- data.frame(
    x = sample(1:2, size = N, replace = TRUE),
    y = sample(1:7, size = N, replace = TRUE),
    type = factor(sample(LETTERS[1:2], N, replace = TRUE))
  ) |>
    group_by(x, y, type) |>
    mutate(
      x = sunflower(x = x, density = 1, aspect_ratio = 1),
      y = sunflower(y = y, density = 1, aspect_ratio = 1),
      x = if_else(type == "A", x - (1 / 8), x + (1 / 8))
    )

  ggplot(dat, aes(x, y, color = type, shape = type)) +
    geom_point() + coord_equal()

```
