# Jitter points on an ellipse to avoid over-plotting

This function adds elliptical random noise to perfectly over-plotted
points, offering a pleasing way to visualize many points that represent
the same position. In contrast to the position_jitter() function, which
samples from a rectangular field, the position_jitter_ellipse() function
samples from an elliptical field. This function takes algorithmic
inspiration from
https://stackoverflow.com/questions/5529148/algorithm-calculate-pseudo-random-point-inside-an-ellipse
and
https://stats.stackexchange.com/questions/120527/simulate-a-uniform-distribution-on-a-disc.

## Usage

``` r
position_jitter_ellipse(width = NULL, height = NULL, seed = NA)
```

## Arguments

- width, height:

  The dimensions of the elliptical field, from which over-plotted points
  are sampled.

- seed:

  A random seed for reproducibility.

## Value

A `ggproto` object of class `PositionJitterEllipse`.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_bluenoise()`](https://alexandercoppock.com/vayr/reference/position_bluenoise.md),
[`position_bluenoisedodge()`](https://alexandercoppock.com/vayr/reference/position_bluenoisedodge.md),
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_honeycomb()`](https://alexandercoppock.com/vayr/reference/position_honeycomb.md),
[`position_honeycombdodge()`](https://alexandercoppock.com/vayr/reference/position_honeycombdodge.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  dat <- data.frame(x = rep(1, 500), y = rep(1, 500))

  # Jitter on an ellipse.
  ggplot(dat, aes(x, y)) +
    geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))


  # Jitter on a rectangle, for comparison.
  ggplot(dat, aes(x, y)) +
    geom_point(position = position_jitter(width = 0.5, height = 0.5)) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

```
