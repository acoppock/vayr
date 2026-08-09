# Scatter over-plotted points evenly at random

This function spreads perfectly over-plotted points across an elliptical
field, like
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
but places them so that no two land much closer together than the rest.
Sampling uniformly at random, which is what jittering does, leaves
visible knots and voids: in a draw of 200 points the closest pair
typically sits about a fifteenth of the median spacing apart. A reader
cannot tell those knots from real structure. The arrangement here has
the even spacing of
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md)
while still looking unstructured, so no reader mistakes a spiral arm for
a finding.

## Usage

``` r
position_bluenoise(width = NULL, height = NULL, candidates = 10, seed = NA)
```

## Arguments

- width, height:

  The dimensions of the elliptical field the points are spread across.

- candidates:

  The number of random draws considered for each point. Larger values
  space the points more evenly and take longer. The default of 10 is
  enough to remove essentially all of the clumping.

- seed:

  A random seed for reproducibility.

## Value

A `ggproto` object of class `PositionBlueNoise`.

## Details

The pattern is the one the eye's own photoreceptors are laid out in,
known as blue noise or a Poisson-disc distribution. It is produced by
Mitchell's best-candidate algorithm: each point is the best of
`candidates` random draws, where best means farthest from every point
already placed.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_bluenoisedodge()`](https://alexandercoppock.com/vayr/reference/position_bluenoisedodge.md),
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_honeycomb()`](https://alexandercoppock.com/vayr/reference/position_honeycomb.md),
[`position_honeycombdodge()`](https://alexandercoppock.com/vayr/reference/position_honeycombdodge.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  dat <- data.frame(x = rep(1, 400), y = rep(1, 400))

  # Evenly scattered.
  ggplot(dat, aes(x, y)) +
    geom_point(position = position_bluenoise(width = 0.5, height = 0.5)) +
    coord_equal(xlim = c(0, 2), ylim = c(0, 2))


  # Uniformly jittered, for comparison. Note the knots and the gaps.
  ggplot(dat, aes(x, y)) +
    geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
    coord_equal(xlim = c(0, 2), ylim = c(0, 2))

```
