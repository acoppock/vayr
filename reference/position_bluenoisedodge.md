# Scatter over-plotted points evenly at random and dodge groups side-to-side

This function dodges groups of points side-to-side and then scatters the
points that share a position across an elliptical field, spacing them
evenly. See the
[`position_bluenoise()`](https://alexandercoppock.com/vayr/reference/position_bluenoise.md)
documentation for more information.

## Usage

``` r
position_bluenoisedodge(
  scatter.width = NULL,
  scatter.height = NULL,
  dodge.width = 1,
  candidates = 10,
  seed = NA,
  orientation = "x"
)
```

## Arguments

- scatter.width, scatter.height:

  The dimensions of the elliptical field the points are spread across.

- dodge.width:

  The dodging width, which defaults to 1.

- candidates:

  The number of random draws considered for each point.

- seed:

  A random seed for reproducibility.

- orientation:

  The axis along which groups are separated, either `"x"` (the default,
  side-to-side) or `"y"` (up and down). Matches the argument of the same
  name in
  [`ggplot2::position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

## Value

A `ggproto` object of class `PositionBlueNoiseDodge`.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_bluenoise()`](https://alexandercoppock.com/vayr/reference/position_bluenoise.md),
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

  dat <- data.frame(x = rep(1, 400), y = rep(1, 400),
                    group = sample(LETTERS[1:2], 400, replace = TRUE))

  ggplot(dat, aes(x, y, shape = group, color = group)) +
    geom_point(position = position_bluenoisedodge(scatter.width = 0.4,
                                                  scatter.height = 0.4,
                                                  dodge.width = 1)) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

```
