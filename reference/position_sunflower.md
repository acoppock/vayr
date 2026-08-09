# Arrange over-plotted points in a sunflower pattern

This function applies the sunflower algorithm, executed by the
sunflower() function, as a position adjustment, arranging overlapping
points at any given x and y into a sunflower pattern. See the
sunflower() documentation for more information.

## Usage

``` r
position_sunflower(density = 1, aspect_ratio = 1)
```

## Arguments

- density:

  The pattern density, which defaults to 1 but will have to be adjusted
  in most cases. The desirable density will depend on both the ranges of
  the axes and the dimensions of the image.

- aspect_ratio:

  An aspect ratio adjustment to compensate for distortion of the
  circular arrangement, which might occur when plotting if coord_equal()
  is not used. A wide aspect ratio (e.g., 2) would adjust for vertical
  stretching, whereas a tall aspect ratio (e.g., 0.5) would adjust for
  horizontal stretching. The default aspect ratio of 1 is appropriate
  when no adjustment is required. Under coord_fixed(), set
  `aspect_ratio` to the same value as that function's `ratio` argument.

## Value

A `ggproto` object of class `PositionSunflower`.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  # Use the sunflower position function to arrange N points
  N <- 100

  dat <- data.frame(
    x = rep(1:4, times = N),
    y = rep(1:4, times = N)
  )

  ggplot(dat, aes(x = x, y = y)) +
    geom_point(size = 1, position = position_sunflower(density = 1, aspect_ratio = 1)) +
    xlim(0, 5) +
    ylim(0, 5) +
    coord_equal()

```
