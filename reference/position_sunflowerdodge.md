# Arrange over-plotted points in a sunflower pattern and dodge groups side-to-side

This function applies the sunflower position adjustment alongside the
dodge position adjustment, arranging overlapping points per x, y, and
group into a sunflower pattern. See the sunflower() documentation for
more information.

## Usage

``` r
position_sunflowerdodge(width = 1, density = 1, aspect_ratio = 1)
```

## Arguments

- width:

  The dodging width, which defaults to 1.

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

A `ggproto` object of class `PositionSunflowerDodge`.

## See also

Other Functions:
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
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

```
