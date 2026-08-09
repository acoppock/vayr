# Arrange over-plotted points on a honeycomb lattice and dodge groups side-to-side

This function applies the honeycomb position adjustment alongside the
dodge position adjustment, arranging overlapping points per x, y, and
group on a hexagonal lattice. See the
[`position_honeycomb()`](https://alexandercoppock.com/vayr/reference/position_honeycomb.md)
documentation for more information.

## Usage

``` r
position_honeycombdodge(
  width = 1,
  density = 1,
  aspect_ratio = 1,
  orientation = "x"
)
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

- orientation:

  The axis along which groups are separated, either `"x"` (the default,
  side-to-side) or `"y"` (up and down). Matches the argument of the same
  name in
  [`ggplot2::position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

## Value

A `ggproto` object of class `PositionHoneycombDodge`.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_bluenoise()`](https://alexandercoppock.com/vayr/reference/position_bluenoise.md),
[`position_bluenoisedodge()`](https://alexandercoppock.com/vayr/reference/position_bluenoisedodge.md),
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_honeycomb()`](https://alexandercoppock.com/vayr/reference/position_honeycomb.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  dat <- data.frame(
    x = rep(1, 300),
    y = rep(1, 300),
    type = factor(sample(LETTERS[1:2], 300, replace = TRUE))
  )

  ggplot(dat, aes(x, y, color = type, shape = type)) +
    geom_point(position = position_honeycombdodge(width = 1, density = 30)) +
    coord_equal()

```
