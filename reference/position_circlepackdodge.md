# Arrange over-plotted points with a circle-packing algorithm and dodge groups side-to-side

This function dodges groups and uses a circle packing algorithm from the
'packcircles' package to arrange perfectly over-plotted points of
varying sizes into an elliptical area.

## Usage

``` r
position_circlepackdodge(width = 1, density = 1, aspect_ratio = 1)
```

## Arguments

- width:

  The dodging width, which defaults to 1.

- density:

  The density of the circle pack, which defaults to 1 but will have to
  be adjusted in most cases. The desirable density will depend on both
  the ranges of the axes and the dimensions of the image. It will also
  depend on the size scale.

- aspect_ratio:

  An aspect ratio adjustment to compensate for distortion of the
  circular arrangement, which might occur when plotting if coord_equal()
  is not used. A wide aspect ratio (e.g., 2) would adjust for vertical
  stretching, whereas a tall aspect ratio (e.g., 0.5) would adjust for
  horizontal stretching. The default aspect ratio of 1 is appropriate
  when no adjustment is required. Under coord_fixed(), set
  `aspect_ratio` to the same value as that function's `ratio` argument.

## Value

A `ggproto` object of class `PositionCirclePackDodge`.

## See also

Other Functions:
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  dat <- data.frame(
    X = c(rep(0, 200)),
    Y = rep(0, 200),
    size = runif(200, 0, 1),
    id = (rep(c("A", "B"), 100))
  )

  ggplot(dat, aes(x = X, y = Y, size = size, color = id)) +
    geom_point(position = position_circlepackdodge(width = 1, density = 1, aspect_ratio = 1),
              alpha = 0.25) +
    coord_equal(xlim = c(-1, 1), ylim = c(-1, 1), expand = TRUE) +
    scale_size_continuous(range = c(1, 3)) +
    theme(legend.position = "none")

```
