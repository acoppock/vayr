# Jitter points on an ellipse and dodge groups side-to-side

This function dodges groups of points side-to-side and adds elliptical
random noise to perfectly over-plotted points. See the
position_jitter_ellipse() documentation for more information.

## Usage

``` r
position_jitterdodge_ellipse(
  jitter.width = NULL,
  jitter.height = NULL,
  dodge.width = 1,
  seed = NA
)
```

## Arguments

- jitter.width, jitter.height:

  The dimensions of the elliptical field, from which over-plotted points
  are sampled.

- dodge.width:

  The dodging width, which defaults to 1.

- seed:

  A random seed for reproducibility.

## Value

A `ggproto` object of class `PositionJitterDodgeEllipse`.

## See also

Other Functions:
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  dat <- data.frame(x = rep(1, 500), y = rep(1, 500),
                    group = sample(LETTERS[1:2], 500, replace = TRUE))

  ggplot(dat, aes(x, y, shape = group, color = group)) +
    geom_point(position = position_jitterdodge_ellipse(jitter.width  = 0.5,
                                                       jitter.height =  0.5,
                                                       dodge.width = 1)) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

```
