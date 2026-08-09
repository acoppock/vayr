# Arrange over-plotted points on a honeycomb lattice

This function arranges perfectly over-plotted points on a hexagonal
lattice, filling outward from the centre in the order of the data. The
hexagonal lattice is the densest packing of equal circles in the plane,
so the cluster is as compact as it can be for a given spacing.

## Usage

``` r
position_honeycomb(density = 1, aspect_ratio = 1)
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

A `ggproto` object of class `PositionHoneycomb`.

## Details

It is the crystalline sibling of
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
which fills the same footprint at the same `density` with a spiral
rather than a lattice. Choose between them on looks: the lattice reads
as countable and orderly, the spiral as organic and without a preferred
direction.

`position_beeswarm()` in the 'ggbeeswarm' package also offers a
hexagonal method, and does a different job. A beeswarm spreads points
along one axis to show the shape of a distribution, so perfectly
over-plotted points come out as a line rather than a cluster, and its
hexagonal and square methods move points off their true value on the
data axis. Reach for a beeswarm to show a distribution, and for this to
show a count.

## See also

Other Functions:
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
[`position_bluenoise()`](https://alexandercoppock.com/vayr/reference/position_bluenoise.md),
[`position_bluenoisedodge()`](https://alexandercoppock.com/vayr/reference/position_bluenoisedodge.md),
[`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md),
[`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md),
[`position_honeycombdodge()`](https://alexandercoppock.com/vayr/reference/position_honeycombdodge.md),
[`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
[`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
[`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
[`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
[`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)

## Examples

``` r
  library(ggplot2)

  dat <- data.frame(
    x = rep(1:3, times = 60),
    y = rep(1:3, times = 60)
  )

  ggplot(dat, aes(x, y)) +
    geom_point(size = 1, position = position_honeycomb(density = 4)) +
    coord_equal()

```
