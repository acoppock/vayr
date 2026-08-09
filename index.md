# vayr

The goal of `vayr` is to provide `ggplot2` extensions that foster
“visualize as you randomize” principles. These principles are outlined
in detail in “Visualize As You Randomize: Design-based Statistical
Graphs for Randomized Experiments,” a chapter in *Advances in
Experimental Political Science*
([PDF](https://alexandercoppock.com/coppock_2021.pdf),
[DOI](https://doi.org/10.1017/9781108777919.022)). The package includes
position adjustments that avoid over-plotting, which helps organize
“data-space” to better contextualize statistical models.

## Installation

The release version of `vayr` can be installed from CRAN, and the
development version can be installed from [GitHub](https://github.com/)
using a package like `remotes`, `devtools`, or `pak`. `vayr` relies on
`ggplot2`, `packcircles`, and `withr`, so these must be installed as
well.

``` r

# From CRAN
install.packages("vayr")

# From GitHub
# install.packages("pak")
pak::pak("acoppock/vayr")
```

## Position adjustments

`vayr` provides six position adjustments that apply to “point-like”
geoms such as
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
and
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
They come in pairs, one that arranges over-plotted points and one that
also dodges groups side-to-side:

- [`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md)
  and
  [`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md)
  sample from an elliptical field rather than the rectangle that
  [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html)
  uses, so the dispersion retains the impression of a single point.
- [`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md)
  and
  [`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md)
  arrange over-plotted points in a sunflower pattern, working from the
  inside out in the order of the data. A point with nothing
  over-plotting it stays where it is.
- [`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md)
  and
  [`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md)
  pack over-plotted points of varying sizes into an elliptical area,
  which is useful when point size carries a weight.

Each takes a `density` argument controlling how tightly the points pack,
and an `aspect_ratio` argument that compensates for a non-square
plotting region.

``` r

library(ggplot2)
library(patchwork)
library(vayr)

set.seed(1)

dat <- data.frame(
  x = rep(0, 200),
  y = rep(0, 200),
  group = rep(c("A", "B", "B", "B"), 50),
  size = runif(200, 0, 1)
)

vayr_theme <- list(
  theme_bw(),
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
)

jitter_ellipse <- ggplot(dat, aes(x, y)) +
  geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  vayr_theme + ggtitle("position_jitter_ellipse()")

jitterdodge_ellipse <- ggplot(dat, aes(x, y, color = group)) +
  geom_point(position = position_jitterdodge_ellipse(dodge.width = 2,
                                                     jitter.width = 0.5,
                                                     jitter.height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  vayr_theme + ggtitle("position_jitterdodge_ellipse()")

sunflower <- ggplot(dat, aes(x, y)) +
  geom_point(position = position_sunflower(density = 1, aspect_ratio = 1)) +
  coord_equal(xlim = c(-2.1, 2.1), ylim = c(-2.1, 2.1)) +
  vayr_theme + ggtitle("position_sunflower()")

sunflowerdodge <- ggplot(dat, aes(x, y, color = group)) +
  geom_point(position = position_sunflowerdodge(width = 4, density = 1,
                                                aspect_ratio = 1)) +
  coord_equal(xlim = c(-2.1, 2.1), ylim = c(-2.1, 2.1)) +
  vayr_theme + ggtitle("position_sunflowerdodge()")

circlepack <- ggplot(dat, aes(x, y, size = size)) +
  geom_point(alpha = 0.25,
             position = position_circlepack(density = 0.25, aspect_ratio = 1)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  vayr_theme + ggtitle("position_circlepack()")

circlepackdodge <- ggplot(dat, aes(x, y, color = group, size = size)) +
  geom_point(alpha = 0.25,
             position = position_circlepackdodge(width = 2, density = 0.25,
                                                 aspect_ratio = 1)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  vayr_theme + ggtitle("position_circlepackdodge()")

(jitter_ellipse + sunflower + circlepack) /
  (jitterdodge_ellipse + sunflowerdodge + circlepackdodge)
```

![the six position adjustments applied to 200 points over-plotted at the
origin](reference/figures/README-overview-1.png)

## Learn more

There are two vignettes. The first walks through every position
adjustment, explains how `density` and `aspect_ratio` interact with the
plotting region, and closes with a worked example that plots an
experiment’s data and its statistical model together. The second
reproduces the seven worked examples from the chapter, one per
experimental design, using the chapter’s own simulated data, which ships
with the package.

``` r

vignette("vayr-vignette", package = "vayr")
vignette("design-based-graphs", package = "vayr")
```

Both are on the package site, along with the reference documentation:
<https://alexandercoppock.com/vayr/>.

`vayr` also provides
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
which prepares the extreme value bounds figure for an experiment that
encountered attrition.

## Citation

``` r

citation("vayr")
```

Coppock, Alexander. 2021. “Visualize As You Randomize: Design-based
Statistical Graphs for Randomized Experiments.” In *Advances in
Experimental Political Science*, edited by James N. Druckman and Donald
P. Green, 320–336. New York: Cambridge University Press.
<https://doi.org/10.1017/9781108777919.022>
