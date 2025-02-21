
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vayr

<!-- badges: start -->
<!-- badges: end -->

The goal of ‘vayr’ is to provide ‘ggplot2’ extensions that foster
“visualize as you randomize” principles, which guide the visualization
of experimental data. Thus far, the package includes position
adjustments to avoid over-plotting, facilitating the plotting of
statistical models in “data-space.” In other words, these position
adjustments help you organize “data-space,” such that it can
contextualize your models. The principles underlying ‘vayr’ are outlined
in detail in “Visualize as You Randomize: Design-Based Statistical
Graphs for Randomized Experiments.” That paper can be accessed
[here](https://alexandercoppock.com/coppock_2020.pdf).

## Installation

At the moment, ‘vayr’ only exists on [GitHub](https://github.com/),
although we ultimately hope to release the package to CRAN. You can
install the development version of the package using ‘pak,’ as shown
below. It is important to note that ‘vayr’ relies on ‘ggplot2,’
‘packcircles,’ and ‘withr,’ so these packages but also be installed.

``` r
# install.packages("pak")
pak::pak("acoppock/vayr")
```

## Contents

The current version of ‘vayr’ contains a handful new ‘ggplot2’
functions, all of which may be applied as position adjustments to the
point geom: **position_jitter_ellipse()**, **position_sunflower()**, and
**position_circlepack()**; as well as their dodged counterparts:
**position_jitterdodge_ellipse()**, **position_sunflowerdodge()**, and
**position_circlepackdodge()**. These functions are useful when plotting
discrete rather than continuous data. As a matter of brief
demonstration, they are used below to visualize synthetic data,
over-plotted at the origin.

``` r
library(dplyr)
library(ggplot2)
library(patchwork)
library(vayr)

dat <- data.frame(
  X = c(rep(0, 200)),
  Y = c(rep(0, 200)),
  Group = (rep(c("A", "B", "B", "B"), 50)),
  Size = runif(200, 0, 1),
  Enum = 1:200
)
```

### Position Jitter Ellipse:

This position adjustment adds elliptical random noise to perfectly
over-plotted points, offering a pleasing way to visualize many points
that represent the same position. The benefit of sampling on an ellipse
(of a given **height** and **width**) rather than a rectangle is that
the dispersion retains the impression of a single point. The size of
these meta-points remains constant across a graph, while the density
varies depending on the amount of over-plotting.

``` r
# position_jitter_ellipse()
plot <- ggplot(dat, aes(x = X, y = Y)) +
  geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank())

# position_jitterdodge_ellipse()
dodged_plot <- ggplot(dat, aes(x = X, y = Y, color = Group)) +
  geom_point(position = position_jitterdodge_ellipse(dodge.width = 2, 
                                                     jitter.width = 0.5, 
                                                     jitter.height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())
  
plot + dodged_plot
```

<img src="man/figures/README-contents_1-1.png" width="100%" />

### Position Sunflower:

This position adjustment arranges perfectly over-plotted points using a
sunflower algorithm, which produces a pattern that resembles the seeds
of a sunflower. The algorithm works from the inside out in the order of
the data. The parameters for this position adjustment (**density** and
**aspect_ratio**) are novel but intuitive. The size of the meta-points
(the flowers, that is) varies depending on the amount over
over-plotting, but the density of the pattern remains constant. It is
possible to adjust the aspect ratio of the meta-points, but it is
generally recommended that the position adjustment be used in tandem
with **coord_equal()**, in which case an aspect ratio of 1 (the default)
should yield perfect circles.

``` r
# position_sunflower()
plot <- ggplot(dat, aes(x = X, y = Y)) +
  geom_point(position = position_sunflower(density = 1, aspect_ratio = 1)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank())
  
# position_sunflowerdodge()
dodged_plot <- ggplot(dat, aes(x = X, y = Y, color = Group)) +
  geom_point(position = position_sunflowerdodge(width = 2, density = 1, aspect_ratio = 1)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_blank())
  
plot + dodged_plot
```

<img src="man/figures/README-contents_2A-1.png" width="100%" />

As stated, the density of the seeds can be adjusted. Density is not
standardized but rather depends on the ranges of the axes and dimensions
of the image. Loosely, it can be thought of as the number of points
within a certain Cartesian area. Thus smaller ranges and/or larger
dimensions will require a greater density to produce the same visual
effect.

``` r
# base plot
plot <- ggplot(dat, aes(x = X, y = Y)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank())

# low density
low <- plot + geom_point(position = position_sunflower(density = 0.5, aspect_ratio = 1),
                         size = 0.25)

# medium density
medium <- plot + geom_point(position = position_sunflower(density = 1, aspect_ratio = 1),
                            size = 0.25)

# high density
high <- plot + geom_point(position = position_sunflower(density = 2, aspect_ratio = 1),
                          size = 0.25)

low + medium + high
```

<img src="man/figures/README-contents_2B-1.png" width="100%" />

The aspect ratio of the flowers can also be tweaked. This is useful when
the position adjustment is used without **coord_equal()**. The flowers
can be made wider or taller to compensate for the aspect ratio of the
axes and/or the image, in which case the aspect ratio of the flower
should be set to the opposite of the aspect ratio for which it must
compensate. For instance, saving a plot with a vertical range of 2 and a
horizontal range of 1 as a square image would result in wide flowers.
Setting the aspect ratio of the flowers to 1/2 would offset the
distortion, although refinement would likely be warranted due to labels,
legends, and the like.

``` r
# base plot
plot <- ggplot(dat, aes(x = X, y = Y)) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank())

# wide aspect ratio 
wide <- plot + geom_point(position = position_sunflower(density = 1, aspect_ratio = 0.5),
                         size = 0.25)

# normal aspect ratio
normal <- plot + geom_point(position = position_sunflower(density = 1, aspect_ratio = 1),
                            size = 0.25)

# tall aspect ratio
tall <- plot + geom_point(position = position_sunflower(density = 1, aspect_ratio = 2),
                          size = 0.25)

wide + normal + tall
```

<img src="man/figures/README-contents_2C-1.png" width="100%" />

### Position Circle Pack:

This position adjustment uses circle packing algorithms from the
‘packcircles’ package to arrange perfectly over-plotted points of
varying sizes into an elliptical area. It also takes **density** and
**aspect_ratio** as parameters, and they function as previously
described. Again, the desirable density will depend on both the ranges
of the axes and the dimensions of the image.

``` r
# position_circlepack()
plot <- ggplot(dat, aes(x = X, y = Y, size = Size)) +
  geom_point(position = position_circlepack(density = 0.25, aspect_ratio = 1),
             alpha = 0.25) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())
  
# position_circlepackdodge()
dodged_plot <- ggplot(dat, aes(x = X, y = Y, color = Group, size = Size)) +
  geom_point(position = position_circlepackdodge(width = 2, density = 0.25, aspect_ratio = 1),
             alpha = 0.25) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1.1, 1.1)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_blank())
  
plot + dodged_plot
```

<img src="man/figures/README-contents_3A-1.png" width="100%" />

Like **position_sunflower()**, **position_circlepack()** creates
meta-points from the inside out in the order of the data. Thus, the
arranging the data by size will organize the meta-points accordingly.

``` r
# random size, base plot
random <- dat |> ggplot(aes(x = X, y = Y, size = Size)) +
  geom_point(position = position_circlepack(density = 0.075, aspect_ratio = 1),
             alpha = 0.25) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())

# ascending size
ascending <- random %+% (dat |> arrange(Size))

# descending size
descending <- random %+% (dat |> arrange(desc(Size)))

random + ascending + descending
```

<img src="man/figures/README-contents_3B-1.png" width="100%" />

## Example:

The current version of ‘vayr’ also includes data from the Patriot Act
experiment described in *Persuasion in Parallel*. The dataset,
**patriot_act**, \[DESCRIBE ME\]. The data is visualized below using
functions from ‘vayr’ so as to demonstrate the package’s functionality
with a real-world example. \[VISUALIZE ME\].

``` r
## [EXAMPLE CODE]
```
