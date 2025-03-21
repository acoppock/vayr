
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vayr

<!-- badges: start -->
<!-- badges: end -->

The goal of `vayr` is to provide `ggplot2` extensions that foster
“visualize as you randomize” principles, which should guide the
visualization of experimental data. So far the package includes position
adjustments to avoid over-plotting, facilitating the use of statistical
models in “data-space.” In other words, these position adjustments help
you organize “data-space” so that it can better contextualize your
models. The principles underlying `vayr` are outlined in detail in
“Visualize as You Randomize: Design-Based Statistical Graphs for
Randomized Experiments.” That paper can be accessed as a
[PDF](https://alexandercoppock.com/coppock_2020.pdf).

## Installation

You can install the release version of `vayr` from CRAN, or you can
install the development version from [GitHub](https://github.com/) using
a package like `remotes`, `devtools`, or `pak`. It is important to note
that `vayr` relies on `ggplot2`, `packcircles`, and `withr`, so you must
have these installed as well.

``` r
# From CRAN
install.packages("vayr")

# From GitHub
# install.packages("pak")
pak::pak("acoppock/vayr")
```

## Contents

The current version of `vayr` contains a handful of `ggplot2` functions
that you can apply as position adjustments to “point-like” geoms such as
`geom_point` or `geom_text`:

- `position_jitter_ellipse()` and `position_jitterdodge_ellipse()`
- `position_sunflower()` and `position_sunflowerdodge()`
- `position_circlepack()` and `position_circlepackdodge()`

These functions avoid over-plotting, so they are especially useful when
you are plotting discrete rather than continuous data in a bivariate
scenario. As a matter of demonstration, they are used below to visualize
synthetic data, over-plotted at the origin.

``` r
library(dplyr)
library(estimatr)
library(ggplot2)
library(patchwork)
library(vayr)

dat <- data.frame(
  x = c(rep(0, 200)),
  y = c(rep(0, 200)),
  group = (rep(c("A", "B", "B", "B"), 50)),
  size = runif(200, 0, 1)
)
```

If position is the product of discrete variables alone, then
over-plotting is of particular concern. To mitigate this concern, you
can introduce variation with `position_jitter()`, which randomly samples
points on a rectangle. This approach is effective but can be
unattractive. The position adjustments in `vayr` are attempts to do
better.

``` r
# perfectly over-plotted points
overplot <- ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle('"perfect over-plotting"')

# position_jitter()
plot <- ggplot(dat, aes(x = x, y = y)) + 
  geom_point(position = position_jitter(width = 0.5, 
                                        height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_jitter()")

overplot + plot
```

<img src="man/figures/README-contents_0-1.png" alt="perfect over-plotting and position_jitter()" width="100%" />

### Position Jitter Ellipse

This position adjustment adds elliptical random noise to perfectly
over-plotted points, offering a pleasing way to visualize many points
that represent the same position. The benefit of sampling on an ellipse
(of a given `height` and `width`) rather than a rectangle is that the
resulting dispersion retains the impression of a single point. The size
of the ellipses stays constant, while their density varies depending on
the amount of over-plotting.

``` r
# position_jitter_ellipse()
plot <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(position = position_jitter_ellipse(width = 0.5, 
                                                height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_jitter_ellipse()")

# position_jitterdodge_ellipse()
dodged_plot <- ggplot(dat, aes(x = x, y = y, color = group)) +
  geom_point(position = position_jitterdodge_ellipse(dodge.width = 2, 
                                                     jitter.width = 0.5, 
                                                     jitter.height = 0.5)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_jitterdodge_ellipse()")
  
plot + dodged_plot
```

<img src="man/figures/README-contents_1-1.png" alt="position_jitter_ellipse() and position_jitterdodge_ellipse()" width="100%" />

### Position Sunflower

This position adjustment arranges perfectly over-plotted points using a
sunflower algorithm, which produces a pattern that resembles the seeds
of a sunflower, working from the inside out in the order of the data.
The parameters for this position adjustment (`density` and
`aspect_ratio`) are novel but intuitive. The size of the flowers varies
depending on the amount over over-plotting, but the density of the
pattern remains constant. You can adjust the aspect ratio of the
flowers, but it is generally recommended that the position adjustment be
used along with `coord_equal()`, in which case the default aspect ratio
of 1 yields perfectly circular flowers.

``` r
# position_sunflower()
plot <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(position = position_sunflower(density = 1, 
                                           aspect_ratio = 1)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_sunflower()")
  
# position_sunflowerdodge()
dodged_plot <- ggplot(dat, aes(x = x, y = y, color = group)) +
  geom_point(position = position_sunflowerdodge(width = 2, 
                                                density = 1, 
                                                aspect_ratio = 1)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_sunflowerdodge()")
  
plot + dodged_plot
```

<img src="man/figures/README-contents_2A-1.png" alt="position_sunflower() and position_sunflowerdodge()" width="100%" />

As stated, you can adjust the density of the pattern. Density is not
standardized but rather depends on the ranges of the axes and the
dimensions of the saved image. Loosely, you can think of it as the
number of points within a certain Cartesian area. Thus smaller ranges
and/or larger dimensions require a greater density to produce the same
visual effect.

``` r
# base plot
plot <- ggplot(dat, aes(x = x, y = y)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# low density
low <- plot + 
  geom_point(size = 0.25,
             position = position_sunflower(density = 0.5, 
                                           aspect_ratio = 1)) + 
  ggtitle("density = 0.5")

# medium density
medium <- plot + 
  geom_point(size = 0.25,
             position = position_sunflower(density = 1, 
                                           aspect_ratio = 1)) + 
  ggtitle("density = 1")

# high density
high <- plot + 
  geom_point(size = 0.25,
             position = position_sunflower(density = 2, 
                                           aspect_ratio = 1)) + 
  ggtitle("density = 2")

low + medium + high
```

<img src="man/figures/README-contents_2B-1.png" alt="density" width="100%" />

If need be, you can also tweak the aspect ratio of the flowers. This is
useful when you are using the position adjustment without
`coord_equal()`. The flowers can be made wider or taller to compensate
for the aspect ratio of the axes and/or the image. The aspect ratio of
the flower should be set to the opposite of the aspect ratio for which
it must compensate. For instance, consider a plot with an x axis that
ranges from 0 to 1, and a y axis that ranges from 0 to 2. Saving this
plot as a square image would effectively squish the y axis, resulting in
wider flowers. Setting the aspect ratio of the flowers to 0.5 would
offset this distortion, although further refinement would likely be
warranted due to labels, legends, and the like.

``` r
# base plot
plot <- ggplot(dat, aes(x = x, y = y)) +
  coord_equal(xlim = c(-1.1, 1.1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# wide aspect ratio 
wide <- plot + 
  geom_point(size = 0.25,
             position = position_sunflower(density = 1, 
                                           aspect_ratio = 2)) + 
  ggtitle("aspect_ratio = 2")

# normal aspect ratio
normal <- plot + 
  geom_point(size = 0.25,
             position = position_sunflower(density = 1, 
                                           aspect_ratio = 1)) + 
  ggtitle("aspect_ratio = 1")

# tall aspect ratio
tall <- plot + 
  geom_point(size = 0.25,
             position = position_sunflower(density = 1, 
                                           aspect_ratio = 0.5)) + 
  ggtitle("aspect_ratio = 0.5")

wide + normal + tall
```

<img src="man/figures/README-contents_2C-1.png" alt="aspect_ratio" width="100%" />

### Position Circle Pack

This position adjustment uses circle packing algorithms from the
`packcircles` package to arrange perfectly over-plotted points of
varying sizes into an elliptical area. It also takes `density` and
`aspect_ratio` as parameters, and they function as previously described.
This position adjustment should not be confused with `geom_circlepack()`
from the `ggcirclepack` package, which can be found on
[GitHub](https://github.com/EvaMaeRey/ggcirclepack).

``` r
# position_circlepack()
plot <- ggplot(dat, aes(x = x, y = y, size = size)) +
  geom_point(alpha = 0.25,
             position = position_circlepack(density = 0.25, 
                                            aspect_ratio = 1)) +
  coord_equal(xlim = c(-1, 1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_circlepack()")
  
# position_circlepackdodge()
dodged_plot <- ggplot(dat, aes(x = x, y = y, color = group, size = size)) +
  geom_point(alpha = 0.25,
             position = position_circlepackdodge(width = 2, 
                                                 density = 0.25, 
                                                 aspect_ratio = 1)) +
  coord_equal(xlim = c(-1, 1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("position_circlepackdodge()")
  
plot + dodged_plot
```

<img src="man/figures/README-contents_3A-1.png" alt="position_circlepack() and position_circlepackdodge()" width="100%" />

Like `position_sunflower()`, `position_circlepack()` works from the
inside out in the order of the data. So arranging the data by size
organizes the points accordingly.

``` r
# random size, base plot
random <- ggplot(dat, aes(x = x, y = y, size = size)) +
  geom_point(alpha = 0.25,
             position = position_circlepack(density = 0.075, 
                                            aspect_ratio = 1)) +
  coord_equal(xlim = c(-1, 1), 
              ylim = c(-1.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("random")

# ascending size
ascending <- random %+% 
  arrange(dat, size) + 
  ggtitle("ascending")

# descending size
descending <- random %+% 
  arrange(dat, desc(size)) + 
  ggtitle("descending")

random + ascending + descending
```

<img src="man/figures/README-contents_3B-1.png" alt="random, ascending, descending" width="100%" />

## Example

The current version of `vayr` also includes data from the Patriot Act
experiment described in *Persuasion in Parallel*. The Patriot Act was an
anti-terrorism law, and the `patriot_act` dataset contains data relating
to an experiment that measured support for this law after exposing
participants to statements that cast it in either a negative or positive
light. The experiment was conducted in 2009 with a nationwide sample,
and it was replicated in 2015 with a sample of MTurkers. In both
instances, the treatments had a parallel effect on Democrats and
Republicans. There are four variables in the data:

- `sample_label`, the study to which the participant belonged
- `pid_3`, the partisanship of the participant
- `T1_content`, the statements to which the participant was exposed
- `PA_support`, the participant’s post-treatment support for the Patriot
  Act

The data is visualized below using `position_sunflowerdodge()` from
`vayr`, demonstrating the package’s functionality with a real-world
example.

``` r
# A df for statistical models
summary_df <- patriot_act |> 
  group_by(T1_content, pid_3, sample_label) |> 
  reframe(tidy(lm_robust(PA_support~1)))

# A df for direct labels
label_df <- summary_df |> 
  filter(sample_label == "Original Study", T1_content == "Control") |> 
  mutate(PA_support = case_when(pid_3 == "Democrat"~conf.low - 0.15, 
                                pid_3 == "Republican"~conf.high + 0.15))

ggplot(patriot_act, aes(T1_content, PA_support, color = pid_3, group = pid_3)) +
  # the data
  geom_point(position = position_sunflowerdodge(width = 0.5, density = 10),
             size = 0.1, alpha = 0.5) +
  # the statistical model
  geom_line(data = summary_df, aes(x = T1_content, y = estimate),  
            position = position_dodge(width = 0.5), linewidth = 0.5) +  
  geom_point(data = summary_df, aes(x = T1_content, y = estimate),  
             position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(data = summary_df, aes(x = T1_content, y = estimate,
                                        ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.5)) +
  # the direct labels
  geom_text(data = label_df, aes(label = pid_3)) +
  # the rest
  scale_color_manual(values = c("blue4", "red3")) +
  scale_y_continuous(breaks = 1:7) +
  coord_equal() +
  facet_wrap(~sample_label) +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Do you oppose or support the Patriot Act?
            [1: Oppose very strongly to 7: Support very strongly]",
       x = "Randomly assigned information")
```

<img src="man/figures/README-patriot_act_visualization-1.png" alt="patriot_act" width="100%" />
