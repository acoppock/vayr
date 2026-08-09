# Changelog

## vayr 1.0.1

Bug fixes. All six position adjustments now have behavioural tests
covering the geometry they promise, which is how these were found.

- The position adjustments no longer require `ggplot2` to be attached.
  They used
  [`resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html),
  [`transform_position()`](https://ggplot2.tidyverse.org/reference/transform_position.html),
  and `PositionDodge` unqualified, so
  [`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md),
  [`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md),
  [`position_sunflowerdodge()`](https://alexandercoppock.com/vayr/reference/position_sunflowerdodge.md),
  and
  [`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md)
  failed for anyone calling
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  without [`library(ggplot2)`](https://ggplot2.tidyverse.org), and for
  any package importing ‘vayr’.

- [`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md)
  gave every group the same jitter, so one group’s cloud was an exact
  translation of the next. Each group is now jittered independently.

- [`position_jitter_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitter_ellipse.md)
  and
  [`position_jitterdodge_ellipse()`](https://alexandercoppock.com/vayr/reference/position_jitterdodge_ellipse.md)
  drew the radius and angle separately for the x and y shifts, which
  placed points outside the requested ellipse whenever `seed = NULL`.
  The two shifts now share one draw.

- [`sunflower()`](https://alexandercoppock.com/vayr/reference/sunflower.md)
  and
  [`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md)
  moved a point that had nothing over-plotting it. A lone point now
  stays at its own coordinates.

- All four splitting position adjustments returned rows in a different
  order than they received them. Row order is now preserved.

- [`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md)
  ran the circle packing twice per group and discarded the first result.

## vayr 1.0.0

CRAN release: 2025-04-15

- Initial CRAN submission.

This first release of ‘vayr’ includes three position adjustments and
their dodged counterparts: position_jitter_ellipse() and
position_jitterdodge_ellipse(); position_sunflower() and
position_sunflower_dodge(); position_circlepack() and
position_circlepack_dodge().
