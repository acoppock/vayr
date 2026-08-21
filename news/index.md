# Changelog

## vayr 1.1.0

CRAN release: 2026-08-21

### New features

- [`position_bluenoise()`](https://alexandercoppock.com/vayr/reference/position_bluenoise.md)
  and
  [`position_bluenoisedodge()`](https://alexandercoppock.com/vayr/reference/position_bluenoisedodge.md)
  scatter over-plotted points across an elliptical field while keeping
  them evenly spaced, in the pattern the eye’s own photoreceptors are
  laid out in. Jittering samples uniformly, which clumps: in a draw of
  250 points the closest pair typically sits about a fifteenth of the
  median spacing apart, and a reader cannot tell those knots from real
  structure. Blue noise looks equally unstructured without the knots, so
  unlike
  [`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md)
  there is no visible spiral for a reader to misread as a finding.

- [`position_honeycomb()`](https://alexandercoppock.com/vayr/reference/position_honeycomb.md)
  and
  [`position_honeycombdodge()`](https://alexandercoppock.com/vayr/reference/position_honeycombdodge.md)
  arrange over-plotted points on a hexagonal lattice, the densest
  packing of equal circles in the plane. At a given `density` the
  footprint matches
  [`position_sunflower()`](https://alexandercoppock.com/vayr/reference/position_sunflower.md),
  so the two are interchangeable and the choice between them is about
  looks: crystalline and countable, or organic and without a preferred
  direction.

- All five dodged position adjustments gain an `orientation` argument,
  matching
  [`ggplot2::position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html).
  They were hard-wired to separate groups side-to-side;
  `orientation = "y"` now separates them up and down instead. ggplot2
  has no `height` argument for dodging, since `width` is the extent
  along whichever axis `orientation` picks. The `height` spelling
  belongs to the superseded ‘ggstance’ package, whose
  `position_dodgev()` predates `orientation`.

- [`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md)
  prepares the extreme value bounds figure for an experiment with
  attrition, imputing the logical best case and worst case for the
  missing outcomes and flagging which points are observed and which are
  imputed. It estimates nothing; `estimator_ev()` in the ‘attrition’
  package (<https://github.com/acoppock/attrition>) provides the bounds
  themselves. The outcome’s logical range is required at the call site
  rather than guessed from the data, because guessing narrows the
  bounds.

- Seven new datasets carry the worked examples from the chapter the
  package implements: `two_arm_trial`, `blocked_experiment`,
  `clustered_experiment`, `covariate_adjustment`,
  `continuous_interaction`, `noncompliance_experiment`, and
  `attrition_experiment`. All are simulated, and all report both
  potential outcomes and the probability of the assigned condition.

- A second vignette,
  [`vignette("design-based-graphs")`](https://alexandercoppock.com/vayr/articles/design-based-graphs.md),
  reproduces the chapter’s seven worked examples, one per experimental
  design, contrasting a strong graph with a weak one in each case.

### Bug fixes

Every position adjustment now has behavioural tests covering the
geometry it promises, which is how these were found.

Four of the fixes change where points are drawn, so a figure built with
1.0.0 will not reproduce pixel for pixel under 1.1.0: the lone-point
sunflower, the row order, the `seed = NULL` ellipse, and the centring of
the circle pack. Estimates and models are unaffected.

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

- [`position_circlepack()`](https://alexandercoppock.com/vayr/reference/position_circlepack.md)
  and
  [`position_circlepackdodge()`](https://alexandercoppock.com/vayr/reference/position_circlepackdodge.md)
  displaced the cluster from the point it stands for.
  `circleProgressiveLayout()` does not centre what it returns: a lone
  circle lands at `(-radius, 0)`, and packs of three or five sit
  off-centre by roughly a third of their radius. The layout is now
  centred, so a lone point stays exactly where it is here too.

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
