# vayr 1.1.0

## New features

* `impute_extreme_values()` prepares the extreme value bounds figure for an
  experiment with attrition, imputing the logical best case and worst case for
  the missing outcomes and flagging which points are observed and which are
  imputed. It estimates nothing; `estimator_ev()` in the 'attrition' package
  (<https://github.com/acoppock/attrition>) provides the bounds themselves. The
  outcome's logical range is required at the call site rather than guessed from
  the data, because guessing narrows the bounds.

* Seven new datasets carry the worked examples from the chapter the package
  implements: `two_arm_trial`, `blocked_experiment`, `clustered_experiment`,
  `covariate_adjustment`, `continuous_interaction`,
  `noncompliance_experiment`, and `attrition_experiment`. All are simulated,
  and all report both potential outcomes and the probability of the assigned
  condition.

* A second vignette, `vignette("design-based-graphs")`, reproduces the
  chapter's seven worked examples, one per experimental design, contrasting a
  strong graph with a weak one in each case.

## Bug fixes

All six position adjustments now have behavioural tests covering the geometry
they promise, which is how these were found.

Three of the fixes change where points are drawn, so a figure built with 1.0.0
will not reproduce pixel for pixel under 1.1.0: the lone-point sunflower, the
row order, and the `seed = NULL` ellipse. Estimates and models are unaffected.

* The position adjustments no longer require `ggplot2` to be attached. They used
  `resolution()`, `transform_position()`, and `PositionDodge` unqualified, so
  `position_jitter_ellipse()`, `position_jitterdodge_ellipse()`,
  `position_sunflowerdodge()`, and `position_circlepackdodge()` failed for anyone
  calling `ggplot2::ggplot()` without `library(ggplot2)`, and for any package
  importing 'vayr'.

* `position_jitterdodge_ellipse()` gave every group the same jitter, so one
  group's cloud was an exact translation of the next. Each group is now jittered
  independently.

* `position_jitter_ellipse()` and `position_jitterdodge_ellipse()` drew the
  radius and angle separately for the x and y shifts, which placed points outside
  the requested ellipse whenever `seed = NULL`. The two shifts now share one draw.

* `sunflower()` and `position_sunflower()` moved a point that had nothing
  over-plotting it. A lone point now stays at its own coordinates.

* All four splitting position adjustments returned rows in a different order than
  they received them. Row order is now preserved.

* `position_circlepackdodge()` ran the circle packing twice per group and
  discarded the first result.

# vayr 1.0.0

* Initial CRAN submission.

This first release of 'vayr' includes three position adjustments and their dodged counterparts:
position_jitter_ellipse() and position_jitterdodge_ellipse();
position_sunflower() and position_sunflower_dodge(); 
position_circlepack() and position_circlepack_dodge().
