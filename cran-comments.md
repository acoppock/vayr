## Summary

This is a minor release of an existing package.

New features:

* Five new exported functions. `position_bluenoise()` and
  `position_honeycomb()`, with their dodged counterparts, are two new families
  of position adjustment, taking the package from six to ten.
  `impute_extreme_values()` prepares the data for a graph of extreme value
  bounds under attrition.
* The five dodged position adjustments gain an `orientation` argument, matching
  `ggplot2::position_dodge()`. They previously separated groups side-to-side
  only.
* Seven new simulated datasets, and a second vignette that uses them to work
  through seven experimental designs.

It also fixes six bugs in the position adjustments. The most serious is that
four of the six exported adjustments used `resolution()`, `transform_position()`
and `PositionDodge` without qualification, so they resolved off the search path
and failed for anyone who called `ggplot2::ggplot()` without first attaching
'ggplot2'. They are now imported explicitly.

Three of the fixes change where points are drawn, so figures produced under
1.0.0 will not reproduce exactly under 1.1.0. NEWS.md says which three.
Estimates are unaffected.

The Description field now cites the book chapter the package implements, using
the \doi{} form.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local aarch64-apple-darwin23, R 4.6.0
* GitHub Actions, macos-latest, R release
* GitHub Actions, windows-latest, R release
* GitHub Actions, ubuntu-latest, R devel
* GitHub Actions, ubuntu-latest, R release
* GitHub Actions, ubuntu-latest, R oldrel-1

All six report 0 errors, 0 warnings, 0 notes.

## Reverse dependencies

There are no reverse dependencies.
