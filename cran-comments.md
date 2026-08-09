## Summary

This is a minor release of an existing package.

It adds one exported function (`impute_extreme_values()`), seven simulated
datasets, and a second vignette. It also fixes six bugs in the position
adjustments. The most serious of those is that four of the six exported
position adjustments used `resolution()`, `transform_position()`, and
`PositionDodge` without qualification, so they resolved off the search path and
failed for anyone who called `ggplot2::ggplot()` without first attaching
'ggplot2'. They are now imported explicitly.

Three of the fixes change where points are drawn, so figures produced under
1.0.0 will not reproduce exactly under 1.1.0. NEWS.md says which three.

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
