# Simulated experiment with two-sided noncompliance

Assignment does not determine receipt in either direction: 50 of the 300
control subjects took the treatment anyway, and 100 of the 300 treated
subjects did not take it. Receipt is a post-treatment variable, so
conditioning on it invites post-treatment bias.

## Usage

``` r
noncompliance_experiment
```

## Format

### `noncompliance_experiment`

A tibble with 600 rows and 3 columns:

- Z:

  Random assignment, "Treatment" or "Control"

- D:

  Treatment receipt, 1 if the subject took the treatment. A
  post-treatment variable

- Y:

  Turnout, 1 if the subject voted

## Source

[doi:10.7910/DVN/VE6VSR](https://doi.org/10.7910/DVN/VE6VSR)

## See also

Other Data:
[`attrition_experiment`](https://alexandercoppock.com/vayr/reference/attrition_experiment.md),
[`blocked_experiment`](https://alexandercoppock.com/vayr/reference/blocked_experiment.md),
[`clustered_experiment`](https://alexandercoppock.com/vayr/reference/clustered_experiment.md),
[`continuous_interaction`](https://alexandercoppock.com/vayr/reference/continuous_interaction.md),
[`covariate_adjustment`](https://alexandercoppock.com/vayr/reference/covariate_adjustment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md),
[`two_arm_trial`](https://alexandercoppock.com/vayr/reference/two_arm_trial.md)
