# Simulated experiment encountering attrition

Nineteen of the 200 subjects have a missing outcome, and missingness is
related to the potential outcomes, so dropping those subjects conditions
the analysis on a post-treatment variable. Use with
[`impute_extreme_values()`](https://alexandercoppock.com/vayr/reference/impute_extreme_values.md),
which imputes the best and worst cases the observed data admit. The
outcome is a seven-point Likert item, so its logical range is 1 to 7
even though no subject in this sample answered 1.

## Usage

``` r
attrition_experiment
```

## Format

### `attrition_experiment`

A tibble with 200 rows and 10 columns:

- ID:

  Subject identifier

- U:

  Unobserved subject-level shock

- Y_Z_0, Y_Z_1:

  Likert potential outcomes under control and treatment

- R_Z_0, R_Z_1:

  Potential response indicators, 1 if the subject would report an
  outcome

- Z:

  Random assignment, 1 for treatment and 0 for control

- Z_cond_prob:

  Probability of the assigned condition, 0.5 throughout

- R:

  Revealed response indicator, 1 when `Y` is observed

- Y:

  Revealed outcome, `NA` for the 19 subjects who did not report

## Source

[doi:10.7910/DVN/VE6VSR](https://doi.org/10.7910/DVN/VE6VSR)

## See also

Other Data:
[`blocked_experiment`](https://alexandercoppock.com/vayr/reference/blocked_experiment.md),
[`clustered_experiment`](https://alexandercoppock.com/vayr/reference/clustered_experiment.md),
[`continuous_interaction`](https://alexandercoppock.com/vayr/reference/continuous_interaction.md),
[`covariate_adjustment`](https://alexandercoppock.com/vayr/reference/covariate_adjustment.md),
[`noncompliance_experiment`](https://alexandercoppock.com/vayr/reference/noncompliance_experiment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md),
[`two_arm_trial`](https://alexandercoppock.com/vayr/reference/two_arm_trial.md)
