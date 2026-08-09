# Simulated experiment with a pretreatment covariate

A completely randomized experiment in which a continuous pretreatment
covariate predicts the outcome, so adjusting for it buys precision.

## Usage

``` r
covariate_adjustment
```

## Format

### `covariate_adjustment`

A tibble with 100 rows and 9 columns:

- ID:

  Subject identifier

- U:

  Unobserved subject-level shock

- X:

  Continuous pretreatment covariate

- Y_Z_0, Y_Z_1:

  Continuous potential outcomes under control and treatment

- Z:

  Random assignment, 1 for treatment and 0 for control

- Z_cond_prob:

  Probability of the assigned condition, 0.5 throughout

- Y:

  Revealed outcome

- condition:

  `Z` labelled "Treatment" and "Control"

## Source

[doi:10.7910/DVN/VE6VSR](https://doi.org/10.7910/DVN/VE6VSR)

## See also

Other Data:
[`attrition_experiment`](https://alexandercoppock.com/vayr/reference/attrition_experiment.md),
[`blocked_experiment`](https://alexandercoppock.com/vayr/reference/blocked_experiment.md),
[`clustered_experiment`](https://alexandercoppock.com/vayr/reference/clustered_experiment.md),
[`continuous_interaction`](https://alexandercoppock.com/vayr/reference/continuous_interaction.md),
[`noncompliance_experiment`](https://alexandercoppock.com/vayr/reference/noncompliance_experiment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md),
[`two_arm_trial`](https://alexandercoppock.com/vayr/reference/two_arm_trial.md)
