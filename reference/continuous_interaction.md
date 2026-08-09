# Simulated experiment whose effect varies with a continuous covariate

The treatment effect is a nonlinear function of `X`, which makes this
dataset useful for showing conditional average treatment effects.
Subjects were also sampled with probability related to `X`, so the
sample is not a simple random sample of the population that generated
it.

## Usage

``` r
continuous_interaction
```

## Format

### `continuous_interaction`

A tibble with 1189 rows and 10 columns:

- ID:

  Subject identifier

- noise:

  Unobserved subject-level shock

- X:

  Continuous pretreatment covariate

- Y_Z_1, Y_Z_0:

  Continuous potential outcomes under treatment and control

- S_inclusion_prob:

  Probability the subject was sampled, which increases in `X`

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
[`covariate_adjustment`](https://alexandercoppock.com/vayr/reference/covariate_adjustment.md),
[`noncompliance_experiment`](https://alexandercoppock.com/vayr/reference/noncompliance_experiment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md),
[`two_arm_trial`](https://alexandercoppock.com/vayr/reference/two_arm_trial.md)
