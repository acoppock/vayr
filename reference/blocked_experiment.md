# Simulated block-randomized experiment

Residents of two neighborhoods, with 25 residents treated in each. The
neighborhoods differ in size, so assignment probabilities differ across
blocks: 0.5 in the 50-resident neighborhood and 0.25 in the 100-resident
one. The treatment effect also differs by neighborhood.

## Usage

``` r
blocked_experiment
```

## Format

### `blocked_experiment`

A tibble with 150 rows and 9 columns:

- neighborhood:

  Block identifier, 1 or 2

- lambda:

  Poisson rate governing the outcome, 10 in neighborhood 1 and 5 in
  neighborhood 2

- resident:

  Subject identifier within neighborhood

- Y_Z_0, Y_Z_1:

  Count potential outcomes under control and treatment

- Z:

  Random assignment, 1 for treatment and 0 for control

- Z_cond_prob:

  Probability of the assigned condition, which varies by block

- Y:

  Revealed outcome

- condition:

  `Z` labelled "Treatment" and "Control"

## Source

[doi:10.7910/DVN/VE6VSR](https://doi.org/10.7910/DVN/VE6VSR)

## See also

Other Data:
[`attrition_experiment`](https://alexandercoppock.com/vayr/reference/attrition_experiment.md),
[`clustered_experiment`](https://alexandercoppock.com/vayr/reference/clustered_experiment.md),
[`continuous_interaction`](https://alexandercoppock.com/vayr/reference/continuous_interaction.md),
[`covariate_adjustment`](https://alexandercoppock.com/vayr/reference/covariate_adjustment.md),
[`noncompliance_experiment`](https://alexandercoppock.com/vayr/reference/noncompliance_experiment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md),
[`two_arm_trial`](https://alexandercoppock.com/vayr/reference/two_arm_trial.md)
