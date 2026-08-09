# Simulated two-arm trial

One of seven simulated datasets behind the worked examples in Coppock
(2021). A completely randomized trial with a binary outcome, in which
100 of 500 subjects are treated, so the two arms have different
assignment probabilities.

## Usage

``` r
two_arm_trial
```

## Format

### `two_arm_trial`

A tibble with 500 rows and 8 columns:

- ID:

  Subject identifier

- U:

  Unobserved subject-level shock

- Y_Z_0, Y_Z_1:

  Binary potential outcomes under control and treatment

- Z:

  Random assignment, 1 for treatment and 0 for control

- Z_cond_prob:

  Probability of the assigned condition, 0.2 for treated and 0.8 for
  control

- Y:

  Revealed outcome

- condition:

  `Z` labelled "Treatment" and "Control"

## Source

[doi:10.7910/DVN/VE6VSR](https://doi.org/10.7910/DVN/VE6VSR)

## Details

Every dataset in this family reports both potential outcomes. They are
knowable here because the data are simulated, and no real experiment
observes them. `Z_cond_prob` is the probability that a unit was assigned
to the condition it is actually in, which is the quantity an inverse
probability weight inverts.

## See also

Other Data:
[`attrition_experiment`](https://alexandercoppock.com/vayr/reference/attrition_experiment.md),
[`blocked_experiment`](https://alexandercoppock.com/vayr/reference/blocked_experiment.md),
[`clustered_experiment`](https://alexandercoppock.com/vayr/reference/clustered_experiment.md),
[`continuous_interaction`](https://alexandercoppock.com/vayr/reference/continuous_interaction.md),
[`covariate_adjustment`](https://alexandercoppock.com/vayr/reference/covariate_adjustment.md),
[`noncompliance_experiment`](https://alexandercoppock.com/vayr/reference/noncompliance_experiment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md)
