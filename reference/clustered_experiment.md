# Simulated cluster-randomized experiment

Thirty classes of varying size are assigned to treatment or control as
whole clusters, so every student in a class shares its assignment.
Outcomes are driven by a class-level shock as well as a student-level
one, which is why uncertainty estimates that ignore the clustering are
overconfident.

## Usage

``` r
clustered_experiment
```

## Format

### `clustered_experiment`

A tibble with 441 rows and 11 columns:

- class:

  Cluster identifier, 30 classes in all

- n_per_class:

  Number of students in the class, between 10 and 20

- class_shock:

  Class-level component of the outcome

- student:

  Student identifier within class

- student_shock:

  Student-level component of the outcome

- Y_Z_0, Y_Z_1:

  Continuous potential outcomes under control and treatment

- Z:

  Random assignment, constant within class

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
[`continuous_interaction`](https://alexandercoppock.com/vayr/reference/continuous_interaction.md),
[`covariate_adjustment`](https://alexandercoppock.com/vayr/reference/covariate_adjustment.md),
[`noncompliance_experiment`](https://alexandercoppock.com/vayr/reference/noncompliance_experiment.md),
[`patriot_act`](https://alexandercoppock.com/vayr/reference/patriot_act.md),
[`two_arm_trial`](https://alexandercoppock.com/vayr/reference/two_arm_trial.md)
