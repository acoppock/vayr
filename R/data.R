#' Original and Replication data for the Patriot Act experiment described in Persuasion in Parallel
#'
#' @family Data
#'
#' @format ## `patriot_act`
#' A tibble with 2062 rows and 4 columns:
#' \describe{
#'   \item{sample_label}{The original study (Chong and Druckman (2011) or the Mechanical Turk replication)}
#'   \item{pid_3}{Subject partisanship (limited to Republicans and Democracts, including leaners)}
#'   \item{T1_content}{Content of assigned treatment condition: pro-Patriot act statements, anti-Patriot act statements, or a control}
#'   \item{PA_support}{Post-treatment support for the Patriot Act on a 1 to 7 Likert scale}
#' }
#' @source \doi{10.7910/DVN/I9GSKI}
#'
"patriot_act"

#' Simulated two-arm trial
#'
#' One of seven simulated datasets behind the worked examples in Coppock (2021).
#' A completely randomized trial with a binary outcome, in which 100 of 500
#' subjects are treated, so the two arms have different assignment probabilities.
#'
#' Every dataset in this family reports both potential outcomes. They are
#' knowable here because the data are simulated, and no real experiment observes
#' them. `Z_cond_prob` is the probability that a unit was assigned to the
#' condition it is actually in, which is the quantity an inverse probability
#' weight inverts.
#'
#' @family Data
#'
#' @format ## `two_arm_trial`
#' A tibble with 500 rows and 8 columns:
#' \describe{
#'   \item{ID}{Subject identifier}
#'   \item{U}{Unobserved subject-level shock}
#'   \item{Y_Z_0, Y_Z_1}{Binary potential outcomes under control and treatment}
#'   \item{Z}{Random assignment, 1 for treatment and 0 for control}
#'   \item{Z_cond_prob}{Probability of the assigned condition, 0.2 for treated and 0.8 for control}
#'   \item{Y}{Revealed outcome}
#'   \item{condition}{`Z` labelled "Treatment" and "Control"}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"two_arm_trial"

#' Simulated block-randomized experiment
#'
#' Residents of two neighborhoods, with 25 residents treated in each. The
#' neighborhoods differ in size, so assignment probabilities differ across
#' blocks: 0.5 in the 50-resident neighborhood and 0.25 in the 100-resident one.
#' The treatment effect also differs by neighborhood.
#'
#' @family Data
#'
#' @format ## `blocked_experiment`
#' A tibble with 150 rows and 9 columns:
#' \describe{
#'   \item{neighborhood}{Block identifier, 1 or 2}
#'   \item{lambda}{Poisson rate governing the outcome, 10 in neighborhood 1 and 5 in neighborhood 2}
#'   \item{resident}{Subject identifier within neighborhood}
#'   \item{Y_Z_0, Y_Z_1}{Count potential outcomes under control and treatment}
#'   \item{Z}{Random assignment, 1 for treatment and 0 for control}
#'   \item{Z_cond_prob}{Probability of the assigned condition, which varies by block}
#'   \item{Y}{Revealed outcome}
#'   \item{condition}{`Z` labelled "Treatment" and "Control"}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"blocked_experiment"

#' Simulated cluster-randomized experiment
#'
#' Thirty classes of varying size are assigned to treatment or control as whole
#' clusters, so every student in a class shares its assignment. Outcomes are
#' driven by a class-level shock as well as a student-level one, which is why
#' uncertainty estimates that ignore the clustering are overconfident.
#'
#' @family Data
#'
#' @format ## `clustered_experiment`
#' A tibble with 441 rows and 11 columns:
#' \describe{
#'   \item{class}{Cluster identifier, 30 classes in all}
#'   \item{n_per_class}{Number of students in the class, between 10 and 20}
#'   \item{class_shock}{Class-level component of the outcome}
#'   \item{student}{Student identifier within class}
#'   \item{student_shock}{Student-level component of the outcome}
#'   \item{Y_Z_0, Y_Z_1}{Continuous potential outcomes under control and treatment}
#'   \item{Z}{Random assignment, constant within class}
#'   \item{Z_cond_prob}{Probability of the assigned condition, 0.5 throughout}
#'   \item{Y}{Revealed outcome}
#'   \item{condition}{`Z` labelled "Treatment" and "Control"}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"clustered_experiment"

#' Simulated experiment with a pretreatment covariate
#'
#' A completely randomized experiment in which a continuous pretreatment
#' covariate predicts the outcome, so adjusting for it buys precision.
#'
#' @family Data
#'
#' @format ## `covariate_adjustment`
#' A tibble with 100 rows and 9 columns:
#' \describe{
#'   \item{ID}{Subject identifier}
#'   \item{U}{Unobserved subject-level shock}
#'   \item{X}{Continuous pretreatment covariate}
#'   \item{Y_Z_0, Y_Z_1}{Continuous potential outcomes under control and treatment}
#'   \item{Z}{Random assignment, 1 for treatment and 0 for control}
#'   \item{Z_cond_prob}{Probability of the assigned condition, 0.5 throughout}
#'   \item{Y}{Revealed outcome}
#'   \item{condition}{`Z` labelled "Treatment" and "Control"}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"covariate_adjustment"

#' Simulated experiment whose effect varies with a continuous covariate
#'
#' The treatment effect is a nonlinear function of `X`, which makes this dataset
#' useful for showing conditional average treatment effects. Subjects were also
#' sampled with probability related to `X`, so the sample is not a simple random
#' sample of the population that generated it.
#'
#' @family Data
#'
#' @format ## `continuous_interaction`
#' A tibble with 1189 rows and 10 columns:
#' \describe{
#'   \item{ID}{Subject identifier}
#'   \item{noise}{Unobserved subject-level shock}
#'   \item{X}{Continuous pretreatment covariate}
#'   \item{Y_Z_1, Y_Z_0}{Continuous potential outcomes under treatment and control}
#'   \item{S_inclusion_prob}{Probability the subject was sampled, which increases in `X`}
#'   \item{Z}{Random assignment, 1 for treatment and 0 for control}
#'   \item{Z_cond_prob}{Probability of the assigned condition, 0.5 throughout}
#'   \item{Y}{Revealed outcome}
#'   \item{condition}{`Z` labelled "Treatment" and "Control"}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"continuous_interaction"

#' Simulated experiment with two-sided noncompliance
#'
#' Assignment does not determine receipt in either direction: 50 of the 300
#' control subjects took the treatment anyway, and 100 of the 300 treated
#' subjects did not take it. Receipt is a post-treatment variable, so
#' conditioning on it invites post-treatment bias.
#'
#' @family Data
#'
#' @format ## `noncompliance_experiment`
#' A tibble with 600 rows and 3 columns:
#' \describe{
#'   \item{Z}{Random assignment, "Treatment" or "Control"}
#'   \item{D}{Treatment receipt, 1 if the subject took the treatment. A post-treatment variable}
#'   \item{Y}{Turnout, 1 if the subject voted}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"noncompliance_experiment"

#' Simulated experiment encountering attrition
#'
#' Nineteen of the 200 subjects have a missing outcome, and missingness is
#' related to the potential outcomes, so dropping those subjects conditions the
#' analysis on a post-treatment variable. Use with [impute_extreme_values()],
#' which imputes the best and worst cases the observed data admit. The outcome
#' is a seven-point Likert item, so its logical range is 1 to 7 even though no
#' subject in this sample answered 1.
#'
#' @family Data
#'
#' @format ## `attrition_experiment`
#' A tibble with 200 rows and 10 columns:
#' \describe{
#'   \item{ID}{Subject identifier}
#'   \item{U}{Unobserved subject-level shock}
#'   \item{Y_Z_0, Y_Z_1}{Likert potential outcomes under control and treatment}
#'   \item{R_Z_0, R_Z_1}{Potential response indicators, 1 if the subject would report an outcome}
#'   \item{Z}{Random assignment, 1 for treatment and 0 for control}
#'   \item{Z_cond_prob}{Probability of the assigned condition, 0.5 throughout}
#'   \item{R}{Revealed response indicator, 1 when `Y` is observed}
#'   \item{Y}{Revealed outcome, `NA` for the 19 subjects who did not report}
#' }
#' @source \doi{10.7910/DVN/VE6VSR}
#'
"attrition_experiment"
