# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

library(DeclareDesign)
library(tidyverse)
library(MASS)

rm(list = ls())
set.seed(343)

# Two Arm Trial -----------------------------------------------------------

design <-
  declare_population(N = 500,
                     U = rnorm(N)) +
  declare_potential_outcomes(formula = Y ~ draw_binary(latent = 0.5 * Z + U, link = "probit")) +
  declare_assignment(m = 100) +
  declare_reveal()

dat <-
  design %>%
  draw_data() %>%
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

summary_df <-
  dat %>%
  group_by(condition) %>%
  do(tidy(lm_robust(Y ~ 1, data = .))) %>%
  mutate(Y = estimate)

write_csv(dat, "two_arm_simulated_data.csv")

# blocked -----------------------------------------------------------------

rm(list = ls())

design <-
  declare_population(
    neighborhood = add_level(N = 2, lambda = c(10, 5)),
    resident = add_level(N = c(50, 100))
  ) +
  declare_potential_outcomes(Y ~ 0 * Z + -4 * Z * (neighborhood == 1) + rpois(N, lambda)) +
  declare_assignment(blocks = neighborhood, m = 25) +
  declare_reveal()

dat <-
  design %>%
  draw_data() %>%
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

write_csv(dat, "blocked_simulated_data.csv")

lm_robust(Y ~ Z, data = dat)
lm_robust(Y ~ Z, weights = 1/Z_cond_prob, data = dat)


# clustered ---------------------------------------------------------------

design <-
  declare_population(
    class = add_level(
      N = 30,
      n_per_class = sample(10:20, size = N, replace = TRUE),
      class_shock = rnorm(N, mean = 1000, sd = 100)
    ),
    student = add_level(N = n_per_class,
                        student_shock = rnorm(N, sd = 175))
  ) +
  declare_potential_outcomes(formula = Y ~ 100 * Z + class_shock + student_shock) +
  declare_assignment(clusters = class) +
  declare_reveal()

dat <-
  design %>%
  draw_data() %>%
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

write_csv(dat, "clustered_simulated_data.csv")



# Covariate adjustment ----------------------------------------------------

rm(list = ls())

design <-
  declare_population(N = 100,
                     U = rnorm(N, sd = 0.5),
                     X = rnorm(N, mean = 3, sd = 1)) +
  declare_potential_outcomes(Y ~ 0.5 * Z + 1.0 * X + 0.5 * Z * X + U) +
  declare_assignment() +
  declare_reveal()

dat <-
  design %>%
  draw_data() %>%
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

write_csv(dat, "covariate_simulated_data.csv")


# Noncompliance -----------------------------------------------------------

rm(list = ls())
dat <-
  tibble(
    Z = rep(c("Treatment", "Control"), each = 300),
    `Treatment Receipt` = rep(c(1, 0, 1, 0), c(200, 100, 50, 250)),
    `Turnout` = rep(c(1, 0, 1, 0, 1, 0, 1, 0),
                    c(170, 30, 20, 80, 25, 50, 125, 100))
  )

write_csv(dat, "noncompliance_simulated_data.csv")


# Attrition ---------------------------------------------------------------

design <-
  declare_population(N = 200,
                     U = rnorm(N)) +
  declare_potential_outcomes(formula = Y ~ as.numeric(draw_likert(0.5 * Z + U))) +
  declare_potential_outcomes(formula = R ~ draw_binary(latent = -0.3 * Z + U + 2, link = "probit")) +
  declare_assignment() +
  declare_reveal(outcome_variables = "R") +
  declare_reveal(outcome_variables = "Y",
                 attrition_variables = "R")

dat <-  design %>% draw_data()

write_csv(dat, "attrition_simulated_data.csv")


# Interactions ------------------------------------------------------------

rm(list = ls())
design <-
  declare_population(N = 2500,
                     noise = rnorm(N),
                     X = rnorm(N)) +
  declare_potential_outcomes(Y_Z_1 = 2*X^2 + 0.50*X + noise,
                             Y_Z_0 = 1.00 * X + noise) +
  declare_sampling(prob_unit = pnorm(X), simple = TRUE) + 
  declare_assignment() +
  declare_reveal()

dat <-
  design %>%
  draw_data() %>%
  mutate(condition = if_else(Z == 1, "Treatment", "Control"))

write_csv(dat, "interaction_simulated_data.csv")

