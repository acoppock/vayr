# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

rm(list = ls())
library(tidyverse)

dat <- read_csv("covariate_simulated_data.csv")

# Residuals ---------------------------------------------------------------

dat <- dat %>% mutate(X_c = X - mean(X))

fit_1 <- lm(Y ~ X_c + X_c:Z, data = dat)
fit_2 <- lm(Z ~ X_c + X_c:Z, data = dat)

dat <- dat %>% mutate(Y_Adjusted = residuals(fit_1),
                      Z_Adjusted = residuals(fit_2),
                      Y_Unadjusted = Y,
                      Z_Unadjusted = Z)

# confirm!
# standard errors v. slightly off; point estimates OK
lm_lin(Y ~ Z, covariates = ~ X, data = dat)
lm_robust(Y ~ Z + X_c + X_c:Z, data = dat)
lm_robust(Y_Adjusted ~ Z_Adjusted, data = dat)

gg_df <-
  dat %>%
  gather(key, value, Z_Unadjusted, Z_Adjusted, Y_Unadjusted, Y_Adjusted) %>%
  separate(key, into = c("variable", "estimation")) %>%
  spread(key = variable, value) %>%
  mutate(estimation = factor(estimation, levels = c("Unadjusted", "Adjusted")))


summary_df <-
  gg_df %>%
  group_by(estimation) %>%
  do(tidy(lm_robust(Y ~ 1, data = .))) %>%
  mutate(Y = estimate)


blank_df <-
  data.frame(
    Z = 0,
    estimation = c("Unadjusted", "Unadjusted", "Adjusted", "Adjusted"),
    Y = c(0, 10,-5, 5)
  )


good <- 
ggplot(gg_df, aes(Z, Y)) +
  geom_point(alpha = 0.4, stroke = 0) +
  geom_blank(data = blank_df) +
  stat_smooth(method = "lm_robust", color = "grey", alpha = 0.5) +
  scale_y_continuous(breaks = -6:12) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ylab("Outcome variable (raw scale is 7-point Likert)") +
  xlab("Randomly assigned treatment") +
  facet_wrap(~estimation, scales = "free") 
good

ggsave("covariate_adjustment_good.pdf", good, width = 4, height = 4)
