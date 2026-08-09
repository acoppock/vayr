# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

rm(list = ls())
library(tidyverse)

dat <- read_csv("noncompliance_simulated_data.csv")

gg_df <-
  dat %>%
  gather(dv, value, `Treatment Receipt`, `Turnout`)


summary_df <-
  gg_df %>%
  group_by(Z, dv) %>%
  do(tidy(lm_robust(value ~ 1, data = .))) %>%
  mutate(value = estimate)

good <-
  ggplot(summary_df, aes(Z, value)) +
  geom_point(
    data = gg_df,
    position = position_jitter(width = 0.2, height = 0.1),
    alpha = 0.1
  ) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme(strip.background = element_blank()) +
  facet_wrap( ~ dv) +
  ylab("Outcome variable [1 = 'Yes', 0 = otherwise]") +
  xlab("Randomly assigned group")

dat <-
  dat %>%
  mutate(`Treatment Receipt` = factor(
    `Treatment Receipt`,
    levels = c(0, 1),
    labels = c("Did not receive treatment", "Did receive treatment")
  ))

summary_df <-
  dat %>% group_by(Z, `Treatment Receipt`) %>%
  do(tidy(lm_robust(Turnout ~ 1, data = .))) %>%
  mutate(Turnout = estimate)



bad <-
  ggplot(summary_df, aes(Z, Turnout)) +
  geom_point(
    data = dat,
    position = position_jitter(width = 0.2, height = 0.1),
    alpha = 0.1
  ) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme(strip.background = element_blank(),
        legend.position = "bottom") +
  facet_wrap( ~ `Treatment Receipt`) +
  ylab("Turnout [1 = 'Yes', 0 = otherwise]") +
  xlab("Randomly assigned group")

ggsave("noncompliance_good.pdf",
       good,
       width = 4,
       height = 4)
ggsave("noncompliance_bad.pdf",
       bad,
       width = 4,
       height = 4)
