# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

rm(list = ls())
library(tidyverse)
library(estimatr)

dat <- read_csv("blocked_simulated_data.csv")

summary_good <-
  dat %>% group_by(condition) %>%
  do(tidy(lm_robust(Y ~ 1, weights = 1/Z_cond_prob, data = .))) %>%
  mutate(Y = estimate)

summary_bad <-
  dat %>% group_by(condition) %>%
  do(tidy(lm_robust(Y ~ 1, data = .))) %>%
  mutate(Y = estimate)

good <- 
  ggplot(dat, aes(condition, Y)) +
  geom_point(aes(size = 1 / Z_cond_prob),
                 position = position_jitter(width = .25, height = .25), 
                 alpha = 0.1, 
                 stroke = 0) +
  geom_point(data = summary_good, size = 4) +
  geom_errorbar(data = summary_good,
                aes(ymin = conf.low, ymax = conf.high),
                width = 0) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  ylab("Outcome variable: count of some behavior")

bad <-
  ggplot(dat, aes(condition, Y)) +
  geom_point(position = position_jitter(width = .25, height = .1), 
             alpha = 0.1, 
             stroke = 0) +
  geom_point(data = summary_bad, size = 4) +
  geom_errorbar(data = summary_bad,
                aes(ymin = conf.low, ymax = conf.high),
                width = 0) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  ylab("Outcome variable: count of some behavior")


ggsave("blocked_1_good.pdf", good, width = 4, height = 4)
ggsave("blocked_1_bad.pdf", bad, width = 4, height = 4)


# By facets ---------------------------------------------------------------

dat <-
  dat %>%
  mutate(neighborhood_lab = paste0("Neighborhood ", neighborhood),
         neighborhood_lab2 = paste0("N/hood ", neighborhood))


summary_df <-
  dat %>% group_by(condition, neighborhood_lab) %>%
  do(tidy(lm_robust(Y ~ 1, data = .))) %>%
  mutate(Y = estimate)

good <- 
ggplot(dat, aes(condition, Y)) +
  geom_point(position = position_jitter(width = .25, height = .1), alpha = 0.1, stroke = 0) +
  facet_wrap( ~ neighborhood_lab) +
  geom_point(data = summary_df, size = 4) +
  geom_errorbar(data = summary_df,
                aes(ymin = conf.low, ymax = conf.high),
                width = 0) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.x = element_blank()) +
  ylab("Outcome variable: count of some behavior")

summary_df <-
  dat %>% group_by(condition, neighborhood_lab2) %>%
  do(tidy(lm_robust(Y ~ 1, data = .))) %>%
  mutate(Y = estimate)

bad <-
  ggplot(dat, aes(neighborhood_lab2, Y)) +
  geom_point(position = position_jitter(width = .25, height = .25), alpha = 0.1, stroke = 0) +
  facet_wrap( ~ condition) +
  geom_point(data = summary_df, size = 4) +
  geom_errorbar(data = summary_df,
                aes(ymin = conf.low, ymax = conf.high),
                width = 0) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.x = element_blank()) +
  ylab("Outcome variable: count of some behavior")

ggsave("blocked_2_good.pdf", good, width = 4, height = 4)
ggsave("blocked_2_bad.pdf", bad, width = 4, height = 4)
