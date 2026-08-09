# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

rm(list = ls())
library(tidyverse)

dat <- read_csv("clustered_simulated_data.csv")

summary_df_good <-
  dat %>%
  group_by(condition) %>%
  do(tidy(lm_robust(Y ~ 1, clusters = class, data = .))) %>%
  mutate(Y = estimate)

summary_df_bad <-
  dat %>%
  group_by(condition) %>%
  do(tidy(lm_robust(Y ~ 1, data = .))) %>%
  mutate(Y = estimate)

class_level <-
  dat %>% group_by(class, condition, n_per_class) %>%
  summarise(Y = mean(Y))

# kinda cool!
lm_robust(Y ~ condition, weights = n_per_class, data = class_level)
lm_robust(Y ~ condition, clusters = class, data = dat)

good <- 
ggplot(summary_df_good, aes(condition, Y)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(data = class_level, aes(size = n_per_class),
             position = position_jitter(width = 0.2, height = 0.1), 
             alpha = 0.2, stroke = 0) +
  coord_cartesian(ylim = c(400, 1600)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  ylab("Outcome variable: Classroom Average SAT score")

bad <-
  ggplot(summary_df_bad, aes(condition, Y)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(data = dat,
             position = position_jitter(width = 0.2, height = 0.1), 
             alpha = 0.2, stroke = 0) +
  coord_cartesian(ylim = c(400, 1600)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  ylab("Outcome variable: Classroom Average SAT score")

ggsave("cluster_good.pdf", good, width = 4, height = 4)
ggsave("cluster_bad.pdf", bad, width = 4, height = 4)

