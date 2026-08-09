# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

rm(list = ls())
library(tidyverse)

dat <- read_csv("attrition_simulated_data.csv")

gg_df <-
  dat %>%
  mutate(
    Condition = factor(
      Z,
      levels = 0:1,
      labels = c("Control", "Treatment")
    ),
    `Lower Bound` =
      case_when(Z == 1 & is.na(Y) ~ 1,
                Z == 0 & is.na(Y) ~ 7,
                TRUE ~ Y),
    `Upper Bound` =
      case_when(Z == 1 & is.na(Y) ~ 7,
                Z == 0 & is.na(Y) ~ 1,
                TRUE ~ Y),
    Y_missing = if_else(is.na(Y), "Outcome imputed", "Outcome available")
  ) %>%
  gather(key, value, `Lower Bound`, `Upper Bound`)

summary_df <-
  gg_df %>%
  group_by(Condition, key) %>%
  do(tidy(lm_robust(value ~ 1, data = .))) %>%
  mutate(value = estimate)

pro_con_colors <- c("#C67800", "#205C8A")

good <- 
ggplot(summary_df, aes(Condition, value)) +
  geom_point(
    data = gg_df,
    aes(color = Y_missing, shape = Y_missing),
    position = position_jitter(width = 0.2, height = 0.1),
    alpha = 0.3
  ) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  scale_y_continuous("Outcome variable \n [1: Strongly Disagree, 7: Strongly Agree]", breaks = 1:7) +
  scale_color_manual(values = c("#205C8A", "#C67800")) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  facet_wrap( ~ key)

ggsave("attrition_good.pdf", good, width = 4, height = 4)
