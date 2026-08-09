# Replication Archive for
# Coppock, Alexander. Visualize as You Randomize: Design-Based Statistical Graphs for Randomized Experiments
# Advances in Experimental Political Science, James N. Druckman and Donald P. Green, editors

rm(list = ls())
library(tidyverse)
library(estimatr)
library(margins)
library(ggrepel)

dat <- read_csv("interaction_simulated_data.csv")

fit <- lm_robust(Y ~ condition * X, data = dat)

pred_df <- data.frame(X = c(1, 1),
                      condition = c("Control", "Treatment"))
pred_df$Y <- predict(fit, newdata = pred_df)

label_df <- data.frame(
  X = c(1,0.5),
  Y = c(-3.5, 7),
  condition = c("Control", "Treatment"),
  label = c("Slope for control units", "Slope for treated units")
)

lines_df <- bind_rows(pred_df, label_df)

good <-
  ggplot(dat, aes(X, Y, shape = condition, group = condition)) +
  geom_point(alpha = 0.2, stroke = 0) +
  stat_smooth(method = "lm_robust", fullrange = TRUE, color = "black") +
  geom_line(data = lines_df) + 
  geom_label(data = label_df, aes(label = label)) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-5, 10)) +
  scale_color_manual(values = c("#205C8A", "#C67800")) +
  xlab("Continuous pre-treatment covariate") +
  ylab("Outcome") +
  theme_bw() +
  theme(legend.position = "none")
good


gg_df <-
  fit %>%
  margins(at = list(X = seq(-2, 2, by = 0.25))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "conditionTreatment")

bad <-
  ggplot(gg_df, aes(X, AME)) +
  geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-5, 10)) +
  geom_errorbar(aes(ymax = lower, ymin = upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Continuous pre-treatment covariate") +
  ylab("Estimated CATE at each level of the covariate") +
  theme_bw()
bad

ggsave("interactions_good.pdf",
       good,
       width = 4,
       height = 4)
ggsave("interactions_bad.pdf",
       bad,
       width = 4,
       height = 4)
