library(vayr)
library(tidyverse)
library(patchwork)

dat <- tibble(x = rep(c(1, 2, 3), c(10, 20, 100)),
              y = 1)


ggplot(dat, aes(x, y)) +
  geom_point(stroke = 0, position = position_sunflower(flower_width = 0.15,
                                                       flower_height = 0.15)) +
  coord_fixed(xlim = c(0, 4)) +
  theme_void()



dat_1 <- dat |> filter(x == 1)
dat_2 <- dat |> filter(x == 2)
dat_3 <- dat |> filter(x == 3)


factor_3 <- 1
factor_2 <- factor_3 / sqrt(5)
factor_1 <- factor_3 / sqrt(10)




100 / (pi * factor_3^2)
20 / (pi * factor_2^2)
10 / (pi * factor_1^2)



g_1 <-
ggplot(dat_1, aes(x, y)) +
  geom_point(stroke = 0, position = position_sunflower(flower_width = factor_1,
                                           flower_height = factor_1)) +
  coord_fixed(xlim = c(0, 4)) +
  theme_void()

g_2 <-
ggplot(dat_2, aes(x, y)) +
  geom_point(stroke = 0, position = position_sunflower(flower_width = factor_2,
                                           flower_height = factor_2)) +
  coord_fixed(xlim = c(0, 4))+
  theme_void()

g_3 <-
ggplot(dat_3, aes(x, y)) +
  geom_point(stroke = 0, position = position_sunflower(flower_width = factor_3,
                                           flower_height = factor_3)) +
  coord_fixed(xlim = c(0, 4))+
  theme_void()

g_1 / g_2 / g_3

