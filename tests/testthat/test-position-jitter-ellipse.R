
library(ggplot2)

test_that("jitter_ellipse works", {
  dat <- data.frame(x = rep(1, 500), y = rep(1, 500))

  g <-
    ggplot(dat, aes(x, y)) +
    geom_point(position = position_jitter_ellipse(width = 0.5, height = 0.5)) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

  expect_s3_class(object = g, class = "gg")
})
