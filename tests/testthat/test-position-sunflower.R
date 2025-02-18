library(ggplot2)

test_that("position_sunflower works", {
  dat <- data.frame(x = rep(1, 500), y = rep(1, 500))

  g <- ggplot(dat, aes(x, y)) +
    geom_point(position = position_sunflower()) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

  expect_s3_class(object = g, class = "gg")
})
