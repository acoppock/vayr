library(ggplot2)

test_that("position_circlepack works", {
  dat <- data.frame(x = rep(1, 500), y = rep(1, 500), size = rep(1, 500))

  g <- ggplot(dat, aes(x, y, size = size)) +
    geom_point(position = position_circlepack()) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

  expect_s3_class(object = g, class = "gg")
})
