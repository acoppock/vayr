make_attrition_data <- function() {
  data.frame(
    Z = rep(c("Control", "Treatment"), each = 4),
    Y = c(2, 5, NA, 4, 6, NA, 3, 7),
    id = letters[1:8],
    stringsAsFactors = FALSE
  )
}

# imputation ----

test_that("both scenarios are returned, stacked, in the original row order", {
  dat <- make_attrition_data()
  bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))

  expect_equal(nrow(bounded), 2 * nrow(dat))
  expect_identical(levels(bounded$scenario), c("Lower bound", "Upper bound"))
  expect_identical(bounded$id, rep(dat$id, 2))
})

test_that("the lower bound minimises the treated group and maximises the control group", {
  dat <- make_attrition_data()
  lower <- subset(impute_extreme_values(dat, "Y", "Z", range = c(1, 7)), scenario == "Lower bound")

  # Row 3 is a missing control unit, row 6 a missing treated unit.
  expect_identical(lower$Y[3], 7)
  expect_identical(lower$Y[6], 1)
})

test_that("the upper bound reverses the imputation", {
  dat <- make_attrition_data()
  upper <- subset(impute_extreme_values(dat, "Y", "Z", range = c(1, 7)), scenario == "Upper bound")

  expect_identical(upper$Y[3], 1)
  expect_identical(upper$Y[6], 7)
})

test_that("the upper bound estimate is never below the lower bound estimate", {
  dat <- make_attrition_data()
  bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))

  effect <- tapply(
    bounded$Y,
    list(bounded$scenario, bounded$Z),
    mean
  )
  difference <- effect[, "Treatment"] - effect[, "Control"]

  expect_lte(difference[["Lower bound"]], difference[["Upper bound"]])
})

test_that("observed values are never altered", {
  dat <- make_attrition_data()
  bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))
  observed <- bounded[bounded$imputed == "Outcome available", ]

  expect_identical(observed$Y, rep(dat$Y[!is.na(dat$Y)], 2))
})

test_that("the imputed flag marks exactly the missing rows", {
  dat <- make_attrition_data()
  bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))

  expect_identical(levels(bounded$imputed), c("Outcome available", "Outcome imputed"))
  expect_identical(bounded$imputed == "Outcome imputed", rep(is.na(dat$Y), 2))
})

test_that("data with no attrition yields two identical scenarios", {
  dat <- make_attrition_data()
  dat$Y <- c(2, 5, 3, 4, 6, 1, 3, 7)
  bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))

  lower <- subset(bounded, scenario == "Lower bound")
  upper <- subset(bounded, scenario == "Upper bound")
  expect_identical(lower$Y, upper$Y)
  expect_true(all(bounded$imputed == "Outcome available"))
})

# arm identification ----

test_that("the treated arm defaults to the second value in sort order", {
  dat <- make_attrition_data()
  dat$Z <- rep(0:1, each = 4)
  lower <- subset(impute_extreme_values(dat, "Y", "Z", range = c(1, 7)), scenario == "Lower bound")

  expect_identical(lower$Y[3], 7)
  expect_identical(lower$Y[6], 1)
})

test_that("treated can be named explicitly to override sort order", {
  dat <- make_attrition_data()
  # Sort order picks "Placebo", so naming "Nudge" has to change the answer.
  dat$Z <- rep(c("Placebo", "Nudge"), each = 4)

  by_default <- subset(
    impute_extreme_values(dat, "Y", "Z", range = c(1, 7)),
    scenario == "Lower bound"
  )
  named <- subset(
    impute_extreme_values(dat, "Y", "Z", range = c(1, 7), treated = "Nudge"),
    scenario == "Lower bound"
  )

  # Row 3 is a Placebo unit and row 6 a Nudge unit, both missing.
  expect_identical(by_default$Y[3], 1)
  expect_identical(by_default$Y[6], 7)

  expect_identical(named$Y[3], 7)
  expect_identical(named$Y[6], 1)
})

test_that("a factor assignment uses level order rather than alphabetical order", {
  dat <- make_attrition_data()
  dat$Z <- factor(dat$Z, levels = c("Treatment", "Control"))

  lower <- subset(impute_extreme_values(dat, "Y", "Z", range = c(1, 7)), scenario == "Lower bound")

  # "Control" is now the second level, so it is treated as the treated arm.
  expect_identical(lower$Y[3], 1)
  expect_identical(lower$Y[6], 7)
})

test_that("range is honoured and its order does not matter", {
  dat <- make_attrition_data()
  forwards <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))
  backwards <- impute_extreme_values(dat, "Y", "Z", range = c(7, 1))

  expect_identical(forwards$Y, backwards$Y)
})

# input checking ----

test_that("bad input is rejected", {
  dat <- make_attrition_data()

  expect_error(impute_extreme_values(dat, "nope", "Z", c(1, 7)), "must name a single column")
  expect_error(impute_extreme_values(dat, "Y", "nope", c(1, 7)), "must name a single column")
  expect_error(impute_extreme_values(dat, "id", "Z", c(1, 7)), "must be a numeric column")
  expect_error(impute_extreme_values(dat, "Y", "Z", c(1, 7, 9)), "length two")
  expect_error(impute_extreme_values(dat, "Y", "Z", c(1, 7), treated = "Nope"), "must be one of")
  expect_error(impute_extreme_values(as.list(dat), "Y", "Z", c(1, 7)), "must be a data frame")
})

test_that("an outcome outside the stated range is rejected", {
  dat <- make_attrition_data()
  expect_error(impute_extreme_values(dat, "Y", "Z", range = c(1, 5)), "fall outside")
})

test_that("assignment must be two-armed and complete", {
  dat <- make_attrition_data()

  three_arms <- dat
  three_arms$Z <- c(rep("Control", 3), rep("Treatment", 3), rep("Placebo", 2))
  expect_error(impute_extreme_values(three_arms, "Y", "Z", c(1, 7)), "exactly two distinct values")

  missing_arm <- dat
  missing_arm$Z[1] <- NA
  expect_error(impute_extreme_values(missing_arm, "Y", "Z", c(1, 7)), "must not have missing values")
})

test_that("colliding column names are refused rather than overwritten", {
  dat <- make_attrition_data()
  dat$scenario <- "something the user already had"

  expect_error(impute_extreme_values(dat, "Y", "Z", c(1, 7)), "already has a `scenario`")
})

# plotting ----

test_that("the result plots with a vayr position adjustment", {
  dat <- make_attrition_data()
  bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))

  expect_no_error(
    ggplot2::layer_data(
      ggplot2::ggplot(bounded, ggplot2::aes(Z, Y, colour = imputed, shape = imputed)) +
        ggplot2::geom_point(position = position_sunflower(density = 10)) +
        ggplot2::facet_wrap(~ scenario)
    )
  )
})
