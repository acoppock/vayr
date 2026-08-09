#' Impute the extreme value bounds for an outcome with attrition
#'
#' Attrition is missingness in the outcome variable. Dropping the units whose
#' outcome is missing conditions the analysis on a post-treatment variable and
#' can induce bias. Extreme value bounds (Manski, 1999) sidestep the problem by
#' imputing the logical best case and worst case instead: the upper bound
#' imputes the largest possible outcome for missing treated units and the
#' smallest possible outcome for missing control units, and the lower bound
#' does the reverse.
#'
#' This function does the imputation only, so that the two scenarios can be
#' plotted alongside the observed data. It estimates nothing. For the bounds
#' themselves and their uncertainty, see `estimator_ev()` in the 'attrition'
#' package, available at <https://github.com/acoppock/attrition>.
#'
#' `range` has no default. The logical minimum and maximum of the outcome are
#' the substantive input the bounds rest on, so they are stated at the call
#' site rather than guessed from the observed data. Guessing would silently
#' narrow the bounds whenever no respondent used an endpoint of the scale.
#'
#' @family Functions
#'
#' @param data A data frame containing the outcome and the assignment.
#' @param outcome The name of the outcome column, as a string. The column must
#' be numeric, and missing values are what gets imputed.
#' @param assignment The name of the random assignment column, as a string. It
#' must have exactly two distinct values and no missing values.
#' @param range The logical minimum and maximum of the outcome, as a numeric
#' vector of length two. For a seven-point Likert item, `c(1, 7)`.
#' @param treated The value of `assignment` that denotes the treated group.
#' Defaults to the second value in sort order, which is `"Treatment"` for
#' `c("Control", "Treatment")` and `1` for `c(0, 1)`. Set it explicitly when
#' sort order does not pick out the treated group.
#'
#' @returns A data frame with twice as many rows as `data`, holding both
#' scenarios stacked. The `outcome` column carries the imputed values, and two
#' columns are added: `scenario`, a factor with levels `"Lower bound"` and
#' `"Upper bound"`, suitable for faceting; and `imputed`, a factor with levels
#' `"Outcome available"` and `"Outcome imputed"`, suitable for mapping to both
#' colour and shape so the distinction survives in grayscale.
#'
#' @references Manski, C. F. (1999). *Identification Problems in the Social
#' Sciences*. Harvard University Press.
#'
#' @export
#'
#' @examples
#'   library(ggplot2)
#'
#'   dat <- data.frame(
#'     Z = rep(c("Control", "Treatment"), each = 100),
#'     Y = c(sample(1:7, 100, replace = TRUE), sample(1:7, 100, replace = TRUE))
#'   )
#'   dat$Y[sample(200, 30)] <- NA
#'
#'   bounded <- impute_extreme_values(dat, "Y", "Z", range = c(1, 7))
#'
#'   ggplot(bounded, aes(Z, Y, colour = imputed, shape = imputed)) +
#'     geom_point(position = position_sunflower(density = 30, aspect_ratio = 1 / 4),
#'                alpha = 0.5) +
#'     facet_wrap(~ scenario) +
#'     scale_y_continuous(breaks = 1:7)
#'
impute_extreme_values <- function(data, outcome, assignment, range, treated = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(outcome) || length(outcome) != 1 || !outcome %in% names(data)) {
    stop("`outcome` must name a single column of `data`.")
  }
  if (!is.character(assignment) || length(assignment) != 1 || !assignment %in% names(data)) {
    stop("`assignment` must name a single column of `data`.")
  }
  if (any(c("scenario", "imputed") %in% names(data))) {
    stop("`data` already has a `scenario` or `imputed` column, which this function would overwrite.")
  }
  if (!is.numeric(data[[outcome]])) {
    stop("`outcome` must be a numeric column.")
  }
  if (!is.numeric(range) || length(range) != 2 || anyNA(range)) {
    stop("`range` must be a numeric vector of length two.")
  }

  if (anyNA(data[[assignment]])) {
    stop("`assignment` must not have missing values; a unit of unknown assignment cannot be bounded.")
  }

  arms <- sort(unique(data[[assignment]]))
  if (length(arms) != 2) {
    stop("`assignment` must have exactly two distinct values.")
  }

  minimum <- min(range)
  maximum <- max(range)

  observed <- data[[outcome]][!is.na(data[[outcome]])]
  if (length(observed) > 0 && (min(observed) < minimum || max(observed) > maximum)) {
    stop("Observed values of `outcome` fall outside `range`.")
  }

  treated <- treated %||% arms[2]
  if (!treated %in% arms) {
    stop("`treated` must be one of the two values of `assignment`.")
  }

  is_treated <- data[[assignment]] == treated
  is_missing <- is.na(data[[outcome]])

  lower <- data
  lower[[outcome]][is_missing & is_treated] <- minimum
  lower[[outcome]][is_missing & !is_treated] <- maximum
  lower$scenario <- "Lower bound"

  upper <- data
  upper[[outcome]][is_missing & is_treated] <- maximum
  upper[[outcome]][is_missing & !is_treated] <- minimum
  upper$scenario <- "Upper bound"

  bounded <- rbind(lower, upper)
  bounded$scenario <- factor(bounded$scenario, levels = c("Lower bound", "Upper bound"))
  bounded$imputed <- factor(
    ifelse(rep(is_missing, 2), "Outcome imputed", "Outcome available"),
    levels = c("Outcome available", "Outcome imputed")
  )

  rownames(bounded) <- NULL
  bounded
}
