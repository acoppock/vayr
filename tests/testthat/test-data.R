datasets <- c(
  "patriot_act", "two_arm_trial", "blocked_experiment", "clustered_experiment",
  "covariate_adjustment", "continuous_interaction", "noncompliance_experiment",
  "attrition_experiment"
)

test_that("every shipped dataset is a plain tibble", {
  for (name in datasets) {
    dataset <- get(name, envir = asNamespace("vayr"))
    expect_identical(class(dataset), c("tbl_df", "tbl", "data.frame"), info = name)
    expect_null(attr(dataset, "spec"), info = name)
  }
})

# Rd_db reads the installed help, which is how R CMD check runs the tests. Under
# load_all() there is no installed help and it errors rather than coming back
# empty, so these two checks skip outside a real check.
installed_rd <- function() {
  tryCatch(tools::Rd_db("vayr"), error = function(e) list())
}

test_that("the documented columns are the actual columns", {
  db <- installed_rd()
  skip_if(length(db) == 0, "package not installed")

  for (name in datasets) {
    rd <- db[[paste0(name, ".Rd")]]
    skip_if(is.null(rd), paste("no Rd for", name))

    text <- paste(as.character(rd), collapse = "")
    items <- regmatches(text, gregexpr("\\\\item\\{[^}]*\\}", text))[[1]]
    documented <- trimws(unlist(strsplit(gsub("\\\\item\\{|\\}", "", items), ",")))

    actual <- names(get(name, envir = asNamespace("vayr")))
    expect_setequal(documented, actual)
  }
})

test_that("the documented dimensions are the actual dimensions", {
  db <- installed_rd()
  skip_if(length(db) == 0, "package not installed")

  for (name in datasets) {
    rd <- db[[paste0(name, ".Rd")]]
    skip_if(is.null(rd), paste("no Rd for", name))

    text <- paste(as.character(rd), collapse = "")
    stated <- regmatches(text, regexpr("[0-9]+ rows and [0-9]+ columns", text))
    skip_if(length(stated) == 0, paste("no stated dimensions for", name))

    numbers <- as.integer(regmatches(stated, gregexpr("[0-9]+", stated))[[1]])
    dataset <- get(name, envir = asNamespace("vayr"))
    expect_identical(numbers, c(nrow(dataset), ncol(dataset)), info = name)
  }
})

test_that("the chapter datasets carry the design information the examples need", {
  expect_true(all(c("Z", "Z_cond_prob", "Y") %in% names(vayr::two_arm_trial)))

  # Blocking induces a different probability of treatment in each block. Within
  # a block the treated and control probabilities can coincide, as they do in
  # neighborhood 1 where half the residents are treated.
  treated <- vayr::blocked_experiment[vayr::blocked_experiment$Z == 1, ]
  by_block <- tapply(treated$Z_cond_prob, treated$neighborhood, unique)
  expect_length(unique(unlist(by_block)), 2)

  # Assignment is constant within cluster.
  by_cluster <- tapply(
    vayr::clustered_experiment$Z,
    vayr::clustered_experiment$class,
    function(z) length(unique(z))
  )
  expect_true(all(by_cluster == 1))

  # Attrition is the whole point of this one, and R flags it.
  expect_gt(sum(is.na(vayr::attrition_experiment$Y)), 0)
  expect_identical(
    is.na(vayr::attrition_experiment$Y),
    vayr::attrition_experiment$R == 0
  )

  # Noncompliance runs in both directions.
  crosstab <- table(vayr::noncompliance_experiment$Z, vayr::noncompliance_experiment$D)
  expect_gt(crosstab["Control", "1"], 0)
  expect_gt(crosstab["Treatment", "0"], 0)
})
