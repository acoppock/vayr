with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
