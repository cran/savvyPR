library(testthat)
library(savvyPR)

set.seed(123)
n <- 50
p <- 5
x <- matrix(rnorm(n * p), n, p)
beta <- matrix(rnorm(p + 1), p + 1, 1)
y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

test_that("savvyPR works without feature selection and without intercept", {
  result <- savvyPR(x, y, val = 0.05, intercept = FALSE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("intercept" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("call" %in% names(result))
})

test_that("savvyPR works with feature selection", {
  result <- savvyPR(x, y, val = 0.05, use_feature_selection = TRUE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result))
  expect_true("lambda" %in% names(result))
})

test_that("savvyPR handles exclusion of columns correctly", {
  result <- savvyPR(x, y, val = 0.05, exclude = c(1, 2))
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_true("intercept" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("call" %in% names(result))
})

test_that("savvyPR handles exclusion of columns correctly and throws error for out of bounds exclusion indices", {
  result <- savvyPR(x, y, val = 0.05, exclude = c(1, 2))
  expect_true(inherits(result, "savvyPR"))
  expect_error(savvyPR(x, y, val = 0.05, exclude = c(1, p + 1)), "Exclusion indices are out of bounds.")
})

test_that("savvyPR gives an error for non-matching dimensions", {
  expect_error(savvyPR(x, y[-1], val = 0.05), "The number of rows in x must match the length of y.")
})

test_that("savvyPR gives an error for NA values", {
  x_with_na <- x
  x_with_na[1, 1] <- NA
  expect_error(savvyPR(x_with_na, y, val = 0.1), "x or y has missing values")
})

test_that("savvyPR works with intercept = TRUE and standardize = TRUE", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE, standardize = TRUE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result) || "fit" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("intercept" %in% names(result))
})

test_that("savvyPR works with intercept = TRUE and standardize = FALSE", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE, standardize = FALSE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result) || "fit" %in% names(result))
})

test_that("savvyPR works with intercept = FALSE and standardize = TRUE", {
  result <- savvyPR(x, y, val = 0.05, intercept = FALSE, standardize = TRUE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result) || "fit" %in% names(result))
})

test_that("savvyPR works with intercept = FALSE and standardize = FALSE", {
  result <- savvyPR(x, y, val = 0.05, intercept = FALSE, standardize = FALSE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result) || "fit" %in% names(result))
})

test_that("savvyPR handles different val correctly", {
  result <- savvyPR(x, y, val = 0.01)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result))
})

test_that("savvyPR works without val (defaulting to 0)", {
  result <- savvyPR(x, y, intercept = TRUE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("fit" %in% names(result))
  expect_true("lambda" %in% names(result))
})

test_that("savvyPR handles minimum and maximum val correctly for budget", {
  result_min <- savvyPR(x, y, method = "budget", val = 0, intercept = TRUE)
  expect_true(inherits(result_min, "savvyPR"))
  expect_true("fit" %in% names(result_min))

  result_max <- savvyPR(x, y, method = "budget", val = 1 / p, intercept = TRUE)
  expect_true(inherits(result_max, "savvyPR"))
  expect_true("orp_fit" %in% names(result_max))
})

test_that("savvyPR creates correct lambda grid", {
  result <- savvyPR(x, y, lambda_val = NULL, intercept = TRUE)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("fit" %in% names(result) || "orp_fit" %in% names(result))
  expect_true("lambda" %in% names(result))
})

test_that("savvyPR handles non-numeric val correctly", {
  expect_error(savvyPR(x, y, val = "invalid"), "val must be a numeric value.")
})

test_that("savvyPR handles negative val correctly", {
  expect_warning(result <- savvyPR(x, y, val = -0.05), "val cannot be negative; setting val to 0.")
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("fit" %in% names(result)) # 0 falls back to fit
})

test_that("savvyPR handles too large val correctly for budget method", {
  expect_warning(result <- savvyPR(x, y, method = "budget", val = 2 / p), "For 'budget' method, val exceeds the maximum allowed value")
  expect_true(inherits(result, "savvyPR"))
  expect_true("orp_fit" %in% names(result))
})

test_that("savvyPR works correctly for target method without warnings on large vals", {
  # Target method does not have a 1/p ceiling
  expect_silent(result <- savvyPR(x, y, method = "target", val = 2 / p))
  expect_true(inherits(result, "savvyPR"))
  expect_true("orp_fit" %in% names(result))
  expect_equal(result$method, "target")
})

test_that("savvyPR handles non-numeric lambda_val correctly", {
  expect_error(savvyPR(x, y, lambda_val = "invalid"), "lambda_val must be a single numeric value.")
})

test_that("savvyPR handles negative lambda_val correctly", {
  expect_warning(result <- savvyPR(x, y, lambda_val = -0.5), "lambda_val must be a non-negative numeric value; setting lambda_val to 0.")
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
})

test_that("savvyPR removes uniform first column if it's all ones", {
  x_with_intercept <- cbind(1, x)
  expect_warning(result <- savvyPR(x_with_intercept, y, val = 0.05),
                 "First column of x is an intercept \\(all ones\\). Removing this column.")
  expect_true(inherits(result, "savvyPR"))
})

test_that("savvyPR checks for columns with low unique values correctly", {
  x_low_unique <- x
  x_low_unique[, 2] <- rep(1:2, length.out = n)
  expect_error(savvyPR(x_low_unique, y, val = 0.05), "Found columns with less than 5% unique values, which are not suitable for parity regression.")
})

test_that("savvyPR handles rank deficiency correctly", {
  x_deficient <- matrix(rnorm(n * (n + 1)), n, n + 1)
  expect_error(savvyPR(x_deficient, y, val = 0.05), "The number of features in x must be less than the number of observations to avoid rank deficiency issues.")
})

test_that("savvyPR uses Ridge regression for rank-deficient matrices", {
  x_rank_deficient <- x
  x_rank_deficient[, 2] <- x_rank_deficient[, 1]  # Making columns 1 and 2 identical

  result <- savvyPR(x_rank_deficient, y, val = 0.05)
  expect_true(inherits(result, "savvyPR"))
  expect_true("coefficients" %in% names(result))
  expect_true("orp_fit" %in% names(result))
})

