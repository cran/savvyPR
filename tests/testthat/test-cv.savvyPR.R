library(testthat)
library(savvyPR)

test_that("getMeasureName returns correct measure names", {
  expect_equal(getMeasureName("mse"), "mse: Mean-Squared Error")
  expect_equal(getMeasureName("mae"), "mae: Mean Absolute Error")
  expect_equal(getMeasureName("rmse"), "rmse: Root Mean Squared Error")
  expect_equal(getMeasureName("rmsle"), "rmsle: Root Mean Squared Logarithmic Error")
  expect_equal(getMeasureName("mape"), "mape: Mean Absolute Percentage Error")
  expect_equal(getMeasureName("invalid"), "Unsupported measure type")
})

set.seed(123)
n <- 50
p <- 5
x <- matrix(rnorm(n * p), n, p)
beta <- matrix(rnorm(p + 1), p + 1, 1)
y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

test_that("calcLoss computes correct loss values", {
  mse_loss <- calcLoss(x, y, beta, "mse", intercept = TRUE)
  expect_true(is.numeric(mse_loss))

  default_loss <- calcLoss(x, y, beta, intercept = TRUE)
  expect_true(is.numeric(default_loss))
  expect_equal(default_loss, mse_loss)

  mae_loss <- calcLoss(x, y, beta, "mae", intercept = TRUE)
  expect_true(is.numeric(mae_loss))

  rmse_loss <- calcLoss(x, y, beta, "rmse", intercept = TRUE)
  expect_true(is.numeric(rmse_loss))

  y_no_zeros <- y + abs(min(y)) + 1
  mape_loss <- calcLoss(x, y_no_zeros, beta, "mape", intercept = TRUE)
  expect_true(is.numeric(mape_loss))

  expect_error(calcLoss(x, y, beta, "unsupported", intercept = TRUE),
               "Unsupported type of measure specified")
})

test_that("calcLoss handles zero values in y for MAPE", {
  y_zero <- y
  y_zero[1:10] <- 0

  expect_warning(mape_loss <- calcLoss(x, y_zero, beta, "mape", intercept = TRUE),
                 "MAPE computation: actual values contain zeros, which will lead to infinity.")
  expect_true(is.na(mape_loss))
})

test_that("cv.savvyPR works with model type PR1 (Budget)", {
  result <- cv.savvyPR(x, y, method = "budget", folds = 5, model_type = "PR1", measure_type = "mse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("optimal_val" %in% names(result))
  expect_true("fixed_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR works with model type PR1 (Target)", {
  result <- cv.savvyPR(x, y, method = "target", folds = 5, model_type = "PR1", measure_type = "mse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("optimal_val" %in% names(result))
  expect_equal(result$method, "target")
})

test_that("cv.savvyPR works with model type PR2", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR2", measure_type = "rmse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("optimal_val" %in% names(result))
  expect_true("fixed_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR works with model type PR3", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", measure_type = "mae")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("fixed_val" %in% names(result))
  expect_true("optimal_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR handles missing vals correctly", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("optimal_val" %in% names(result))
  expect_true("fixed_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR handles nval correctly", {
  result <- cv.savvyPR(x, y, nval = 50, folds = 5, model_type = "PR1", measure_type = "mse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("optimal_val" %in% names(result))
  expect_true("fixed_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR handles missing lambda_vals correctly", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", measure_type = "mse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("fixed_val" %in% names(result))
  expect_true("optimal_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR handles nlambda correctly", {
  result <- cv.savvyPR(x, y, nlambda = 50, folds = 5, model_type = "PR3", measure_type = "mse")
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("mean_error_cv" %in% names(result))
  expect_true("model_type" %in% names(result))
  expect_true("measure_type" %in% names(result))
  expect_true("PR_fit" %in% names(result))
  expect_true("fixed_val" %in% names(result))
  expect_true("optimal_lambda_val" %in% names(result))
  expect_true("optimal_index" %in% names(result))
})

test_that("cv.savvyPR gives an error for non-matching dimensions", {
  expect_error(cv.savvyPR(x, y[-1], folds = 5, model_type = "PR1"), "The number of rows in x must match the length of y.")
})

test_that("cv.savvyPR gives an error for NA values", {
  x_with_na <- x
  x_with_na[1, 1] <- NA
  expect_error(cv.savvyPR(x_with_na, y, folds = 5, model_type = "PR1"), "x or y has missing values; consider using appropriate methods to impute them before analysis.")
})

test_that("cv.savvyPR works with standardize off", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", standardize = FALSE)
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
})

test_that("cv.savvyPR works with intercept included", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = TRUE)
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
})

test_that("cv.savvyPR works without intercept included", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = FALSE)
  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
})

test_that("cv.savvyPR handles different measure types correctly", {
  result_mse <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse")
  result_mae <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mae")
  result_rmse <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "rmse")
  expect_true(is.list(result_mse))
  expect_true(is.list(result_mae))
  expect_true(is.list(result_rmse))
  expect_true("coefficients" %in% names(result_mse))
  expect_true("coefficients" %in% names(result_mae))
  expect_true("coefficients" %in% names(result_rmse))
})

test_that("cv.savvyPR works with different number of folds", {
  result_5folds <- cv.savvyPR(x, y, model_type = "PR1", measure_type = "mse", folds = 5)
  result_3folds <- cv.savvyPR(x, y, model_type = "PR1", measure_type = "mse", folds = 3)
  expect_true(is.list(result_5folds))
  expect_true(is.list(result_3folds))
  expect_true("coefficients" %in% names(result_5folds))
  expect_true("coefficients" %in% names(result_3folds))
})

test_that("cv.savvyPR handles minimum and maximum vals correctly (budget)", {
  result_min <- cv.savvyPR(x, y, vals = c(0, 0.00001), method = "budget", folds = 5, model_type = "PR1", measure_type = "mse")
  result_max <- cv.savvyPR(x, y, vals = c(1/p - 0.00001, 1/p), method = "budget", folds = 5, model_type = "PR1", measure_type = "mse")
  expect_true(is.list(result_min))
  expect_true(is.list(result_max))
  expect_true("coefficients" %in% names(result_min))
  expect_true("coefficients" %in% names(result_max))
})

test_that("cv.savvyPR handles minimum and maximum lambda_vals correctly", {
  result_min_lambda <- cv.savvyPR(x, y, lambda_vals = c(0, 0.00001), folds = 5, model_type = "PR2", measure_type = "mse")
  result_max_lambda <- cv.savvyPR(x, y, lambda_vals = c(10^2 - 0.1, 10^2), folds = 5, model_type = "PR2", measure_type = "mse")
  expect_true(is.list(result_min_lambda))
  expect_true(is.list(result_max_lambda))
  expect_true("coefficients" %in% names(result_min_lambda))
  expect_true("coefficients" %in% names(result_max_lambda))
})

test_that("cv.savvyPR works with specific lambda values for PR2 and PR3", {
  specific_lambda_vals <- c(0.001, 0.01, 0.1, 1)
  result_pr2 <- cv.savvyPR(x, y, folds = 5, model_type = "PR2", measure_type = "rmse", lambda_vals = specific_lambda_vals)
  result_pr3 <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", measure_type = "mae", lambda_vals = specific_lambda_vals)
  expect_true(is.list(result_pr2))
  expect_true(is.list(result_pr3))
  expect_true("coefficients" %in% names(result_pr2))
  expect_true("coefficients" %in% names(result_pr3))
  expect_true("fixed_lambda_val" %in% names(result_pr2))
  expect_true("optimal_lambda_val" %in% names(result_pr3))
})

test_that("cv.savvyPR handles vals with length less than 2", {
  expect_error(cv.savvyPR(x, y, vals = c(0), folds = 5, model_type = "PR1", measure_type = "mse"),
               "Need more than one value of tuning parameter \\(vals\\) for meaningful cross-validation.")
})

test_that("cv.savvyPR handles lambda_vals with length less than 2", {
  expect_error(cv.savvyPR(x, y, folds = 5, lambda_vals = c(0), model_type = "PR2", measure_type = "mse"),
               "Need more than one value of lambda_val for meaningful cross-validation.")
})

test_that("cv.savvyPR validates folds input correctly", {
  expect_error(cv.savvyPR(x, y, model_type = "PR1", measure_type = "mse", folds = 2),
               "Number of folds must be an integer greater than or equal to 3.")
})

test_that("cv.savvyPR handles invalid model_type correctly", {
  expect_error(cv.savvyPR(x, y, folds = 5, model_type = "INVALID", measure_type = "mse"),
               "'arg' should be one of \"PR3\", \"PR1\", \"PR2\"")
})

test_that("cv.savvyPR handles invalid measure_type correctly", {
  expect_error(cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "INVALID"),
               "'arg' should be one of \"mse\", \"mae\", \"rmse\", \"mape\"")
})

test_that("cv.savvyPR includes fold assignments if foldid is TRUE", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", foldid = TRUE)
  expect_true(is.list(result))
  expect_true("fold_assignments" %in% names(result))
  expect_length(result$fold_assignments, n)
})

