library(testthat)
library(savvyPR)
library(ggplot2)

set.seed(123)
n <- 50
p <- 5
base_var <- rnorm(n)
x <- matrix(rnorm(n * p, sd = 0.1), n, p) + base_var
beta <- matrix(rnorm(p + 1), p + 1, 1)
y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

expect_ggplot <- function(object) {
  expect_true(inherits(object, "ggplot"))
}

expect_gtable <- function(object) {
  expect_true(inherits(object, "gtable"))
}

test_that("plot for parity_model works with estimated coefficients with intercept", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)
  plot_output <- plot(result, plot_type = "estimated_coefficients", label = FALSE)
  expect_ggplot(plot_output)
})

test_that("plot for parity_model works with estimated coefficients without intercept", {
  result <- savvyPR(x, y, val = 0.05, intercept = FALSE)
  plot_output <- plot(result, plot_type = "estimated_coefficients", label = TRUE)
  expect_ggplot(plot_output)
})

test_that("plot_coefficients cannot work without coefficients", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)
  result$coefficients <- NULL
  expect_warning(plot_output <- plot(result, plot_type = "estimated_coefficients", label = TRUE),
                 "Coefficients are missing in the result.")
})

test_that("plot for parity_model works with risk contributions", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)
  plot_output <- plot(result, plot_type = "risk_contributions", label = TRUE)
  expect_gtable(plot_output)
})

test_that("plot for parity_model gives warning when risk contributions are unavailable", {
  result <- savvyPR(x, y, val = 0, intercept = TRUE)

  expect_warning(
    plot(result, plot_type = "risk_contributions", label = TRUE),
    "No 'orp_fit' found in the model. This usually occurs when the tuning parameter is 0. Cannot plot risk contributions.",
    fixed = TRUE
  )
})

test_that("plot for parity_model gives warning when optimization variables or 'relativeRiskContrib' are unavailable", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)
  result$orp_fit$weights <- rep(NA, length(result$orp_fit$weights))
  expect_warning(plot(result, plot_type = "risk_contributions", label = TRUE),
                 "Cannot generate risk contributions plot: optimization variables or 'relativeRiskContrib' contains NA values.")
})

test_that("plot for parity_cv_model works with estimated coefficients", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "estimated_coefficients", label = TRUE)
  expect_ggplot(plot_output)
})

test_that("plot for parity_model gives warning when risk contributions are unavailable", {
  result <- savvyPR(x, y, val = 0, intercept = TRUE)

  expect_warning(
    plot(result, plot_type = "risk_contributions", label = TRUE),
    "No 'orp_fit' found in the model. This usually occurs when the tuning parameter is 0. Cannot plot risk contributions.",
    fixed = TRUE
  )
})

test_that("plot for parity_cv_model works with risk contributions", {
  result <- cv.savvyPR(x, as.numeric(y),
                       method = "budget",
                       vals = seq(0.01, 0.1, length.out = 10),
                       folds = 5,
                       model_type = "PR1",
                       intercept = TRUE)
  expect_warning(
    plot_output <- plot(result, plot_type = "risk_contributions", label = TRUE),
    NA
  )
  expect_gtable(plot_output)
})

test_that("cv_errors handles missing mean_error_cv", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$mean_error_cv <- NULL
  expect_error(plot(result, plot_type = "cv_errors", label = FALSE),
               "The input cv_results must contain mean cross-validation errors and optimal indices.")
})

test_that("cv_errors handles missing optimal_index", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$optimal_index <- NULL
  expect_error(plot(result, plot_type = "cv_errors", label = TRUE),
               "The input cv_results must contain mean cross-validation errors and optimal indices.")
})

test_that("cv_errors handles missing vals for PR1 and PR2", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$vals <- NULL
  expect_error(plot(result, plot_type = "cv_errors", label = FALSE),
               "vals must be provided in cv_results for model_type 'PR1' or 'PR2'.")
})

test_that("cv_errors handles missing lambda_vals for PR3", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3")
  result$lambda_vals <- NULL
  expect_error(plot(result, plot_type = "cv_errors", label = TRUE),
               "lambda_vals must be provided in cv_results for model_type 'PR3'.")
})

measure_types <- c("mse", "mae", "rmse", "mape")

for (measure_type in measure_types) {
  test_that(paste("plot_cv_errors works for PR1 or PR2 when measure_type =", measure_type), {
    result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = measure_type)
    plot <- plot(result, plot_type = "cv_errors", label = TRUE)
    expect_ggplot(plot)
  })
}

for (measure_type in measure_types) {
  test_that(paste("plot_cv_errors works for PR3 when measure_type =", measure_type), {
    result <- cv.savvyPR(x, y, nlambda = 50,  folds = 5, model_type = "PR3", measure_type = measure_type)
    plot <- plot(result, plot_type = "cv_errors", label = TRUE)
    expect_ggplot(plot)
  })
}

test_that("plot for parity_cv_model works with cv_coefficients and PR1 with val", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "val", max_vars_per_plot = 5, label = TRUE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR1 with norm", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = FALSE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "norm", max_vars_per_plot = 5, label = TRUE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR1 with dev", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "dev", max_vars_per_plot = 5, label = FALSE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR2 with val", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR2", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "val", max_vars_per_plot = 5, label = FALSE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR2 with norm", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR2", measure_type = "mse", intercept = FALSE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "norm", max_vars_per_plot = 5, label = TRUE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR2 with dev", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR2", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "dev", max_vars_per_plot = 5, label = FALSE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR3 with lambda", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "lambda", max_vars_per_plot = 5, label = TRUE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR3 with norm", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", measure_type = "mse", intercept = FALSE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "norm", max_vars_per_plot = 5, label = TRUE)

  expect_no_error(plot_output)
})

test_that("plot for parity_cv_model works with cv_coefficients and PR3 with dev", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_coefficients", xvar = "dev", max_vars_per_plot = 5, label = TRUE)

  expect_no_error(plot_output)
})

test_that("cv_coefficients fails for invalid model_type", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$model_type <- "invalid"
  expect_error(plot(result, plot_type = "cv_coefficients", xvar = "norm"),
               "Invalid model_type or xvar combination.")
})

test_that("cv_coefficients fails for missing coefficient paths", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$coefficients_cv <- NULL
  expect_error(plot(result, plot_type = "cv_coefficients", xvar = "norm"),
               "The input result_list must contain coefficient paths.")
})

test_that("cv_coefficients cannot exceed 10 variables per plot", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  expect_warning(plot(result, plot_type = "cv_coefficients", xvar = "norm", max_vars_per_plot = 11),
                 "max_vars_per_plot cannot exceed 10. Setting max_vars_per_plot to 10.")
})

test_that("plot_cv_coefficients handles invalid combination PR1 with lambda", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  expect_error(plot(result, plot_type = "cv_coefficients", xvar = "lambda", max_vars_per_plot = 10),
               "Invalid combination: PR1 and PR2 models cannot use 'lambda' as xvar.")
})

test_that("plot_cv_coefficients handles invalid combination PR3 with val", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR3")
  expect_error(plot(result, plot_type = "cv_coefficients", xvar = "val", max_vars_per_plot = 10),
               "Invalid combination: PR3 model cannot use 'val' as xvar.")
})

test_that("plot for parity_cv_model works with cv_errors", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = TRUE)
  plot_output <- plot(result, plot_type = "cv_errors", label = TRUE)
  expect_ggplot(plot_output)
})

test_that("plot for parity_model handles invalid plot_type", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)

  expect_error(
    plot(result, plot_type = "invalid_type"),
    "'arg' should be one of \"estimated_coefficients\", \"risk_contributions\""
  )
})

test_that("plot for parity_cv_model handles invalid plot_type", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", measure_type = "mse", intercept = TRUE)

  # Expect an error when an invalid plot type is provided
  expect_error(
    plot(result, plot_type = "invalid_type"),
    "'arg' should be one of \"estimated_coefficients\", \"risk_contributions\", \"cv_coefficients\", \"cv_errors\""
  )
})

test_that("plot for parity_model handles missing coefficients", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)
  result$coefficients <- NULL
  expect_warning(plot(result, plot_type = "estimated_coefficients", label = TRUE),
                 "Coefficients are missing in the result.")
})

test_that("plot for parity_model handles missing risk parity fit", {
  result <- savvyPR(x, y, val = 0.05, intercept = TRUE)
  result$orp_fit <- NULL

  expect_warning(
    plot(result, plot_type = "risk_contributions", label = TRUE),
    "No 'orp_fit' found in the model. This usually occurs when the tuning parameter is 0. Cannot plot risk contributions.",
    fixed = TRUE
  )
})

test_that("plot for parity_cv_model handles missing mean_error_cv", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$mean_error_cv <- NULL
  expect_error(plot(result, plot_type = "cv_errors"),
               "The input cv_results must contain mean cross-validation errors and optimal indices.")
})

test_that("plot for parity_cv_model handles missing coefficient paths", {
  result <- cv.savvyPR(x, y, folds = 5, model_type = "PR1")
  result$coefficients_cv <- NULL
  expect_error(plot(result, plot_type = "cv_coefficients"),
               "The input result_list must contain coefficient paths.")
})

