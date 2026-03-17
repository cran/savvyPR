library(testthat)
library(savvyPR)

set.seed(123)
n <- 50
p <- 5
x <- matrix(rnorm(n * p), n, p)
y <- rnorm(n)
model <- savvyPR(x, y, intercept = TRUE)
cv_model <- cv.savvyPR(x, y, model_type = "PR1", intercept = FALSE)

test_that("predict method works correctly for 'response' type", {
  newx <- x[1:5, ]
  predictions <- predict(model, newx = newx, type = "response")

  expect_type(predictions, "double")
  expect_length(predictions, 5)
  expect_error(predict(model, newx = x[, 1:3], type = "response"),
               "The number of variables in newx must match the number of coefficients in the model.")
})

test_that("predict method raises warning when newx is missing for 'response' type", {
  expect_error(predict(model, type = "response"),
               "You need to supply a matrix of new values for 'newx'.")
})

test_that("predict method works correctly for 'coefficients' type", {
  coefficients <- predict(model, type = "coefficients")
  expect_type(coefficients, "double")
  expect_length(coefficients, p + 1)
})

test_that("predict method works correctly for 'response' type", {
  newx <- x[1:5, ]
  predictions <- predict(cv_model, newx = newx, type = "response")

  expect_type(predictions, "double")
  expect_length(predictions, 5)
  expect_error(predict(cv_model, newx = x[, 1:3], type = "response"),
               "The number of variables in newx must match the number of coefficients in the model.")
})

test_that("predict method raises warning when newx is missing for 'response' type", {
  expect_error(predict(cv_model, type = "response"),
               "You need to supply a matrix of new values for 'newx'.")
})

test_that("predict method works correctly for 'coefficients' type", {
  coefficients <- predict(cv_model, type = "coefficients")
  expect_type(coefficients, "double")
  expect_length(coefficients, p)
})

test_that("coef method works correctly for savvyPR", {
  coefficients <- coef(model)
  expect_type(coefficients, "double")
  expect_length(coefficients, p + 1)

  predicted_coefficients <- predict(model, type = "coefficients")
  expect_equal(coefficients, predicted_coefficients)
})

test_that("coef method works correctly for cv.savvyPR", {
  coefficients <- coef(cv_model)
  expect_type(coefficients, "double")
  expect_length(coefficients, p)

  predicted_coefficients <- predict(cv_model, type = "coefficients")
  expect_equal(coefficients, predicted_coefficients)
})

test_that("print.savvyPR prints correct output with intercept", {
  x <- matrix(rnorm(100 * 10), 100, 10)
  y <- rnorm(100)
  model_with_intercept <- savvyPR(x, y, intercept = TRUE)
  output <- capture.output(print(model_with_intercept))

  expect_true(any(grepl("Call:", output)))
  expect_true(any(grepl("Method", output)))
  expect_true(any(grepl("Number of Non-Zero Coefficients", output)))
  expect_true(any(grepl("Intercept Included", output)))
  expect_true(any(grepl("Coefficients", output)))

  non_zero_coefs <- sum(model_with_intercept$coefficients != 0)
  expect_true(any(grepl(as.character(non_zero_coefs), output)))

  expect_true(any(grepl("Yes", output)))
  digits <- max(3, getOption("digits") - 3)

  if (!is.null(model_with_intercept$lambda)) {
    expect_true(any(grepl(as.character(signif(model_with_intercept$lambda, digits)), output)))
  }
})

test_that("print.savvyPR prints correct output without intercept", {
  x <- matrix(rnorm(100 * 10), 100, 10)
  y <- rnorm(100)
  model_without_intercept <- savvyPR(x, y, intercept = FALSE)
  output <- capture.output(print(model_without_intercept))

  expect_true(any(grepl("Call:", output)))
  expect_true(any(grepl("Method", output)))
  expect_true(any(grepl("Number of Non-Zero Coefficients", output)))
  expect_true(any(grepl("Intercept Included", output)))
  expect_true(any(grepl("Coefficients", output)))

  non_zero_coefs <- sum(model_without_intercept$coefficients != 0)
  expect_true(any(grepl(as.character(non_zero_coefs), output)))

  expect_true(any(grepl("No", output)))
  digits <- max(3, getOption("digits") - 3)

  if (!is.null(model_without_intercept$lambda)) {
    expect_true(any(grepl(as.character(signif(model_without_intercept$lambda, digits)), output)))
  }
})

test_that("print.cv.savvyPR prints correct output for PR1", {
  cv_model_PR1 <- cv.savvyPR(x, y, folds = 5, model_type = "PR1", intercept = TRUE)
  output_PR1 <- capture.output(print(cv_model_PR1))

  expect_true(any(grepl("Call:", output_PR1)))
  expect_true(any(grepl("Method", output_PR1)))
  expect_true(any(grepl("Number of Non-Zero Coefficients", output_PR1)))
  expect_true(any(grepl("Intercept Included", output_PR1)))
  expect_true(any(grepl("Optimal Val", output_PR1)))
  expect_true(any(grepl("Fixed Lambda Value", output_PR1)))

  non_zero_coefs_PR1 <- sum(cv_model_PR1$coefficients != 0)
  expect_true(any(grepl(as.character(non_zero_coefs_PR1), output_PR1)))

  expect_true(any(grepl("Yes", output_PR1)))
  digits <- max(3, getOption("digits") - 3)

  expect_true(any(grepl(as.character(signif(cv_model_PR1$optimal_val, digits)), output_PR1)))
  expect_true(any(grepl(as.character(signif(cv_model_PR1$fixed_lambda_val, digits)), output_PR1)))
})

test_that("print.cv.savvyPR prints correct output for PR2", {
  cv_model_PR2 <- cv.savvyPR(x, y, folds = 5, model_type = "PR2", intercept = FALSE)
  output_PR2 <- capture.output(print(cv_model_PR2))

  expect_true(any(grepl("Call:", output_PR2)))
  expect_true(any(grepl("Method", output_PR2)))
  expect_true(any(grepl("Number of Non-Zero Coefficients", output_PR2)))
  expect_true(any(grepl("Intercept Included", output_PR2)))
  expect_true(any(grepl("Optimal Val", output_PR2)))
  expect_true(any(grepl("Fixed Lambda Value", output_PR2)))

  non_zero_coefs_PR2 <- sum(cv_model_PR2$coefficients != 0)
  expect_true(any(grepl(as.character(non_zero_coefs_PR2), output_PR2)))

  expect_true(any(grepl("No", output_PR2)))
  digits <- max(3, getOption("digits") - 3)

  expect_true(any(grepl(as.character(signif(cv_model_PR2$optimal_val, digits)), output_PR2)))
  expect_true(any(grepl(as.character(signif(cv_model_PR2$fixed_lambda_val, digits)), output_PR2)))
})

test_that("print.cv.savvyPR prints correct output for PR3", {
  cv_model_PR3 <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", intercept = FALSE)
  output_PR3 <- capture.output(print(cv_model_PR3))

  expect_true(any(grepl("Call:", output_PR3)))
  expect_true(any(grepl("Method", output_PR3)))
  expect_true(any(grepl("Number of Non-Zero Coefficients", output_PR3)))
  expect_true(any(grepl("Intercept Included", output_PR3)))
  expect_true(any(grepl("Fixed Val", output_PR3)))
  expect_true(any(grepl("Optimal Lambda Value", output_PR3)))

  non_zero_coefs_PR3 <- sum(cv_model_PR3$coefficients != 0)
  expect_true(any(grepl(as.character(non_zero_coefs_PR3), output_PR3)))

  expect_true(any(grepl("No", output_PR3)))
  digits <- max(3, getOption("digits") - 3)

  expect_true(any(grepl(as.character(signif(cv_model_PR3$fixed_val, digits)), output_PR3)))
  expect_true(any(grepl(as.character(signif(cv_model_PR3$optimal_lambda_val, digits)), output_PR3)))
})

test_that("summary.savvyPR generates correct summary output with intercept", {
  set.seed(123)
  n <- 100
  p <- 10
  x <- matrix(rnorm(n * p), n, p)
  beta <- matrix(rnorm(p + 1), p + 1, 1)
  y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

  model <- savvyPR(x, y, intercept = TRUE)
  output <- capture.output(summary(model))

  expect_true(any(grepl("Summary of Parity Model", output)))
  expect_true(any(grepl("Parameterization Method:", output)))
  expect_true(any(grepl("Intercept:", output)))
  expect_true(any(grepl("Residual standard error", output)))
  expect_true(any(grepl("Multiple R-squared", output)))
  expect_true(any(grepl("F-statistic", output)))
  expect_true(any(grepl("AIC", output)))
  expect_true(any(grepl("BIC", output)))
  expect_true(any(grepl("Deviance", output)))

  r_squared <- summaryStats(x, y, model$coefficients, model$intercept)$r_squared
  expect_true(any(grepl(as.character(round(r_squared, 4)), output)))

  f_stat <- summaryStats(x, y, model$coefficients, model$intercept)$f_statistic
  expect_true(any(grepl(as.character(round(f_stat, 4)), output)))

  expect_true(any(grepl(as.character(length(model$coefficients)), output)))
})

test_that("summary.savvyPR generates correct summary output without intercept", {
  set.seed(123)
  n <- 100
  p <- 10
  x <- matrix(rnorm(n * p), n, p)
  beta <- matrix(rnorm(p + 1), p + 1, 1)
  y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

  model <- savvyPR(x, y, intercept = FALSE)
  output <- capture.output(summary(model))

  expect_true(any(grepl("Summary of Parity Model", output)))
  expect_true(any(grepl("Parameterization Method:", output)))
  expect_true(any(grepl("Intercept:", output)))
  expect_true(any(grepl("Residual standard error", output)))
  expect_true(any(grepl("Multiple R-squared", output)))
  expect_true(any(grepl("F-statistic", output)))
  expect_true(any(grepl("AIC", output)))
  expect_true(any(grepl("BIC", output)))
  expect_true(any(grepl("Deviance", output)))

  r_squared <- summaryStats(x, y, model$coefficients, model$intercept)$r_squared
  expect_true(any(grepl(as.character(round(r_squared, 4)), output)))

  f_stat <- summaryStats(x, y, model$coefficients, model$intercept)$f_statistic
  expect_true(any(grepl(as.character(round(f_stat, 4)), output)))

  expect_true(any(grepl(as.character(length(model$coefficients)), output)))
})

test_that("summary.cv.savvyPR works for PR1 model", {
  set.seed(123)
  n <- 100
  p <- 10
  x <- matrix(rnorm(n * p), n, p)
  beta <- matrix(rnorm(p + 1), p + 1, 1)
  y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

  cv_model_PR1 <- cv.savvyPR(x, y, folds = 3, model_type = "PR1", intercept = TRUE)
  output_PR1 <- capture.output(summary(cv_model_PR1))

  expect_true(any(grepl("Summary of Cross-Validated Parity Model", output_PR1)))
  expect_true(any(grepl("Parameterization Method:", output_PR1)))
  expect_true(any(grepl("Intercept:", output_PR1)))
  expect_true(any(grepl("Residual standard error", output_PR1)))
  expect_true(any(grepl("Multiple R-squared", output_PR1)))
  expect_true(any(grepl("F-statistic", output_PR1)))
  expect_true(any(grepl("AIC", output_PR1)))
  expect_true(any(grepl("BIC", output_PR1)))
  expect_true(any(grepl("Deviance", output_PR1)))

  r_squared <- summaryStats(x, y, cv_model_PR1$coefficients, cv_model_PR1$PR_fit$intercept)$r_squared
  expect_true(any(grepl(as.character(round(r_squared, 4)), output_PR1)))

  f_stat <- summaryStats(x, y, cv_model_PR1$coefficients, cv_model_PR1$PR_fit$intercept)$f_statistic
  expect_true(any(grepl(as.character(round(f_stat, 4)), output_PR1)))

  expect_true(any(grepl(as.character(length(cv_model_PR1$coefficients)), output_PR1)))

  cv_error <- min(cv_model_PR1$mean_error_cv)
  expect_true(any(grepl(as.character(round(cv_error, 4)), output_PR1)))

  expect_true(any(grepl("Cross-Validation Summary", output_PR1)))
  expect_true(any(grepl("Optimal Val", output_PR1)))
  expect_true(any(grepl("Fixed Lambda", output_PR1)))
  expect_false(any(grepl("Optimal Lambda", output_PR1)))

  expect_true(any(grepl(as.character(signif(cv_model_PR1$optimal_val, 4)), output_PR1)))
  expect_true(any(grepl(as.character(signif(cv_model_PR1$fixed_lambda_val, 4)), output_PR1)))
})

test_that("summary.cv.savvyPR works for PR2 model", {
  set.seed(123)
  n <- 100
  p <- 10
  x <- matrix(rnorm(n * p), n, p)
  beta <- matrix(rnorm(p + 1), p + 1, 1)
  y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

  cv_model_PR2 <- cv.savvyPR(x, y, model_type = "PR2", intercept = FALSE)
  output_PR2 <- capture.output(summary(cv_model_PR2))

  expect_true(any(grepl("Summary of Cross-Validated Parity Model", output_PR2)))
  expect_true(any(grepl("Parameterization Method:", output_PR2)))
  expect_true(any(grepl("Intercept:", output_PR2)))
  expect_true(any(grepl("Residual standard error", output_PR2)))
  expect_true(any(grepl("Multiple R-squared", output_PR2)))
  expect_true(any(grepl("F-statistic", output_PR2)))
  expect_true(any(grepl("AIC", output_PR2)))
  expect_true(any(grepl("BIC", output_PR2)))
  expect_true(any(grepl("Deviance", output_PR2)))

  r_squared <- summaryStats(x, y, cv_model_PR2$coefficients, cv_model_PR2$PR_fit$intercept)$r_squared
  expect_true(any(grepl(as.character(round(r_squared, 4)), output_PR2)))

  f_stat <- summaryStats(x, y, cv_model_PR2$coefficients, cv_model_PR2$PR_fit$intercept)$f_statistic
  expect_true(any(grepl(as.character(round(f_stat, 4)), output_PR2)))

  expect_true(any(grepl(as.character(length(cv_model_PR2$coefficients)), output_PR2)))

  cv_error <- min(cv_model_PR2$mean_error_cv)
  expect_true(any(grepl(as.character(round(cv_error, 4)), output_PR2)))

  expect_true(any(grepl("Cross-Validation Summary", output_PR2)))
  expect_true(any(grepl("Optimal Val", output_PR2)))
  expect_true(any(grepl("Fixed Lambda", output_PR2)))
  expect_false(any(grepl("Optimal Lambda", output_PR2)))

  expect_true(any(grepl(as.character(signif(cv_model_PR2$optimal_val, 4)), output_PR2)))
  expect_true(any(grepl(as.character(signif(cv_model_PR2$fixed_lambda_val, 4)), output_PR2)))
})


test_that("summary.cv.savvyPR works for PR3 model", {
  set.seed(123)
  n <- 100
  p <- 10
  x <- matrix(rnorm(n * p), n, p)
  beta <- matrix(rnorm(p + 1), p + 1, 1)
  y <- cbind(1, x) %*% beta + rnorm(n, sd = 0.5)

  cv_model_PR3 <- cv.savvyPR(x, y, folds = 5, model_type = "PR3", intercept = FALSE)
  output_PR3 <- capture.output(summary(cv_model_PR3))

  expect_true(any(grepl("Summary of Cross-Validated Parity Model", output_PR3)))
  expect_true(any(grepl("Parameterization Method:", output_PR3)))
  expect_true(any(grepl("Intercept:", output_PR3)))
  expect_true(any(grepl("Call:", output_PR3)))
  expect_true(any(grepl("Residual standard error", output_PR3)))
  expect_true(any(grepl("Multiple R-squared", output_PR3)))
  expect_true(any(grepl("F-statistic", output_PR3)))
  expect_true(any(grepl("AIC", output_PR3)))
  expect_true(any(grepl("BIC", output_PR3)))
  expect_true(any(grepl("Deviance", output_PR3)))
  expect_true(any(grepl("Coefficients:", output_PR3)))
  expect_true(any(grepl("Estimate", output_PR3)))
  expect_true(any(grepl("Std. Error", output_PR3)))
  expect_true(any(grepl("t value", output_PR3)))
  expect_true(any(grepl("Pr(>|t|)", output_PR3)))
  expect_true(any(grepl("Signif.", output_PR3)))
  expect_true(any(grepl("Adjusted R-squared:", output_PR3)))
  expect_true(any(grepl("p-value:", output_PR3)))

  expect_true(any(grepl("Signif\\. codes:  0 '\\*\\*\\*' 0\\.001 '\\*\\*' 0\\.01 '\\*' 0\\.05 '\\.' 0\\.1 ' ' 1", output_PR3)))
  expect_true(any(grepl("Residual standard error:", output_PR3)))
  expect_true(any(grepl("on [0-9]+ degrees of freedom", output_PR3)))

  r_squared <- summaryStats(x, y, cv_model_PR3$coefficients, cv_model_PR3$PR_fit$intercept)$r_squared
  expect_true(any(grepl(as.character(round(r_squared, 4)), output_PR3)))

  f_stat <- summaryStats(x, y, cv_model_PR3$coefficients, cv_model_PR3$PR_fit$intercept)$f_statistic
  expect_true(any(grepl(as.character(round(f_stat, 4)), output_PR3)))
  expect_true(any(grepl(as.character(length(cv_model_PR3$coefficients)), output_PR3)))

  cv_error <- min(cv_model_PR3$mean_error_cv)
  expect_true(any(grepl(as.character(round(cv_error, 4)), output_PR3)))

  expect_true(any(grepl("Cross-Validation Summary", output_PR3)))
  expect_true(any(grepl("Fixed Val", output_PR3)))
  expect_true(any(grepl("Optimal Lambda", output_PR3)))

  expect_true(any(grepl(as.character(signif(cv_model_PR3$fixed_val, 4)), output_PR3)))
  expect_true(any(grepl(as.character(signif(cv_model_PR3$optimal_lambda_val, 4)), output_PR3)))
})

