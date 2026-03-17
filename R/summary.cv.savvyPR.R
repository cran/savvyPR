#' Summary of a Fitted Cross-Validated Parity Regression Model with Statistics
#'
#' @name summary.cv.savvyPR
#' @title Summary of Cross-Validated Parity Regression Model
#' @description Prints a comprehensive statistical summary of a fitted cross-validated
#' parity regression model object. It displays the model's configuration, statistical
#' summaries of the estimated coefficients, model fit statistics, and cross-validation results.
#'
#' @param object A fitted model object of class \code{cv.savvyPR} returned by \code{\link{cv.savvyPR}}.
#' @param ... Additional arguments passed to the generic \code{summary} function (currently unused).
#'
#' @details
#' This function is an S3 method for the generic \code{summary} function. It formats and prints
#' a detailed statistical overview of the optimal cross-validated model. The output includes:
#' \itemize{
#'   \item The parameterization method used (\code{"budget"} or \code{"target"}).
#'   \item The matched call that produced the model.
#'   \item Residual quantiles.
#'   \item A table of estimated optimal coefficients with their corresponding standard errors, t-values, p-values, confidence intervals, and significance codes.
#'   \item Overall model fit statistics, including Residual Standard Error, Multiple and Adjusted R-squared, F-statistic, AIC, BIC, and Deviance.
#'   \item A Cross-Validation Summary displaying the minimum mean cross-validation error and the optimal tuning values (\code{val} and/or \code{lambda}).
#' }
#'
#' @return Invisibly returns \code{NULL}. This function is primarily called for its side
#' effect of printing the summary to the console.
#'
#' @examples
#' \donttest{
#' # Simulate some data
#' set.seed(123)
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- matrix(rnorm(p), p, 1)
#' y <- x %*% beta + rnorm(n, sd = 0.5)
#'
#' # Example 1: Fit and summarize a budget-based cross-validated model
#' cv_fit_budget <- cv.savvyPR(x, y, method = "budget", model_type = "PR3")
#' summary(cv_fit_budget)
#'
#' # Example 2: Fit and summarize a target-based cross-validated model
#' cv_fit_target <- cv.savvyPR(x, y, method = "target", model_type = "PR1")
#' summary(cv_fit_target)
#' }
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso \code{\link{cv.savvyPR}}
#' @importFrom stats pf predict pt qnorm quantile
#' @method summary cv.savvyPR
#' @export
summary.cv.savvyPR <- function(object, ...) {
  cat("Summary of Cross-Validated Parity Model\n")
  cat("===================================================================\n\n")

  method_used <- if (is.null(object$method)) "budget" else object$method
  cat("Parameterization Method:", method_used, "\n")
  cat("Intercept:", if (object$PR_fit$intercept) "Included\n" else "Not included\n")

  cat("\nCall:\n")
  print(object$call)

  x <- as.matrix(object$PR_fit$model[-1])
  y <- object$PR_fit$model$y

  stats <- summaryStats(x, y, object$coefficients, object$PR_fit$intercept)

  cat("\nResiduals:\n")
  print(stats$residual_quants)

  signif_codes <- add_significance_codes(stats$p_values)
  formatted_p_values <- format_p_values(stats$p_values)

  coef_names <- if (object$PR_fit$intercept) c("(Intercept)", colnames(x)) else colnames(x)

  if (length(coef_names) != length(object$coefficients)) {
    coef_names <- if (object$PR_fit$intercept) c("(Intercept)", paste0("V", 1:(length(object$coefficients)-1))) else paste0("V", 1:length(object$coefficients))
  }

  coef_table <- cbind(
    Estimate = round(object$coefficients, 4),
    `Std. Error` = round(stats$std_err, 4),
    `t value` = round(stats$t_values, 4),
    `Pr(>|t|)` = formatted_p_values,
    `2.5 %` = round(stats$confint_lower, 4),
    `97.5 %` = round(stats$confint_upper, 4),
    `Signif.` = signif_codes
  )

  rownames(coef_table) <- coef_names

  cat("\nCoefficients:\n")
  print(coef_table, quote = FALSE)

  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("Residual standard error:", round(stats$residual_se, 4), "on", stats$df_residual, "degrees of freedom\n")
  cat("Multiple R-squared:", round(stats$r_squared, 4),", Adjusted R-squared:", round(stats$adj_r_squared, 4), "\n")

  cat("F-statistic:", round(stats$f_statistic, 4), "on", length(object$coefficients) - 1, "and", stats$df_residual,
      "DF,  p-value:", format_p_values(stats$f_p_value), "\n")

  cat("AIC:", round(stats$AIC, 4),", BIC:", round(stats$BIC, 4),", Deviance:", round(stats$deviance, 4),"\n")

  cat("\nCross-Validation Summary:\n")
  if (!is.null(object$mean_error_cv)) {
    cat("Mean Cross-Validation Error (", object$measure_type, "):", round(min(object$mean_error_cv), 4), "\n")

    if (object$model_type == "PR1" || object$model_type == "PR2") {
      cat("Optimal Val:", signif(object$optimal_val, 4), "\n")
      if (!is.null(object$fixed_lambda_val)) {
        cat("Fixed Lambda:", signif(object$fixed_lambda_val, 4), "\n")
      }
    } else if (object$model_type == "PR3") {
      cat("Fixed Val:", signif(object$fixed_val, 4), "\n")
      if (!is.null(object$optimal_lambda_val)) {
        cat("Optimal Lambda:", signif(object$optimal_lambda_val, 4), "\n")
      }
    }
  }
  cat("\n")
}

