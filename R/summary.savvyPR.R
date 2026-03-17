#' Summary of a Fitted Parity Regression Model with Statistics
#'
#' @name summary.savvyPR
#' @title Summary of Parity Regression Model
#' @description Prints a comprehensive statistical summary of a fitted parity regression
#' model object. It displays the model's configuration, statistical summaries of the
#' estimated coefficients, and overall model fit statistics.
#'
#' @param object A fitted model object of class \code{savvyPR} returned by \code{\link{savvyPR}}.
#' @param ... Additional arguments passed to the generic \code{summary} function (currently unused).
#'
#' @details
#' This function is an S3 method for the generic \code{summary} function. It formats and prints
#' a detailed statistical overview of the fitted model. The output includes:
#' \itemize{
#'   \item The parameterization method used (\code{"budget"} or \code{"target"}).
#'   \item The matched call that produced the model.
#'   \item Residual quantiles.
#'   \item A table of estimated coefficients with their corresponding standard errors, t-values, p-values, confidence intervals, and significance codes.
#'   \item Overall model fit statistics, including Residual Standard Error, Multiple and Adjusted R-squared, F-statistic, AIC, BIC, and Deviance.
#' }
#'
#' @return Invisibly returns \code{NULL}. This function is primarily called for its side
#' effect of printing the summary to the console.
#'
#' @examples
#' # Simulate some data
#' set.seed(123)
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- matrix(rnorm(p), p, 1)
#' y <- x %*% beta + rnorm(n, sd = 0.5)
#'
#' # Example 1: Fit and summarize a Budget-based parity regression model
#' fit_budget <- savvyPR(x, y, method = "budget", val = 0.05, intercept = FALSE)
#' summary(fit_budget)
#'
#' # Example 2: Fit and summarize a Target-based parity regression model
#' fit_target <- savvyPR(x, y, method = "target", val = 1, intercept = TRUE)
#' summary(fit_target)
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso \code{\link{savvyPR}}
#' @importFrom stats pf predict pt qnorm quantile
#' @method summary savvyPR
#' @export
summary.savvyPR <- function(object, ...) {
  cat("Summary of Parity Model\n")
  cat("===================================================================\n\n")

  method_used <- if (is.null(object$method)) "budget" else object$method
  cat("Parameterization Method:", method_used, "\n")
  cat("Intercept:", if (object$intercept) "Included\n" else "Not included\n")

  cat("\nCall:\n")
  print(object$call)

  x <- as.matrix(object$model[-1])
  y <- object$model$y

  stats <- summaryStats(x, y, object$coefficients, object$intercept)

  cat("\nResiduals:\n")
  print(stats$residual_quants)

  signif_codes <- add_significance_codes(stats$p_values)
  formatted_p_values <- format_p_values(stats$p_values)

  coef_names <- if (object$intercept) c("(Intercept)", colnames(x)) else colnames(x)

  if (length(coef_names) != length(object$coefficients)) {
    coef_names <- if (object$intercept) c("(Intercept)", paste0("V", 1:(length(object$coefficients)-1))) else paste0("V", 1:length(object$coefficients))
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
  cat("Multiple R-squared:", round(stats$r_squared, 4), ", Adjusted R-squared:", round(stats$adj_r_squared, 4), "\n")

  cat("F-statistic:", round(stats$f_statistic, 4), "on", length(object$coefficients) - 1, "and", stats$df_residual,
      "DF,  p-value:", format_p_values(stats$f_p_value), "\n")
  cat("AIC:", round(stats$AIC, 4),", BIC:", round(stats$BIC, 4),", Deviance:", round(stats$deviance, 4),"\n")

  cat("\n")
}

