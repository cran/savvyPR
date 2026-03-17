#' Print a Cross-Validated Parity Regression Model Object
#'
#' @title Print a Cross-Validated Parity Regression Model Object
#' @description Prints a summarized output of a fitted cross-validated parity regression
#' model object. It clearly displays the optimal tuning parameters and the resulting
#' estimated coefficients.
#'
#' @param x A fitted model object of class \code{"cv.savvyPR"} returned by \code{\link{cv.savvyPR}}.
#' @param digits Significant digits to be used in the printout.
#' @param ... Additional arguments passed to the generic \code{print} function.
#'
#' @details
#' This function is an S3 method for the generic \code{print} function. It formats and prints
#' the matched call that produced the \code{cv.savvyPR} object, followed by a summary data frame.
#' This summary includes:
#' \itemize{
#'   \item The parameterization method used (\code{"budget"} or \code{"target"}).
#'   \item The number of non-zero coefficients.
#'   \item Whether an intercept was included.
#'   \item The optimal tuning value (\code{val}) and/or \code{lambda} parameter, depending on the \code{model_type} (\code{PR1}, \code{PR2}, or \code{PR3}).
#' }
#' Finally, it prints a data frame of the optimally tuned estimated coefficients.
#'
#' @return Invisibly returns a data frame summarizing the cross-validation results,
#' including the parameterization method, number of non-zero coefficients, and optimal tuning parameters.
#'
#' @examples
#' \donttest{
#' # Generate synthetic data
#' set.seed(123)
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- matrix(rnorm(p), p, 1)
#' y <- x %*% beta + rnorm(n, sd = 0.5)
#'
#' # Fit and print a cross-validated budget-based parity regression model
#' cv_fit_budget <- cv.savvyPR(x, y, method = "budget", model_type = "PR3")
#' print(cv_fit_budget)
#'
#' # Fit and print a cross-validated target-based parity regression model
#' cv_fit_target <- cv.savvyPR(x, y, method = "target", model_type = "PR1")
#' print(cv_fit_target)
#' }
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso \code{\link{cv.savvyPR}}
#' @method print cv.savvyPR
#' @export
print.cv.savvyPR <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ", deparse(x$call), "\n\n")

  intercept_included <- ifelse(x$PR_fit$intercept, "Yes", "No")
  non_zero_coefs <- sum(x$coefficients != 0)

  method_used <- if (is.null(x$method)) "budget" else x$method

  if (x$model_type == "PR1" || x$model_type == "PR2") {
    summary_output <- data.frame(
      "Method" = method_used,
      "Number of Non-Zero Coefficients" = non_zero_coefs,
      "Intercept Included" = intercept_included,
      "Optimal Val" = signif(x$optimal_val, digits),
      "Fixed Lambda Value" = signif(x$fixed_lambda_val, digits),
      check.names = FALSE
    )
  } else if (x$model_type == "PR3") {
    summary_output <- data.frame(
      "Method" = method_used,
      "Number of Non-Zero Coefficients" = non_zero_coefs,
      "Intercept Included" = intercept_included,
      "Fixed Val" = signif(x$fixed_val, digits),
      "Optimal Lambda Value" = signif(x$optimal_lambda_val, digits),
      check.names = FALSE
    )
  }

  print(summary_output, row.names = FALSE)

  cat("\nCoefficients:\n")

  coef_names <- if (x$PR_fit$intercept) c("(Intercept)", colnames(x$PR_fit$model)[-1]) else colnames(x$PR_fit$model)[-1]

  if (length(coef_names) != length(x$coefficients)) {
    coef_names <- if (x$PR_fit$intercept) c("(Intercept)", paste0("V", 1:(length(x$coefficients)-1))) else paste0("V", 1:length(x$coefficients))
  }

  coef_df <- data.frame(
    Coefficient = coef_names,
    Estimate = round(x$coefficients, digits)
  )
  print(coef_df, row.names = FALSE)

  invisible(summary_output)
}

