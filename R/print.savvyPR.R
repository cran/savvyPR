#' Print a Parity Regression Model Object
#'
#' @title Print a Parity Regression Model Object
#' @description Prints a summarized output of a fitted parity regression model object.
#' It clearly displays the model's configuration and the resulting estimated coefficients.
#'
#' @param x A fitted model object of class \code{"savvyPR"} returned by \code{\link{savvyPR}}.
#' @param digits Significant digits to be used in the printout.
#' @param ... Additional arguments passed to the generic \code{print} function.
#'
#' @details
#' This function is an S3 method for the generic \code{print} function. It formats and prints
#' the matched call that produced the \code{savvyPR} object, followed by a summary data frame.
#' This summary includes:
#' \itemize{
#'   \item The parameterization method used (\code{"budget"} or \code{"target"}).
#'   \item The number of non-zero coefficients.
#'   \item Whether an intercept was included.
#'   \item The penalty value (\code{lambda}), if any was applied.
#' }
#' Finally, it prints a data frame of the estimated coefficients.
#'
#' @return Invisibly returns a data frame summarizing the model, including the
#' parameterization method, number of non-zero coefficients, intercept status, and lambda value.
#'
#' @examples
#' # Generate synthetic data
#' set.seed(123)
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- matrix(rnorm(p), p, 1)
#' y <- x %*% beta + rnorm(n, sd = 0.5)
#'
#' # Fit and print a Budget-based model
#' fit_budget <- savvyPR(x, y, method = "budget", val = 0.05)
#' print(fit_budget)
#'
#' # Fit and print a Target-based model
#' fit_target <- savvyPR(x, y, method = "target", val = 1)
#' print(fit_target)
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso \code{\link{savvyPR}}
#' @method print savvyPR
#' @export
print.savvyPR <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ", deparse(x$call), "\n\n")

  intercept_included <- ifelse(x$intercept, "Yes", "No")
  non_zero_coefs <- sum(x$coefficients != 0)
  lambda_val <- if (is.null(x$lambda)) NA else signif(x$lambda, digits)

  method_used <- if (is.null(x$method)) "budget" else x$method

  summary_output <- data.frame(
    "Method" = method_used,
    "Number of Non-Zero Coefficients" = non_zero_coefs,
    "Intercept Included" = intercept_included,
    "Lambda Value" = lambda_val,
    check.names = FALSE
  )

  print(summary_output, row.names = FALSE)

  cat("\nCoefficients:\n")

  coef_names <- if (x$intercept) c("(Intercept)", colnames(x$model)[-1]) else colnames(x$model)[-1]

  if (length(coef_names) != length(x$coefficients)) {
    coef_names <- if (x$intercept) c("(Intercept)", paste0("V", 1:(length(x$coefficients)-1))) else paste0("V", 1:length(x$coefficients))
  }

  coef_df <- data.frame(
    Coefficient = coef_names,
    Estimate = round(x$coefficients, digits)
  )
  print(coef_df, row.names = FALSE)

  invisible(summary_output)
}

