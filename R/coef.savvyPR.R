#' Extract Coefficients From a Parity Regression Model Object
#'
#' @title Extract Coefficients From a Parity Regression Model Object
#' @description Extracts the estimated coefficients from a fitted parity regression model object.
#' It seamlessly handles models fitted using either the \code{"budget"} or \code{"target"}
#' parameterization method.
#'
#' @param object A fitted model object of class \code{savvyPR}.
#' @param ... Additional arguments passed to the \code{\link{predict.savvyPR}} method.
#'
#' @details
#' This function is an S3 method for the generic \code{coef} function. It extracts the
#' estimated coefficients of the fitted model. The extraction is internally delegated to
#' the \code{predict.savvyPR} method with \code{type = "coefficients"}.
#'
#' @return A named numeric vector of the estimated coefficients from the fitted model.
#' If the model was fitted with an intercept, it will be included as the first element.
#'
#' @method coef savvyPR
#' @examples
#' # Generate synthetic data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), 100, 10)
#' y <- rnorm(100)
#'
#' # Example 1: Extract coefficients from a Budget-based model
#' model_budget <- savvyPR(x, y, method = "budget", val = 0.05)
#' coef(model_budget)
#'
#' # Example 2: Extract coefficients from a Target-based model
#' model_target <- savvyPR(x, y, method = "target", val = 1)
#' coef(model_target)
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @importFrom stats predict coef
#' @seealso \code{\link{savvyPR}}, \code{\link{predict.savvyPR}}
#' @export
coef.savvyPR <- function(object, ...) {
  predict(object, type = "coefficients", ...)
}

