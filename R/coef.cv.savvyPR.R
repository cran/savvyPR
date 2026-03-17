#' Extract Coefficients From a Cross-Validated Parity Regression Model Object
#'
#' @title Extract Coefficients From a Cross-Validated Parity Regression Model Object
#' @description Extracts the estimated coefficients corresponding to the optimal tuning parameters
#' from a fitted cross-validated parity regression model object. It seamlessly handles models
#' optimized using either the \code{"budget"} or \code{"target"} parameterization method.
#'
#' @param object A fitted model object of class \code{cv.savvyPR}.
#' @param ... Additional arguments passed to the \code{\link{predict.cv.savvyPR}} method.
#'
#' @details
#' This function is an S3 method for the generic \code{coef} function. It extracts the coefficients
#' of the optimal model identified during the cross-validation procedure. The extraction is
#' internally delegated to the \code{predict.cv.savvyPR} method with \code{type = "coefficients"}.
#'
#' @return A named numeric vector of the estimated coefficients from the optimally tuned model.
#' If the model was fitted with an intercept, it will be included as the first element.
#'
#' @method coef cv.savvyPR
#'
#' @examples
#' \donttest{
#' # Generate synthetic data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), 100, 10)
#' y <- rnorm(100)
#'
#' # Example 1: Extract coefficients from a Budget-based CV model
#' cv_model_budget <- cv.savvyPR(x, y, method = "budget", model_type = "PR3")
#' coef(cv_model_budget)
#'
#' # Example 2: Extract coefficients from a Target-based CV model
#' cv_model_target <- cv.savvyPR(x, y, method = "target", model_type = "PR1")
#' coef(cv_model_target)
#' }
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @importFrom stats predict coef
#' @seealso \code{\link{cv.savvyPR}}, \code{\link{predict.cv.savvyPR}}
#' @export
coef.cv.savvyPR <- function(object, ...) {
  predict(object, type = "coefficients", ...)
}

