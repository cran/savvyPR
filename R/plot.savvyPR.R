#' Plot for a Parity Regression Model
#'
#' @title Plot for a Parity Regression Model
#' @description Generates various visualizations for a fitted parity regression
#' model object. It supports plotting estimated coefficients and risk contributions
#' based on the specified \code{plot_type}.
#'
#' @param x A fitted model object of class \code{"savvyPR"} returned by \code{\link{savvyPR}}.
#' @param plot_type Character string specifying the type of plot to generate. Can be \code{"estimated_coefficients"} or \code{"risk_contributions"}.
#'                  Defaults to \code{"estimated_coefficients"}.
#' @param label Logical; if \code{TRUE}, labels are added based on the plot type:
#' \describe{
#'   \item{estimated_coefficients}{Numeric values are added to the coefficient plot.}
#'   \item{risk_contributions}{Numeric labels are added above the bars.}
#' }
#' Default is \code{TRUE}.
#' @param ... Additional arguments passed to the underlying \code{ggplot} function.
#'
#' @details
#' This function offers two types of plots, depending on the value of \code{plot_type}:
#'
#' \describe{
#'   \item{\strong{Estimated Coefficients}}{Generates a line plot with points for the estimated
#'       coefficients of the regression model. If an intercept term is included in the
#'       model, it will be labeled as \code{beta_0}. Otherwise, the coefficients are
#'       labeled sequentially as \code{beta_1}, \code{beta_2}, etc., based on the
#'       covariates. This plot helps to visualize the contribution of each predictor
#'       variable to the model. If \code{label = TRUE}, numeric values are displayed.}
#'
#'   \item{\strong{Risk Contributions}}{If the model includes a risk parity component, the
#'       function will check if the optimization results (e.g., \code{orp_fit$weights}
#'       for the budget method, or \code{orp_fit$x} for the target method, along with
#'       \code{orp_fit$relativeRiskContrib}) are available. If available, two bar
#'       plots are created:
#'       \itemize{
#'           \item \strong{Optimization Variables: }A bar plot that visualizes the optimal
#'           variables assigned to each covariate and the response variable (weights
#'           for budget, target parameters for target).
#'           \item \strong{Risk Contributions: }A bar plot that visualizes the relative
#'           risk contributions of each covariate and the response variable.
#'       }
#'       If \code{label = TRUE}, numeric labels are added above the bars for clarity.
#'       If they are not found, a warning is issued.}
#' }
#'
#' @return A \code{ggplot} object representing the requested plot.
#'
#' @examples
#' # Example usage for `savvyPR` with Correlated Data:
#' set.seed(123)
#' n <- 100
#' p <- 12
#' # Create highly correlated predictors to demonstrate parity regression
#' base_var <- rnorm(n)
#' x <- matrix(rnorm(n * p, sd = 0.1), n, p) + base_var
#' beta <- matrix(rnorm(p), p, 1)
#' y <- x %*% beta + rnorm(n, sd = 0.5)
#'
#' # Fit a Budget-based parity regression model
#' result_budget <- savvyPR(x, y, method = "budget", val = 0.05, intercept = TRUE)
#' plot(result_budget, plot_type = "estimated_coefficients", label = FALSE)
#' plot(result_budget, plot_type = "risk_contributions", label = TRUE)
#'
#' # Fit a Target-based parity regression model
#' result_target <- savvyPR(x, y, method = "target", val = 1, intercept = TRUE)
#' plot(result_target, plot_type = "risk_contributions", label = TRUE)
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline geom_bar geom_text
#'             theme_minimal labs theme element_text element_line element_blank ylim
#' @importFrom gridExtra grid.arrange
#'
#' @seealso \code{\link{savvyPR}}
#' @method plot savvyPR
#' @export
plot.savvyPR <- function(x, plot_type = c("estimated_coefficients", "risk_contributions"), label = TRUE, ...) {
  plot_type <- match.arg(plot_type)

  if (plot_type == "estimated_coefficients") {
    plotCoef(x$coefficients, intercept = x$intercept, label = label, ...)
  } else if (plot_type == "risk_contributions") {
    if (!is.null(x$orp_fit)) {
      method_used <- if(is.null(x$method)) "budget" else x$method
      plotRiskContr(x$orp_fit, label = label, method = method_used, ...)
    } else {
      warning("No 'orp_fit' found in the model. This usually occurs when the tuning parameter is 0. Cannot plot risk contributions.")
    }
  }
}

