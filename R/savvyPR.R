#' Parity Regression Model Estimation
#'
#' @title Parity Regression Model Estimation
#'
#' @description Implements the Parity Regression (PR) methodology for multiple linear regression.
#' Instead of minimizing the aggregate prediction error in the dependent variable, PR distributes
#' the total prediction error evenly across all parameters. This approach ensures stability in the
#' presence of high multicollinearity and is particularly suitable for data affected by substantial
#' noise, such as time series data experiencing structural changes and evolving trends.
#'
#' @param x A matrix of predictors with rows as observations and columns as variables. Must not contain \code{NA} values, and should not include an intercept column of ones.
#' @param y A numeric vector of the response variable, should have the same number of observations as \code{x}. Must not contain \code{NA} values.
#' @param method Character string specifying the parameterization method to use: \code{"budget"} (default) or \code{"target"}.
#' @param val Numeric tuning parameter. If \code{method = "budget"}, this represents \code{c} (a value between \code{0} and \code{1/p}).
#'            If \code{method = "target"}, this represents \code{t} (a target risk parameter \code{> 0}).
#' @param lambda_val Optional; a numeric value specifying the regularization strength.
#'                   If \code{NULL}, \code{lambda} is determined via cross-validation.
#' @param use_feature_selection Logical; if \code{TRUE}, applies Lasso to perform feature selection before model estimation. Defaults to \code{FALSE}.
#' @param standardize Logical; if \code{TRUE}, scale and center predictor variables before fitting the model. Defaults to \code{TRUE}.
#' @param intercept Logical; if \code{TRUE}, includes an intercept in the model, otherwise, the model will not estimate an intercept term. Defaults to \code{TRUE}.
#' @param exclude Optional; an array of indices specifying columns to be excluded from the analysis.
#'
#' @details
#' The PR methodology assigns an equal risk contribution constraint on each predictor variable within a specific search cone,
#' leading to a robust solution. This solution is defined in the context of a penalized regression,
#' wherein the penalty term is a function of both the regularization parameter (\code{lambda_val}) and the proportional contribution parameter (\code{val}).
#' The function uses the \code{nleqslv} package to solve non-linear systems of equations.
#'
#' The function supports two parameterization methods:
#' \itemize{
#' \item \strong{Budget}: Uses \code{val} (acting as \code{c}) to set a strict budget constraint on the risk contributions.
#' \item \strong{Target}: Uses \code{val} (acting as \code{t}) to set a risk target for the response variable relative to the predictors.
#' }
#'
#' The function can handle different practical scenarios:
#' \itemize{
#' \item While the PR theorem is not specifically designed for variable selection, the package makes this available as an optional preprocessing step.
#'  If \code{use_feature_selection} is \code{TRUE}, Lasso regression is performed to select features by zeroing out non-contributive predictors before applying the PR model.
#' \item It checks the matrix rank of predictors and applies Ridge regression as a fallback to ordinary least squares if the matrix is not full rank, ensuring computational stability.
#' }
#'
#' For the \strong{budget} method, the PR methodology optimizes an objective function that includes a variance term and a penalization term:
#' \deqn{1/2 * RRSS(x, x_{p+1}; \lambda) - \widetilde{\mu} ( c \sum_{k=0}^p \log(\delta_k x_k) + (1 - (p+1)c) \log(x_{p+1}) )}
#'
#' For the \strong{target} method, the methodology optimizes a related objective function defined by the target parameter \eqn{t}:
#' \deqn{1/2 * RRSS(x, x_{p+1}; \lambda) - \widetilde{\mu} ( \sum_{k=0}^p \log(\delta_k x_k) + t \log(x_{p+1}) )}
#'
#' In both formulas, \eqn{x} represents the parameters (with \eqn{x_{p+1}} as an auxiliary parameter set to 1), \eqn{\lambda} is the regularization parameter,
#' \eqn{p} is the number of predictors, and \eqn{\widetilde{\mu}} is a constant with respect to \eqn{\lambda}.
#' The resulting model provides estimates of the regression coefficients that are equitable across all predictors in terms of contribution to the model's predictive power.
#'
#' @return Returns an S3 object of class \code{"savvyPR"} containing the following components:
#'   \item{call}{The matched call to the function.}
#'   \item{coefficients}{A numeric vector of estimated coefficients. If the tuning parameter is zero, coefficients are obtained from Ridge regression or OLS. Otherwise, they are obtained from the parity regression model.}
#'   \item{method}{The optimization method used (\code{"budget"} or \code{"target"}).}
#'   \item{fit}{The fitted object returned by \code{glmnet} when the tuning parameter is zero.}
#'   \item{orp_fit}{The fitted object returned by the risk parity optimizer (\code{optimizeRiskParityBudget} or \code{optimizeRiskParityTarget}) when the tuning parameter is non-zero.}
#'   \item{lambda}{The regularization parameter \code{lambda} used in the model.}
#'   \item{intercept}{A logical value indicating whether an intercept is included in the model.}
#'   \item{model}{A data frame containing the response variable \code{y} and the covariates \code{x} used in the model.}
#'
#' @examples
#' library(glmnet)
#' library(nleqslv)
#'
#' # Generate synthetic data
#' set.seed(123)
#' n <- 100 # Number of observations
#' p <- 10  # Number of variables
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- matrix(rnorm(p), p, 1)
#' y <- x %*% beta + rnorm(n, sd = 0.5) # Linear combination with noise
#'
#' # Example 1: Run PR estimation using the "budget" method (acting as c)
#' result_budget <- savvyPR(x, y, method = "budget", val = 0.05, intercept = TRUE)
#' print(result_budget$coefficients)
#'
#' # Example 2: Run PR estimation using the "target" method (acting as t)
#' result_target <- savvyPR(x, y, method = "target", val = 1, intercept = TRUE)
#' print(result_target$coefficients)
#'
#' # Example 3: Run PR estimation with feature selection
#' result_fs <- savvyPR(x, y, method = "budget", val = 0.05, use_feature_selection = TRUE)
#' print(result_fs$coefficients)
#'
#' # Inspect the risk parity portfolio object for more details
#' if(!is.null(result_fs$orp_fit)) {
#'   print("Risk parity portfolio details:")
#'   print(result_fs$orp_fit)
#' }
#'
#' # Example 4: Run PR estimation excluding some predictors
#' result_exclude <- savvyPR(x, y, method = "budget", val = 0.05, exclude = c(1, 2))
#' print("Coefficients with first two predictors excluded:")
#' print(result_exclude$coefficients)
#'
#' @author Ziwei Chen, Vali Asimit and Pietro Millossovich\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @references
#' Asimit, V., Chen, Z., Ichim, B., & Millossovich, P. (2026).
#' \emph{Prity Regression Estimation}.
#' Retrieved from \url{https://openaccess.city.ac.uk/id/eprint/37017/}
#'
#' The optimization technique employed follows the algorithm described by:
#' F. Spinu (2013). An Algorithm for Computing Risk Parity Weights. SSRN Preprint. doi:10.2139/ssrn.2297383
#'
#' @importFrom Matrix rankMatrix
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom stats coef cov lm sd
#' @importFrom nleqslv nleqslv
#'
#' @seealso \code{\link{cv.savvyPR}}, \code{\link[glmnet:glmnet]{glmnet}}, \code{\link[glmnet:cv.glmnet]{cv.glmnet}}
#'
#' @export
savvyPR <- function(x, y, method = c("budget", "target"), val = NULL,
                    lambda_val = NULL, use_feature_selection = FALSE,
                    standardize = FALSE, intercept = TRUE, exclude = NULL) {

  if (missing(method) || is.null(method)) {
    method <- "budget"
  } else {
    method <- match.arg(method)
  }

  x <- as.matrix(x)
  y <- as.vector(y)

  if (nrow(x) != length(y)) {
    stop("The number of rows in x must match the length of y.")
  }

  if (anyNA(x) || anyNA(y)) {
    stop("x or y has missing values; consider using appropriate methods to impute them before analysis.")
  }

  model <- data.frame(y, x)

  if (!is.null(exclude) && length(exclude) > 0) {
    if (any(exclude > ncol(x))) {
      stop("Exclusion indices are out of bounds.")
    }
    x <- x[, -exclude, drop = FALSE]
  }

  if (all(x[, 1] == 1)) {
    warning("First column of x is an intercept (all ones). Removing this column.")
    x <- x[, -1, drop = FALSE]
  }

  low_unique_threshold <- 0.05 * nrow(x)
  low_unique_columns <- apply(x, 2, function(col) length(unique(col)) < low_unique_threshold)
  if (any(low_unique_columns)) {
    stop("Found columns with less than 5% unique values, which are not suitable for parity regression.")
  }

  if (is.null(colnames(x))) {
    colnames(x) <- paste("V", seq_along(x[1, ]), sep = "")
  }

  nobs <- nrow(x)
  nvars <- ncol(x)
  if (nvars >= nobs) {
    stop("The number of features in x must be less than the number of observations to avoid rank deficiency issues.")
  }

  if (is.null(val)) {
    val <- 0
  } else {
    if (!is.numeric(val)) stop("val must be a numeric value.")
    if (val < 0) {
      warning("val cannot be negative; setting val to 0.")
      val <- 0
    }
    if (method == "budget" && val > 1 / nvars) {
      warning(sprintf("For 'budget' method, val exceeds the maximum allowed value of 1/%d; setting val to 1/%d.", nvars, nvars))
      val <- 1 / nvars
    }
  }

  is_zero_param <- (val == 0)

  if (!is.null(lambda_val)) {
    if (!is.numeric(lambda_val) || length(lambda_val) != 1) {
      stop("lambda_val must be a single numeric value.")
    }
    if (lambda_val < 0) {
      warning("lambda_val must be a non-negative numeric value; setting lambda_val to 0.")
      lambda_val <- 0
    }
  }

  if (use_feature_selection) {
    cv_model <- cv.glmnet(x, y, alpha = 1)
    lambda_min <- cv_model$lambda.min
    fit <- glmnet(x, y, alpha = 1, lambda = lambda_min)  # Lasso for feature selection
    no_zero_coef <- as.vector(coef(fit))[-1]
    selected_features <- which(no_zero_coef != 0)
    x <- x[, selected_features, drop = FALSE]
    nvars <- ncol(x)
  }

  # Apply main PR models
  if (is_zero_param) {
    # Use Ridge or OLS depending on lambda_val
    if (is.null(lambda_val)) {
      cv_model <- cv.glmnet(x, y, alpha = 0, intercept = intercept)
      lambda_val <- cv_model$lambda.min
    }
    fit <- glmnet(x, y, alpha = 0, lambda = lambda_val, intercept = intercept)
    coefficients <- as.vector(coef(fit))
    if (!intercept) {
      coefficients <- coefficients[-1]
    }
    result <- list(call = match.call(), coefficients = coefficients, method = method, fit = fit,
                   lambda = lambda_val, intercept = intercept, model = model)
  } else {
    mu_x <- colMeans(x)
    sd_x <- apply(x, 2, function(x) sqrt((sd(x)^2) * (length(x) - 1) / length(x)))

    # Case 1: intercept = TRUE
    if (intercept) {
      if (standardize) {
        x <- sweep(sweep(x, 2, mu_x, "-"), 2, sd_x, "/")
      } else {
        x <- sweep(x, 2, mu_x, "-")
      }
      mu_y <- mean(y)
      y <- y - mu_y

      # Case 2: intercept = FALSE
    } else {
      if (standardize) {
        x <- sweep(sweep(x, 2, mu_x, "-"), 2, sd_x, "/")
      }
    }

    if (Matrix::rankMatrix(x)[1] < nvars) {
      if (is.null(lambda_val)) {
        cv_model <- cv.glmnet(x, y, alpha = 0, intercept = intercept)
        lambda_min <- cv_model$lambda.min
      }
      fit <- glmnet(x, y, alpha = 0, lambda = lambda_min, intercept = intercept)
      lm_coef <- as.vector(coef(fit))[-1]
    } else {
      # Full rank, apply OLS
      lm_fit <- lm(y ~ x - 1)
      lm_coef <- lm_fit$coefficients
    }

    c_vec <- sign(lm_coef) # Sign vector
    x_new <- sweep(x, 2, c_vec, "*")

    cov_matrix <- cov(cbind(x_new, -y))
    if (!is.null(lambda_val) && lambda_val != 0) {
      cov_matrix <- ((nobs - 1) / nobs) * cov_matrix + diag(c(rep(lambda_val, nvars), 0))
    }

    # Risk parity portfolio calculation based on method
    if (method == "budget") {
      b_val <- c(rep(val, nvars), 1 - val * nvars)
      b_val[length(b_val)] <- b_val[length(b_val)] + (1 - sum(b_val))

      orp_fit <- optimizeRiskParityBudget(cov_matrix, budgetVec = b_val)
      orp_vars <- orp_fit$weights
    } else if (method == "target") {
      orp_fit <- optimizeRiskParityTarget(cov_matrix, t = val)
      orp_vars <- orp_fit$x
    }

    orp_coef <- (c_vec * orp_vars[-(nvars + 1)]) / orp_vars[nvars + 1]
    pr_coef <- if (standardize) orp_coef / sd_x else orp_coef

    if (intercept) {
      pr_intercept <- mu_y - sum(pr_coef * mu_x)
      coefficients = as.vector(c(intercept = pr_intercept, pr_coef))
    } else {
      coefficients = as.vector(pr_coef)
    }

    result <- list(call = match.call(), coefficients = coefficients, method = method,
                   orp_fit = orp_fit, lambda = lambda_val, intercept = intercept, model = model)
  }

  class(result) <- "savvyPR"

  return(result)
}

