# Gradient of the objective function (target-based)
calcGradientTarget <- function(x, covMat, t) {
  x <- pmax(x, .Machine$double.eps)
  gradient <- covMat %*% x - c(1 / x[1:(length(x) - 1)], t / x[length(x)])
  return(gradient)
}

objFunctionRiskParityTarget <- function(x, covMat, t) {
  return(calcGradientTarget(x, covMat, t))
}

calcObjFunctionTarget <- function(covMat, x, t) {
  x <- pmax(x, .Machine$double.eps)
  return(0.5 * sum(x * (covMat %*% x)) - sum(log(x[1:(length(x) - 1)])) - t * log(x[length(x)]))
}

optimizeRiskParityTarget <- function(covMat, t = 1, maxIter = 200, tol = 1e-10) {
  n <- nrow(covMat)

  if (t < 0) {
    return(list(x = rep(NaN, n),
                relativeRiskContrib = rep(NaN, n),
                objFunction = NaN,
                isFeasible = FALSE,
                message = "Parameter t must be greater than 0"))
  }

  init_x <- sqrt(1 / sum(covMat)) * rep(1, n)

  opt_result <- nleqslv(
    x = init_x,
    fn = objFunctionRiskParityTarget,
    covMat = covMat,
    t = t,
    method = "Newton",
    control = list(maxit = maxIter, ftol = tol)
  )

  optimal_x <- opt_result$x

  riskContrib <- optimal_x * (covMat %*% optimal_x)
  relative_risk_contrib <- riskContrib / sum(riskContrib)

  is_feasible <- all(optimal_x >= 0)

  return(list(x = as.numeric(optimal_x),
              relativeRiskContrib = as.numeric(relative_risk_contrib),
              objFunction = calcObjFunctionTarget(covMat, optimal_x, t),
              isFeasible = is_feasible,
              message = if (is_feasible) "Optimization successful" else "Optimization failed"))
}
