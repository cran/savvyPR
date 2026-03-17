# Gradient of the objective function (budget-based)
calcGradientBudget <- function(x, covMat, budgetVec) {
  x <- pmax(x, .Machine$double.eps)
  gradient <- covMat %*% x - budgetVec / x
  return(gradient)
}

objFunctionRiskParityBudget <- function(x, covMat, budgetVec) {
  return(calcGradientBudget(x, covMat, budgetVec))
}

calcObjFunctionBudget <- function(covMat, x, budgetVec) {
  x <- pmax(x, .Machine$double.eps)
  return(0.5 * sum(x * (covMat %*% x)) - sum(budgetVec * log(x)))
}

optimizeRiskParityBudget <- function(covMat, budgetVec = rep(1 / nrow(covMat), nrow(covMat)),
                                     maxIter = 200, tol = 1e-10) {
  n <- nrow(covMat)

  if (any(budgetVec < 0) || abs(sum(budgetVec) - 1) > tol) {
    return(list(weights = rep(NaN, n),
                relativeRiskContrib = rep(NaN, n),
                objFunction = NaN,
                isFeasible = FALSE,
                message = "Parameter b must be greater than 0 and sum to 1"))
  }

  init_x <- sqrt(sum(budgetVec) / sum(covMat)) * rep(1, n)

  opt_result <- nleqslv(
    x = init_x,
    fn = objFunctionRiskParityBudget,
    covMat = covMat,
    budgetVec = budgetVec,
    method = "Newton",
    control = list(maxit = maxIter, ftol = tol)
  )

  optimal_x <- opt_result$x

  riskContrib <- optimal_x * (covMat %*% optimal_x)
  relative_risk_contrib <- riskContrib / sum(riskContrib)

  weights <- optimal_x / sum(optimal_x)

  is_feasible <- all(weights >= 0) && abs(sum(weights) - 1) < tol

  return(list(weights = as.numeric(weights),
              relativeRiskContrib = as.numeric(relative_risk_contrib),
              objFunction = calcObjFunctionBudget(covMat, weights, budgetVec),
              isFeasible = is_feasible,
              message = if (is_feasible) "Optimization successful" else "Optimization failed"))
}


