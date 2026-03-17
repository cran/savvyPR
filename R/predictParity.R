#' @keywords internal
predictParity <- function(coefficients, newx = NULL, intercept = TRUE, type = c("response", "coefficients")) {
  type <- match.arg(type)

  if (type == "coefficients") {
    coef_names <- if (intercept) {
      c("(Intercept)", paste0("V", seq_along(coefficients[-1])))
    } else {
      paste0("V", seq_along(coefficients))
    }

    if (length(coef_names) == length(coefficients)) {
      names(coefficients) <- coef_names
    } else {
      stop("Mismatch between the number of coefficients and the names generated.")
    }

    return(coefficients)
  }

  if (is.null(newx)) {
    stop("You need to supply a matrix of new values for 'newx'.")
  }

  newx <- as.matrix(newx)
  nvars <- length(coefficients) - as.integer(intercept)
  if (ncol(newx) != nvars) {
    stop("The number of variables in newx must match the number of coefficients in the model.")
  }

  model_intercept <- if (intercept) coefficients[1] else 0
  coefs <- if (intercept) coefficients[-1] else coefficients

  fitted_values <- model_intercept + newx %*% coefs

  return(fitted_values)
}
