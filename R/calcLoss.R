# Extended Loss Calculation Function

#' @keywords internal
calcLoss <- function(x, y, beta, measure_type = "default", intercept) {
  if (measure_type == "default") {
    measure_type <- "mse"
  }

  if (intercept) {
    x <- cbind(1, x)
  }

  predictions <- x %*% beta

  loss <- switch(measure_type,
                 "mse" = mean((y - predictions)^2),
                 "mae" = mean(abs(y - predictions)),
                 "rmse" = sqrt(mean((y - predictions)^2)),
                 "mape" = {
                   if (any(y == 0)) {
                     warning("MAPE computation: actual values contain zeros, which will lead to infinity.")
                     NA
                   } else {
                     mean(abs((y - predictions) / y)) * 100
                   }
                 },
                 stop("Unsupported type of measure specified")
  )

  return(loss)
}
