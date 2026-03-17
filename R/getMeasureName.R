measure_descriptions <- list(
  mse = "Mean-Squared Error",
  mae = "Mean Absolute Error",
  rmse = "Root Mean Squared Error",
  rmsle = "Root Mean Squared Logarithmic Error",
  mape = "Mean Absolute Percentage Error"
)

#' @keywords internal
getMeasureName <- function(measure_type) {
  if (measure_type == "default") {
    measure_type <- "mse"
  }

  if (measure_type %in% names(measure_descriptions)) {
    return(paste(measure_type, measure_descriptions[[measure_type]], sep = ": "))
  } else {
    return("Unsupported measure type")
  }
}
