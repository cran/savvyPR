utils::globalVariables(c("x", "y"))

#' @keywords internal
plotCVErr <- function(cv_results, label = TRUE, ...) {
  if (is.null(cv_results$mean_error_cv) || is.null(cv_results$optimal_index)) {
    stop("The input cv_results must contain mean cross-validation errors and optimal indices.")
  }

  mean_errors <- cv_results$mean_error_cv
  optimal_index <- cv_results$optimal_index
  measure_type <- cv_results$measure_type
  model_type <- cv_results$model_type

  if (model_type %in% c("PR1", "PR2") && is.null(cv_results$vals)) {
    stop("vals must be provided in cv_results for model_type 'PR1' or 'PR2'.")
  }
  if (model_type == "PR3" && is.null(cv_results$lambda_vals)) {
    stop("lambda_vals must be provided in cv_results for model_type 'PR3'.")
  }

  if (model_type %in% c("PR1", "PR2")) {
    vals <- cv_results$vals
    optimal_val <- vals[optimal_index$min_val]
    plot_data <- data.frame(x = log(vals), y = mean_errors, measure = measure_type)
    x_label <- expression(log(Val))
    optimal_value <- log(optimal_val)
  } else if (model_type == "PR3") {
    lambda_vals <- cv_results$lambda_vals
    optimal_lambda <- lambda_vals[optimal_index$min_lambda]
    plot_data <- data.frame(x = log(lambda_vals), y = mean_errors, measure = measure_type)
    x_label <- expression(log(lambda))
    optimal_value <- log(optimal_lambda)
  }

  std_errors <- sd(mean_errors) / sqrt(length(mean_errors))

  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_errorbar(aes(ymin = y - std_errors, ymax = y + std_errors), width = 0.1, color = "darkgray") +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "red", size = 2) +
    labs(
      title = "Cross-Validation Curve",
      x = x_label,
      y = measure_type
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray", linewidth = 0.2),
      panel.grid.minor = element_line(color = "gray", linewidth = 0.1)
    )

  if (model_type %in% c("PR1", "PR2")) {
    p <- p + geom_vline(xintercept = log(optimal_val), linetype = "dashed", color = "blue") +
      annotate("text", x = log(optimal_val), y = min(mean_errors), label = "Optimal Val",
               hjust = 0.1, vjust = 1.5, color = "blue", fontface = "bold", angle = 90, size = 3)
  } else if (model_type == "PR3") {
    p <- p + geom_vline(xintercept = log(optimal_lambda), linetype = "dashed", color = "blue") +
      annotate("text", x = log(optimal_lambda), y = min(mean_errors), label = "Optimal Lambda",
               hjust = 0.1, vjust = 1.5, color = "blue", fontface = "bold", angle = 90, size = 3)
  }

  optimal_y <- min(mean_errors)
  p <- p + annotate("point", x = optimal_value, y = optimal_y, color = "turquoise", size = 3, shape = 18)

  if (label) {
    label_text <- paste0("(", round(optimal_value, 3), ", ", round(optimal_y, 3), ")")
    p <- p + annotate("text", x = optimal_value, y = optimal_y, label = label_text,
                      hjust = -1.2, vjust = 1.5, size = 3, fontface = "bold", angle = 90, color = "black")
  }

  print(p)
  invisible(p)
}

