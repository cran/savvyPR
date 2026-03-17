utils::globalVariables(c("index", "coefficient", "variable"))

#' @keywords internal
plotCVCoef <- function(result_list, label = TRUE, xvar = c("norm", "lambda", "dev", "val"), max_vars_per_plot = 10, intercept = TRUE) {
  if (max_vars_per_plot > 10) {
    warning("max_vars_per_plot cannot exceed 10. Setting max_vars_per_plot to 10.")
    max_vars_per_plot <- 10
  }

  if (is.null(result_list$coefficients_cv)) {
    stop("The input result_list must contain coefficient paths.")
  }

  coefs <- as.matrix(result_list$coefficients_cv)
  model_type <- result_list$model_type
  xvar <- match.arg(xvar)

  if (model_type %in% c("PR1", "PR2") && xvar == "lambda") {
    stop("Invalid combination: PR1 and PR2 models cannot use 'lambda' as xvar.")
  }
  if (model_type == "PR3" && xvar == "val") {
    stop("Invalid combination: PR3 model cannot use 'val' as xvar.")
  }

  x_vals <- NULL
  x_label <- NULL
  if (model_type %in% c("PR1", "PR2")) {
    if (xvar == "val") {
      x_vals <- result_list$vals
      x_label <- expression(log(Val))
    } else if (xvar == "norm") {
      l1_norm <- apply(abs(coefs), 2, sum)
      x_vals <- l1_norm
      x_label <- expression(L[1]~"Norm")
    } else if (xvar == "dev") {
      deviance <- if (!is.null(result_list$PR_fit$objFunction)) result_list$PR_fit$objFunction else apply(abs(coefs), 2, sum)
      x_vals <- deviance
      x_label <- "Fraction Deviance Explained"
    }
  } else if (model_type == "PR3") {
    if (xvar == "lambda") {
      x_vals <- result_list$lambda_vals
      x_label <- expression(log(lambda))
    } else if (xvar == "norm") {
      l1_norm <- apply(abs(coefs), 2, sum)
      x_vals <- l1_norm
      x_label <- expression(L[1]~"Norm")
    } else if (xvar == "dev") {
      deviance <- if (!is.null(result_list$PR_fit$objFunction)) result_list$PR_fit$objFunction else apply(abs(coefs), 2, sum)
      x_vals <- deviance
      x_label <- "Fraction Deviance Explained"
    }
  } else {
    stop("Invalid model_type or xvar combination.")
  }

  if (is.null(rownames(coefs))) {
    if (intercept) {
      rownames(coefs) <- c("V0", paste0("V", seq_len(nrow(coefs) - 1)))
    } else {
      rownames(coefs) <- paste0("V", seq_len(nrow(coefs)))
    }
  }

  plot_data <- data.frame(
    index = rep(x_vals, each = nrow(coefs)),
    coefficient = as.vector(coefs),
    variable = factor(rep(rownames(coefs), times = length(x_vals)), levels = rownames(coefs)),
    x_vals = rep(x_vals, each = nrow(coefs))
  )

  num_vars <- nrow(coefs)
  num_plots <- ceiling(num_vars / max_vars_per_plot)
  plot_list <- list()

  for (i in 1:num_plots) {
    start_var <- (i - 1) * max_vars_per_plot + 1
    end_var <- min(i * max_vars_per_plot, num_vars)
    plot_subset <- plot_data[plot_data$variable %in% levels(plot_data$variable)[start_var:end_var], ]

    p <- ggplot(plot_subset, aes(x = index, y = coefficient, color = variable, group = variable)) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.5) +
      labs(
        title = paste("Coefficient Profile Plot (Variables", start_var, "to", end_var, ")"),
        x = x_label,
        y = "Coefficients"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.position = "right"
      ) +
      scale_color_brewer(palette = "Set3")

    if (label) {
      if (xvar %in% c("lambda", "val")) {
        label_data <- plot_subset[plot_subset$index == min(plot_subset$index), ]
        hjust_pos <- 0.1
        nudge_x_val <- -0.08 * (max(x_vals) - min(x_vals))
      } else {
        label_data <- plot_subset[plot_subset$index == max(plot_subset$index), ]
        hjust_pos <- -0.1
        nudge_x_val <- 0.05 * (max(x_vals) - min(x_vals))
      }

      p <- p + geom_text(data = label_data, aes(label = variable),
                         hjust = hjust_pos, vjust = -0.5, nudge_x = nudge_x_val,
                         size = 3, fontface = "bold", show.legend = FALSE)
    }
    p <- p + coord_cartesian(
      xlim = range(x_vals) + c(-0.05, 0.05) * (max(x_vals) - min(x_vals))
    )

    plot_list[[i]] <- p
  }

  for (p in plot_list) {
    print(p)
  }
  invisible(plot_list)
}

