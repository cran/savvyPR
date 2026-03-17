utils::globalVariables(c("Variable", "Value"))

#' @keywords internal
plotRiskContr <- function(orp_fit, label = TRUE, method = "budget", ...) {
  opt_vars <- if (method == "budget") orp_fit$weights else orp_fit$x
  var_title <- if (method == "budget") "Weights" else "Target Variables (x)"

  if (any(is.na(opt_vars)) || any(is.na(orp_fit$relativeRiskContrib))) {
    warning(paste("Cannot generate risk contributions plot: optimization variables or 'relativeRiskContrib' contains NA values."))
  } else {
    n_covariates <- length(opt_vars) - 1
    var_names <- c(paste0("V", seq_len(n_covariates)), "Y")

    df_vars <- data.frame(
      Variable = factor(var_names, levels = var_names),
      Value = opt_vars
    )

    df_risk <- data.frame(
      Variable = factor(var_names, levels = var_names),
      Value = orp_fit$relativeRiskContrib
    )

    p1 <- ggplot(df_vars, aes(x = Variable, y = Value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Optimization Output -", var_title), x = "Variables", y = var_title) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      ) +
      ylim(0, max(df_vars$Value, na.rm = TRUE) * 1.2)

    p2 <- ggplot(df_risk, aes(x = Variable, y = Value)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      theme_minimal() +
      labs(title = "Risk Contributions", x = "Variables", y = "Relative Risk Contributions") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      ) +
      ylim(0, max(df_risk$Value, na.rm = TRUE) * 1.2)

    if (label) {
      p1 <- p1 + geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 3, fontface = "bold", color = "black")
      p2 <- p2 + geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 3, fontface = "bold", color = "black")
    }

    p_combined <- grid.arrange(p1, p2, nrow = 2)
    invisible(p_combined)
  }
}

