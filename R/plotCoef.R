utils::globalVariables(c("Coefficient", "Value"))

#' @keywords internal
plotCoef <- function(coefficients, intercept = TRUE, label = TRUE, ...) {
  if (is.null(coefficients)) {
    warning("Coefficients are missing in the result.")
  } else {
    n_coeff <- length(coefficients)

    if (intercept) {
      coeff_names <- c("V0", paste0("V", seq_len(n_coeff - 1)))
    } else {
      coeff_names <- paste0("V", seq_len(n_coeff))
    }

    df <- data.frame(
      Coefficient = factor(coeff_names, levels = coeff_names),
      Value = coefficients
    )

    p <- ggplot(df, aes(x = Coefficient, y = Value)) +
      geom_point(color = "darkcyan", size = 3) +
      geom_line(group = 1, color = "darkcyan") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(title = "Estimated Coefficients", x = "Coefficients", y = "Values") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      )

    if (label) {
      p <- p + geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 3, fontface = "bold")
    }

    print(p)
    return(invisible(p))
  }
}
