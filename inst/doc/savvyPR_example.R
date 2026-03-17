## ----setup, include=TRUE------------------------------------------------------
library(savvyPR)
library(ggplot2)

## -----------------------------------------------------------------------------
# Install the development version from GitHub
# devtools::install_github("Ziwei-ChenChen/savvyPR)
library(savvyPR)
library(MASS)
library(glmnet)

## ----fig.width=7, fig.height=5------------------------------------------------
library(MASS)
library(glmnet)

# Function to create a correlation matrix for X
create_corr_matrix <- function(rho, p) {
  corr_matrix <- diag(1, p)
  for (i in 2:p) {
    for (j in 1:(i-1)) {
      corr_matrix[i, j] <- rho^(abs(i - j))
      corr_matrix[j, i] <- corr_matrix[i, j] # symmetric matrix
    }
  }
  return(corr_matrix)
}

# Function to generate beta values with both positive and negative signs
generate_beta <- function(p) {
  half_p <- ceiling(p / 2)
  beta <- rep(c(1, -1), length.out = p) * rep(1:half_p, each = 2)[1:p]
  return(beta)
}

set.seed(123)
n <- 1500  
p <- 15  
rho <- -0.5  

corr_matrix <- create_corr_matrix(rho, p)
x <- mvrnorm(n = n, mu = rep(0, p), Sigma = corr_matrix)
beta <- generate_beta(p + 1)
sigma_vec <- abs(rnorm(n = n, mean = 15, sd = sqrt(1)))
y <- rnorm(n, mean = as.vector(cbind(1,x)%*%beta), sd = sigma_vec)
  
# 1. Run OLS estimation with intercept
result_ols <- lm(y ~ x)
coef_ols <- coef(result_ols)

# 2. Run Ridge Regression (RR) estimation
result_RR <- glmnet(x, y, alpha = 0, lambda = 1)
coef_RR <- coef(result_RR)

# 3. Run PR estimation (Budget Method)
result_pr_budget <- savvyPR(x, y, method = "budget", val = 0.05, intercept = TRUE)
print(result_pr_budget)
coef_pr_budget <- coef(result_pr_budget)

# 4. Run PR estimation (Target Method)
result_pr_target <- savvyPR(x, y, method = "target", val = 1, intercept = TRUE)
print(result_pr_target)
coef_pr_target <- coef(result_pr_target)

# Calculate the L2 distance to true beta
ols_L2 <- sqrt(sum((beta - coef_ols)^2))
print(paste("OLS L2:", ols_L2))

RR_L2 <- sqrt(sum((beta - coef_RR)^2))
print(paste("Ridge L2:", RR_L2))

pr_budget_L2 <- sqrt(sum((beta - coef_pr_budget)^2))
print(paste("PR Budget L2:", pr_budget_L2))

pr_target_L2 <- sqrt(sum((beta - coef_pr_target)^2))
print(paste("PR Target L2:", pr_target_L2))

## ----fig.width=7, fig.height=5------------------------------------------------
summary(result_pr_budget)
summary(result_pr_target)

## ----basic-plots, error=TRUE, fig.height=5, fig.width=7, fig.alt="Four-panel visualization: The top two plots show estimated regression coefficients for budget and target methods. The bottom two plots display the risk parity distribution, including optimization weights and relative risk contributions across predictors."----
# Plot the estimated coefficients
plot(result_pr_budget, plot_type = "estimated_coefficients", label = TRUE)
plot(result_pr_target, plot_type = "estimated_coefficients", label = FALSE)

# Plot the risk contributions and weights/target variables
plot(result_pr_budget, plot_type = "risk_contributions", label = TRUE)
plot(result_pr_target, plot_type = "risk_contributions", label = FALSE)

## ----fig.width=7, fig.height=5------------------------------------------------
# Cross-validation with Ridge
result_rr_cv <- cv.glmnet(x, y, alpha = 0, folds = 5)
fit_rr1 <- glmnet(x, y, alpha = 0, lambda = result_rr_cv$lambda.min)
coef_rr_cv <- coef(fit_rr1)[,1]

# Cross-validation with model type PR1 (Budget Method)
result_pr_cv1 <- cv.savvyPR(x, y, method = "budget", folds = 5, model_type = "PR1", measure_type = "mse")
coef_pr_cv1 <- coef(result_pr_cv1)

# Cross-validation with model type PR2 (Target Method)
result_pr_cv2 <- cv.savvyPR(x, y, method = "target", folds = 5, model_type = "PR2", measure_type = "mse")
coef_pr_cv2 <- coef(result_pr_cv2)

# Cross-validation with model type PR3 (Budget Method)
result_pr_cv3 <- cv.savvyPR(x, y, method = "budget", folds = 5, model_type = "PR3", measure_type = "mse")
coef_pr_cv3 <- coef(result_pr_cv3)

# Calculate the L2 distance 
print(paste("Ridge CV L2:", sqrt(sum((beta - coef_rr_cv)^2))))
print(paste("PR1 CV (Budget) L2:", sqrt(sum((beta - coef_pr_cv1)^2))))
print(paste("PR2 CV (Target) L2:", sqrt(sum((beta - coef_pr_cv2)^2))))
print(paste("PR3 CV (Budget) L2:", sqrt(sum((beta - coef_pr_cv3)^2))))

## ----fig.width=7, fig.height=5------------------------------------------------
summary(result_pr_cv1)
summary(result_pr_cv2)
summary(result_pr_cv3)

## ----cv-model-plots, error=TRUE, fig.height=5, fig.width=7, fig.alt="Coefficient and risk contribution plots for cross-validated PR1 and PR2 models, illustrating the impact of optimal tuning on model parameters."----
# Plot coefficients and risk contributions for PR1
plot(result_pr_cv1, plot_type = "estimated_coefficients", label = TRUE)
plot(result_pr_cv1, plot_type = "risk_contributions",label = TRUE)

# Plot coefficients and risk contributions for PR2
plot(result_pr_cv2, plot_type = "estimated_coefficients", label = FALSE)
# Cannot plot risk-contribution for PR2 since the tuning parameter val=0 is fixed.
#plot(result_pr_cv2, plot_type = "risk_contributions", label = FALSE)

## ----cv-error-curves, error=TRUE, fig.height=5, fig.width=7, fig.alt="Cross-validation MSE curves for Ridge and PR models. Each plot shows mean squared error against the log of the tuning parameter, with vertical dashed lines marking the optimal values."----
# Plot the cross-validation errors for each model
plot(result_rr_cv)
plot(result_pr_cv1, plot_type = "cv_errors", label = TRUE)
plot(result_pr_cv2, plot_type = "cv_errors")
plot(result_pr_cv3, plot_type = "cv_errors", label = FALSE)

## ----coef-paths, error=TRUE, fig.height=5, fig.width=7, fig.alt="Series of coefficient path plots showing how individual variable estimates evolve as the regularization parameter (lambda) or parity parameter (val) changes."----
# Plot the coefficient paths for cross-validation models
plot(result_pr_cv1, plot_type = "cv_coefficients", xvar = "val", max_vars_per_plot = 10, label = TRUE)

# Show what happens when max_vars_per_plot exceeds the limit (will trigger a warning and reset to 10)
plot(result_pr_cv2, plot_type = "cv_coefficients", xvar = "norm", max_vars_per_plot = 12, label = FALSE)

# PR3 uses dual-optimization, so we can plot against lambda as well
plot(result_pr_cv3, plot_type = "cv_coefficients", xvar = "norm", max_vars_per_plot = 10, label = TRUE)
plot(result_pr_cv3, plot_type = "cv_coefficients", xvar = "lambda", max_vars_per_plot = 10, label = TRUE)
plot(result_pr_cv3, plot_type = "cv_coefficients", xvar = "dev", max_vars_per_plot = 10, label = TRUE)

