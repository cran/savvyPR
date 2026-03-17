# `savvyPR`: Parity Regression Model for Linear Estimation

The `savvyPR` package implements Parity Regression (PR), a novel regularization framework designed to distribute prediction error equilibrium across model parameters. This methodology is particularly effective for high-dimensional data characterized by substantial noise and high multicollinearity, such as financial time series with structural changes or evolving trends.

This package is based on the theoretical foundations and algorithms discussed in:

Asimit, V., Chen, Z., Ichim, B., & Millossovich, P. (2026). [*Parity Regression Estimation*](https://openaccess.city.ac.uk/id/eprint/37017/).

The official documentation site is available at: <https://ziwei-chenchen.github.io/savvyPR/>

## Installation Guide

You can install the development version of `savvyPR` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("Ziwei-ChenChen/savvySR")
```

Once installed, load the package:

``` r
library(savvySR)
```

## Core Methodology

Unlike standard shrinkage methods like Ridge or Lasso, Parity Regression ensures that no single predictor dominates the model's risk profile. It optimizes objective functions using two distinct parameterizations:

### 1. Budget-based ($c$-tuning)

The $c$-parameterization explicitly allocates a fixed loss contribution to each predictor. The objective function is defined as:

$$\frac{1}{2} RRSS(x, x_{p+1}; \lambda) - \tilde{\mu} \left( c \sum_{k=0}^{p} \log(\delta_k x_k) + (1 - (p + 1)c) \log(x_{p+1}) \right)$$

To ensure positive risk allocation, $c$ is constrained by $0 \le c < 1/(p+1)$.

### 2. Target-based ($t$-tuning)

The $t$-parameterization treats $t$ as the relative elasticity weight for the target variable (response):

$$\frac{1}{2} RRSS(x, x_{p+1}; \lambda) - \tilde{\mu} \left( \sum_{k=0}^{p} \log(\delta_k x_k) + t \log(x_{p+1}) \right)$$

## Features

-   **Three Model Types:**

    -   `PR1`: Pure parity optimization (fixing $\lambda=0$).

    -   `PR2`: Parity optimization using a fixed $\lambda$ derived from Ridge regression via `cv.glmnet`.

    -   `PR3`: Dual-stage optimization that tunes both the parity parameter and $\lambda$.

-   **Built-in Cross-Validation:** Automated selection of optimal $c$, $t$, or $\lambda$ using `cv.savvyPR`.

-   **Rich Visualizations:** Support for plotting estimated coefficients, risk contributions, coefficient paths, and CV error curves.

## Basic Usage

``` r
# Generate highly correlated data
set.seed(123)
n <- 100; p <- 10
base <- rnorm(n)
x <- matrix(rnorm(n * p, sd = 0.1), n, p) + base
y <- as.numeric(x %*% rep(1, p) + rnorm(n, sd = 0.5))

# Fit a cross-validated PR3 model (Budget method)
cv_fit <- cv.savvyPR(x, y, method = "budget", model_type = "PR3")
coef(cv_fit)

# Plot the results
plot(cv_fit, plot_type = "estimated_coefficients")
plot(cv_fit, plot_type = "risk_contributions")
```

## Authors

-   Ziwei Chen – [ziwei.chen.3\@citystgeorges.ac.uk](mailto:ziwei.chen.3@citystgeorges.ac.uk)

-   Vali Asimit – [asimit\@citystgeorges.ac.uk](mailto:asimit@citystgeorges.ac.uk)

-   Pietro Millossovich – [Pietro.Millossovich.1\@citystgeorges.ac.uk](mailto:Pietro.Millossovich.1@citystgeorges.ac.uk)

## License

This package is licensed under the GPL (\>= 3) License.
