# Sensitivity Analysis for Causal Effects using sensemakr

# 0. Setup ------
set.seed(831213)                  
library(tidyverse)              
library(broom)                  
library(sensemakr)             

# 1. Simulate observational data ------
# Covariate X1 (confounder), U (unmeasured confounder)
N <- 500
X1 <- rnorm(N, mean = 50, sd = 10)
U  <- rnorm(N, mean = 0, sd = 1)  # unobserved

# Treatment D influenced by X1 and U
logit_p <- -0.5 + 0.004 * (X1 - mean(X1)) + 0.5 * U
p_D <- plogis(logit_p)
D    <- rbinom(N, size = 1, prob = p_D)

# Outcome Y influenced by D, X1, and U
Y <- 100 + 5 * D + 0.6 * (X1 - mean(X1)) + 0.5 * U + rnorm(N, 0, 5)

df <- tibble(Y, D, X1)

# 2. Naive estimation ------
model_naive <- lm(Y ~ D + X1, data = df)
summary(model_naive)
# Extract coefficient on D
beta_hat <- coef(model_naive)["D"]
se_beta  <- summary(model_naive)$coefficients["D","Std. Error"]

# 3. Sensitivity analysis with sensemakr ------
## 3.1 Basic sensemakr analysis
sens_basic <- sensemakr(model = model_naive,
                        treatment = "D",
                        benchmark_covariates = "X1",
                        kd = 0.5,   # confounder strength relative to X1
                        q = 1)    # target effect size: 1*beta_hat

## 3.2. Summary of robustness
print(sens_basic)
plot(sens_basic)


# 4. Reporting ------
rv <- robustness_value(model_naive, covariates = "X1")
cat("Robustness value (R^2 of confounder with outcome):", rv, "\n")

