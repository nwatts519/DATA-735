# Simulating Potential Outcomes & Treatment Effects

# 0. Setup ------
set.seed(831213)                
library(tidyverse)            
library(gridExtra)            

# 1. Define population size ------
N <- 1000                     

# 2. Simulate a pre-treatment covariate: baseline earnings ------
X <- rnorm(N, mean = 50, sd = 10)  

# 3. Specify potential outcomes ------
## 3.1. Under control (no training)
epsilon0 <- rnorm(N, mean = 0, sd = 20)
Y0 <- 200 + 1.2 * X + epsilon0

## 3.2 Additive individual treatment effect
tau_add <- 20 + 0.5 * (X - mean(X))
Y1_add <- Y0 + tau_add

## 3.3 Multiplicative individual treatment effect
factor_mult <- 2 + 0.02 * (X - mean(X))
Y1_mult <- Y0 * factor_mult
# Define multiplicative effect as a ratio
tau_mult <- Y1_mult / Y0

# 4. Assemble the full potential-outcomes table ------
df_pot <- tibble(
  id = 1:N,
  X = X,
  Y0 = Y0,
  tau_add = tau_add,
  Y1_add = Y1_add,
  tau_mult = tau_mult,
  Y1_mult = Y1_mult
)

# 5. Peek at the first few rows ------
df_pot %>%
  select(id, X, Y0, Y1_add, tau_add, Y1_mult, tau_mult) %>%
  slice(1:5) %>%
  print()

# 6. Random assignment to treatment ------
df_obs <- df_pot %>%
  mutate(
    D = rbinom(N, size = 1, prob = 0.5),
    Y_add = if_else(D == 1, Y1_add, Y0),
    Y_mult = if_else(D == 1, Y1_mult, Y0)
  )

# 7. Estimate the Average Treatment Effect (ATE) ------
## 7.1. True ATE / AVERAGE TREATMENT RATIO (simulation)
true_ATE_add  <- mean(df_pot$tau_add)
true_ATE_mult <- mean(df_pot$tau_mult)
cat("True ATE (additive):", round(true_ATE_add, 2), "\n")
cat("True mean ratio effect (multiplicative):", round(true_ATE_mult, 2), "\n")

## 7.2. Difference-in-Means estimator (on the original scale)
est_ATE_add  <- df_obs %>% 
  filter(D == 1) %>%
  summarize(mean(Y_add)) %>%
  pull() -
  df_obs %>% 
  filter(D == 0) %>% 
  summarize(mean(Y_add)) %>%
  pull()
est_ATE_mult <- df_obs %>%
  filter(D == 1) %>%
  summarize(mean(Y_mult)) %>%
  pull() -
  df_obs %>% 
  filter(D == 0) %>%
  summarize(mean(Y_mult)) %>%
  pull()
cat("Estimated ATE (additive):", round(est_ATE_add, 2), "\n")
cat("Estimated ATE (multiplicative):", round(est_ATE_mult, 2), "\n")

# 8. Repeat random assignment to see sampling variability ------
R <- 500
ate_estimates_add  <- replicate(R, {
  D_r <- rbinom(N, size = 1, prob = 0.5)
  Y_r <- if_else(D_r == 1, Y1_add, Y0)
  mean(Y_r[D_r == 1]) - mean(Y_r[D_r == 0])
})
ate_estimates_mult <- replicate(R, {
  D_r <- rbinom(N, size = 1, prob = 0.5)
  Y_r <- if_else(D_r == 1, Y1_mult, Y0)
  mean(Y_r[D_r == 1]) - mean(Y_r[D_r == 0])
})

mean_add <- mean(ate_estimates_add)
sd_add   <- sd(ate_estimates_add)
mean_mult <- mean(ate_estimates_mult)
sd_mult   <- sd(ate_estimates_mult)
cat("Sampling Additive: mean=", round(mean_add, 2), "; SD=", round(sd_add, 2), "\n")
cat("Sampling Multiplicative: mean=", round(mean_mult, 2), "; SD=", round(sd_mult, 2), "\n")

# 9. Plot the sampling distributions ------
tibble(estimate = ate_estimates_add) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 30, fill = "grey80", color = "white") +
  geom_vline(xintercept = true_ATE_add, color = "red", size = 1) +
  labs(title = "Sampling Distribution of ATE (Additive)", x = "Estimated ATE", y = "Count") +
  theme_minimal()

tibble(estimate = ate_estimates_mult) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 30, fill = "grey80", color = "white") +
  geom_vline(xintercept = true_ATE_mult, color = "blue", size = 1) +
  labs(title = "Sampling Distribution of ATE (Multiplicative)", x = "Estimated ATE", y = "Count") +
  theme_minimal()

# 10. Use slightly better, downward biased estimator for multiplicative effects ------
ate_estimates_geom <- replicate(R, {
  D_r <- rbinom(N, size = 1, prob = 0.5)
  Y_r <- if_else(D_r == 1, Y1_mult, Y0)
  exp(mean(log(Y_r[D_r == 1])) - mean(log(Y_r[D_r == 0])))
})

tibble(estimate = ate_estimates_geom) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 30, fill = "grey80", color = "white") +
  geom_vline(xintercept = true_ATE_mult, color = "blue", size = 1) +
  labs(title = "Sampling Distribution of Geometric Mean Estimator", x = "Estimated ATE", y = "Count") +
  theme_minimal()

# 11. Geometric mean unbiased if multiplicative effects are constant ------
Y1_mult_const <- Y0 * 2
ate_estimates_geom_const <- replicate(R, {
  D_r <- rbinom(N, size = 1, prob = 0.5)
  Y_r <- if_else(D_r == 1, Y1_mult_const, Y0)
  exp(mean(log(Y_r[D_r == 1])) - mean(log(Y_r[D_r == 0])))
})

tibble(estimate = ate_estimates_geom_const) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 30, fill = "grey80", color = "white") +
  geom_vline(xintercept = 2, color = "blue", size = 1) +
  labs(title = "Sampling Distribution of Geometric Mean Estimator (constant effect)", x = "Estimated ATE", y = "Count") +
  theme_minimal()
