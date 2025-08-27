# Simulating Bias induced by Bad Controls

# 0. Setup ------
library(tidyverse)

# 1. Simulate Data ------
set.seed(831213)
N <- 10000
conf <- rnorm(N)
treat <- rbinom(N, 1, plogis(conf))
# true outcome only depends on treat and conf
outcome <- 0.5 + 1.5 * treat + 0.7 * conf + rnorm(N)
# collider is a function of both treatment and outcome
collider <- 0.5 + 0.8 * treat + 0.5 * outcome  + rnorm(N, 0, 0.5)
df <- tibble(conf, treat, collider, outcome)

# 2. Estimate ATE without controlling for collider ------
fit_no_coll <- lm(outcome ~ treat + conf, data = df)
cat("ATE without controlling for collider:", coef(fit_no_coll)["treat"], "\n")

# 3. Estimate ATE when controlling for collider ------
fit_with_coll <- lm(outcome ~ treat + conf + collider, data = df)
cat("ATE controlling for collider:", coef(fit_with_coll)["treat"], "\n")

# 4. Vary Collider Strength ------
alphas <- c(0.1, 0.5, 1, 2)
bias_df <- map_dfr(alphas, function(a) {
  res <- replicate(500,
  { # resimulate collider with varying strength a
  coll2   <- 0.5 + a * treat + a * outcome + rnorm(N, 0, 0.5)
  tmp     <- tibble(conf, treat, collider = coll2, outcome)
  coef(lm(outcome ~ treat + conf + collider, data = tmp))["treat"]
  })
  tibble(bias = mean(res)-1.5, alpha = a)
})

# 5. Plot Bias from Controlling a Collider ------
bias_df %>%
  ggplot(aes(alpha, bias)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Collider Strength",
         y = "Bias") +
    theme_bw()

# 6. Controlling for a child of the collider ----
# create a “child” of the collider (collider + noise)
df <- df %>%
  mutate(child = 1.3 * collider + rnorm(n(), 0, 0.5))

# fit ATE controlling for child
fit_with_child <- lm(outcome ~ treat + conf + child, data = df)
cat("ATE controlling for child of collider:", coef(fit_with_child)["treat"], "\n")

    