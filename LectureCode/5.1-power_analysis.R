# Power Analysis 

# 0. Setup ------
library(tidyverse)
library(pwr)
library(DeclareDesign)

# 1. Compute Required Sample Size ------
#    args pwr.t.test:
#      d: effect size; power: desired power; sig.level: significance level
#      type: type of test; alternative: one- or two-sided
study <- pwr.t.test(
  d = 0.5, power = 0.8, sig.level = 0.05,
  type = "two.sample", alternative = "two.sided"
)
cat("Sample size per group for 80% power:", ceiling(study$n), "\n")

# 2. Plot Power Curves ------
sizes <- seq(20, 100, 5)
powers <- map_dbl(sizes, ~ pwr.t.test(
  d = 0.5, n = .x, sig.level = 0.05,
  type = "two.sample"
)$power)
df <- tibble(n = sizes, power = powers)
ggplot(df, aes(n, power)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(x = "Sample Size", y = "Power", title = "Power vs. Sample Size") +
  theme_bw()

# 3. Effect-Size Sensitivity ------
#    Vary d and compute power at fixed n=64
effects <- seq(-0.8, 0.8, 0.05)
powers_effect <- map_dbl(effects, ~ pwr.t.test(
  d = .x, n = 64, sig.level = 0.05,
  type = "two.sample"
)$power)
df_effect <- tibble(d = effects, power = powers_effect)
ggplot(df_effect, aes(d, power)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(x = "Effect Size", y = "Power") +
  theme_bw()

# 4. More complex designs -------
# Use DeclareDesign  to simulate more complex designs! (see https://declaredesign.org/getting-started/ for more details!)
##   4.1. Declare the design (stratified randomized experiment ----
##.        with two treatment arms)
N = 100
design <- declare_model(
  N = N,
  e = rnorm(N),
  age = sample(18:80, N, replace = TRUE),
  age_group = cut(age,
                  breaks = c(17, 30, 45, 60, 81),
                  labels = c("17-29", "30-44", "45-59", "60+")),
  potential_outcomes(
    Y ~  0.2 * (D=="1") + e,
    conditions = list(D=c("0", "1")))) +
  
  declare_assignment(
    D = block_ra(blocks = age_group)
  ) +
  
  declare_measurement(
    Y = reveal_outcomes(Y ~ D)
  ) +
  
  declare_inquiry(
    ATE = mean(Y_D_1 - Y_D_0)
  ) +
  
  declare_estimator(
    Y ~ (D=="1"),
    .method = lm_robust, 
    term = 'D == "1"TRUE',
    inquiry = "ATE")

run_design(design)

## 4.2. Redesign the design to let N vary ----
designs_vary_N <- redesign(design, N = seq(100, 1100, 200))

## 4.3. Diagnose the design and plot power curve ----
diagnose_design(designs_vary_N,
                sims = 250) %>% 
  tidy() %>% 
  filter(diagnosand == "power") %>% 
  ggplot(aes(N, estimate)) +
  facet_wrap(~inquiry) +
  geom_line() +
  labs(x="Sample Size", y="Power") +
  theme_bw()
