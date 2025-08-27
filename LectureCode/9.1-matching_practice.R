# Matching with MatchIt

# 0. Setup  ------
library(tidyverse)
library(MatchIt)
library(CBPS)
library(optmatch)
library(cobalt)
library(causalsens) # for data
data("lalonde.exp", package = "causalsens")
data("lalonde", package = "cobalt")

# 1. Obtain experimental benchmark
experimental_ate <- lm(re78 ~ treat + age + education + black +
                           hispanic,
                       data = lalonde.exp) %>%
    coef() %>%
    pluck("treat") 
cat("Experimental ATE:", experimental_ate, "\n")

# 2. Estimate Propensity Scores using GLM & Match Using OM ------
propensity_formula <- treat ~ age + educ + race + married +
    nodegree + re74 + re75
match_out_glm <- matchit(propensity_formula,
    data = lalonde,
    method = "optimal",
    distance = "glm",
    link = "logit"
)
summary(match_out_glm)

# 3. Propensity Score Distribution Plot ------
plot(match_out_glm, type = "hist")

# 4. Estimate Propensity Scores using CBPS & Match Using OM ------
match_out_cb <- matchit(propensity_formula,
    data = lalonde,
    method = "optimal",
    distance = "cbps",
    estimand = "ATT"
)

summary(match_out_cb)

# 5. Balance Diagnostics ------
love.plot(match_out_glm,
          stars = "raw",
          stats = "m",
          threshold = 0.25)
love.plot(match_out_cb,
          stars = "raw",
          stats = "m",
          thresholds = 0.25)


# 6. Extract Matched Sample ------
matched_df <- match.data(match_out_glm)

# 7. Estimate Treatment Effect ------
effect_adj <- lm(re78 ~ treat + age + educ + race,
                 data = matched_df)
summary(effect_adj)

# 8. Compare to unadjusted estimate
effect_uadj <- lm(re78 ~ treat +age + educ + race,
                 data = lalonde)
summary(effect_uadj)
