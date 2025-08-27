# Weighting Estimators with WeightIt

# 1. Load Libraries & Data ------
library(tidyverse)
library(WeightIt)
library(cobalt)
data("lalonde", package="MatchIt")

# 2. Compute IPTW Weights ------
propensity_formula <- treat ~ age + educ + race +
        married + nodegree + re74 + re75

w_out <- weightit(propensity_formula,
                  data=lalonde,
                  method="cbps",
                  estimand="ATT")

# 3.Check Weight Distribution ------
hist(w_out$weights,
     main="IPTW Weights Distribution",
     xlab="Weights",
     breaks=50,
     col="lightblue",
     border="black")


# 4. Check Covariate Balance ------
bal.tab(w_out, un=TRUE)

# 5. Weighted Outcome Regression ------
lalonde$weights <- w_out$weights
wt_mod <- lm(re78 ~ treat, data=lalonde,
             weights=weights)
summary(wt_mod)

# 6. Weight Trimming Demonstration ------
w_trim <- trim(w_out, at=0.1)
hist(w_trim$weights,
     main="IPTW Weights Distribution, Trimmed",
     xlab="Weights",
     breaks=50,
     col="lightblue",
     border="black")
lalonde$weights2 <- w_trim$weights
wt_mod2 <- lm(re78 ~ treat, data=lalonde,
              weights=weights2)
summary(wt_mod2)
