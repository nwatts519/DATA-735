# Regression for Causal Inference

# 0. Setup ------
library(tidyverse)
library(interactions)
library(sandwich)
library(lmtest)
library(causalsens) #For data
data("lalonde.exp", package="causalsens")

# 1. Inspect Data ------
glimpse(lalonde.exp)

# 2. Fit Baseline OLS ------
model1 <- lm(re78 ~ treat + age + education + black + hispanic,
             data=lalonde.exp)
## 2.1 Compute robust standard errors
coeftest(model1, vcov = vcovHC(model1, type = "HC0"))


# 3. Compare to simple difference in means ------
with(lalonde.exp,
    mean(re78[treat==1]) - mean(re78[treat==0])
)

# 4. FWL to compute partial coefficient of treatment ------
#    4.1. Regress re78 on covariates
model_y <- lm(re78 ~ age + education + black + hispanic,
              data=lalonde.exp)
#    4.2. Regress treat on covariates
model_t <- lm(treat ~ age + education + black + hispanic,
              data=lalonde.exp)
#    4.3. Get residuals
res_y <- resid(model_y)
res_t <- resid(model_t)
#    4.4. Regress residuals
model_fwl <- lm(res_y ~ res_t)
coeftest(model_fwl, vcov = vcovHC(model_fwl, type = "HC0"))

# 5. Add Interaction ------
#    treat:education for effect heterogeneity
model2 <- lm(re78 ~ treat * education + age + black + hispanic,
             data=lalonde.exp)
coeftest(model2, vcov = vcovHC(model2, type = "HC0"))


# 6. Visualize Interaction ------
#    interact_plot(): visualize interaction effects
interact_plot(model2, pred="education", modx="treat",
              interval=TRUE, robust="HC0")
