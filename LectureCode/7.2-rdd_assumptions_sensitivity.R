# RDD Assumptions and Sensitivity

# 0. Setup ------
library(tidyverse)
library(rdd)
library(rdlocrand)

# 1. Simulate Data with Covariates ------
set.seed(831213)
n <- 2000
score <- runif(n, 0, 100)
treat <- ifelse(score >= 50, 1, 0)

## define a small window around the cutoff where covariates are balanced
window <- 5

## age: overall imbalance but balanced within ±window of cutoff
base_age <- rnorm(n, mean = 50, sd = 10)
age <- ifelse(
    abs(score - 50) > window,
    base_age + treat * 5,  # shift for treated outside the window
    base_age               # no shift within the window
)
## income: same design
base_income <- rnorm(n, mean = 50000, sd = 15000)
income <- ifelse(
    abs(score - 50) > window,
    base_income + treat * 10000,
    base_income
)

## Define outcome 
outcome <- 0.5 +
    ifelse(score >= 50,
           0.3 * score,
           0.02 * (score^2)
    ) +
    0.5 * age + 0.01 * income +
    rnorm(n, 0, 5)

df <- tibble(score, treat, outcome, age, income)

# 2. Check Covariate Balance Using boxplots ------
## compute local‐randomization window
win_res <- rdwinselect(
    R           = df$score,
    X           = as.matrix(cbind(df$age, df$income)),
    cutoff      = 50
)
print(win_res)

## extract the selected half‐window
h_lr <- with(win_res, (w_right-w_left) / 2)

## subset data within ± h_lr of cutoff
df_lr <- df %>% 
    filter(abs(score - 50) <= h_lr)

## boxplot of age and income by treatment within local window
df_lr_long <- df_lr %>%
    pivot_longer(
        cols      = c(age, income),
        names_to  = "covariate",
        values_to = "value"
    )

ggplot(df_lr_long, aes(x = factor(treat), y = value, fill = covariate)) +
    geom_boxplot() +
    facet_wrap(~ covariate, scales = "free_y",
               labeller = labeller(covariate = c(age = "Age", income = "Income"))) +
    scale_fill_manual(values = c(age = "lightblue", income = "lightgreen")) +
    labs(
        title = paste0("Covariates by Treatment within ±", round(h_lr,2), " of Cutoff"),
        x     = "Treatment (0 = control, 1 = treated)",
        y     = "Value"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
## boxplot of age and income by treatment across entire sample
df_long <- df %>%
    pivot_longer(
        cols      = c(age, income),
        names_to  = "covariate",
        values_to = "value"
    )
ggplot(df_long, aes(x = factor(treat), y = value, fill = covariate)) +
    geom_boxplot() +
    facet_wrap(~ covariate, scales = "free_y",
               labeller = labeller(covariate = c(age = "Age", income = "Income"))) +
    scale_fill_manual(values = c(age = "lightblue", income = "lightgreen")) +
    labs(
        title = paste0("Covariates by Treatment across Entire Sample"),
        x     = "Treatment (0 = control, 1 = treated)",
        y     = "Value"
    ) +
    theme_minimal() +
    theme(legend.position = "none")


# 3. McCrary Density Test ------
dtest <- DCdensity(df$score, cutpoint=50, htest=TRUE)
print(dtest)

# 5. Density Plot around the cutoff for McCrary test -------
ggplot(df, aes(x = score, fill = factor(treat))) +
    geom_density(alpha = 0.5, color = NA) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "black") +
    scale_fill_manual(
        name   = "Treatment",
        values = c("0" = "lightblue", "1" = "salmon"),
        labels = c("Control (< 50)", "Treated (>= 50)")
    ) +
    labs(
        title = "Density of Running Variable by Treatment Status",
        x     = "Score (Running Variable)",
        y     = "Density"
    ) +
    theme_minimal()

# 6. Simulate data with sorting and check using McCrary test
# Introduce sorting: a fraction of observations just below the cutoff are pushed above
manip_prob <- 0.15
below_cut <- score < 50
to_move   <- below_cut & runif(n) < manip_prob
score[to_move] <- runif(sum(to_move), 50, 51)

treat <- ifelse(score >= 50, 1, 0)
df_sort <- tibble(score = score, treat = treat)

# McCrary density test on manipulated data
dtest_sort <- DCdensity(df_sort$score, cutpoint = 50, htest = TRUE)
print(dtest_sort)

# Density plot to visualize the sorting manipulation
ggplot(df_sort, aes(x = score, fill = factor(treat))) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "black") +
    scale_fill_manual(
        name   = "Treatment",
        values = c("0" = "lightblue", "1" = "salmon"),
        labels = c("Control (<50)", "Treated (>=50)")
    ) +
    labs(
        title = "Density of Running Variable with Sorting Manipulation",
        x     = "Score",
        y     = "Density"
    ) +
    theme_minimal()



