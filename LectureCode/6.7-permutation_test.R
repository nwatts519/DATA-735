# Permutation Test on Lalonde Experimental Data

# 0. Load Libraries & Data ------
library(tidyverse)
library(causalsens) #for data
data("lalonde.exp", package="causalsens")

# 1. Prepare Data ------
df <- lalonde.exp %>%
  mutate(
    treat = factor(treat, labels = c("control", "treatment"))
  )

# 2. Observed Difference in re78 ------
obs_diff <- df %>%
  group_by(treat) %>%
  summarize(mean_re78 = mean(re78, na.rm = TRUE)) %>%
  pull(mean_re78) %>%
  diff()
cat("Observed re78 difference (treatment - control):", obs_diff, "\n")

# 3. Permutation Distribution ------
set.seed(831213)
B <- 5000
diffs_h0 <- replicate(B, {
  df_perm <- df %>% 
    mutate(treat = sample(treat)) %>% 
    group_by(treat) %>%
    summarize(mean_re78 = mean(re78, na.rm = TRUE)) %>% #Impute values under sharp
    pull(mean_re78) %>%
    diff()
})

# 4. Compute p-value ------
pval <- mean(abs(diffs_h0) >= abs(obs_diff))
cat("Permutation p-value:", pval, "\n")

# 5. Plot Distribution Under The Sharp Null ------
tibble(diffs_h0) %>%
  ggplot(aes(x = diffs_h0)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  geom_vline(xintercept = obs_diff, linetype = "dashed", color = "red") +
  labs(
    title = "Null Distribution of re78 Difference",
    x = "Permuted Difference in Mean re78",
    y = "Frequency"
  ) +
  theme_bw()

# 6. Threshold Exploration ------
alphas <- c(0.01, 0.05, 0.1)
rej_rates <- map_dbl(alphas,
                     ~ mean(abs(diffs_h0) >= quantile(abs(diffs_h0), 1 - .x)))
tibble(alpha = alphas, rejection_rate = rej_rates) %>%
  print()
