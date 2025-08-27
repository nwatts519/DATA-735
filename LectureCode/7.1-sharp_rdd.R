# Sharp Regression Discontinuity Design

# 0. Setup ------
library(tidyverse)
library(rdrobust)

# 1. Simulate Data ------
set.seed(831213)
n <- 2000
score <- runif(n, 0, 100)
treat <- ifelse(score >= 50, 1, 0)
outcome <- 0.5 + ifelse(score>=50,
                        0.3*score,
                        0.02*(score^2))+ rnorm(n,0,5)
df <- tibble(score, treat, outcome)

# 2. Scatter & Smooth ------
ggplot(df, aes(score, outcome, color=factor(treat)))+
  geom_point(alpha=0.3)+
  geom_smooth(method="loess", se=FALSE)+
  geom_vline(xintercept=50, linetype="dashed")+
  labs(title="Outcome vs Score around Cutoff",
       color="Treatment") +
  theme_bw()

# 3. Global Linear Fit ------
model_glob <- lm(outcome ~ score + treat, data=df)
summary(model_glob)

# 4. Local Polynomial RDD ------
#    rdrobust(): y, x, cutoff c
rdd_res <- rdrobust(y=df$outcome, x=df$score, c=50)
summary(rdd_res)

# 5. Bandwidth Selection ------
bw <- rdbwselect(y=df$outcome, x=df$score, c=50)
print(bw)

# 6. Placebo Cutoff Tests ------
for(cut in c(40,60)){
  cat("Placebo cutoff at ", cut, ":\n")
  print(rdrobust(y=df$outcome, x=df$score, c=cut)$ci)
}

# 7. RDD Plot ------
rdplot(y=df$outcome, x=df$score, c=50)

