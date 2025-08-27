# Upper Confidence Bound (UCB1) Algorithm

# 1. Load Libraries ------
library(tidyverse)

# 2. Define True Rates & Initialize ------
set.seed(831213)
rates <- c(0.05, 0.1, 0.15)
n_rounds <- 10000
counts <- rep(0, length(rates))
rewards <- rep(0, length(rates))
history <- tibble(round=integer(), chosen=integer(), reward=integer())

# 3. Run UCB1 Algorithm ------
for(t in 1:n_rounds){
  if(t <= length(rates)){
    arm <- t
  } else {
    ucbs <- rewards/counts + sqrt(2*log(t)/counts)
    arm <- which.max(ucbs)
  }
  click <- rbinom(1,1,rates[arm])
  counts[arm] <- counts[arm] + 1
  rewards[arm] <- rewards[arm] + click
  history <- history %>%
    add_row(round=t, chosen=arm, reward=click)
}

# 4. Cumulative Reward Plot ------
hist_cum <- history %>%
  mutate(cum=cumsum(reward))
ggplot(hist_cum, aes(round,cum)) +
  geom_line() +
  labs(title="UCB1 Cumulative Reward",
       x="Round",
       y="Cumulative Clicks") +
  theme_bw()

# 5. Random Allocation Benchmark ------
set.seed(831213)
random_history <- tibble(round=1:n_rounds,
                         reward=rbinom(n_rounds,1,sample(rates,n_rounds,replace=TRUE))) %>%
                  mutate(cum=cumsum(reward))
ggplot(random_history, aes(round,cum)) +
  geom_line() +
  labs(title="Random Allocation Cumulative Reward") +
  theme_bw()

# 6. Compare Cumulative Regret ------
opt_rate <- max(rates)
tibble(round=1:n_rounds,
       UCB=hist_cum$cum,
       Random=random_history$cum) %>%
  pivot_longer(-round,
               names_to="strategy",
               values_to="cumulative") %>% 
  group_by(strategy) %>%
  mutate(regret = round*(opt_rate) - cumulative) %>% 
  ggplot(aes(round, regret, color=strategy)) +
  geom_line() +
  labs(title="Cumulative Regret by Strategy", x="Round", y="Regret") +
  theme_bw()

