# Thompson Sampling for a Multi-Armed Bandit

# 0. Setup  ------
set.seed(831213)                
library(tidyverse)            
library(cowplot)

# 1. Define bandit parameters  ------
K <- 3                        
true_probs <- c(0.3, 0.5, 0.7) # true reward probabilities for each arm
M <- 1000                     

# 2. Initialize prior counts (Beta(1,1) for each arm)  ------
successes <- rep(0, K)       
failures <- rep(0, K)        

# 3. Storage for simulation outcomes  ------
chosen_arm <- integer(M)      
rewards    <- integer(M)      

default_best_prob <- max(true_probs)
regret <- numeric(M)          # instantaneous regret per round

# 4. Thompson Sampling loop  ------
for(t in seq_len(M)) {
  # 4a. Sample theta from posterior Beta(1 + successes, 1 + failures)
  theta_samples <- rbeta(K, 1 + successes, 1 + failures)
  # 4b. Choose arm with highest sampled theta
  arm <- which.max(theta_samples)
  # 4c. Simulate reward from the chosen arm
  reward <- rbinom(1, 1, true_probs[arm])
  # 4d. Update counts
  successes[arm] <- successes[arm] + reward
  failures[arm]  <- failures[arm] + (1 - reward)
  # 4e. Record outcomes
  chosen_arm[t] <- arm
  rewards[t]    <- reward
  # 4f. Compute regret: difference between optimal and selected arm
  regret[t] <- default_best_prob - true_probs[arm]
}

# 5. Summarize results  ------
results <- tibble(
  round       = seq_len(M),
  chosen_arm  = chosen_arm,
  reward      = rewards,
  cumulative_reward = cumsum(rewards),
  cumulative_regret = cumsum(regret)
)

# Final performance metrics
total_reward <- sum(rewards)
total_regret <- sum(regret)
cat("Total reward after", M, "rounds:", total_reward, "\n")
cat("Total regret after", M, "rounds:", round(total_regret, 2), "\n")

# 6. Plotting  ------
# 6a. Cumulative reward over time
p1 <- results %>%
  ggplot(aes(x = round, y = cumulative_reward)) +
  geom_line(size = 1) +
  labs(y = "Cumulative Reward") +
  theme_minimal() +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank()
  ) 

# 6b. Cumulative regret over time
p2 <- results %>%
  ggplot(aes(x = round, y = cumulative_regret)) +
  geom_line(color = "red", size = 1) +
  labs(y = "Cumulative Regret") +
  theme_minimal() +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank()
  )

# 6c. Proportion of times optimal arm is selected
optimal_arm <- which.max(true_probs)
results <- results %>%
  mutate(optimal_selected = (chosen_arm == optimal_arm)) %>%
  mutate(prop_optimal = cumsum(optimal_selected)/round)

p3 <- results %>%
  ggplot(aes(x = round, y = prop_optimal)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(x = "Round",
       y = "Proportion Optimal") +
  theme_minimal()

# Arrange three plots vertically
plot_grid(p1, p2, p3, ncol = 1, align = "v", axis="lr")
