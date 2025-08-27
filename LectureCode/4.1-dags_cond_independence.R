# DAGs and Conditional Independence in R

# 0. Setup ------
library(tidyverse)
library(dagitty)
library(ggdag)

# 1. Define DAG ------
#    dagitty(): nodes and their connections
#    key args:
#      graph string: string of edges separated by semi-colons
dag <- dagitty('dag { S [e,pos="1,0"]; L [o,pos="4,0"]; T [pos="2.5,0"]; G [pos="2.5,1"]; 
               S -> T; T -> L; G -> {L S}}')

# 2. Visualize DAG ------
#    ggdag(): plots the dagitty object
#    key arg: dag object
p1 <- ggdag(dag) +
  theme_dag()
print(p1)

# 3. Test D-Separation ------
#    dsep(): returns TRUE if x and y are d-separated given z
#    args: dag object, X: source node, Y: target node, Z: optional conditioning node(s)
res1 <- dseparated(dag, X = "S", Y = "L", Z = c("T"))
cat("1) Are S and L independent given T? ", res1, "\n")
res2 <- dseparated(dag, X = "S", Y = "L", Z = c("T", "G"))
cat("2) Are S and L independent given T and G? ", res2, "\n")

# 4. List Adjustment Sets ------
#    adjustmentSets(): finds minimal sets to block confounding
#    args: dag objects, exposure: treatment variable, outcome: outcome variable
adj <- adjustmentSets(dag, exposure = "S", outcome = "L")
print("3) Adjustment sets for unbiased S -> L:")
print(adj)

# 5. Simulate Data and Check ------
#    Demonstrate correlation before and after conditioning on Tar
set.seed(831213)
n <- 500
G <- rbinom(n, 1, 0.3) # G is a confounder
S <- rbinom(n, 1, plogis(0.5 * G)) # S is a function of G
Tar <- rbinom(n, 1, plogis(1.5 * S)) # Tar is a function of S
L <- rbinom(n, 1, plogis(1.2 * Tar + 1.2 * G)) # L is a function of Tar and Genetics
dat <- tibble(S, Tar, L, G)
cor_full <- cor(dat$S, dat$L)
cor_cond1 <- cor(filter(dat, Tar == 0)$S, filter(dat, Tar == 0)$L)
cor_cond2 <- cor(filter(dat, G == 1)$S, filter(dat, G == 1)$L)
cat(
  "4) Correlation S-L overall:", round(cor_full, 2),
  "; when Tar=0:", round(cor_cond2, 2),
  "; when G=1:", round(cor_cond2, 2), "\n"
)


# 6. Simulate Data using Daggity ------
corr_dist <- replicate(1000,
# simulateLogistic(): simulates data from a DAG with binary nodes
#    args: dag object, N: number of samples, 
#          b.upper: upper bound for coefficients, b.lower: lower bound for coefficients
{sim_data <- simulateLogistic(dag, 
  N = 500,
  b.upper = 1.5, b.lower = -1.5
) %>%
  as_tibble() %>%
  mutate( #turn into numeric values
    S = ifelse(S == "1", 1, 0),
    T = ifelse(T == "1", 1, 0),
    L = ifelse(L == "1", 1, 0),
    G = ifelse(G == "1", 1, 0)
  )
  cor(sim_data$S, sim_data$L)
}
)
## 6.1 Plot Correlation Distribution ------
ggplot(tibble(corr_dist), aes(x = corr_dist)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Correlation S-L",
       x = "Correlation", y = "Frequency") +
  theme_minimal()


