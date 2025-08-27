# Visualizing Panel Data Effects

# 0. Setup ------
library(tidyverse)
library(fect)
library(panelView)
library(causalsens) #for data
data("lalonde.exp", package = "causalsens")

# 1. Create Panel Data ------
panel <- lalonde.exp %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = starts_with("re"),
               names_to = "year",
               values_to = "earnings") %>%
  mutate(year = dplyr::recode(year,
                       re74 = 1,
                       re75 = 2,
                       re78 = 3)) %>% 
  mutate(treat = ifelse(year < 3, 0, treat)) 

# 2. Create Treatment Plot ------
panelview(panel,
          index   = c("id", "year"),
          Y       = "earnings",
          D       = "treat",
          xlab    = "Time",
          ylab    = "Earnings") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "red") +
  theme_minimal()

# 3. Estimate Dynamic Effects ------
fe_res <- fect(earnings  ~ treat,
               data  = panel,
               Y = "earnings",
               D = "treat",
               index = c("id","year"),
               seed  = 831213,
               se    = TRUE)
fe_res

# 4. Effects plot ------
plot(fe_res)

# 6. Cumulative Effects Plot ------
cum_df <- fect:::att.cumu(fe_res, 
                          period = c(0,2),
                          plot = TRUE)
