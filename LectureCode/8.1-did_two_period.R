#Two-Period Difference-in-Differences

# 0. Setup ------
library(tidyverse)
library(plm)
library(fect)
library(causalsens)
data("lalonde.exp", package="causalsens")

# 1. Create Panel Data ------
df_pre <- lalonde.exp %>% 
  select(treat, re74) %>%
  rename(earn=re74) %>%
  mutate(time=1,
         ever_treated = treat,
         treat=0)

df_post <- lalonde.exp %>% 
  select(treat, re78) %>% 
  mutate(ever_treated=treat) %>% 
  rename(earn=re78) %>%
  mutate(time=2)

panel <- bind_rows(df_pre, df_post) %>% 
  mutate(id=rep(1:nrow(lalonde.exp),2)) %>% 
  mutate(ever_treated = factor(ever_treated),
         id = factor(id))

# 2. Plot Pre/Post Trends ------
ggplot(panel, aes(x=time, y=earn, color=ever_treated)) +
  geom_smooth(alpha=0.1, method="lm") +
  labs(title="Pre-/Post- Earnings by Group",
       x    ="Time",
       y    ="Earnings",
       color="Treatment") +
  theme_bw()

# 3. Fit DiD Estimation using plm ------
plm.data <- pdata.frame(panel, index=c("id", "time"))
did_lm <- plm(earn ~ treat,
              data   = plm.data,
              model  = "within",
              effect = "twoway")
summary(did_lm)

# 4. fect Fixed Effects ------
fe_res <- fect(earn  ~ treat,
               data  = panel,
               Y = "earn",
               D = "treat",
               index = c("id","time"),
               seed  = 831213,
               se    = TRUE)
fe_res

# 5. Effect Plot ------
plot(fe_res, count=FALSE, return.test=TRUE)

