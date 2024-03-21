library(tidyverse)
library(grf)
library(ggforce)
library(kableExtra)

data_full <- readRDS('data.rds')
set.seed(30)
X <- data_full$X
Y <- data_full$Y
W <- data_full$W
weights <- data_full$weights
w_hat <- data_full$What
y_hat <- data_full$Yhat
X_id <- data_full$Xid

main_cf <- causal_forest(X, Y, W, y_hat, w_hat, num.trees = 1000, sample.weights = weights)

# ATE

ate_est <- average_treatment_effect(main_cf)

data.frame(est = names(ate_est), val = ate_est) %>% write_csv('estimate.csv')

# Variable importance

main_vi <- variable_importance(main_cf)
main_vi <- data.frame(importance = main_vi, name = names(X_id) %>% str_wrap(80))
ggplot(main_vi %>% arrange(desc(importance)) %>% head(20), 
       aes(x = importance, y = reorder(name, importance))) +
  geom_col()

ggsave('figures/varimp.png')


# Variables for appendix
main_vi %>%
  select(name, importance) %>%
  mutate(name = name %>% str_wrap(60)) %>%
  rename(Name = name, `Variable importance` = importance) %>%
  kable(format = 'latex', booktabs = TRUE) %>%
  cat(file = "latex_table.txt")
