
library(tidyverse)
library(grf)
library(ggforce)

# source('predict_fn.R')
data_full <- readRDS('data.rds')
set.seed(30)
X <- data_full$X
Y <- data_full$Y
W <- data_full$W
weights <- data_full$weights
w_hat <- data_full$What
y_hat <- data_full$Yhat
X_id <- data_full$Xid


# Placebo tests


# Placebo
placebo_w <- W[sample.int(length(W))]

placebo_cf <- causal_forest(X, 
                            Y, 
                            placebo_w,
                            sample.weights = weights,
                            num.trees = 10000)

average_treatment_effect(placebo_cf)

# Random income
placebo_y <- Y[sample.int(length(Y))]

randy_cf <- causal_forest(X, 
                          placebo_y,
                          W,
                          sample.weights = weights,
                          num.trees = 10000)



average_treatment_effect(randy_cf)

# Both random
rand_all <- causal_forest(X, 
                          placebo_y,
                          placebo_w,
                          sample.weights = weights,
                          num.trees = 10000)


# Refutation graphs
graph_data <- data.frame(scores_w = get_scores(placebo_cf), 
                         treat = W, 
                         pred_w = placebo_cf$predictions,
                         scores_y = get_scores(randy_cf),
                         income = Y,
                         pred_y = randy_cf$predictions,
                         rand_y = placebo_y,
                         rand_w = placebo_w,
                         w_hat = placebo_cf$W.hat,
                         y_hat = placebo_cf$Y.hat,
                         scores_all_rand = get_scores(rand_all),
                         pred_all_rand = get_scores(rand_all)
                         )

ggplot(data = graph_data, aes(x = income, y = pred_y)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(aes(y = scores_y), se = FALSE, color ="red") +
  labs(x = "Actual income", y = "Predicted treatment effects") +
  scale_x_continuous(limits = c(0,3e+05)) +
  scale_y_continuous(limits = c(-10000, 10000)) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

ggsave("figures/fig7a.png")

ggplot(data = graph_data, 
       aes(x = treat, y = pred_y)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(aes(y=scores_y), fun=mean, geom="point", color ="red", size=3.5) +
  labs(x = "Actual income", y = "Predicted treatment effects") +
  scale_y_continuous(limits = c(-10000, 10000)) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

ggsave("figures/fig7b.png")

ggplot(data = graph_data, aes(x = income, y = pred_w)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(aes(y = scores_w), se = FALSE, color ="red") +
  labs(x = "Actual income", y = "Predicted treatment effects") +
  scale_x_continuous(limits = c(0,3e+05)) +
  scale_y_continuous(limits = c(-10000, 10000)) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

ggsave("figures/fig7c.png")

ggplot(data = graph_data, 
       aes(x = treat, y = pred_w)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(aes(y=scores_w), fun=mean, geom="point", color ="red", size=3.5) +
  labs(x = "Actual income", y = "Predicted treatment effects") +
  scale_y_continuous(limits = c(-10000, 10000)) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

ggsave("figures/fig7d.png")


# All random
ggplot(data = graph_data, aes(x = income, y = pred_all_rand)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(aes(y = scores_all_rand), se = FALSE, color ="red") +
  labs(x = "Actual income", y = "Predicted treatment effects") +
  scale_x_continuous(limits = c(0,3e+05)) +
  scale_y_continuous(limits = c(-10000, 10000)) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

ggsave("figures/fig7e.png")

ggplot(data = graph_data, 
       aes(x = treat, y = pred_all_rand)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(aes(y=scores_all_rand), fun=mean, geom="point", color ="red", size=3.5) +
  labs(x = "Actual income", y = "Predicted treatment effects") +
  scale_y_continuous(limits = c(-10000, 10000)) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

ggsave("figures/fig7f.png")


# Supplemental plots to diagnose misfit (actual vs randomised)

ggplot(data = graph_data, aes(x = treat, y = abs(treat - data_full$What))) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun=mean, geom="point", aes(group=1), color ="red", size=3.5) +
  labs(x = "Treatment value", y = "Absolute loss")

ggsave('figures/sup1_fit_w.png')

ggplot(data = graph_data, aes(x = income, y = abs(income - data_full$Yhat))) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(se = FALSE, color ="red") +
  scale_x_continuous(limits = c(0,3e+05)) +
  scale_y_continuous(limits = c(0,3e+05))
  labs(x = "Income", y = "Absolute loss")

ggsave('figures/sup2_fit_y.png')
