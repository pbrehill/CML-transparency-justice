
library(tidyverse)
library(grf)
library(ggforce)

source('predict_fn.R')
data_full <- readRDS('data.rds')
set.seed(30)



X <- data_full$X
Y <- data_full$Y
W <- data_full$W
weights <- data_full$weights
w_hat <- data_full$What
y_hat <- data_full$Yhat
X_id <- data_full$Xid




# 10000 tree causal forest for benchmark
t100000 <- causal_forest(X_id, 
                         Y, 
                         W,
                         Y.hat = y_hat,
                         W.hat = w_hat,
                         sample.weights = weights,
                         num.trees = 100000)

# 10000 tree causal forest for benchmark
t10000 <- causal_forest(X_id, 
                        Y, 
                        W,
                        Y.hat = y_hat,
                        W.hat = w_hat,
                        sample.weights = weights,
                        num.trees = 10000)

# 1000 tree causal forest
t1000 <- causal_forest(X_id, 
                       Y, 
                       W,
                       Y.hat = y_hat,
                       W.hat = w_hat,
                       sample.weights = weights,
                       num.trees = 1000)

# 100 tree causal forest
t100 <- causal_forest(X_id, 
                      Y, 
                      W,
                      Y.hat = y_hat,
                      W.hat = w_hat,
                      sample.weights = weights,
                      num.trees = 100)

# 10 tree causal forest
t10 <- causal_forest(X_id, 
                     Y, 
                     W,
                     Y.hat = y_hat,
                     W.hat = w_hat,
                     sample.weights = weights,
                     num.trees = 10)

# 2 tree causal forest
t1 <- causal_forest(X, 
                    Y, 
                    W,
                    Y.hat = y_hat,
                    W.hat = w_hat,
                    sample.weights = weights,
                    num.trees = 1,
                    ci.group.size = 1)


# # Causal tree with unlimited depth
# t1 <- causal_forest(t10000 %>% get_tree(70), 
#                          y_hat, 
#                          w_hat,
#                          sample.weights = data_s$uhhwte)
# 
# # Causal tree with minsize = 50
# t1_lim <- causal_forest(X, 
#                          Y %>% missMethods::impute_median() %>% pull(), 
#                          W %>% missMethods::impute_median() %>% pull,
#                          Y.hat = y_hat,
#                          W.hat = w_hat,
#                          sample.weights = data_s$uhhwte,
#                          num.trees = 10000)





losses <- data.frame(
  t10000 = abs((t100000$prediction - t10000$predictions) / t100000$prediction),
  t1000 = abs((t100000$prediction - t1000$predictions) / t100000$prediction),
  t100 = abs((t100000$prediction - t100$predictions) / t100000$prediction),
  t10 = abs((t100000$prediction - t10$predictions) / t100000$prediction),
  t1 = abs((t100000$prediction - t1$predictions) / t100000$prediction)
)



losses %>%
  mutate_all(abs) %>%
  gather("Number of trees", "Absolute loss (as percentage of estimate)") %>%
  mutate(`Number of trees` = factor(`Number of trees`,
                                    levels = c("t1", "t10", "t100", "t1000", "t10000"),
                                    labels = c("1", "10", "100", "1000", "10000"))) %>%
  ggplot(aes(x = `Number of trees`, y = `Absolute loss (as percentage of estimate)`, group = 1)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun=mean, geom="line", color ="red", alpha = 0.6, size=2) +
  stat_summary(fun=mean, geom="point", color ="red", size=4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 100)) +
  facet_zoom(ylim = c(0, 1.5))

ggsave('figures/rashomon_main.png')

# ggsave('loss_graph.png')

losses %>%
  mutate_all(abs) %>%
  gather("Number of trees", "Absolute loss") %>%
  mutate(`Number of trees` = factor(`Number of trees`,
                                    levels = c("t1", "t10", "t100", "t1000", "t10000"),
                                    labels = c("1", "10", "100", "1000", "10000"))) %>%
  ggplot(aes(x = `Number of trees`, y = `Absolute loss`)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun=mean, geom="line", aes(group=1), color ="red", size=2)


## Nusiance Rashomon


# Identifying models
# y_hat100000 = regression_forest(X, Y%>% missMethods::impute_median() %>% pull(), num.trees = 100000)
# w_hat100000 = regression_forest(X, W%>% missMethods::impute_median() %>% pull(), num.trees = 100000)

y_hat10000 = regression_forest(X_id, Y, num.trees = 10000, honesty = FALSE)
w_hat10000 = regression_forest(X_id, W, num.trees = 10000, honesty = FALSE)

y_hat1000 = regression_forest(X_id, Y, num.trees = 1000, honesty = FALSE)
w_hat1000 = regression_forest(X_id, W, num.trees = 1000, honesty = FALSE)

y_hat100 = regression_forest(X_id, Y, num.trees = 100, honesty = FALSE)
w_hat100 = regression_forest(X_id, W, num.trees = 100, honesty = FALSE)

y_hat10 = regression_forest(X_id, Y, num.trees = 10, honesty = FALSE)
w_hat10 = regression_forest(X_id, W, num.trees = 10, honesty = FALSE)

y_hat1 = regression_forest(X_id, Y, num.trees = 1, ci.group.size = 1, honesty = FALSE)
w_hat1 = regression_forest(X_id, W, num.trees = 1, ci.group.size = 1, honesty = FALSE)




# 10000 tree causal forest for benchmark
# n100000 <- causal_forest(X_id,
#                          Y %>% missMethods::impute_median() %>% pull(),
#                          W %>% missMethods::impute_median() %>% pull,
#                          Y.hat = y_hat100000$predictions %>% unlist(),
#                          W.hat = w_hat100000$predictions %>% unlist(),
#                          sample.weights = data_s$uhhwte,
#                          num.trees = 10000)

# 10000 tree causal forest for benchmark
n10000 <- causal_forest(X_id, 
                        Y, 
                        W,
                        Y.hat = predict(y_hat10000, X_id)$predictions,
                        W.hat = predict(w_hat10000, X_id)$predictions,
                        sample.weights = weights,
                        num.trees = 10000)

# 1000 tree causal forest
n1000 <- causal_forest(X_id, 
                       Y, 
                       W,
                       Y.hat = predict(y_hat1000, X_id)$predictions,
                       W.hat = predict(w_hat1000, X_id)$predictions,
                       sample.weights = weights,
                       num.trees = 10000)

# 100 tree causal forest
n100 <- causal_forest(X_id, 
                      Y, 
                      W,
                      Y.hat = predict(y_hat100, X_id)$predictions,
                      W.hat = predict(w_hat100, X_id)$predictions,
                      sample.weights = weights,
                      num.trees = 10000)

# 10 tree causal forest
n10 <- causal_forest(X_id, 
                     Y, 
                     W,
                     Y.hat = predict(y_hat10, X_id)$predictions,
                     W.hat = predict(w_hat10, X_id)$predictions,
                     sample.weights = weights,
                     num.trees = 10000)

# 2 tree causal forest
n1 <- causal_forest(X_id, 
                    Y, 
                    W,
                    Y.hat = predict(y_hat1, X_id)$predictions,
                    W.hat = predict(w_hat1, X_id)$predictions,
                    sample.weights = weights,
                    num.trees = 10000)




losses_n <- data.frame(
  t1000 = abs((n10000$predictions - n1000$predictions) / n10000$predictions),
  t100 = abs((n10000$predictions - n100$predictions) / n10000$predictions),
  t10 = abs((n10000$predictions - n10$predictions) / n10000$predictions),
  t1 = abs((n10000$predictions - n1$predictions)/ n10000$predictions))



losses_n %>%
  mutate_all(abs) %>%
  gather("Number of trees", "Absolute loss (as percentage of estimate)") %>%
  mutate(`Number of trees` = factor(`Number of trees`,
                                    levels = c("t1", "t10", "t100", "t1000"),
                                    labels = c("1", "10", "100", "1000"))) %>%
  ggplot(aes(x = `Number of trees`, y = `Absolute loss (as percentage of estimate)`, group = 1)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun=mean, geom="line", color ="red", alpha = 0.6, size=2) +
  stat_summary(fun=mean, geom="point", color ="red", size=4) +
  scale_y_continuous(labels = scales::percent) +
  facet_zoom(ylim = c(0, 1))

ggsave('figures/rashomon_nuisance.png')



losses_n %>%
  gather("Number of trees", "Absolute loss") %>%
  mutate(`Number of trees` = factor(`Number of trees`,
                                    levels = c("t1", "t10", "t100", "t1000"),
                                    labels = c("1", "10", "100", "1000"))) %>%
  ggplot(aes(x = `Number of trees`, y = `Absolute loss`)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun=mean, geom="line", aes(group=1), color ="red", size=2) +
  labs(x = "Number of trees", y = "Absolute loss")

ggsave('figures/figure6-bias_graph_n.png')


