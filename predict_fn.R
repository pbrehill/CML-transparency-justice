library(tidyverse)
library(causalTree)
library(grf)


predict_grf_tree <- function(tree, data, Y, W) {
  assignments <- c()
  
  names(data) <- paste0('v', 1:ncol(data))
  
  # Run through to assign leaf
  for (i in 1:nrow(data)) {
    assigned <- 1
    cur_node <- tree$nodes[[assigned]]
    while(!cur_node$is_leaf) {
      if (data[i, cur_node$split_variable] <= cur_node$split_value) {
        assigned <- cur_node$left_child
      } else {
        assigned <- cur_node$right_child
      }
      cur_node <- tree$nodes[[assigned]]
    }
    assignments <- c(assignments, assigned)
  }
  
  data$assignments_ <- assignments
  data$Y <- Y
  data$W <- W
  est_sample <- !(1:nrow(data) %in% stree$drawn_samples)
  apes <- data[est_sample,] %>%
    group_by(assignments_) %>%
    summarise(ape = cov(Y, W) / var(W))
  
  apes %>% 
    right_join(data, by = 'assignments_') %>%
    select()
  
}

# predict_grf_tree(stree, X, Y, W)