pacman::p_load('dplyr', 'mlr3verse', 'purrr', 'here')

# Create Latin Hypercube Samples of hyperparameters for tuning. 

# Define LHS tuner
search_space <- ps(
  eta = p_dbl(lower = 0.025, upper = 0.3, logscale = TRUE),
  max_depth = p_int(lower = 12, upper = 25),
  gamma = p_dbl(lower = 0.1, upper = 0.3),
  colsample_bylevel = p_dbl(lower = 0.7, upper = 0.9) 
)

set.seed(243444)
design = generate_design_lhs(param_set = search_space, n = 50)

design = as.data.table(design$data)
design$eta <- exp(design$eta)

design <- design %>% split(1:nrow(.)) %>% map(as.list)

saveRDS(design, here::here('Analysis', 'Model_Training', 'Low_Access', 'lhs_hyperparameters_training.rds'))
