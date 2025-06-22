
library(here)
library(pacman)
p_load('dplyr', 'purrr', 'tidyr', 'stringr', 'xgboost')

model_geography_str <- c('Urban', 'Rural')
model_dep_var <- 'low_access'

dep_var_dir <- model_dep_var %>% str_replace_all(., '_', ' ') %>% str_to_title() %>% str_replace_all(., ' ', '_')

# Load optimal models containing hyperparameters. 
urb_opt_model <- readRDS(here::here('Analysis', 'Model_Training', dep_var_dir, 'xgboost_10m_urban_low_access_final.rds'))
rural_opt_model <- readRDS(here::here('Analysis', 'Model_Training', dep_var_dir, 'xgboost_10m_rural_low_access_final.rds'))

hyperparams <- list('Urban' = urb_opt_model$tuning_params_opt, 
                    'Rural' = rural_opt_model$tuning_params_opt)

# Extract the numeric and character elements, respectively. 
hyperparams_num <- hyperparams %>%
  map(function(.x){
    keep(.x, .p = ~is.numeric(.))
  }) %>%
  bind_rows(., .id = 'Geography') %>%
  pivot_longer(cols = c(eta:scale_pos_weight), 
               names_to = 'Hyperparameter', 
               values_to = 'Value') %>%
  mutate(Value = round(Value, digits = 4)) %>%
  pivot_wider(names_from = Geography, 
              values_from = Value)

hyperparams_char <- hyperparams %>%
  map(function(.x){ 
    keep(.x, .p = ~is.character(.))
    }) %>%
  bind_rows(., .id = 'Geography') %>%
  pivot_longer(cols = c(booster:eval_metric), 
               names_to = 'Hyperparameter', 
               values_to = 'Value') %>%
  pivot_wider(names_from = Geography, 
              values_from = Value)
  
# Create a separate data frame containing the parameter bounds used during optimization. 
hyperparams_bounds <- data.frame(Hyperparameter = hyperparams_num$Hyperparameter, 
                                 Bounds = c('[0.025, 0.3]', '[12, 25]', '[0.1, 0.3]', '[0.7, 0.9]', '0.80', '0.75', 'sum(0)/sum(1)'))

# Join to the hyperparameter table. 
hyperparams_num <- hyperparams_num %>% left_join(hyperparams_bounds, by = 'Hyperparameter') 

# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# Obtain CV errors to compute average. 
# -------------------------------------------------------------------------------------------- #

source(here::here('Code', 'Functions', 'Function_load_bootstrap_errors_and_predictions.R'))

# Acquire only the CV Error from the $predictions list element.  
# -------------------------------------------------------------------------------------------- #
boot_data <- model_geography_str %>% 
  
  map(function(.x){
    
    seq(1, 499, 1) %>%
  
  map_dfr(function(.iter){ 
    
    dta <- load_bootstrap_errors_and_predictions_array(model_geography_str = .x, model_dep_var_str = dep_var_dir, 
                                                bootstrap_by_tracts = '_tracts', bootstrap_iter = .iter)
    
    dta <- dta$predictions %>% select(cv_mse, id) %>% distinct(cv_mse, id)
    
  })
    
  })

# -------------------------------------------------------------------------------------------- #

boot_data <- set_names(boot_data, nm = model_geography_str) %>% 
  bind_rows(., .id = 'Geography')

# Obtain empirical classification errors. 
emp_data_err <- data.frame(Geography = model_geography_str, 
                           cv_mse = c(urb_opt_model$min_cv_mse, rural_opt_model$min_cv_mse), 
                           id = rep(0, 2))

# Combine empirical with bootstrap errors. 
emp_data_err <- bind_rows(emp_data_err, boot_data)

# Compute average classification error. 
avg_err <- emp_data_err %>% group_by(Geography) %>% 
  summarise(across(.cols = cv_mse, .fns = mean)) %>%
  mutate(cv_mse = round(cv_mse, digits = 4))

avg_err <- avg_err %>% 
  pivot_wider(names_from = Geography, 
              values_from = cv_mse) %>%
  mutate(Hyperparameter = 'Avg. Error') %>%
  relocate(Hyperparameter)

# Combine the CV errors to the hyperparameter tables. 
hyperparams_table <- bind_rows(hyperparams_num, avg_err)
hyperparams_table <- hyperparams_table %>% relocate(Bounds, .after = Hyperparameter)
# -------------------------------------------------------------------------------------------- #

library(readr)

write_csv(hyperparams_table, here::here('Analysis', 'Tables', dep_var_dir, 'optimal_hyperparameters_and_cv_error.csv'))

# -------------------------------------------------------------------------------------------- #