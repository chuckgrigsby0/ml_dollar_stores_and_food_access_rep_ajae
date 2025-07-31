# Script that obtains optimal hyperparameters from model training, saving them to a separate data file for use in final models. 
# -------------------------------------------------------------------------------------------- # 
library('pacman')
library('here')
p_load('furrr', 'fixest', 'xgboost', 'ParBayesianOptimization', 'future', 'parallelly', 'tictoc')
# -------------------------------------------------------------------------------------------- # 
# Load and prepare data. 
# -------------------------------------------------------------------------------------------- # 
get_hyperparameters <- function(model_dep_var_str, model_geography){ 
  
  model_dep_var = model_dep_var_str
  model_geography = model_geography # Used in script below to subset by either Urban or Rural.
  # -------------------------------------------------------------------------------------------- #  
  source(here::here('Code', 'Analysis_Placebo', 'data_preparation_imputation_estimation.R'))
  # -------------------------------------------------------------------------------------------- #  
  
  dir_dep_var = str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
  
  xgb_opt <- readRDS(here::here('Analysis_Placebo', 'Model_Training', dir_dep_var, 
                                paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var_str, '_final.rds')))
                     
                     opt_hyperparams <- xgb_opt$tuning_params_opt
                     
                     
                     hyperparameter_list <- list(hyperparameters = opt_hyperparams)
                     
                     filename <- paste0(str_to_lower(model_geography), '_', model_dep_var_str, '_', 'hyperparameters.rds')
                     
                     saveRDS(hyperparameter_list, here::here('Analysis_Placebo', 'Model_Training', dir_dep_var, filename))
                     

}
# -------------------------------------------------------------------------------------------- #  
# Urban Models.  
# -------------------------------------------------------------------------------------------- #  
# Note that because of the use of functions within functions, 
# it is easier to specify model_geography outside of function. 
# -------------------------------------------------------------------------------------------- #  
model_geography = 'Urban'
get_hyperparameters(model_dep_var_str = 'low_access', model_geography = 'Urban')
# -------------------------------------------------------------------------------------------- #  
rm(list = ls())
gc()
# -------------------------------------------------------------------------------------------- #  
# Rural Models. 
# -------------------------------------------------------------------------------------------- #  
model_geography = 'Rural'
get_hyperparameters(model_dep_var_str = 'low_access', model_geography = 'Rural')
# -------------------------------------------------------------------------------------------- #  

