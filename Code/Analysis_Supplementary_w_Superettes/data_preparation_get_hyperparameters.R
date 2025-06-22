library('pacman')
library('here')
p_load('furrr', 'fixest', 'xgboost', 'ParBayesianOptimization', 'future', 'parallelly', 'tictoc') # 'doParallel'
# -------------------------------------------------------------------------------------------- # 
# Load and prepare data. 
# -------------------------------------------------------------------------------------------- # 
get_hyperparameters <- function(model_dep_var_str, model_geography){ 
  
  model_dep_var = model_dep_var_str
  model_geography = model_geography # Used in script below to subset by either Urban or Rural.
  # -------------------------------------------------------------------------------------------- #  
  source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
  # -------------------------------------------------------------------------------------------- #  
  
  # Number of low-access to access (0/1)
  # class_counts <- dta_untreated_wfolds %>% summarise(across(.cols = matches('^low_access'), .fns = table)) ; class_counts
  # Ratio of not low-access to low-access. (0 to 1)
  # class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ]); class_ratios
  # class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)
  
  # scale_pos_weight_data = class_ratios %>% filter(dep_var == model_dep_var_str) %>% pull(ratio); scale_pos_weight_data
  dir_dep_var = str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
  
  xgb_opt <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, 
                                paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var_str, '_final_w_superettes.rds')))
                     
                     opt_hyperparams <- xgb_opt$tuning_params_opt
                     
    #                 scale_pos_weight_ratio <- opt_hyperparams$scale_pos_weight/scale_pos_weight_data
                     
                     hyperparameter_list <- list(hyperparameters = opt_hyperparams)
                                                 #scale_pos_weight_ratio = scale_pos_weight_ratio)
                     
                     filename <- paste0(str_to_lower(model_geography), '_', model_dep_var_str, '_', 'hyperparameters.rds')
                     
                     saveRDS(hyperparameter_list, here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, filename))
                     

}
# -------------------------------------------------------------------------------------------- #  
# Urban Moels.  
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

