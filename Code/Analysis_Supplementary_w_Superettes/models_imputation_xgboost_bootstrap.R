# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample', 'tictoc', 
       'xgboost', 'fixest', 'future', 'parallelly')
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap estimation run number', bootstrap_id))
# -------------------------------------------------------------------------------------------- #
# Load and prepare data. 
# -------------------------------------------------------------------------------------------- #
model_dep_var <- Sys.getenv("model_dep_var") # Used in script below to subset by either Urban or Rural.
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
ncores <- parallelly::availableCores() # Used in functions below. 
print(paste0('Model depdendent variable: ', model_dep_var, '; Model geography: ', model_geography ) )
# -------------------------------------------------------------------------------------------- #  
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts' # or NULL to bootstrap by block-group and stratify by relative time. 
bootstrap_type <- paste0('data_preparation_bootstrap_estimation', bootstrap_by_tracts, '.R')
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', bootstrap_type))
# -------------------------------------------------------------------------------------------- #  
# source(here::here('Code', 'Analysis', 'data_preparation_bootstrap_estimation.R'))
# source(here::here('Code', 'Analysis', 'data_preparation_bootstrap_estimation_tracts.R'))
# -------------------------------------------------------------------------------------------- #  
# Retrieve the optimal hyperparameters from training. 
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
hyperparameters <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var,
                                      paste(str_to_lower(model_geography), model_dep_var, 'hyperparameters.rds', sep = '_')))

# Ratio between optimal hyperparam value to recommended default value. 
# scale_pos_weight_ratio <- hyperparameters$scale_pos_weight_ratio

# Number of low-access to access (0/1)
class_counts <- dta_untreated_wfolds %>% reframe(across(.cols = matches('^low_access'), .fns = table)) ; class_counts

# Ratio of not low-access to low-access. (0 to 1)
class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ]); class_ratios
class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)
scale_pos_weight_data <- class_ratios %>% filter(dep_var == model_dep_var) %>% pull(ratio); scale_pos_weight_data

if (model_geography == 'Rural'){ 
  scale_pos_weight_data = 1 # sqrt(scale_pos_weight_data)
} else if (model_geography == 'Urban'){ 
  scale_pos_weight_data = scale_pos_weight_data
}

# We use the ratio between (# of negative)/(# of positive) times the ratio (optimal scale_pos_weight)/(recommended default scale_pos_weight)
# scale_pos_weight_opt <- scale_pos_weight_data*scale_pos_weight_ratio 

hyperparameters <- hyperparameters$hyperparameters 

hyperparameters$scale_pos_weight <- scale_pos_weight_data

# Given that we do not use Function_XGB_CV_Tuning, we also need to pre-specify the number of boosting 
# rounds and early stopping criterion. 

ntrees = 2000
early_stop_global = 10


# -------------------------------------------------------------------------------------------- #  
# We need to derive new fold ids for the XGBoost models. 
# The following function can be used to obtain two lists of training and validation fold indeces. 
# -------------------------------------------------------------------------------------------- #  
source(here::here('Code', 'Functions', 'Function_Train_and_Test_Fold_IDs.R'))
print(args(Train_and_Test_Fold_IDs_Function))
# -------------------------------------------------------------------------------------------- #
# Create fold IDs to iterate over. 

val_ids <- sort( unique(dta_untreated_wfolds$fold_id) ); val_ids
val_ids <- val_ids[val_ids > 0]; val_ids

val_folds_list <- map(val_ids, function(.x){ 
  
  Train_and_Test_Fold_IDs_Function(dta_w_cv_folds = dta_untreated_wfolds, 
                                   cv_fold_ids = .x, 
                                   return_training_ids = FALSE)
})
val_folds_list <- set_names(val_folds_list, nm = paste0('Validation_', val_ids))
# -------------------------------------------------------------------------------------------- #
# Obtain the validation folds with the GEOIDs and the year corresponding to each holdout/validation sample. 
# -------------------------------------------------------------------------------------------- #
n_folds_iter <- 1:length(val_folds_list); n_folds_iter

val_folds_list_w_geoids <- n_folds_iter %>%
  map(function(.k){
    val_fold_k <- dta_untreated_wfolds[val_folds_list[[.k]], ]
    val_fold_k <- val_fold_k %>% select(fold_id, GEOID, year)
  })

val_folds_list_w_geoids <- set_names(val_folds_list_w_geoids, nm = paste0('Validation_', val_ids))
# -------------------------------------------------------------------------------------------- #
train_folds_list <- map(val_ids, function(.x){ 
  
  Train_and_Test_Fold_IDs_Function(dta_w_cv_folds = dta_untreated_wfolds, 
                                   cv_fold_ids = .x, 
                                   return_training_ids = TRUE)
})
# -------------------------------------------------------------------------------------------- #
train_folds_list <- set_names(train_folds_list, nm = paste0('Training_', val_ids))
# -------------------------------------------------------------------------------------------- #
gc()
# -------------------------------------------------------------------------------------------- #
# Acquire nothing that ends in perm, pers, and that exactly begins and ends in low_access. 
# Create a simple function to obtain the covariates for each model. 
# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
Get_Vars_Function <- function(grepl_string, dta){
  covars = names(dta)[!grepl(grepl_string, names(dta))]; return(covars)
}
covars_string <- c('perm$|pers$|^low_access$|GEOID|^year$|fold_id')
# -------------------------------------------------------------------------------------------- #
# List of three elements. 
covars_by_model <- Get_Vars_Function(grepl_string = covars_string, dta = dta_untreated_wfolds)
# -------------------------------------------------------------------------------------------- #
# The inclusion of -1 is because we do not want an intercept in the matrix.
la_formula <- xpd(regex('^low_access$') ~ ..ctrl-1, ..ctrl = covars_by_model, data = dta_untreated_wfolds)
# -------------------------------------------------------------------------------------------- #
# Training data (Untreated and yet-to-be-treated observations)
#--------------------------------------------------------------------------------------------#
x_train <- model.matrix(la_formula, data = dta_untreated_wfolds)
y_train <- dta_untreated_wfolds %>% select(all_of(model_dep_var)) %>% pull()
xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
#--------------------------------------------------------------------------------------------#
# Treated data (Treated data)
#--------------------------------------------------------------------------------------------#
x_treat <- model.matrix(la_formula, data = dta_treated)
y_treat <- dta_treated %>% select(all_of(model_dep_var)) %>% pull()
xgb_treat <- xgb.DMatrix(data = x_treat, label = y_treat)
# -------------------------------------------------------------------------------------------- #
# Clean up environment. 
# -------------------------------------------------------------------------------------------- #
rm(x_train, y_train, x_treat, dta_untreated_non_model_vars, dta_treated_non_model_vars, val_folds_list_w_geoids, 
   acs_covars, covars_string, econ_geog_vars, non_model_vars, val_ids, covars_by_model, n_folds_iter) 

gc()
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_XGB_Final_Estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Note: 
# cv_models = FALSE to only save necessary information from bootstrap
# -------------------------------------------------------------------------------------------- #
tic()
xgb_model_results <- xgboost_imputation_estimation(dep_var_string = 'low_access', cv_models = FALSE) 
toc()
# -------------------------------------------------------------------------------------------- #
filename = paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'bootstrap', '_', bootstrap_id, '.rds')
# Format dependent variable string to save in correct directory. 
dir_model_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, pattern = '_', replacement = ' ')), ' ', '_')
saveRDS(xgb_model_results, here::here('Analysis_Supplementary_w_Superettes', 
                                      paste0(model_geography, '_', 'Bootstrap'), 
                                      dir_model_dep_var, 
                                      paste0('bootstrap_01_499', bootstrap_by_tracts),
                                      filename))
# -------------------------------------------------------------------------------------------- #
