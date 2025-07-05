# -------------------------------------------------------------------------------------------- #
# Script to estimate counterfactual low-access status using random forest and bootstrap procedure. 
# -------------------------------------------------------------------------------------------- #
library(pacman)
pacman::p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 
               'recipes', 'rsample', 'fixest', 'sf', 'tictoc', 'ranger', 'mlr3verse', 'mlr3tuningspaces', 
               'future', 'parallelly', 'furrr')
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
source(here::here('Code', 'Analysis', bootstrap_type))
# -------------------------------------------------------------------------------------------- #  
# We need to derive new fold ids for the RF models. 
# The following function can be used to obtain two lists of training and validation fold indices. 
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
# Create a simple function to obtain the model covariates. 
# -------------------------------------------------------------------------------------------- #
Get_Vars_Function <- function(grepl_string, dta){
  covars = names(dta)[!grepl(grepl_string, names(dta))]; return(covars)
}
covars_string <- c('perm$|pers$|^low_access$|GEOID|^year$|fold_id',
                   'low_access$|pers$|^low_access_perm$|GEOID|^year$|fold_id', 
                   'low_access$|perm$|^low_access_pers$|GEOID|^year$|fold_id')
# -------------------------------------------------------------------------------------------- #
# List of three elements. 
covars_by_model <- map(covars_string, function(.x) Get_Vars_Function(grepl_string = .x, dta = dta_untreated_wfolds))
# -------------------------------------------------------------------------------------------- #
# The inclusion of -1 is because we do not want an intercept in the matrix.
library(fixest)
la_formula <- xpd(regex('^low_access$') ~ ..ctrl - 1, ..ctrl = covars_by_model[[1]], data = dta_untreated_wfolds)
# -------------------------------------------------------------------------------------------- #
# Training data (Untreated and yet-to-be-treated observations)
#--------------------------------------------------------------------------------------------#
x_train <- model.matrix(la_formula, data = dta_untreated_wfolds)
y_train <- dta_untreated_wfolds %>% select(all_of(model_dep_var)) %>% pull()
y_train <- factor(y_train, levels = c('0', '1'))
#--------------------------------------------------------------------------------------------#
# Test data (Treated data)
#--------------------------------------------------------------------------------------------#
x_treat <- model.matrix(la_formula, data = dta_treated)
y_treat <- dta_treated %>% select(all_of(model_dep_var)) %>% pull()
y_treat <- factor(y_treat, levels = c('0', '1'))
# -------------------------------------------------------------------------------------------- #
# Clean up environment. 
# -------------------------------------------------------------------------------------------- #
rm(dta_untreated_non_model_vars, dta_treated_non_model_vars, val_folds_list_w_geoids, 
   acs_covars, covars_string, econ_geog_vars, non_model_vars, val_ids, covars_by_model, n_folds_iter) 

gc()
# -------------------------------------------------------------------------------------------- #

# Load optimal tuning params. 
#--------------------------------------------------------------------------------------------#
ntrees <- list.files(path = here::here('Analysis', 'Model_Training', 'Low_Access', 'RF'), 
                            pattern = paste0('^rf_opt_num_trees.*', str_to_lower(model_geography) ) ) 
ntrees <- readRDS(here::here('Analysis', 'Model_Training', 'Low_Access', 'RF', ntrees) )
#--------------------------------------------------------------------------------------------#
tuning_params <- list.files(path = here::here('Analysis', 'Model_Training', 'Low_Access', 'RF'), 
                            pattern = paste0('^rf_opt_hyper.*', str_to_lower(model_geography) ) ) 

tuning_params <- readRDS(file = here::here('Analysis', 'Model_Training', 'Low_Access', 'RF', tuning_params) ) 
# -------------------------------------------------------------------------------------------- #
# The following function is used to implement k-fold cross validation using the val_folds_list and train_folds_list. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_RF_Ranger_CV_Training_and_Final_Estimation.R'))
# -------------------------------------------------------------------------------------------- #
tic()

rf_results <- rf_training_and_imputation(model_type = 'bootstrap')

toc()
# -------------------------------------------------------------------------------------------- #
# Save work. 
# -------------------------------------------------------------------------------------------- #
dep_var_dir <- str_replace_all(model_dep_var, '_', ' ') %>% 
  str_to_title() %>% 
  str_replace_all(., pattern = ' ', replacement = '_')
# -------------------------------------------------------------------------------------------- #
filename = paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'rf_bootstrap', '_', bootstrap_id, '.rds')
geography_dir <- paste0(model_geography, '_Bootstrap'); geography_dir
bootstrap_dir <- 'bootstrap_01_499_tracts_rf'
saveRDS(rf_results, here::here('Analysis', geography_dir, dep_var_dir, bootstrap_dir, filename))
# -------------------------------------------------------------------------------------------- #