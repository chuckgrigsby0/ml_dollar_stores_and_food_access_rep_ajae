# -------------------------------------------------------------------------------------------- #
# .libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
# .libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample', 'tictoc', 'fixest', 'future', 'parallelly')
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
bootstrap_type <- paste0('data_preparation_bootstrap_estimation', bootstrap_by_tracts, '_ols.R')
source(here::here('Code', 'Analysis', 'OLS', bootstrap_type))
# -------------------------------------------------------------------------------------------- #  

# -------------------------------------------------------------------------------------------- #  
# We need to derive new fold ids for the OLS/LPM models. 
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
la_formula <- xpd(regex('^low_access$') ~ ..ctrl-1, ..ctrl = covars_by_model[[1]], data = dta_untreated_wfolds)
# la_formula <- xpd(regex('^low_access_perm$') ~ ..ctrl-1, ..ctrl = covars_by_model[[2]], data = dta_untreated_wfolds)
# la_formula <- xpd(regex('^low_access_pers$') ~ ..ctrl-1, ..ctrl = covars_by_model[[3]], data = dta_untreated_wfolds)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Clean up environment. 
# -------------------------------------------------------------------------------------------- #
rm(dta_untreated_non_model_vars, dta_treated_non_model_vars, val_folds_list_w_geoids, 
   acs_covars, covars_string, econ_geog_vars, non_model_vars, val_ids, covars_by_model, n_folds_iter) 

gc()
# -------------------------------------------------------------------------------------------- #
# The following function is used to implement k-fold cross validation using the val_folds_list and train_folds_list. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'OLS', 'Functions', 'Function_OLS_LPM_Training.R'))
# -------------------------------------------------------------------------------------------- #
tic()
lpm_res <- lpm_imputation_estimation(dep_var_string = model_dep_var, 
                                     dta_untreat =  dta_untreated_wfolds, 
                                     dta_treat = dta_treated)
toc()
# -------------------------------------------------------------------------------------------- #
# Save work. 
# -------------------------------------------------------------------------------------------- #
# Create string for saving in correct directory corresponding to dependent variable used in modeling. 
# -------------------------------------------------------------------------------------------- #
dep_var_dir <- str_replace_all(model_dep_var, '_', ' ') %>% 
  str_to_title() %>% 
  str_replace_all(., pattern = ' ', replacement = '_')

geography_dir <- paste0(model_geography, '_Bootstrap')
# -------------------------------------------------------------------------------------------- #
filename = paste0(
  str_to_lower(model_geography), '_',
  model_dep_var, 
  '_bootstrap_', 
  bootstrap_id, 
  '.rds'
)

saveRDS(lpm_res, here::here('Analysis', geography_dir, dep_var_dir, 'bootstrap_01_499_tracts_ols', filename))
# -------------------------------------------------------------------------------------------- #