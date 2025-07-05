# The following script trains xgboost models using pre-treatment data across 50 hyperparameter combinations
# using LHS to create hyperparameter sets. The optimal set obtains the lowest CV error. 
# ------------------------------ #
# Load packages and prepare data. 
# ------------------------------ #
library('pacman')
p_load('here', 'furrr', 'fixest', 'xgboost', 'mlr3verse', 'future', 'parallelly', 'tictoc')
# ------------------------------ #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below.
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
param_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
param_id <- as.numeric(param_id)
print(paste('Training run number', param_id))
print(model_geography); print(model_dep_var)
ncores = parallelly::availableCores() # Used in functions below. 
# -------------------------------------------------------------------------------------------- #  
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #  
# We need to derive new fold ids for the XGBoost models. 
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
# Acquire nothing that ends in perm, pers, and that exactly begins and ends in low_access. 
# Create a simple function to obtain the covariates for each model. 
# -------------------------------------------------------------------------------------------- #
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
la_formula <- xpd(regex('^low_access$') ~ ..ctrl - 1, ..ctrl = covars_by_model[[1]], data = dta_untreated_wfolds)
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
# Number of low-access to access (0/1)
class_counts <- dta_untreated_wfolds %>% reframe(across(.cols = matches('^low_access'), .fns = table))
# Ratio of not low-access to low-access. (0 to 1)
class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ])
class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)

scale_pos_weight_data = class_ratios %>% filter(dep_var == model_dep_var) %>% pull(ratio)

if (model_geography == 'Rural'){ 
  scale_pos_weight_data = 1 
} else if (model_geography == 'Urban'){ 
  scale_pos_weight_data = scale_pos_weight_data
}

print(paste('scale_pos_weight_data =', scale_pos_weight_data))
#--------------------------------------------------------------------------------------------#
#Set up early stopping during model training. 
#--------------------------------------------------------------------------------------------#
#If after 10 cross-validation iterations, the model does not improve w.r.t cv error, stop tree building/boosting. 
#--------------------------------------------------------------------------------------------#
early_stop_global = 10
#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#
#Number of trees = Number of iterations of tree building. 
#--------------------------------------------------------------------------------------------#
ntrees = 2000
#--------------------------------------------------------------------------------------------#
# Parameters generated via LHS 
#--------------------------------------------------------------------------------------------#
param_set <- readRDS(here::here('Analysis', 'Model_Training', 'Low_Access', 'LHS_Results', 'lhs_hyperparameters_training.rds') )
#--------------------------------------------------------------------------------------------#
fixed_tuning_params <- list(
  
  scale_pos_weight = scale_pos_weight_data, 
  
  subsample = 0.80, 
  
  colsample_bytree = 0.75, 
  
  booster = 'gbtree', 
  
  tree_method = 'hist', 
  
  objective = 'binary:logistic', 
  
  eval_metric = 'error'
) 

list_param_set <- c(param_set[[param_id]], fixed_tuning_params)
#--------------------------------------------------------------------------------------------#
plan(multicore, workers = ncores)
#--------------------------------------------------------------------------------------------#
xgb_cv_model <- xgb.cv(data = xgb_train,
                       params = list_param_set,
                       nrounds = ntrees, # Specified above. 
                       showsd = TRUE, 
                       early_stopping_rounds = early_stop_global, # Specified above. 
                       maximize = FALSE, #We are not maximizing the logloss function. 
                       folds = val_folds_list, 
                       train_folds = train_folds_list, 
                       prediction = TRUE, 
                       nthread = ncores, 
                       verbose = FALSE)

opt_nrounds <- xgb_cv_model$best_iteration

cv_error <- xgb_cv_model$evaluation_log[opt_nrounds, ]

training_params <- bind_cols(list_param_set)

training_params <- bind_cols(training_params, cv_error)

# -------------------------------------------------------------------------------------------- #
# Save work. 
# -------------------------------------------------------------------------------------------- #

# Create string for saving in correct directory.  

# -------------------------------------------------------------------------------------------- #
dep_var_dir <- str_replace_all(model_dep_var, '_', ' ') %>% 
  str_to_title() %>% 
  str_replace_all(., pattern = ' ', replacement = '_')

filename <- paste0(str_to_lower(model_geography), '_lhs_training_params_', param_id, '.rds'); filename

saveRDS(training_params, here::here('Analysis', 'Model_Training', dep_var_dir, 'LHS_Results', model_geography, filename))
# -------------------------------------------------------------------------------------------- #