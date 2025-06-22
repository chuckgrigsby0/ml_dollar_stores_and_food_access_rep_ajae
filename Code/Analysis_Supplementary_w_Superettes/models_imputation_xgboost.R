library('pacman')
library('here')
p_load('furrr', 'fixest', 'xgboost', 'ParBayesianOptimization', 'future', 'parallelly', 'tictoc') # 'doParallel'
# -------------------------------------------------------------------------------------------- # 
# Load and prepare data. 
# -------------------------------------------------------------------------------------------- # 
model_dep_var = 'low_access'
model_geography = 'Urban' # Used in script below to subset by either Urban or Rural.
ncores = parallelly::availableCores() # Used in functions below. 
# -------------------------------------------------------------------------------------------- #  
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #  
# geoids <- sample(unique(dta_untreated_wfolds$GEOID), 500)
# subsample <- function(dta){ dta <- dta %>% filter(GEOID %in% geoids) }
# dta_untreated_wfolds <- subsample(dta_untreated_wfolds)
# dta_untreated_non_model_vars <- subsample(dta_untreated_non_model_vars)
# dta_treated <- subsample(dta_treated)
# dta_treated_non_model_vars <- subsample(dta_treated_non_model_vars)
# -------------------------------------------------------------------------------------------- # 
# We need to derive new fold ids for the XGBoost models. 
# The following function can be used to obtain two lists of training and validation fold indeces. 
# -------------------------------------------------------------------------------------------- #  
source(here::here('Code', 'Functions', 'Function_Train_and_Test_Fold_IDs.R'))
print(args(Train_and_Test_Fold_IDs_Function))
# -------------------------------------------------------------------------------------------- #
# Create fold IDs to iterate over. 
# -------------------------------------------------------------------------------------------- # 
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
la_formula <- xpd(regex('^low_access$') ~ ..ctrl-1, ..ctrl = covars_by_model[[1]], data = dta_untreated_wfolds)
# la_perm_formula <- xpd(regex('^low_access_perm$') ~ ..ctrl-1, ..ctrl = covars_by_model[[2]], data = dta_ut)
# la_pers_formula <- xpd(regex('^low_access_pers$') ~ ..ctrl-1, ..ctrl = covars_by_model[[3]], data = dta_ut)
# -------------------------------------------------------------------------------------------- #
# The following function is used to implement k-fold cross validation using the val_folds_list and train_folds_list. 
# -------------------------------------------------------------------------------------------- #
getDoParWorkers()
# ncores = detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_XGB_CV_Wrapper.R'))
# -------------------------------------------------------------------------------------------- #
XGBoost_Classification_Function <- function(la_formula_arg,
                                            dta_train, 
                                            dep_var_string, 
                                            dta_treat, 
                                            save_string){
  
  # -------------------------------------------------------------------------------------------- #
  # Training data (Untreated and yet-to-be-treated observations)
  # -------------------------------------------------------------------------------------------- #
  x_train=model.matrix(la_formula_arg, data=dta_train)
  y_train <- dta_train %>% select(matches(dep_var_string)) %>% pull()
  xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
  # -------------------------------------------------------------------------------------------- #
  # Treated data (Treated data)
  # Training data
  # -------------------------------------------------------------------------------------------- #
  x_treat=model.matrix(la_formula_arg, data=dta_treat)
  y_treat <- dta_treat %>% select(matches(dep_var_string)) %>% pull()
  xgb_treat <- xgb.DMatrix(data = x_treat, label = y_treat)
  # -------------------------------------------------------------------------------------------- #
  # Model Training
  # -------------------------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------------------------- #
  tuning_params_ids <- seq(1, nrow(tuning_params), 1)
  # -------------------------------------------------------------------------------------------- #
  gc()
  # set.seed(243444)
  # -------------------------------------------------------------------------------------------- #
  xgb_cv <- map(tuning_params_ids, function(.id) XGB_CV_Classification_Function_Wrapper(xgb_train_arg = xgb_train, 
                                                                                            tuning_params_arg = tuning_params, 
                                                                                            id = .id, 
                                                                                            test_folds_arg = val_folds_list, 
                                                                                            training_folds_arg = train_folds_list))
  # xgb_cv is list of length(tuning_params)
  # Each of these list elements contains two inner-list elements. 
  # nrounds_opt is the list element containing the CV evaluation logs from each combination of tuning_params. 
  # It contains the minimum CV MSE and optimal number of boosting rounds. 
  # -------------------------------------------------------------------------------------------- #
  nrounds_opt_df <- xgb_cv %>% 
    map(function(.x) keep(.x, .p = grepl('nrounds_opt', names(.x)))) %>%
    flatten_dfr()
  # -------------------------------------------------------------------------------------------- #
  # Based on model training from the cross-validation models
  # Find the global minimum and the combination of tuning parameters 
  # that yields the global minimum, including model parameters and nrounds/ntrees/niter. 
  # -------------------------------------------------------------------------------------------- #
  # Minimum cv error in terms of log loss across each hyperparameter combination. 
  # Each row of xgb_cv contains a column called iter and the training and test errors for the given number of iter, 
  # which corresponds to the number of boosting rounds for the best/lowest error for the given hyperparameters. 
  # -------------------------------------------------------------------------------------------- #
  # min_cv_mse <- min(xgb_cv$test_logloss_mean)
  min_cv_mse <- min(nrounds_opt_df$test_error_mean)
  # -------------------------------------------------------------------------------------------- #
  # Retrieve the row corresponds to the lowest logloss. 
  # The index refers to the row of tuning_params that yields lowest MSE/classification error
  # -------------------------------------------------------------------------------------------- #
  # tuning_params_index <- which.min(xgb_cv$test_logloss_mean)
  tuning_params_index <- which.min(nrounds_opt_df$test_error_mean) 
  # -------------------------------------------------------------------------------------------- #
  #Using the optimal index, save the number of iterations/rounds required
  # -------------------------------------------------------------------------------------------- #
  ntrees_opt = nrounds_opt_df$iter[tuning_params_index]
  # -------------------------------------------------------------------------------------------- #
  #Save the row in the tuning_params object that yields the lowest CV logloss. 
  # -------------------------------------------------------------------------------------------- #
  tuning_params_opt <- tuning_params[tuning_params_index, ]
  # -------------------------------------------------------------------------------------------- #
  # Save the cross-validated predictions from the optimal model 
  # -------------------------------------------------------------------------------------------- #
  xgb_cv_opt <- xgb_cv[[tuning_params_index]]$preds_opt
  xgb_cv_opt <- bind_cols(select(dta_train, fold_id, GEOID, year, matches(dep_var_string)), xgb_cv_opt)
  xgb_cv_opt <- xgb_cv_opt %>% mutate(cv_preds = as.numeric(pred_probs > 0.5))
  
  gc()
  #--------------------------------------------------------------------------------------------#
  # Train model using full set of training data using optimal parameters. 
  # This final model is saved to make predictions of the counterfactuals. 
  # -------------------------------------------------------------------------------------------- #
  # set.seed(352)
  # -------------------------------------------------------------------------------------------- #
  plan(multicore, workers = ncores)
  # -------------------------------------------------------------------------------------------- #
  xgb_model_final <- xgb.train(params = list(booster = 'gbtree', 
                                             objective = 'binary:logistic', 
                                             nthread = ncores, 
                                             eta = tuning_params$eta[[tuning_params_index]], 
                                             gamma = tuning_params$gamma[[tuning_params_index]], 
                                             max_depth = tuning_params$max_depth[[tuning_params_index]], 
                                             subsample = tuning_params$subsample[[tuning_params_index]], 
                                             colsample_bytree = tuning_params$colsample_bytree[[tuning_params_index]], 
                                             colsample_bylevel = tuning_params$colsample_bylevel[[tuning_params_index]],
                                             scale_pos_weight = tuning_params$scale_pos_weight[[tuning_params_index]],
                                             tree_method = 'hist',
                                             eval_metric = 'error'), 
                               data = xgb_train,
                               nrounds = ntrees_opt, 
                               early_stopping_rounds = NULL, 
                               verbose = 0)
  
  # saveRDS(xgb_model_final, here::here('Analysis', paste0('xgb_model_final_', save_string, '.rds')))
  
  # -------------------------------------------------------------------------------------------------------------------- #
  # Make predictions of counterfactuals using the treated data. 
  # -------------------------------------------------------------------------------------------------------------------- #
  dta_cf_preds <- data.frame(pred_probs_cf = predict(xgb_model_final,
                                                     newdata = xgb_treat))
  dta_cf_preds <- dta_cf_preds %>%
    mutate(actual = y_treat, 
           pred_class_cf = as.numeric(pred_probs_cf > 0.5), 
           tau = actual-pred_class_cf) %>%
    relocate(c(actual, pred_class_cf, tau), 
             .before = pred_probs_cf)
  
  # -------------------------------------------------------------------------------------------- #
  model_results <- list(cv_mse_by_tuning_params = nrounds_opt_df, # error rates for each tuning parameter combination. 
                        min_cv_mse = min_cv_mse, #The min_cv_mse is not the MSE, but is actually the mininum misclassification rate.  
                        min_cv_mse_index = tuning_params_index, # Index corresponding to the tuning-parameter row in which the CV error is minimized.
                        tuning_params_opt = tuning_params_opt, # Optimal set of tuning parameters corresponding to min_cv_mse_index. 
                        cv_errors_opt = xgb_cv_opt, # The combined cross-validated errors from each cross-validation fit. 
                        xgb_model_final = xgb_model_final, # The final XGBoost model using the complete set of training observations. 
                        data_cf_preds = dta_cf_preds) # Estimated counterfactuals. 
  
  return(model_results)
  
}
# -------------------------------------------------------------------------------------------- #
start.time <- Sys.time()
# -------------------------------------------------------------------------------------------- #
xgb_model_results <- XGBoost_Classification_Function(la_formula_arg = la_formula,
                                                     dta_train = dta_untreated_wfolds, 
                                                     dep_var_string = '^low_access$', 
                                                     dta_treat = dta_treated, 
                                                     save_string = 'low_access')
# -------------------------------------------------------------------------------------------- #
end.time <- Sys.time()
end.time-start.time
filename = paste0('xgboost_trial_10m_', str_to_lower(model_geography), '.rds')
saveRDS(xgb_model_results, here::here('Analysis', filename))
# -------------------------------------------------------------------------------------------- #
#xgb_trial <- readRDS(here::here('Analysis', 'xgboost_trial.rds'))
