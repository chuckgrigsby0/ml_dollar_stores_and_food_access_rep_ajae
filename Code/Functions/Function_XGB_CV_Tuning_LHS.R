#--------------------------------------------------------------------------------------------#
# Number of low-access to access (0/1)
class_counts <- dta_untreated_wfolds %>% reframe(across(.cols = matches('^low_access'), .fns = table)) ; class_counts
# Ratio of not low-access to low-access. (0 to 1)
class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ]); class_ratios
class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)

scale_pos_weight_data = class_ratios %>% filter(dep_var == model_dep_var) %>% pull(ratio); scale_pos_weight_data

if (model_geography == 'Rural'){ 
  scale_pos_weight_data = 1 #
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
param_set <- readRDS(here::here('Analysis', 'Model_Training', 'Low_Access', 'lhs_hyperparameters_training.rds') )
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

filename <- paste0(str_to_lower(model_geography), '_lhs_training_params_', param_id, '.rds'); filename

saveRDS(training_params, here::here('Analysis', 'Model_Training', 'Low_Access', 'LHS_Results', model_geography, filename))
