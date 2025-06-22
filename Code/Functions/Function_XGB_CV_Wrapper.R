#Wrapper function for performing grid search in xgb.cv. 
#Note that the main argument is id, where tuning_params is the grid of hyperparameters. 
#Therefore, tuning_params must be a grid in the global environment. 
#--------------------------------------------------------------------------------------------#
#Function for classification. 
#--------------------------------------------------------------------------------------------#
class_counts <- dta_untreated_wfolds %>% summarise(across(.cols = matches('^low_access'), .fns = table)) ; class_counts
# Number of low-access to access (0/1)
class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ]); class_ratios
class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)
scale_pos_weight = class_ratios %>% filter(dep_var == model_dep_var) %>% pull(ratio); scale_pos_weight
#--------------------------------------------------------------------------------------------#
#Set up grid of tuning parameters. 
#--------------------------------------------------------------------------------------------#

tuning_params <- expand.grid(eta = 0.04929913, # Controls the learning rate. If you increase eta, then you need to increase ntrees.

                           gamma = 0.1826024, # Controls regularization. 
                                                # Increasing gamma reduces the complexity of the trees

                           max_depth = 25, # Controls the depth of each tree built for each iteration. 
                                                  # Increasing makes trees more complex. 6 is the default 

                          subsample = 0.80, # On each iteration we sample 0.75 without replacement. 

                         colsample_bylevel = 0.70, 

                        colsample_bytree = 0.75, 
                        
                        scale_pos_weight = 6.431597) # Randomly samples a subset of columns at #each internal node of the tree. 
                                                      # The value should be specified as a ratio. 
#--------------------------------------------------------------------------------------------#

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
XGB_CV_Classification_Function_Wrapper <- function(xgb_train_arg, tuning_params_arg, id, test_folds_arg, training_folds_arg){
  
  plan(multicore, workers = ncores)
  
  xgb_cv_model <- xgb.cv(data = xgb_train_arg,
                         params = list(booster = 'gbtree', 
                                       objective = 'binary:logistic', 
                                       nthread = ncores, 
                                       eta = tuning_params_arg$eta[[id]], 
                                       gamma = tuning_params_arg$gamma[[id]], 
                                       max_depth = tuning_params_arg$max_depth[[id]], 
                                       subsample = tuning_params_arg$subsample[[id]], 
                                       colsample_bytree = tuning_params_arg$colsample_bytree[[id]],
                                       colsample_bylevel = tuning_params_arg$colsample_bylevel[[id]],
                                       scale_pos_weight = tuning_params_arg$scale_pos_weight[[id]], 
                                       tree_method = 'hist'),
                         nrounds = ntrees, #Specified outside of function
                         showsd = TRUE, 
                         early_stopping_rounds = early_stop_global, 
                         metrics = 'error', # binary classification error rate. 
                         maximize = FALSE, #We are not maximizing the logloss function. 
                         folds = test_folds_arg, #These are specified as arguments in the XGBoost_Classification_Function
                         train_folds = training_folds_arg, #These are specified as arguments in the XGBoost_Classification_Function 
                         verbose = FALSE, 
                         prediction = TRUE)
  
  
  # Note that best_ntreelimit and best_iteration are identical. 
  ntree_opt = xgb_cv_model$best_ntreelimit
  
  nrounds_opt = xgb_cv_model$best_iteration
  
  # Indicates the best number of boosting iterations for the given hyperparameter values.   
  xgb_cv_error_opt <- xgb_cv_model$evaluation_log[nrounds_opt, ] 
  #--------------------------------------------------------------------------------------------#
  
  # Save the cross-validated predictions. 
  
  #--------------------------------------------------------------------------------------------#
  xgb_cv_preds <- data.frame(pred_probs = xgb_cv_model$pred)
  
  cv_output <- list(nrounds_opt = xgb_cv_error_opt, 
                    preds_opt = xgb_cv_preds)
  
  return(cv_output)
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: XGB_CV_Classification_Function_Wrapper')
#--------------------------------------------------------------------------------------------#
#Function for Regresssion 
#--------------------------------------------------------------------------------------------#
XGB_CV_Regression_Function_Wrapper <- function(xgb_train_arg, tuning_params_arg, id, test_folds_arg, training_folds_arg){
  
  xgb_cv_model <- xgb.cv(data = xgb_train_arg,
                         params = list(booster = 'gbtree', 
                                       objective = 'reg:squarederror', 
                                       nthread = ncores, 
                                       eta = tuning_params_arg$eta[[id]], 
                                       gamma = tuning_params_arg$gamma[[id]], 
                                       max_depth = tuning_params_arg$max_depth[[id]], 
                                       subsample = tuning_params_arg$subsample[[id]], 
                                       colsample_bytree = tuning_params_arg$colsample_bytree[[id]],
                                       colsample_bylevel = tuning_params_arg$colsample_bylevel[[id]], 
                                       tree_method = 'hist'),
                         nrounds = ntrees, #Specified outside of function
                         showsd = TRUE, 
                         early_stopping_rounds = early_stop_global, 
                         metrics = 'rmse',
                         maximize = FALSE, #We are not minimizing the logloss function. 
                         folds = test_folds_arg, #These are specified as arguments in the XGBoost_Classification_Function
                         train_folds = training_folds_arg, #These are specified as arguments in the XGBoost_Classification_Function 
                         verbose = FALSE, 
                         prediction = TRUE)
  
  
  ntree_opt = xgb_cv_model$best_ntreelimit
  
  nrounds_opt = xgb_cv_model$best_iteration
  
  xgb_cv_error_opt <- xgb_cv_model$evaluation_log[nrounds_opt, ]
  #--------------------------------------------------------------------------------------------#
 
  # Save the cross-validated predictions. 
 
  #--------------------------------------------------------------------------------------------#
   xgb_cv_preds <- data.frame(pred_probs = xgb_cv_model$pred)
  
  cv_output <- list(nrounds_opt = xgb_cv_error_opt, 
                    preds_opt = xgb_cv_preds)
  
  return(cv_output)
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: XGB_CV_Regression_Function_Wrapper')