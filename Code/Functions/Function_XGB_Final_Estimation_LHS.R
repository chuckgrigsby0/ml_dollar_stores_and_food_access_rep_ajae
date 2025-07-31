# Estimate final CV model and apply the estimated model to predict counterfactuals. 
# -------------------------------------------------------------------------------------------- #
xgboost_imputation_estimation <- function(dep_var_string, cv_models){
  
  if (isTRUE(cv_models)){ 
    
    opt_tuning_params <- opt_tuning_params %>% select(-nrounds)
    
  } else { 
    
    opt_tuning_params <- opt_tuning_params %>% select(-nrounds)
    
    opt_tuning_params$scale_pos_weight <- scale_pos_weight_data
    
  }
  
  opt_tuning_params <- opt_tuning_params %>% transpose() %>% flatten()

  #--------------------------------------------------------------------------------------------#  
  # Run final model using k-fold CV and optimal set of hyperparameters. 
  #--------------------------------------------------------------------------------------------#
  plan(multicore, workers = ncores)
  
  if (isTRUE(cv_models)){ 
    set.seed(123444)
  } else{ 
    print('Bootstrap without seed.')
  }
  
  xgb_cv_model_final <- xgb.cv(data = xgb_train,
                               params = opt_tuning_params,
                               nrounds = ntrees, # Specified outside of function
                               showsd = TRUE, 
                               early_stopping_rounds = early_stop_global, # Specified outside of function 
                               maximize = FALSE, 
                               folds = val_folds_list, 
                               train_folds = train_folds_list, 
                               prediction = TRUE, 
                               nthread = ncores, 
                               verbose = FALSE)  
  #--------------------------------------------------------------------------------------------#
  opt_nrounds <- xgb_cv_model_final$best_iteration
  
  min_cv_mse <- min(xgb_cv_model_final$evaluation_log$test_error_mean)
  
  xgb_cv_preds <- data.frame(pred_probs = xgb_cv_model_final$pred)
  
  xgb_cv_preds <- xgb_cv_preds %>% mutate(cv_preds = as.numeric(pred_probs >= 0.5))
  
  xgb_cv_preds <- bind_cols(select(dta_untreated_wfolds, fold_id, GEOID, year, all_of(dep_var_string)), xgb_cv_preds)
  
  xgb_cv_preds <- xgb_cv_preds %>% 
    mutate(cv_preds = as.integer(cv_preds)) %>% 
    mutate(across(.cols = starts_with('low_access'), 
                  .fn = ~as.integer(.)))
  #--------------------------------------------------------------------------------------------#
  # Train model using full set of training data using optimal parameters. 
  # This final model is used to make predictions of the counterfactuals. 
  # -------------------------------------------------------------------------------------------- #
  plan(multicore, workers = ncores)
  
  if (isTRUE(cv_models)){ 
    set.seed(352)
  } else{ 
    print('Bootstrap without seed.') 
  }
  # -------------------------------------------------------------------------------------------- #
  xgb_model_final <- xgb.train(params = opt_tuning_params, 
                               data = xgb_train,
                               nrounds = opt_nrounds, 
                               early_stopping_rounds = NULL, 
                               nthread = ncores,
                               verbose = FALSE)
  # -------------------------------------------------------------------------------------------------------------------- #
  # Make predictions of counterfactuals using the treated data. 
  # -------------------------------------------------------------------------------------------------------------------- #
  dta_cf_preds <- data.frame(pred_probs_cf = predict(xgb_model_final, newdata = xgb_treat))
  
  dta_cf_preds <- dta_cf_preds %>%
    mutate(actual = y_treat, 
           pred_class_cf = as.numeric(pred_probs_cf >= 0.5), 
           tau = actual-pred_class_cf) %>%
    relocate(c(actual, pred_class_cf, tau), 
             .before = pred_probs_cf) %>%
    mutate(pred_class_cf = as.integer(pred_class_cf), 
           actual = as.integer(actual))
  
  dta_cf_preds <- bind_cols(select(dta_treated, GEOID, year), dta_cf_preds)
  
  # -------------------------------------------------------------------------------------------- #
  if (isTRUE(cv_models)){ 
    
    model_results <- list(min_cv_mse = min_cv_mse, #The min_cv_mse is not the MSE, but is the mininum misclassification rate.  
                          tuning_params_opt = opt_tuning_params, # Optimal set of tuning parameters corresponding to min_cv_mse_index. 
                          cv_errors_opt = xgb_cv_preds, # The combined cross-validated errors from each cross-validation fit. 
                          xgb_model_final = xgb_model_final, # The final XGBoost model using the complete set of training observations. 
                          data_cf_preds = dta_cf_preds) # Estimated counterfactuals.
  } else { 
    
    model_results <- list(min_cv_mse = min_cv_mse, #The min_cv_mse is the mininum misclassification rate.  
                          tuning_params_opt = opt_tuning_params, # Optimal set of tuning parameters corresponding to min_cv_mse_index. 
                          cv_errors_opt = xgb_cv_preds, # The combined cross-validated errors from each cross-validation fit. 
                          data_cf_preds = dta_cf_preds) # Estimated counterfactuals.
    
  }
  
  
  return(model_results)
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: xgboost_imputation_estimation')
#--------------------------------------------------------------------------------------------#