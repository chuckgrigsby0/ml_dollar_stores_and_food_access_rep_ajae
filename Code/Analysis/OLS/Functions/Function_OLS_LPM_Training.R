# Estimate CV model and the estimated model to find counterfactuals using OLS (i.e., LPM)
# -------------------------------------------------------------------------------------------- #
lpm_imputation_estimation <- function(dep_var_string, dta_untreat, dta_treat){
  
  # Function to run k-fold CV, obtain predictions for pre-treatment data. 
  
  lpm_cv_function <- function(train_ids, val_ids){
    
    lpm_cv <- feols(la_formula, data = dta_untreat[train_ids, ])
    
    pred_lpm_cv <- data.frame(
      pred_probs = predict(lpm_cv, newdata = dta_untreat[val_ids, ])
    )
    
    pred_lpm_cv <- pred_lpm_cv %>%
      
      mutate(
        preds = as.integer(if_else(pred_probs >= 0.5, 1, 0))
        ) %>%
      
      bind_cols(
        select(
          dta_untreat[val_ids, ], fold_id, GEOID, year, all_of(dep_var_string)
        )
      ) %>%
      
      rename_with(.fn = \(x) str_replace_all(x, dep_var_string, 'actual') , 
                  .cols = all_of(dep_var_string) ) %>%
      
      select(
        fold_id, GEOID, year, actual, preds, pred_probs
      )
    
    return(pred_lpm_cv)
    
  }
  
  # Run lpm_cv_function to obtain CV predictions for pre-treatment data. 
  
  lpm_cv_preds <- map2_dfr(train_folds_list, val_folds_list, 
                           function(.x, .y){ 
                             
                             lpm_cv_function(train_ids = .x, 
                                             val_ids = .y)
                             
                             }) 
  
  # Re-estimate OLS model using full set of pre-treatment data and obtain counteractual predictions. 
  
  lpm_full <- feols(la_formula, data = dta_untreat)
  
  lpm_cf_preds <- data.frame( 
    
    pred_probs = predict(lpm_full, newdata = dta_treat)
    
    ) %>%
    
    mutate(
      
      preds = as.integer(if_else(pred_probs >= 0.5, 1, 0)), 
      
      actual = as.integer(dta_treat[[dep_var_string]]),
      
      tau = actual - preds
      
    ) %>%
    
    relocate(c(actual, preds, tau), 
             
             .before = pred_probs) %>%
    
    bind_cols(select(dta_treat, GEOID, year)) %>%
    
    select(GEOID, year, everything())
      
  # -------------------------------------------------------------------------------------------- #

    model_results <- list(data_cv_preds = lpm_cv_preds, # The combined cross-validated errors from each cross-validation fit. 
                          data_cf_preds = lpm_cf_preds) # Estimated counterfactuals.
  
    # -------------------------------------------------------------------------------------------- #
  
  return(model_results)
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: lpm_imputation_estimation')
#--------------------------------------------------------------------------------------------#