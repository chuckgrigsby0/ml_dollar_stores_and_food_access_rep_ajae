print('Sourced: preds_and_actual_on_time <- function(model_preds_dta, geography_str)')
#--------------------------------------------------------------------------------------------#
preds_and_actual_on_time <- function(model_preds_dta, national, geography_str){ 
  
  
  if (isTRUE(national)){
    
    pretr_preds <- model_preds_dta %>% 
      filter(rel_year < 0)
    
    posttr_preds <- model_preds_dta %>% 
      filter(rel_year >= 0) 
    
  } else {
    
    pretr_preds <- model_preds_dta %>% 
      filter(rel_year < 0) %>% 
      filter(grepl(geography_str, Geography)) 
    
    posttr_preds <- model_preds_dta %>% 
      filter(rel_year >= 0) %>% 
      filter(grepl(geography_str, Geography)) 
    
  }
  #--------------------------------------------------------------------------------------------#
  # Function to replicate each model. 
  #--------------------------------------------------------------------------------------------#
  regress_low_access_on_year <- function(dta, depvar, model_type){
    
    reg_form <- paste(depvar, '~ year - 1')
    reg_form <- as.formula(reg_form)
    
    outcome_on_year <- feols(reg_form, data = dta)
    
    outcome_on_year <- broom::tidy(outcome_on_year) %>% 
      tidyr::separate(term, 
                      into = c('string', 'year'), 
                      sep = '(?<=[a-z])(?=[0-9])') %>% # preceeded by character and followed by #
      select(-string) %>%
      mutate(Outcome = model_type, 
             year = as.numeric(year))  
    
  }
  #--------------------------------------------------------------------------------------------#
  pretr_preds_on_year <- regress_low_access_on_year(dta = pretr_preds, depvar = 'preds', model_type = 'CV Prediction')
  pretr_act_on_year <- regress_low_access_on_year(dta = pretr_preds, depvar = 'actual', model_type = 'CV Actual') # Note that model_dep_var is specified in the global env.
  
  posttr_preds_on_year <- regress_low_access_on_year(dta = posttr_preds, depvar = 'preds', model_type = 'Post-Treatment Counterfactual')
  posttr_act_on_year <- regress_low_access_on_year(dta = posttr_preds, depvar = 'actual', model_type = 'Post-Treatment Actual')
  #--------------------------------------------------------------------------------------------#
  # Combine results. 
  #--------------------------------------------------------------------------------------------#
  preds_and_actual_on_year <- bind_rows(pretr_preds_on_year, pretr_act_on_year, 
                                        posttr_preds_on_year, posttr_act_on_year)
  
  #--------------------------------------------------------------------------------------------#
  return(preds_and_actual_on_year) 
  
}
  

