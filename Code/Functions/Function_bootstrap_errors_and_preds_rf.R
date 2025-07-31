print('Sourced: bootstrap_errors_and_preds_rf <- function(bootstrap_ids, iter)')

bootstrap_errors_and_preds <- function(bootstrap_ids, iter, bootstrap_by_tracts){ 
  
  dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access
  
  dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts, '_rf') # bootstrap_ids = '01_499'; bootstrap_by_tracts = NULL or '_tracts'
  
  filename <- paste(str_to_lower(model_geography), model_dep_var, 'rf', 'bootstrap', paste0(iter, '.rds'), sep = '_')
  
  model_output <- readRDS(here::here('Analysis',
                                     dir_geography,
                                     dir_dep_var, 
                                     dir_bootstrap, 
                                     filename))
  # -------------------------------------------------------------------------------------------- #
  # Note: We filter >= 2007 because for the untreated/yet-to-be-treated observations, we only have 
  # holdout predictions for years 2007-2020. 
  # -------------------------------------------------------------------------------------------- #
  untreated_preds <- model_output$cv_errors_opt %>% 
    
    left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
    
    select(GEOID, year, event_year, rel_year, Geography, actual, cv_preds, cv_error) %>%
    
    rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
    
    rename(err = cv_error) %>%
    
    filter(year >= '2007') # Cross-validated predictions are made for 2007-2020. 
  # -------------------------------------------------------------------------------------------- #
  treated_preds <- model_output$data_cf_preds %>%
    
    left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
    
    select(GEOID, year, event_year, rel_year, Geography, actual, cf_preds, tau) %>%
    
    rename(preds = cf_preds) %>%
    
    filter(year >= '2006') # Post-treatment effects are assessed from 2006-2020.
  # -------------------------------------------------------------------------------------------- #
  model_preds <- bind_rows(untreated_preds, treated_preds) %>%
    
    filter(is.finite(rel_year))
  # -------------------------------------------------------------------------------------------- #
  
  # Data to assess errors by relative time.
  pretr_preds <- model_preds %>% 
    filter(rel_year < 0) %>% 
    filter(grepl(model_geography, Geography)) %>%
    select(-tau) %>% 
    mutate(rel_year = factor(rel_year))
  
  posttr_preds <- model_preds %>% 
    
    mutate(rel_year = factor(rel_year)) %>%
    
    filter(grepl(model_geography, Geography)) %>%
    
    filter(year >= '2006')
  # -------------------------------------------------------------------------------------------- #
  
  # Regress errors on relative time without intercept. 
  pretr_model_errors <- lm(err ~ rel_year - 1, data = pretr_preds)
  
  model_errors_coefs <- tidy_regression(lm_model = pretr_model_errors, 
                                        dep_var_string = 'CV Error')
  
  # For plots, convert rel_year to numeric and add a new column called id to identify bootstrap iteration. 
  model_errors_coefs <- model_errors_coefs %>% mutate(rel_year = as.numeric(rel_year), 
                                                      id = iter)
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  # Regress predictions on relative time, which includes both pre- and post-treatement data. 
  cf_preds_coefs <- lm(preds ~ rel_year - 1, data = posttr_preds)
  
  cf_preds_coefs <- tidy_regression(lm_model = cf_preds_coefs, 
                                    dep_var_string = 'Predicted')
  
  cf_preds_coefs <- cf_preds_coefs %>% mutate(id = iter)
  
  # Regress actual on relative time, which includes both pre- and post-treatement data.
  
  reg_form <- xpd(actual ~ rel_year - 1, data = posttr_preds)
  
  actual_coefs <- lm(reg_form, data = posttr_preds)
  
  actual_coefs <- tidy_regression(lm_model = actual_coefs, 
                                  dep_var_string = 'Actual')
  
  actual_coefs <- actual_coefs %>% mutate(id = iter) # Add bootstrap id.
  
  # Combine the tables. 
  # convert rel_year to numeric for plots
  
  preds_vs_actual <- bind_rows(actual_coefs, cf_preds_coefs) %>% mutate(rel_year = as.numeric(rel_year))
  
  # Add the CV MSE from the model. 
  
  preds_vs_actual <- preds_vs_actual %>% mutate(cv_mse = model_output$min_cv_mse) %>% relocate(id, .after = last_col())
  
  # -------------------------------------------------------------------------------------------- #
  
  # Estimation of causal effects.   
  
  # Effects on relative time. 
  # Relative time at period 0 and greater implies years from 2006-2020. 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects <- model_preds %>% 
    filter(rel_year >= 0) %>% 
    filter(grepl(model_geography, Geography)) %>%
    mutate(rel_year = factor(rel_year))
  
  # -------------------------------------------------------------------------------------------- #
  effects_rel_year <- lm(tau ~ rel_year - 1, data = posttre_effects)
  
  effects_rel_year <- tidy_regression(lm_model = effects_rel_year, 
                                      dep_var_string = 'Tau')
  
  # For plots, convert rel_year to numeric. 
  effects_rel_year <- effects_rel_year %>% mutate(rel_year = as.numeric(rel_year), 
                                                  id = iter)
  
  #--------------------------------------------------------------------------------------------#
  
  reg_results <- list('errors' = model_errors_coefs, 
                      'predictions' = preds_vs_actual, 
                      'effects_rel_year' = effects_rel_year) 
  
  return(reg_results)
  
}