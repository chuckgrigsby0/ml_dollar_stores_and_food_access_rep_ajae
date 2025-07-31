print('Sourced: bootstrap_errors_and_preds <- function(bootstrap_ids, iter)')
bootstrap_errors_and_preds <- function(prep_bootstrap_data, bootstrap_ids, iter, bootstrap_by_tracts){ 
  
  if (isTRUE(prep_bootstrap_data)){
    
    dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap
    
    dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access
    
    dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) # bootstrap_ids = '01_499'; bootstrap_by_tracts = NULL or '_tracts'
    
    filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(iter, '.rds'), sep = '_')
    
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
      
      select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds, pred_probs) %>%
      
      rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
      
      rename_with(.cols = starts_with('low_access'), 
                  .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                         replacement = 'actual') ) %>%
      
      filter(year >= '2007') # We obtain CV predictions from 2007-2020
    # -------------------------------------------------------------------------------------------- #
    treated_preds <- model_output$data_cf_preds %>%
      
      left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
      
      select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, pred_probs_cf) %>%
      
      rename(preds = pred_class_cf) %>%
      rename(pred_probs = pred_probs_cf) %>%
      
      filter(year >= '2006') # Post-treatment counterfactuals from 2006 onward.
    # -------------------------------------------------------------------------------------------- #
    model_preds <- bind_rows(untreated_preds, treated_preds) %>%
      # Filter out observations with rel_year == Inf because these are never-treated observations. 
      filter(is.finite(rel_year)) 
    
  }
    # -------------------------------------------------------------------------------------------- #
  dta_for_metrics <- model_preds %>%
    filter(rel_year < 0) %>% 
    mutate(across(.cols = c(actual, preds), 
                  .fns = \(x) factor(x, levels = c('1', '0') ) ) ) %>% 
    select(rel_year, actual, pred_probs, preds)
  
  roc_dta <- dta_for_metrics %>%
    roc_curve(truth = actual, 
              pred_probs) %>%
    mutate(j_index = (sensitivity + specificity - 1), 
           fpr = 1 - specificity, 
           positive = sum(dta_for_metrics$actual == '1'), 
           negative = sum(dta_for_metrics$actual == '0'), 
           tp = sensitivity * positive, 
           fp = fpr * negative, 
           precision = tp/(tp+fp), 
           f1 = (2*(precision*sensitivity))/(precision + sensitivity))
  
  # Find optimal youden's index/j-index
  opt_j_index <- roc_dta[which.max(roc_dta$j_index), ]
  opt_j_index$threshold_metric <- 'j_index'
  
  # Find optimal F1 measure
  opt_f1 <- roc_dta[which.max(roc_dta$f1), ]
  opt_f1$threshold_metric <- 'f1'
  
  # Average the j-index and f1 measure
  opt_avg_j_f1 = mean(c(opt_j_index$.threshold, opt_f1$.threshold))
  opt_avg_j_f1 <- data.frame(.threshold = opt_avg_j_f1, threshold_metric = 'avg')
  
  # Optimal F-1 divided by two. 
  opt_lipton <- data.frame(.threshold = (opt_f1$f1 / 2), threshold_metric = 'lipton')
  
  # Combine the three optimal thresholds in a data frame. 
  opt_threshold_dta <- bind_rows(opt_j_index, opt_f1, opt_avg_j_f1, opt_lipton)
  
  opt_threshold_dta$id <- iter
  # -------------------------------------------------------------------------------------------- #
  # Create new predictions based on updated thresholds. 
  # -------------------------------------------------------------------------------------------- #
  model_preds <- model_preds %>% 
    mutate(preds_j_index = if_else(pred_probs >= opt_j_index$.threshold, 1, 0), 
           preds_f1 = if_else(pred_probs >= opt_f1$.threshold, 1, 0), 
           preds_avg = if_else(pred_probs >= opt_avg_j_f1$.threshold, 1, 0), 
           preds_lipton = if_else(pred_probs >= opt_lipton$.threshold, 1, 0))
  # -------------------------------------------------------------------------------------------- #
  
  # Data to assess errors by relative time. 2007-2019
  pretr_preds <- model_preds %>% 
    filter(rel_year < 0) %>% 
    filter(grepl(model_geography, Geography)) %>%
    select(-tau) %>% 
    mutate(err_orig = actual - preds, 
           err_j_index = actual - preds_j_index, 
           err_f1 = actual - preds_f1, 
           err_avg = actual - preds_avg, 
           err_lipton = actual - preds_lipton, 
           rel_year = factor(rel_year))
  # -------------------------------------------------------------------------------------------- #
  
  # Regress errors on relative time without intercept. 
  est_errors <- str_subset(names(pretr_preds), '^err')
  
  pretr_model_errors <- feols(.[est_errors] ~ rel_year - 1, data = pretr_preds)
  
  pretr_model_errors <- set_names(pretr_model_errors, nm = est_errors)
  
  model_errors_coefs <- map2_dfr(pretr_model_errors, est_errors, 
            function(.x, .y){ 
      
      tidy_res <- tidy(.x) %>% 
        mutate(term = str_remove_all(term, 'rel_year'), 
               Outcome = .y, 
               id = iter) %>%
        rename('rel_year' = 'term') %>%
        mutate(rel_year = as.numeric(rel_year))
      
      })
  # -------------------------------------------------------------------------------------------- #
  
  # Data to assess actual vs predicted outcomes by relative time. 
  posttr_preds <- model_preds %>% 
    
    mutate(rel_year = factor(rel_year)) %>%
    
    filter(grepl(model_geography, Geography)) %>%
    
    filter(year >= '2006') %>%
    
    rename('preds_orig' = 'preds')
  
  # -------------------------------------------------------------------------------------------- #
  est_preds <- str_subset(names(posttr_preds), '^preds'); est_preds
  
  # Regress predictions on relative time, which includes both pre- and post-treatement data. 
  
  cf_preds_coefs <- feols(.[est_preds] ~ rel_year - 1, data = posttr_preds)
  
  cf_preds_coefs <- set_names(cf_preds_coefs, nm = est_preds)
  
  cf_preds_coefs <- map2_dfr(cf_preds_coefs, est_preds, 
                                 function(.x, .y){ 
                                   
                                   tidy_res <- tidy(.x) %>% 
                                     mutate(term = str_remove_all(term, 'rel_year'), 
                                            Outcome = .y, 
                                            id = iter) %>%
                                     rename('rel_year' = 'term') %>%
                                     mutate(rel_year = as.numeric(rel_year))
                                   
                                 })
  # -------------------------------------------------------------------------------------------- #
  # Regress actual on relative time, which includes both pre- and post-treatment data.
  
  reg_form <- xpd(actual ~ rel_year - 1, data = posttr_preds)
  
  actual_coefs <- lm(reg_form, data = posttr_preds)
  
  actual_coefs <- tidy_regression(lm_model = actual_coefs, 
                                  dep_var_string = 'actual')
  
  actual_coefs <- actual_coefs %>% mutate(id = iter) # Add bootstrap id.
  
  actual_coefs <- actual_coefs %>% mutate(rel_year = as.numeric(rel_year))
  
  # Combine the tables. 
  # convert rel_year to numeric for plots
  preds_vs_actual <- bind_rows(actual_coefs, cf_preds_coefs)
  
  # Add the CV MSE from the model. 
  
  preds_vs_actual <- preds_vs_actual %>% relocate(id, .after = last_col())
  
  # -------------------------------------------------------------------------------------------- #
  # Estimation of causal effects.   
  
  # Effects on relative time. 
  # Relative time at period 0 and greater implies years from 2006-2020. 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects <- model_preds %>% 
    filter(rel_year >= 0) %>% 
    filter(grepl(model_geography, Geography)) %>%
    mutate(tau_j_index = actual - preds_j_index, 
           tau_f1 = actual - preds_f1, 
           tau_avg = actual - preds_avg, 
           tau_lipton = actual - preds_lipton,
           rel_year = factor(rel_year)) %>%
    rename_with(.cols = c('tau', 'preds'), 
                .fn = \(x) paste0(x, '_orig') )
  # -------------------------------------------------------------------------------------------- #
  est_tau <- str_subset(names(posttre_effects), 'tau'); est_tau
  
  effects_rel_year <- feols(.[est_tau] ~ rel_year - 1, data = posttre_effects)
  
  effects_rel_year <- set_names(effects_rel_year, nm = est_tau)
  
  effects_rel_year <- map2_dfr(effects_rel_year, est_tau, 
                              function(.x, .y){ 
                               
                               tidy_tau <- tidy(.x) %>% 
                                 mutate(term = str_remove_all(term, 'rel_year'), 
                                        Outcome = .y, 
                                        id = iter) %>%
                                 rename('rel_year' = 'term') %>%
                                 mutate(rel_year = as.numeric(rel_year))
                               
                             })
  #--------------------------------------------------------------------------------------------#
  
  # Overall Pre-treatment CV Errors
  
  #--------------------------------------------------------------------------------------------#
  cv_errors <- pretr_preds %>% 
  
    rename_with(.cols = c('preds'), 
                .fn = \(x) paste0(x, '_orig') ) 
  
  summary_vars_pretr <- c('actual', est_errors, str_subset(names(cv_errors), '^preds'))
  
  cv_errors <- cv_errors %>%
    
    summarise(across(.cols = all_of(summary_vars_pretr), 
                     .fn = list('avg' = mean), 
                     .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_err_orig = err_orig_avg/preds_orig_avg,
           pct_err_j_index = err_j_index_avg/preds_j_index_avg,
           pct_err_f1 = err_f1_avg/preds_f1_avg,
           pct_err_avg = err_avg_avg/preds_avg_avg, 
           pct_err_lipton = err_lipton_avg/preds_lipton_avg,
           id = iter, 
           percentage_type = 'All')

  # -------------------------------------------------------------------------------------------- #
  
  # Percentage change in the ATT by relative time. 
  
  summary_vars <- c(est_tau, est_preds, 'actual')
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_relyear <- posttre_effects %>% 
    
    group_by(rel_year) %>%
    
    summarise( across(.cols = all_of(summary_vars), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%

    mutate(pct_att_orig = tau_orig_avg/preds_orig_avg,
           pct_att_j_index = tau_j_index_avg/preds_j_index_avg,
           pct_att_f1 = tau_f1_avg/preds_f1_avg,
           pct_att_avg = tau_avg_avg/preds_avg_avg,
           pct_att_lipton = tau_lipton_avg/preds_lipton_avg,
           id = iter, 
           percentage_type = 'Relative Time')
  # -------------------------------------------------------------------------------------------- #
  # Percentage change in the ATT by year. 
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_year <- posttre_effects %>% 
    
    group_by(year) %>%
    
    summarise( across(.cols = all_of(summary_vars), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att_orig = tau_orig_avg/preds_orig_avg,
           pct_att_j_index = tau_j_index_avg/preds_j_index_avg,
           pct_att_f1 = tau_f1_avg/preds_f1_avg,
           pct_att_avg = tau_avg_avg/preds_avg_avg, 
           pct_att_lipton = tau_lipton_avg/preds_lipton_avg,
           id = iter, 
           percentage_type = 'Year')
  # -------------------------------------------------------------------------------------------- #
  # Overall percentage change in the ATT.
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_att <- posttre_effects %>% 
    
    summarise( across(.cols = all_of(summary_vars), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att_orig = tau_orig_avg/preds_orig_avg,
           pct_att_j_index = tau_j_index_avg/preds_j_index_avg,
           pct_att_f1 = tau_f1_avg/preds_f1_avg,
           pct_att_avg = tau_avg_avg/preds_avg_avg, 
           pct_att_lipton = tau_lipton_avg/preds_lipton_avg,
           id = iter, 
           percentage_type = 'All')
  
  
  outcome_change <- bind_rows(outcome_pct_change_relyear, 
                              outcome_pct_change_year, 
                              outcome_pct_change_att)
  # -------------------------------------------------------------------------------------------- #
  # Save results in list. 
  # -------------------------------------------------------------------------------------------- #
  reg_results <- list('cv_errors_summary' = cv_errors, 
                      'errors_rel_year' = model_errors_coefs, 
                      'predictions' = preds_vs_actual, 
                      'effects_rel_year' = effects_rel_year, 
                      'att' = outcome_change,
                      'optimal_thresholds' = opt_threshold_dta, 
                      'roc_dta' = roc_dta) 
  
  return(reg_results)
  
}