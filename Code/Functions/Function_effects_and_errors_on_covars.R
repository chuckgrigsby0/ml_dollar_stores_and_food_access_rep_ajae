print('Sourced: effects_and_errors_on_covars <- function(national, geography_str, model_preds_dta, pretr_binned_covars_dta)')

effects_and_errors_on_covars <- function(national, geography_str, model_preds_dta, pretr_binned_covars_dta, boot_iter){
  
  
  if (isTRUE(national)){ 
    
    posttre_effects <- model_preds_dta %>% 
      
      filter(rel_year >= 0) %>% 
      
      mutate(rel_year = factor(rel_year))
    
  } else { 
    
    posttre_effects <- model_preds_dta %>% 
      
      filter(rel_year >= 0) %>% 
      
      filter(grepl(geography_str, Geography)) %>%
      
      mutate(rel_year = factor(rel_year))
  }
  # -------------------------------------------------------------------------------------------- #
  # Prepare data for pre-treatment analyses and post-treatment analyses. 
  # Note: data preparation is already completed for pre-treatment data. See pretr_binned_covars. 
  # -------------------------------------------------------------------------------------------- #
  
  # tau and predictors
  
  posttre_effects_wcovars <- posttre_effects %>%
    select(GEOID, year, event_year, rel_year, tau, all_of(model_covars)) 
  
  # tau and dollar store entry/counts; regional-divisional dummies.
  
  # Regional/Divisional var. string. 
  geog_vars <- c('REGION', 'DIVISION'); geog_vars
  
  posttre_effects_wdsentry <- posttre_effects %>%
    select(GEOID, year, event_year, rel_year, tau, all_of(geog_vars)) %>%
    left_join(posttr_binned_dsvars, by = c('GEOID', 'year', 'event_year', 'rel_year')) # Joins binned dollar store variables. 
  
  # tau and binned predictors
  
  posttre_effects_wbinnedcovars <- posttre_effects %>%
    select(GEOID, year, event_year, rel_year, tau) %>%
    left_join(posttr_binned_covars, by = c('GEOID', 'year', 'event_year', 'rel_year')) # Joins binned post-treatment covars. 
  # -------------------------------------------------------------------------------------------- #
  posttr_binned_covars_str <- names(posttre_effects_wbinnedcovars)[!grepl('GEOID|^year$|event_year|rel_year|tau', 
                                                                          names(posttre_effects_wbinnedcovars))]
  # -------------------------------------------------------------------------------------------- #
  # Separate binned covariates from the unbinned (factor/integer/dummy) predictors 
  # Not binned vars. 
  # -------------------------------------------------------------------------------------------- #
  posttr_unbinned_covars_str <- posttr_binned_covars_str[!grepl('bins', posttr_binned_covars_str)]
  
  # -------------------------------------------------------------------------------------------- #
  # Filter out urban_area and uc_area from the Rural models. 
  # -------------------------------------------------------------------------------------------- #
  if (model_geography == 'Rural'){ 
    posttr_unbinned_covars_str <- posttr_unbinned_covars_str[!grepl('^urban_area$|^uc_area$', posttr_unbinned_covars_str)]
  } else if (model_geography == 'Urban'){ 
    posttr_unbinned_covars_str <- posttr_unbinned_covars_str[!grepl('^uc_area$', posttr_unbinned_covars_str)]
    }
  print(posttr_unbinned_covars_str)
  # -------------------------------------------------------------------------------------------- #
  # Binned vars. 
  posttr_binned_covars_str <- posttr_binned_covars_str[grepl('bins', posttr_binned_covars_str)]; posttr_binned_covars_str
  # -------------------------------------------------------------------------------------------- #
  
  
  # -------------------------------------------------------------------------------------------- #
  # CV Errors on covariates. 
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  errors_on_covars_binned <- function(dta_binned, covariates, id){
    
    out <- tryCatch({ 
      
    dta_binned <- dta_binned %>% select(all_of(pretr_key_vars), err, all_of(covariates[id]))
    
    reg_form_bins <- xpd(err ~ .[ covariates[id] ] - 1)
    
    model_errors_vs_covars <- feols(reg_form_bins, data = dta_binned)
    
    model_errors_vs_covars <- tidy_regression_err_vs_covar_binned(model_errors_vs_covars) # Called from Function_tidy_regression.R
    
    model_errors_vs_covars <- model_errors_vs_covars %>% mutate(label = 'Average CV Error in Bin', 
                                                                outcome = 'err', 
                                                                covariate = covariates[id], # For plots and combining with all covariates. 
                                                                boot_iteration = boot_iter)
      
    
    return(model_errors_vs_covars) }, 
    
    error = function(cond){ 
      message(paste('Not enough bins in', covariates[id]) )
      message(paste(cond))
      return(NULL) },
    warning = function(cond){ 
      message(paste('Warning emitted for', covariates[id]))
      message(paste(cond))
      return(NULL)
    })
    return(out)
      }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  errors_on_binned_covars_df <- seq_along(pretr_binned_covars_str) %>% 
    
    map_dfr(function(.x){ 
      
      errors_on_covars_binned(dta_binned = pretr_binned_covars_dta, 
                              covariates = pretr_binned_covars_str, 
                              id = .x)
      
    })
  # -------------------------------------------------------------------------------------------- #
  # Errors on integers/factors. 
  # -------------------------------------------------------------------------------------------- #
  errors_on_integer_covars <- function(dta_binned, id, covariates){
    
    reg_form_int <- xpd(err ~ .[ covariates[id] ] - 1)
    
    model_errors_vs_covars_int <- feols(reg_form_int, data = dta_binned)
    
    model_errors_vs_covars_int <- broom::tidy(model_errors_vs_covars_int)
    
    model_errors_vs_covars_int <- model_errors_vs_covars_int %>% 
      
      mutate(term = str_replace_all(term, covariates[id], '')) %>% # From the column term, remove the covariate string name. 
    
      mutate(label = 'Average CV Error in Bin', 
             outcome = 'err', 
             covariate = covariates[id], 
             boot_iteration = boot_iter)
  }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  errors_on_int_covars_df <- seq_along(pretr_unbinned_covars_str) %>% 
    
    map_dfr(function(.x){ 
      
      errors_on_integer_covars(dta_binned = pretr_binned_covars_dta, 
                               covariates = pretr_unbinned_covars_str, 
                               id = .x)
      
    })
  # -------------------------------------------------------------------------------------------- #
  
  
  # -------------------------------------------------------------------------------------------- #
  # CV errors on normalized/standardized covariates. 
  # -------------------------------------------------------------------------------------------- #
  # Top code values at and above (3rd Quartile + 1.5*IQR). 
  # -------------------------------------------------------------------------------------------- #
  top_code_vars <- c('park_access_7nn', 'distance_to_urban_area', 'total_population') 
  
  top_code_vars_df <- top_code_vars %>% 
    
    map_dfc(function(.x){ 
    
    top_code_vars_iqr(dta = pretr_preds, 
                      covariate = .x) 
  })
  # Save original column names for the same ordering after adding the top-coded (bottom-coded) values. 
  names_orig <- names(pretr_preds)
  
  pretr_preds_norm <- pretr_preds %>% 
    select(-all_of(top_code_vars)) %>% # Remove original vars. 
    bind_cols(top_code_vars_df) %>% # Add top (bottom) coded vars. 
    select(all_of(names_orig)) # Reorder cols to original order. 
  # -------------------------------------------------------------------------------------------- #
  # Normalize the covariates such that their mean is 0 and variance is 1. 
  # -------------------------------------------------------------------------------------------- #
  # Create a recipe. 
  # -------------------------------------------------------------------------------------------- #
  rec <- pretr_preds_norm %>% recipe(err ~ ., data = .)
  
  # Compute a step for the recipe. 
  normalize_covars <- rec %>% step_normalize(all_of(model_covars))
  
  # Prepare the recipe. 
  prep_norm <- normalize_covars %>% prep(training = pretr_preds_norm)
  
  # Bake the recipe.
  pretr_preds_norm <- prep_norm %>% bake(new_data = NULL, 
                                         all_of(pretr_key_vars), err, 
                                         all_of(model_covars))
  # -------------------------------------------------------------------------------------------- #
  # Function that regresses errors on normalized covariates, covariate-by-covariate.
  # -------------------------------------------------------------------------------------------- #
  errors_on_covariates_norm <- function(covar_id, dta){
    
    
    dta <- dta %>% select(err, model_covars[covar_id])
    # -------------------------------------------------------------------------------------------- #
    
    # -------------------------------------------------------------------------------------------- #
    reg_form <- xpd(err ~ .[ model_covars[covar_id] ], data = dta )
    
    errors_on_covars <- lm(reg_form, data = dta)
    
    errors_on_covars <- broom::tidy(errors_on_covars) %>% 
      filter(!grepl('Intercept', term)) %>% 
      mutate(label = 'Coefficient Estimate', 
             outcome = 'err', 
             covariate = model_covars[covar_id], 
             boot_iteration = boot_iter)
    
    return(errors_on_covars)
  }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  
  errors_on_norm_covars_df <- seq_len(length(model_covars)) %>% 
    
    map_dfr(function(.x){ 
      
      errors_on_covariates_norm(covar_id = .x, 
                                dta = pretr_preds_norm)
      
    })
  
  # -------------------------------------------------------------------------------------------- #
  # Effects on binned covariates. 
  # -------------------------------------------------------------------------------------------- #
  
  # Create a vector of keys to be added to the binned and standardized covariates in post-treatment periods. 
  
  posttr_key_vars <- setdiff(names(posttre_effects_wcovars), model_covars); posttr_key_vars # Note: Includes tau
  
  
  # -------------------------------------------------------------------------------------------- #
  effects_on_binned_covars <- function(dta_binned, covariates, id){ 
    
    out <- tryCatch({ 
      
    dta_binned <- dta_binned %>% select(all_of(posttr_key_vars), all_of(covariates[id]))
    
    reg_form_bins <- xpd(tau ~ .[ covariates[id] ] - 1)
    
    model_effects_vs_covars <- feols(reg_form_bins, data = dta_binned)
    
    # Function can be used with err or tau. 
    model_effects_vs_covars <- tidy_regression_err_vs_covar_binned(model_effects_vs_covars) # Called from Function_tidy_regression.R
    
    model_effects_vs_covars <- model_effects_vs_covars %>% mutate(label = 'Average Effect in Bin', 
                                                                outcome = 'tau', 
                                                                covariate = covariates[id], # For plots and combining with all covariates. 
                                                                boot_iteration = boot_iter) 
    
    return(model_effects_vs_covars) }, 
    
    error = function(cond){ 
      message(paste('Not enough bins in', covariates[id]) )
      message(paste(cond))
      return(NULL) },
    warning = function(cond){ 
      message(paste('Warning emitted for', covariates[id]))
      message(paste(cond))
      return(NULL)
    } )
    return(out)
  }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  effects_on_binned_covars_df <- seq_along(posttr_binned_covars_str) %>% 
    
    map_dfr(function(.x){ 
      
      effects_on_binned_covars(dta_binned = posttre_effects_wbinnedcovars, 
                               covariates = posttr_binned_covars_str, 
                               id = .x)
      
    })
  # -------------------------------------------------------------------------------------------- #
  # Effects on integers/factors. 
  # -------------------------------------------------------------------------------------------- #
  effects_on_integer_covars <- function(dta_binned, id, covariates){
    
    reg_form_int <- xpd(tau ~ .[ covariates[id] ] - 1)
    
    model_effects_vs_covars_int <- feols(reg_form_int, data = dta_binned)
    
    model_effects_vs_covars_int <- broom::tidy(model_effects_vs_covars_int)
    
    model_effects_vs_covars_int <- model_effects_vs_covars_int %>% 
      
      mutate(term = str_replace_all(term, covariates[id], '')) %>% # From the column term, remove the covariate string name. 
      
      mutate(label = 'Average Effect in Bin', 
             outcome = 'tau', 
             covariate = covariates[id], 
             boot_iteration = boot_iter)
  }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  effects_on_int_covars_df <- seq_along(posttr_unbinned_covars_str) %>% 
    
    map_dfr(function(.x){ 
      
      effects_on_integer_covars(dta_binned = posttre_effects_wbinnedcovars, 
                                covariates = posttr_unbinned_covars_str, 
                                id = .x)
      
    })
  # -------------------------------------------------------------------------------------------- #
  # Effects (tau) on normalized/standardized covariates. 
  # -------------------------------------------------------------------------------------------- #
  # Top code values at and above (3rd Quartile + 1.5*IQR). 
  # -------------------------------------------------------------------------------------------- #
  top_code_vars <- c('park_access_7nn', 'distance_to_urban_area', 'total_population')
  
  top_code_vars_df <- top_code_vars %>% map_dfc(function(.x){ 
    
    top_code_vars_iqr(dta = posttre_effects_wcovars, 
                      covariate = .x) 
  })
  # Save original column names for the same ordering after adding the top-coded (bottom-coded) values. 
  names_orig <- names(posttre_effects_wcovars)
  
  posttre_effects_wcovars_norm <- posttre_effects_wcovars %>% 
    select(-all_of(top_code_vars)) %>% # Remove original vars. 
    bind_cols(top_code_vars_df) %>% # Add top (bottom) coded vars. 
    select(all_of(names_orig)) # Reorder cols to original order.
  # -------------------------------------------------------------------------------------------- #
  # Normalize the covariates such that their mean is 0 and variance is 1. 
  # -------------------------------------------------------------------------------------------- #
  # Create a recipe. 
  # -------------------------------------------------------------------------------------------- #
  
  rec <- posttre_effects_wcovars_norm %>% recipe(tau ~ ., data = .)
  
  # Compute a step for the recipe. 
  normalize_covars <- rec %>% step_normalize(all_of(model_covars))
  
  # Prepare the recipe. 
  prep_norm <- normalize_covars %>% prep(training = posttre_effects_wcovars_norm)
  
  # Bake the recipe.
  posttre_effects_wcovars_norm <- prep_norm %>% bake(new_data = NULL, 
                                                     all_of(posttr_key_vars), 
                                                     all_of(model_covars))
  # -------------------------------------------------------------------------------------------- #
  # Function that regresses treatment effect on normalized covariates, covariate-by-covariate.
  # -------------------------------------------------------------------------------------------- #
  effects_on_covariates_norm <- function(covar_id, dta){ 
   
     
    dta <- dta %>% select(tau, model_covars[covar_id])
    
    reg_form <- xpd(tau ~ .[ model_covars[covar_id] ] )
    
    effects_on_covars <- lm(reg_form, data = dta)
    
    effects_on_covars <- broom::tidy(effects_on_covars) %>% 
      filter(!grepl('Intercept', term)) %>% 
      mutate(label = 'Coefficient Estimate', 
             outcome = 'tau', 
             covariate = model_covars[covar_id], 
             boot_iteration = boot_iter)
    
    return(effects_on_covars)
  }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  
  effects_on_norm_covars_df <- seq_len(length(model_covars)) %>% 
    
    map_dfr(function(.x){ 
      
      effects_on_covariates_norm(covar_id = .x, 
                                 dta = posttre_effects_wcovars_norm)
      
    })
  # -------------------------------------------------------------------------------------------- #
  
  # Effects on dollar store entries and regional and divisional dummy variables. 
  
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  # Create character vector strings to use in regression models. 
  # -------------------------------------------------------------------------------------------- #
  ds_vars_factors <- sort(names(posttre_effects_wdsentry)[grepl('_int$|_bins$', names(posttre_effects_wdsentry))])
  # -------------------------------------------------------------------------------------------- #
  
  # Effects regressed on factor dollar store counts, net cumulative entry, and entry events. 
  
  # -------------------------------------------------------------------------------------------- #
  effects_on_ds_fact_vars <- function(posttr_dta, id, covariates){
    
    reg_form <- xpd(tau ~ .[ covariates[id] ] - 1)
    
    effects_on_dsvar <- lm(reg_form, data = posttr_dta)
    
    effects_on_dsvar <- broom::tidy(effects_on_dsvar)
    
    effects_on_dsvar <- effects_on_dsvar %>% 
      
      mutate(term = str_remove_all(term, covariates[id])) %>% # From the column term, remove the covariate string name. 
      
      mutate(label = 'Average Effect in Bin', 
             outcome = 'tau', 
             covariate = covariates[id], 
             boot_iteration = boot_iter)
    
    
    # Add less than or equal to sign to first term. 
    first_term <- effects_on_dsvar$term[1] 
    first_term <- str_replace_all(first_term, '^\\[.*,', '\u2264')
    first_term <- str_remove_all(first_term, '\\]')
    effects_on_dsvar$term[1] <- first_term
    
    # Add greater than  sign to last term. 
    last_term <- nrow(effects_on_dsvar)
    last_term_str <- effects_on_dsvar$term[last_term]
    last_term_str <- str_replace_all(last_term_str, '\\(', '\u003e ') %>% 
      str_remove_all(., ',[[:digit:]]+\\]') %>% str_trim(., side = 'right')
    
    effects_on_dsvar$term[last_term] <- last_term_str
    
    return(effects_on_dsvar)
    
  }
  
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  effects_on_factor_ds_vars_df <- seq_along(ds_vars_factors) %>%
    
    map_dfr(function(.x){ 
  
      effects_on_ds_fact_vars(posttr_dta = posttre_effects_wdsentry, 
                              id = .x,
                              covariates = ds_vars_factors)    
      
      })
  # -------------------------------------------------------------------------------------------- #
  
  # dollar store counts, entry events, and cumulative entry on years and relative time. 
  
  # -------------------------------------------------------------------------------------------- #
  ds_vars <- c("DS_Count_10mile", "net_entry_cumsum", "entry_events", 'gross_entry_cumsum'); ds_vars
  
  time_vars <- c('year', 'rel_year') # Used in inner loop of ds_vars_numeric_on_time_df
  
  # Use in ds_vars_on_time function below for the label variable in mutate().
  
  tidy_time_str <- c('_' = ' ', 
                     'year' = 'Year', 
                     'rel Year' = 'Relative Time')
  # -------------------------------------------------------------------------------------------- #
  ds_vars_on_time <- function(posttr_dta, outcomes, covariates, y_id, x_id){
    
    reg_form <- xpd(.[ outcomes[y_id] ] ~ .[ covariates[x_id] ] - 1)
    
    dsvar_on_time <- lm(reg_form, data = posttr_dta)
    
    dsvar_on_time <- broom::tidy(dsvar_on_time)
    
    dsvar_on_time <- dsvar_on_time %>% 
      
      mutate(term = str_remove_all(term, covariates[x_id])) %>% # From the column term, remove the covariate string name. 
      
      mutate(label = paste('Average by', str_replace_all(covariates[x_id], tidy_time_str)),
             outcome = outcomes[y_id],
             covariate = covariates[x_id], 
             boot_iteration = boot_iter)
  }
  # -------------------------------------------------------------------------------------------- #
  # Apply function
  # -------------------------------------------------------------------------------------------- #
  ds_vars_numeric_on_time_df <- seq_along(ds_vars) %>%
    
    map_dfr(function(.y){ # For each dollar store outcome. 
      
      seq_along(time_vars) %>%
        
        map_dfr(function(.x){ # For each time independent variable. 
      
      ds_vars_on_time(posttr_dta = posttre_effects_wdsentry,
                      outcomes = ds_vars, 
                      covariates = time_vars, 
                      y_id = .y, 
                      x_id = .x)    
      
    }) 
      })
  # -------------------------------------------------------------------------------------------- #
  # Effects by Region and Division
  # -------------------------------------------------------------------------------------------- #
  # Data Preparation. 
  # -------------------------------------------------------------------------------------------- #
  init_region_levels = sort(us_regions_key$REGION) # To tidy factors. 
  init_division_levels = sort(us_divisions_key$DIVISION)
  # -------------------------------------------------------------------------------------------- #
  # Convert predictors to factors. 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_wdsentry <- posttre_effects_wdsentry %>% 
    
    mutate(REGION = factor(REGION, 
                           levels = init_region_levels, 
                           ordered = FALSE), 
           DIVISION = factor(DIVISION, 
                             levels = init_division_levels, 
                             ordered = FALSE) ) 
  
 
  effects_by_geography <- function(posttr_dta, geographic_var){
    
    # -------------------------------------------------------------------------------------------- #
    
    reg_form <- xpd(tau ~ .[geographic_var] - 1) # DIVISION or REGION
    
    effects_on_geog <- lm(reg_form, data = posttr_dta)
    
    effects_on_geog <- broom::tidy(effects_on_geog)
    
    effects_on_geog <- effects_on_geog %>% 
      mutate(label = paste('Average Effect by', str_to_title(str_to_lower(geographic_var))), # e.g., REGION or DIVISION. 
             outcome = 'tau', 
             covariate = levels(posttr_dta[[geographic_var]]), 
             boot_iteration = boot_iter)
    
    return(effects_on_geog)
  }
  
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  
  # Effects by Region. 
  
  # -------------------------------------------------------------------------------------------- #
  effects_by_region_df <- effects_by_geography(posttr_dta = posttre_effects_wdsentry, 
                                               geographic_var = 'REGION') %>%
    
    left_join(us_regions_key, by = c('covariate' = 'REGION')) %>%  # Join the region name key-value pairs. 
    
    relocate(REGION_NAME, .after = last_col() )

  # -------------------------------------------------------------------------------------------- #
  
  # Effects by Division 
  
  # -------------------------------------------------------------------------------------------- #
  effects_by_division_df <- effects_by_geography(posttr_dta = posttre_effects_wdsentry, 
                                               geographic_var = 'DIVISION') %>%
    
    left_join(us_divisions_key, by = c('covariate' = 'DIVISION')) %>%  # Join the region name key-value pairs. 
    
    relocate(DIVISION_NAME, .after = last_col() )
  # -------------------------------------------------------------------------------------------- #
  err_and_tau_on_covars <- list('errors_on_bins' = errors_on_binned_covars_df, 
                                'errors_on_ints' = errors_on_int_covars_df, 
                                'errors_on_norm_covars' = errors_on_norm_covars_df, 
                                'effects_on_binned_covars' = effects_on_binned_covars_df,
                                'effects_on_int_covars' = effects_on_int_covars_df,
                                'effects_on_norm_covars' = effects_on_norm_covars_df, 
                                'effects_on_dsvars_fact' = effects_on_factor_ds_vars_df, 
                                'ds_vars_on_time' = ds_vars_numeric_on_time_df, 
                                'effects_by_region' = effects_by_region_df, 
                                'effects_by_division' = effects_by_division_df)
  
  return(err_and_tau_on_covars)
  
}
