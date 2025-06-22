print('Sourced: effects_on_ds_entry_x_covars_x_grocery <- function(national, geography_str, model_preds_dta)')

effects_on_ds_entry_x_covars_x_grocery <- function(national, boot_iter){

dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(boot_iter, '.rds'), sep = '_')

model_output <- readRDS(here::here('Analysis',
                                   dir_geography,
                                   dir_dep_var, 
                                   dir_bootstrap, 
                                   filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  left_join(select(dta_untreated_wfolds, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(dta_treated, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
if (isTRUE(national)){ 
  
  posttre_effects <- model_preds %>% 
    
    filter(rel_year >= 0) %>% 
    
    mutate(rel_year = factor(rel_year))
  
} else { 
  
  posttre_effects <- model_preds %>% 
    
    filter(rel_year >= 0) %>% 
    
    filter(grepl(model_geography, Geography)) %>%
    
    mutate(rel_year = factor(rel_year))
}

# -------------------------------------------------------------------------------------------- #
# Prepare data for pre-treatment analyses and post-treatment analyses. 
# Note: data preparation is already completed for pre-treatment data. See pretr_binned_covars. 
# -------------------------------------------------------------------------------------------- #
posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

# tau and dollar store entry/counts w/ grocery stores from 2005. 

posttre_effects_wdsentry <- posttre_effects %>%
  select(all_of(posttr_key_vars), tau) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) # Joins binned dollar store and grocery store variables. 

# Select relevant covariates for analyses. 

sel_covars <- c('inc_per_capita_bins', 'poverty_rate_bins', 'pop_black_bins', 'pop_white_bins')
sel_dsvars <- c('gross_entry_cumsum_bins', 'entry_events_bins')
sel_grocery <- c('Grocery_Count_10mile_2005_bins')

# Prepare data. 
# Note: While posttre_effects_wdsentry originates with bootstrap output, posttr_binned_covars is created with 
# original data, allowing for smooth joins.

posttre_effects_winteracts <- posttre_effects_wdsentry %>% # dta
  
  select(all_of(posttr_key_vars), tau, all_of(sel_dsvars), all_of(sel_grocery)) %>%
  
  left_join(select(posttr_binned_covars, all_of(posttr_key_vars), all_of(sel_covars)), 
            by =  posttr_key_vars)

# -------------------------------------------------------------------------------------------- #
# Effects on DS entries x inc. per capita/poverty/race.   
#--------------------------------------------------------------------------------------------#
# combine.quick = FALSE means that fixed effects using names when combining interaction FEs, 
#--------------------------------------------------------------------------------------------#
effects_by_ds_entry_x_covars <- function(dta, covariate, entry_var_str){ 
  
  levels(dta[[covariate]]) <- paste(c('First', 'Middle', 'Third'), 'Quartile')
  
  reg_formula <- xpd(tau ~ -1 + .[entry_var_str]:.[covariate], data = dta)
  
  ds_entry_x_covar_reg <- feols(reg_formula, data = dta)
  
  tidy_reg <- broom::tidy(ds_entry_x_covar_reg)
  
  tidy_reg <- tidy_reg %>% 
    
    mutate(across(.cols = term, 
                  .fn = ~str_remove_all(., entry_var_str)  ) ) %>%
    
    mutate(across(.cols = term, 
                  .fn = ~str_remove_all(., covariate)  ) )  %>%
    
    filter(!grepl('^0:', term)) # In the gross_entry_cumsum case, remove cases in which treated obs. 
  # had NAs at the start of treatment but later included data. 
  
  # Split up the term column and add to the estimates data.frame().
  new_cols <- data.frame((str_split_fixed(tidy_reg$term, ':', n = 2)))
  names(new_cols) <- c('entry', 'quartile')
  new_cols$entry <- str_replace_all(new_cols$entry, '\\(', '\u003e ') %>% # For entries, replace '(' at start of string with >
    str_remove_all(., ',[[:digit:]]+\\]')  # Remove ',' followed by one or more digits and the ']'
  
  tidy_reg <- bind_cols(new_cols, tidy_reg) # Combine the new, clean columns to tidy_reg output. 
  
  tidy_reg <- tidy_reg %>%
    mutate(label = 'Average Effect in Bin', 
           outcome = 'tau', 
           covariate = covariate,
           ds_entry = entry_var_str,
           boot_iteration = boot_iter) %>%
    mutate(across(.cols = c(entry, quartile), .fn = ~factor(., levels = unique(.))))
  
  return(tidy_reg)
}
#--------------------------------------------------------------------------------------------#
# Apply function. 
effects_on_ds_entry_x_covars <- sel_dsvars %>%
  map_dfr(function(.x){ 
    sel_covars %>% 
      map_dfr(function(.y){ 
        
        effects_by_ds_entry_x_covars(dta = posttre_effects_winteracts, 
                                     covariate = .y, 
                                     entry_var_str = .x)
        
      }) 
  })
#--------------------------------------------------------------------------------------------#
# Effects on DS entry x 2005 Grocery stores. 
#--------------------------------------------------------------------------------------------#
effects_by_ds_entry_x_grocery <- function(dta, covariate, entry_var_str){  
  
  reg_formula <- xpd(tau ~ -1 + .[entry_var_str]:.[covariate], data = dta)
  
  ds_entry_x_grocery_reg <- feols(reg_formula, data = dta)
  
  tidy_reg <- broom::tidy(ds_entry_x_grocery_reg)
  
  tidy_reg <- tidy_reg %>% 
    
    mutate(across(.cols = term, 
                  .fn = ~str_remove_all(., entry_var_str)  ) ) %>%
    
    mutate(across(.cols = term, 
                  .fn = ~str_remove_all(., covariate)  ) )  %>%
    
    filter(!grepl('^0:', term))
  
  
  # Split up the term column and add to the estimates data.frame().
  new_cols <- data.frame((str_split_fixed(tidy_reg$term, ':', n = 2)))
  names(new_cols) <- c('entry', 'grocery_stores')
  new_cols$entry <- str_replace_all(new_cols$entry, '\\(', '\u003e ') %>% 
    str_remove_all(., ',[[:digit:]]+\\]')
  
  tidy_reg <- bind_cols(new_cols, tidy_reg)
  
  tidy_reg <- tidy_reg %>%
    mutate(label = 'Average Effect in Bin', 
           outcome = 'tau', 
           covariate = covariate, 
           ds_entry = entry_var_str,
           boot_iteration = boot_iter) 
  
  return(tidy_reg)
  
}
#--------------------------------------------------------------------------------------------#
effects_on_ds_entry_x_grocery <- sel_dsvars %>%
  map_dfr(function(.x){ 
    
    effects_by_ds_entry_x_grocery(dta = posttre_effects_winteracts, 
                                  covariate = sel_grocery, 
                                  entry_var_str = .x)
    
  }) 
#--------------------------------------------------------------------------------------------#

output <- list('effects_on_ds_x_covars' = effects_on_ds_entry_x_covars, 
               'effects_on_ds_x_grocery' = effects_on_ds_entry_x_grocery)

return(output)

}
