# -------------------------------------------------------------------------------------------- #
# Load data based on parameters specified in analysis_plot_effects_on_ds_entry_x_covars_interactions_sourced.R
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.g., Low Access
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
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
  
  filter(year >= '2006') %>% # We obtain counterfactual predictions from 2006 to 2020.
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #
# See file 'data_preparation_prepare_binned_and_factor_covars.R' and associated Functions called in script. 

# Note: These data.frames are from the original data so they must be joined 
# to the bootrapped data for the current bootstrap iteration. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment bins (quartiles) 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_quartile_covars_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_covars <- posttr_binned_covars %>% select(-tau)


# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)


# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

posttr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_grocery))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# tidy_regression functions are used in script: 'Function_effects_and_errors_on_covars.R'
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R')) 
source(here::here('Code', 'Functions', 'Function_effects_on_ds_entry_x_covars.R'))
# -------------------------------------------------------------------------------------------- #
# Obtain the empirical/original point estimates to which we will join the bootstrapped estimates and
# compute bootstrap SEs. 
# -------------------------------------------------------------------------------------------- #
effects_on_interactions <- effects_on_ds_entry_x_covars_x_grocery(national = FALSE, 
                                                                  geography_str = model_geography, 
                                                                  model_preds_dta = model_preds, 
                                                                  boot_iter = 0)
# -------------------------------------------------------------------------------------------- #
# Load the bootstrapped estimates. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_effects_by_interactions', bootstrap_by_tracts)

boot_data <- seq(1, 499, 1) %>%
  
  map(function(.iter){ 
    
    filename <- paste0('bootstrap_',
                       'tau_by_ds_x_covars_',
                       str_to_lower(model_geography), '_', 
                       model_dep_var,
                       bootstrap_by_tracts, '_', 
                       .iter, '.rds')
    
    readRDS(here::here('Analysis',
                       dir_geography,
                       dir_dep_var, 
                       dir_bootstrap, 
                       filename))    
    
  })
# -------------------------------------------------------------------------------------------- #
# Functions to subset the named elements 'errors' or the 'predictions', 
# compute standard errors, and join standard errors to empirical point estimates. 
# -------------------------------------------------------------------------------------------- #
# Select from the list of bootstrapped output. 
source(here::here('Code', 'Functions', 'Function_bootstrap_subset_errors_and_predictions.R')) 
# Simple function to compute SEs by some grouping variables. 
source(here::here('Code', 'Functions', 'Function_bootstrap_compute_standard_errors.R')) 
# Join the empirical estimates and the bootstrapped SEs.
source(here::here('Code', 'Functions', 'Function_bootstrap_join_ses_to_emp_estimates.R')) 
# -------------------------------------------------------------------------------------------- #
# Models included: 
# Regression of treatment effects on binned covariates interacted with dollar store entries.  

# Regression of treatment effects on binned grocery stores in 2005 with dollar store entries. 
# -------------------------------------------------------------------------------------------- #
empirical_estimates_w_bootstrap_se <- function(grep_list_element_name_str, 
                                               list_element_name_str, 
                                               group_and_join_vars_str){ 
  
  # -------------------------------------------------------------------------------------------- #
  # Extract the element containing bootstrapped estimates of interest.  
  boot_data_subset <- subset_errors_and_predictions(bootstrap_data = boot_data, 
                                                    list_element_name = grep_list_element_name_str)
  
  # Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
  boot_data_subset <- bind_rows(effects_on_interactions[[list_element_name_str]], boot_data_subset)
  
  # Compute standard error. 
  boot_se_subset <- compute_bootstrap_standard_errors(dta = boot_data_subset, 
                                                      group_vars = group_and_join_vars_str)  %>%
    arrange(covariate, ds_entry, .by_group = TRUE)
  
  # Combine bootstrap standard errors with the empirical estimates. 
  emp_estimates <- join_bootstrap_ses_to_emp_estimates(empirical_estimates_dta = effects_on_interactions[[list_element_name_str]], 
                                                       bootstrap_estimates_dta = boot_se_subset, 
                                                       join_vars = group_and_join_vars_str)
  
  return(emp_estimates)
  
}
# -------------------------------------------------------------------------------------------- #
empirical_estimates <- pmap(list(grep_list_element_name_str = c('^effects_on_ds_x_covars$', '^effects_on_ds_x_grocery$'),
                                 list_element_name_str = c('effects_on_ds_x_covars', 'effects_on_ds_x_grocery'),
                                 group_and_join_vars_str =  list(c('entry', 'quartile', 'term', 'outcome', 'covariate', 'ds_entry'),
                                                                 c('entry', 'grocery_stores', 'term', 'outcome', 'covariate', 'ds_entry'))), 
                            
                            empirical_estimates_w_bootstrap_se)

empirical_estimates <- set_names(empirical_estimates, nm = names(effects_on_interactions))
# -------------------------------------------------------------------------------------------- #