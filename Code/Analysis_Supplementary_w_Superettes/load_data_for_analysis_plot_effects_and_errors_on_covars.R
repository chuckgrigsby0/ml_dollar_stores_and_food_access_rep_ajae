# Load data based on parameters specified in analysis_plot_effects_and_errors_on_covars 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Filter out urban_area and uc_area from the Rural models. 
# -------------------------------------------------------------------------------------------- #
# if (model_geography == 'Rural'){ 
#   model_covars <- model_covars[!grepl('^urban_area$|^uc_area$', model_covars)]
# }
# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final_w_superettes', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.e., Low Access
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, filename))
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

  # For consistency with the out-of-sample predictions during CV, 
  # we obtain counterfactual predictions from 2007 to 2020.
    
  filter(year >= '2006') %>%
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# Create a pre-treatment data set to be used later in functions. 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- model_preds %>% # Varies by bootstrap iteration. 
  
  filter(rel_year < 0) %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  select(-tau) %>% 
  
  mutate(err = actual - preds, # Bootstrap error. 
         
         rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #
# Load pre-treatment and post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #
# See files 'data_preparation_prepare_binned_and_factor_covars.R'; 'Function_prepare_binned_and_factor_covars_pretreatment.R'
# 'Function_prepare_binned_and_factor_covars_posttreatment.R' and 'Function_prepare_binned_and_factor_dsvars_posttreatment.R'
# for the creation of this file using the original data. 

# Note: These data.frames are from the original data so they must be joined 
# to the bootrapped data for the current bootstrap iteration. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Pre-treatment covariate bins. 
# -------------------------------------------------------------------------------------------- #
fname_pretr_binned_covars = paste0('pretreatment_binned_and_factor_covariates_w_superettes_', str_to_lower(model_geography), '.rds')

pretr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_covars))

# Remove the error calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
pretr_binned_covars <- pretr_binned_covars %>% select(-err)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
pretr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

pretr_preds_sel <- pretr_preds %>% select(all_of(pretr_key_vars), err)
# -------------------------------------------------------------------------------------------- #
# Join the prepared binned data to the bootstrap data. 
pretr_binned_covars <- pretr_preds_sel %>% left_join(pretr_binned_covars, by = pretr_key_vars)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
pretr_binned_covars_str <- names(pretr_binned_covars)[!grepl('GEOID|^year$|event_year|rel_year|err', names(pretr_binned_covars))]

# Separate binned covariates from the unbinned (factor/integer/dummy) predictors 
# Not binned vars. 
pretr_unbinned_covars_str <- pretr_binned_covars_str[!grepl('bins', pretr_binned_covars_str)]

# -------------------------------------------------------------------------------------------- #
# Filter out urban_area and uc_area from the Rural models. 
# -------------------------------------------------------------------------------------------- #
if (model_geography == 'Rural'){ 
  pretr_unbinned_covars_str <- pretr_unbinned_covars_str[!grepl('^urban_area$|^uc_area$', pretr_unbinned_covars_str)]
} else if (model_geography == 'Urban'){
  pretr_unbinned_covars_str <- pretr_unbinned_covars_str[!grepl('^uc_area$', pretr_unbinned_covars_str)]
}
print(pretr_unbinned_covars_str)
# -------------------------------------------------------------------------------------------- #
# Binned vars.
pretr_binned_covars_str <- pretr_binned_covars_str[grepl('bins', pretr_binned_covars_str)]; pretr_binned_covars_str

# -------------------------------------------------------------------------------------------- #
# Post-treatment bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_w_superettes_', str_to_lower(model_geography), '.rds')

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


# -------------------------------------------------------------------------------------------- #
# Functions used in script: 'Function_effects_and_errors_on_covars.R'
source(here::here('Code', 'Functions', 'Function_tidy_regression.R')) 
source(here::here('Code', 'Functions', 'Function_top_and_bottom_code_vars.R')) 
source(here::here('Code', 'Functions', 'Function_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
err_and_tau_on_covars <- effects_and_errors_on_covars(national = FALSE, geography_str = model_geography, 
                                                      model_preds_dta = model_preds, 
                                                      pretr_binned_covars_dta = pretr_binned_covars, 
                                                      boot_iter = 0)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load bootstrap data/estimates created in analysis_model_bootstrap_effects_and_errors_on_covars.R 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_errors_and_effects_by_covars', bootstrap_by_tracts) # NULL or '_tracts'

boot_data <- seq(1, 499, 1) %>%
  
  map(function(.iter){ 
    
    filename <- paste0('bootstrap_',
                       'err_and_tau_by_covars_',
                       str_to_lower(model_geography), '_', 
                       model_dep_var,
                       bootstrap_by_tracts, '_', 
                       .iter, '.rds')

    readRDS(here::here('Analysis_Supplementary_w_Superettes',
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
# Regression of CV pre-treatment errors on binned covariates. 

# Regression of CV pre-treatment errors on factor covariates. 

# Regression of post-treatment effects on covariates

# Regression of post-treatment effects on factor dollar store counts/entries. 

# Regression of dollar store counts/entries and year and relative year. 

# Regression of effects by region

# Regression of effects by division
# -------------------------------------------------------------------------------------------- #
empirical_estimates_w_bootstrap_se <- function(grep_list_element_name_str, 
                                               list_element_name_str, 
                                               group_and_join_vars_str, 
                                               flag_erros_on_bins){ 
  
  # -------------------------------------------------------------------------------------------- #
  # Extract the element containing bootstrapped estimates of interest.  
  boot_data_subset <- subset_errors_and_predictions(bootstrap_data = boot_data, 
                                                    list_element_name = grep_list_element_name_str)
  
  # Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
  boot_data_subset <- bind_rows(err_and_tau_on_covars[[list_element_name_str]], boot_data_subset)
  
  # Compute standard error. 
  boot_se_subset <- compute_bootstrap_standard_errors(dta = boot_data_subset, 
                                                      group_vars = group_and_join_vars_str)  %>%
    arrange(covariate, .by_group = TRUE)
  
  # Combine bootstrap standard errors with the empirical estimates. 
  emp_estimates <- join_bootstrap_ses_to_emp_estimates(empirical_estimates_dta = err_and_tau_on_covars[[list_element_name_str]], 
                                                       bootstrap_estimates_dta = boot_se_subset, 
                                                       join_vars = group_and_join_vars_str)
  if (isTRUE(flag_erros_on_bins)){ 
    
    # Convert term to character. 
    emp_estimates$term <- as.character(emp_estimates$term)
    
    # Remove any bracket([] followed by anything up until the comma (,))
    emp_estimates$term[grepl('\u2264', emp_estimates$term)] <- str_remove_all(emp_estimates$term[grepl('\u2264', emp_estimates$term)], '\\[.*,') 
    # -------------------------------------------------------------------------------------------- #
  }
  
  return(emp_estimates)
  
}
# -------------------------------------------------------------------------------------------- #
empirical_estimates <- pmap(list(grep_list_element_name_str = c('^errors_on_bins$', '^errors_on_ints$', '^errors_on_norm_covars$', 
                                                                '^effects_on_binned_covars$', '^effects_on_int_covars$', 
                                                                '^effects_on_norm_covars$', '^effects_on_dsvars_fact$', 
                                                                '^ds_vars_on_time$', 
                                                                '^effects_by_region$', '^effects_by_division$'),
                                 list_element_name_str = c('errors_on_bins', 'errors_on_ints', 'errors_on_norm_covars', 
                                                           'effects_on_binned_covars', 'effects_on_int_covars', 
                                                           'effects_on_norm_covars', 'effects_on_dsvars_fact', 
                                                           'ds_vars_on_time', 
                                                           'effects_by_region', 'effects_by_division'),
                                 group_and_join_vars_str =  list(c('term', 'covariate', 'outcome'),
                                                                 c('term', 'covariate', 'outcome'),
                                                                 c('term', 'covariate', 'outcome'),
                                                                 c('term', 'covariate', 'outcome'), 
                                                                 c('term', 'covariate', 'outcome'), 
                                                                 c('term', 'covariate', 'outcome'),
                                                                 c('term', 'covariate', 'outcome'), 
                                                                 c('term', 'covariate', 'outcome', 'label'), 
                                                                 c('term', 'covariate', 'outcome', 'label', 'REGION_NAME'), 
                                                                 c('term', 'covariate', 'outcome', 'label', 'DIVISION_NAME')),
                                 flag_erros_on_bins = c(TRUE, rep(FALSE, 2), TRUE, rep(FALSE, 6))), 
                            
                            empirical_estimates_w_bootstrap_se)
             
empirical_estimates <- set_names(empirical_estimates, nm = names(err_and_tau_on_covars))
# -------------------------------------------------------------------------------------------- #
