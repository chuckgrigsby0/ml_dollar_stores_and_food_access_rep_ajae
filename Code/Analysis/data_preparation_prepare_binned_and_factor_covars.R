# Script creates pre- and post-treatment binned covariates, dollar store entry counts, and baseline grocers. 
# Output data are used in analyses of CV errors and treatment effect heterogeneity.
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on CT bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural'
model_dep_var <- 'low_access'
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
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
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title
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
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, 
         DS_Count_10mile, DS_Count_10mile_diff, entry_events, net_entry_cumsum, Grocery_Count_10mile_2005) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(dta_treated, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2006') %>% # We obtain counterfactual predictions from 2006 to 2020.
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Pre-treatment binned data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_and_factor_covars_pretreatment.R'))
pretr_binned_covars <- prepare_binned_and_factor_covars_pre(national = FALSE, 
                                                        geography_str = model_geography, 
                                                        model_preds_dta = model_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('pretreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')
saveRDS(pretr_binned_covars, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment binned data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_and_factor_covars_posttreatment.R'))
posttr_binned_covars <- prepare_binned_and_factor_covars_post(national = FALSE, 
                                                            geography_str = model_geography, 
                                                            model_preds_dta = model_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('posttreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')
saveRDS(posttr_binned_covars, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment binned dollar store entry and count data.  
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_and_factor_dsvars_posttreatment.R'))
posttr_binned_dsvars <- prepare_binned_and_factor_dsvars_post(national = FALSE, 
                                                              geography_str = model_geography, 
                                                              model_preds_dta = model_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')
saveRDS(posttr_binned_dsvars, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment binned (quartiles) for demographic/socioeconomic covariates. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_quartile_covars_posttreatment.R'))
posttr_binned_quartile_covars <- prepare_binned_quartile_covars_post(national = FALSE, 
                                                              geography_str = model_geography, 
                                                              model_preds_dta = model_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('posttreatment_binned_quartile_covars_', str_to_lower(model_geography), '.rds')
saveRDS(posttr_binned_quartile_covars, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment binned year 2005 Grocery Store Counts. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_grocery_stores_posttreatment.R'))
posttr_binned_grocery <- prepare_binned_grocery_stores_post(national = FALSE, 
                                                            geography_str = model_geography, 
                                                            model_preds_dta = model_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('posttreatment_binned_grocery_', str_to_lower(model_geography), '.rds')
saveRDS(posttr_binned_grocery, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #


