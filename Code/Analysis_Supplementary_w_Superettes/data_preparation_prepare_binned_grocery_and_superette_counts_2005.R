# -------------------------------------------------------------------------------------------- #
.libPaths()
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on BG bootstrap or CT bootstrap. 
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #
# Pre-Entry Retail Store Counts
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'retail_store_counts_2_and_10mile_2005_feature.RData')) #10min, 3mile
# -------------------------------------------------------------------------------------------- #
# Add year 2005 grocery store counts, superette counts, and grocery + superette counts. 
# -------------------------------------------------------------------------------------------- #
retail_counts_2005_2_and_10mile <- retail_counts_2005_2_and_10mile %>% 
  select(GEOID, Grocery_Count_10mile_2005, Superette_Count_10mile_2005)
retail_counts_2005_2_and_10mile <- retail_counts_2005_2_and_10mile %>% 
  mutate(
    Grocery_and_Superette_Count_10mile_2005 = Grocery_Count_10mile_2005 + Superette_Count_10mile_2005
    )
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
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, 
         DS_Count_10mile, DS_Count_10mile_diff, entry_events, net_entry_cumsum) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(dta_treated, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  # For consistency with the out-of-sample predictions during CV, 
  # we obtain counterfactual predictions from 2007 to 2020.
  
  filter(year >= '2006') %>%
  
  left_join(bg_regs_and_divs, by = 'GEOID') %>% # Regional and divisional indicators. 
  
  left_join(retail_counts_2005_2_and_10mile, by = 'GEOID')
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
# Post-treatment binned year 2005 Grocery Store, Superette, and Grocery Store + Superette Counts. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_grocery_stores_w_superettes_posttreatment.R'))
posttr_binned_grocery <- prepare_binned_grocery_stores_post(national = FALSE, 
                                                            geography_str = model_geography, 
                                                            model_preds_dta = model_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('posttreatment_binned_grocery_and_superette_', str_to_lower(model_geography), '.rds')
saveRDS(posttr_binned_grocery, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #