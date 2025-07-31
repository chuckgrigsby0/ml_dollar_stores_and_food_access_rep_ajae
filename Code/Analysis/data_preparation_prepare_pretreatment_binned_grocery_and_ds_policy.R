# Script to create binned dollar store policy variables and baseline grocery store variables to assess CV errors across these variables. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban'
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
ds_bans <- readr::read_csv(here::here('Data', 'block_groups_w_ds_policies.csv'), show_col_types = FALSE)
ds_bans <- ds_bans %>% select(GEOID, Defeated, Moratorium, Ordinance, policy_total)
source(here::here('Code', 'Functions', 'Function_prepare_binned_ds_policy_vars.R'))
ds_bans_binned <- prepare_binned_ds_policy_vars(df = ds_bans)
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
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds, 
         DS_Count_10mile, Grocery_Count_10mile_2005) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007')  %>% # We obtain CV predictions from 2007-2020
  
  left_join(bg_regs_and_divs, by = 'GEOID') %>% # Regional and divisional indicators. 
  
  left_join(select(ds_bans_binned, GEOID, 
                   Defeated_and_Restrictions_bins, Restrictions_and_Defeated_bins, 
                   Defeated_and_Restrictions, Restrictions_and_Defeated, 
                   policy_total_binary_bins, policy_total_binary, policy_total), 
            by = 'GEOID', relationship = 'many-to-one', multiple = 'all')
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, 
         DS_Count_10mile, entry_events, net_entry_cumsum, Grocery_Count_10mile_2005) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006')
# -------------------------------------------------------------------------------------------- #
# Select only the block-group ID, event_year, and future entry events. 
# -------------------------------------------------------------------------------------------- #
treated_preds <- treated_preds %>% 
  select(GEOID, event_year, entry_events, net_entry_cumsum)
# -------------------------------------------------------------------------------------------- #
pretr_preds <- untreated_preds %>%
  
  filter(is.finite(rel_year) ) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
# Post-treatment binned year 2005 Grocery Store Counts. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_grocery_stores_pretreatment.R'))
pretr_binned_grocery <- prepare_binned_grocery_stores_pre(model_preds_dta = pretr_preds)
# -------------------------------------------------------------------------------------------- #
filename = paste0('pretreatment_binned_grocery_', str_to_lower(model_geography), '.rds')
saveRDS(pretr_binned_grocery, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #
