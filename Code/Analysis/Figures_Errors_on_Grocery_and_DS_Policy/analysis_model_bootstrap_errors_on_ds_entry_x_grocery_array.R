# Script to estimate bootstrap average CV errors by presence of dollar store policies, geographic region, 
# and baseline grocery stores interacted with future dollar store entries. 
# -------------------------------------------------------------------------------------------- #
# Load data and point estimates. 
# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below.
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
bootstrap_by_tracts = '_tracts' 
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
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
ds_bans <- readr::read_csv(here::here('Data', 'block_groups_w_ds_policies.csv'), show_col_types = FALSE)
ds_bans <- ds_bans %>% select(GEOID, Defeated, Moratorium, Ordinance, policy_total)
source(here::here('Code', 'Functions', 'Function_prepare_binned_ds_policy_vars.R'))
ds_bans_binned <- prepare_binned_ds_policy_vars(df = ds_bans)
# -------------------------------------------------------------------------------------------- #
# Pre-treatment observations, year 2005 Grocery Store bins. 
# -------------------------------------------------------------------------------------------- #
fname_pretr_binned_grocery = paste0('pretreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

pretr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_grocery))
# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load the bootstrapped ML model. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_id))
bootstrap_ids = '01_499' # Folder designated in directory specifying number of bootstrap iterations. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) 

filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(bootstrap_id, '.rds'), sep = '_')

model_output <- readRDS(here::here('Analysis',
                                   dir_geography,
                                   dir_dep_var, 
                                   dir_bootstrap, 
                                   filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007') %>% # We obtain CV predictions from 2007-2020
  
  left_join(bg_regs_and_divs, by = 'GEOID') %>% # Regional and divisional indicators. 
  
  left_join(select(ds_bans_binned, GEOID, 
                   Defeated_and_Restrictions_bins, Restrictions_and_Defeated_bins, 
                   Defeated_and_Restrictions, Restrictions_and_Defeated, 
                   policy_total_binary_bins, policy_total_binary, policy_total), 
            by = 'GEOID', relationship = 'many-to-one', multiple = 'all') 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- untreated_preds %>%
  
  filter(is.finite(rel_year)) %>% # Filter out observations with rel_year == Inf because these are never-treated observations. 
  
  left_join(select(pretr_binned_grocery, GEOID, year, Grocery_Count_10mile_2005_bins), 
            by = c('GEOID', 'year') )
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%

  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))
# -------------------------------------------------------------------------------------------- #
# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
# -------------------------------------------------------------------------------------------- #
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# Join the dollar store entry information to post-treatment data. 
# -------------------------------------------------------------------------------------------- #
treated_preds <- treated_preds %>%
  left_join(select(posttr_binned_dsvars, GEOID, year, 
                   entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins),
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all' ) %>%
  select(GEOID, event_year, matches('bins$') )
# -------------------------------------------------------------------------------------------- #
# Get the distinct combinations of entry events for each treated block group. 
# -------------------------------------------------------------------------------------------- #
treated_preds <- treated_preds %>% 
  
  group_by(GEOID, event_year) %>%
  
  distinct(entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins)
# -------------------------------------------------------------------------------------------- #
pretr_preds_aug <- pretr_preds %>%

  filter(is.finite(rel_year) ) %>% # Filter out observations with rel_year == Inf because these are never-treated observations.

    left_join(treated_preds,  by = c('GEOID', 'event_year'),
              relationship = 'many-to-many', multiple = 'all' ) %>% 
  
  drop_na(.) # A few block group observations are present in the pre-treatment data but drop out of the post-treatment data. 
            # Therefore, there is no post-treatment information on entries to join
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_errors_on_grocery_policy_and_geography.R') )
errors_on_grocery_x_entry_and_policy_combined <- errors_on_grocery_x_entry_and_policy() # No arguments are required because all data in the global env. are employed. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_error_by_grocery_entry_policy_region', bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'error_by_grocery_entry_policy_region_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(errors_on_grocery_x_entry_and_policy_combined, 
        here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename) )
# -------------------------------------------------------------------------------------------- #