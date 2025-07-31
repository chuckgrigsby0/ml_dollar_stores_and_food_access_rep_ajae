# Script to estimate bootstrap average CV errors and treatment effects across multiple dollar store entries and baseline retail store counts. 
# Does not include estimates for baseline grocery stores, which are produced in other scripts. 
# -------------------------------------------------------------------------------------------- #
# Load data.
# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below. 
model_geography = Sys.getenv("model_geography") # Used in script below.
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
# Pre-treatment observations, binned and factorized covariates. 
# -------------------------------------------------------------------------------------------- #
fname_pretr_binned_covars = paste0('pretreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

pretr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_covars))
# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, binned and factorized covariates. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))
# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 

posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
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

dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) # NULL or '_tracts'

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
  
  left_join(select(pretr_binned_covars, GEOID, year,
                   matches('Count_.*_2005')), 
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all') 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- untreated_preds %>%
  
  filter(is.finite(rel_year)) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(select(posttr_binned_covars, GEOID, year,
                   matches('Count_.*_2005')), 
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all') %>%
  left_join(select(posttr_binned_dsvars, GEOID, year, # Join the dollar store entry information to post-treatment data.  
                   entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins),
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all' ) 
# -------------------------------------------------------------------------------------------- #
# Get the distinct combinations of entry events for each treated block group. 
# -------------------------------------------------------------------------------------------- #
entries_by_bg <- treated_preds %>% 
  
  group_by(GEOID, event_year) %>%
  
  distinct(entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins)
# -------------------------------------------------------------------------------------------- #
pretr_preds_aug <- pretr_preds %>%
  
  filter(is.finite(rel_year) ) %>% # Filter out observations with rel_year == Inf because these are never-treated observations.
  
  left_join(entries_by_bg,  by = c('GEOID', 'event_year'),
            relationship = 'many-to-many', multiple = 'all' ) %>% 
  
  drop_na(.) # A few block group observations are present in the pre-treatment data but drop out of the post-treatment data. 
              # Therefore, there is no post-treatment information on entries to join
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_errors_on_baseline_store_counts_x_entry.R') )
errors_on_baseline_store_counts_x_entry <- errors_on_baseline_store_counts_x_entry() # No arguments are used because all data in the global env. are employed. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_effects_on_baseline_store_counts_x_entry.R') )
effects_on_baseline_store_counts_x_entry <- effects_on_baseline_store_counts_x_entry() # No arguments are used because all data in the global env. are employed. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_errors_on_baseline_store_counts_x_entry', bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'errors_by_baseline_store_counts_x_entry_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(errors_on_baseline_store_counts_x_entry, 
        here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename) )
# -------------------------------------------------------------------------------------------- #
dir_bootstrap <- paste0('bootstrap_effects_on_baseline_store_counts_x_entry', bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'effects_by_baseline_store_counts_x_entry_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(effects_on_baseline_store_counts_x_entry, 
        here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename) )
# -------------------------------------------------------------------------------------------- #