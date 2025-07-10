# Script to compute bootstrap ATTs as a percent of average counterfactual share in low access. 
# Used to compute bootstrapped SEs. 
# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below.
model_geography = Sys.getenv("model_geography") # Used in script below.
options(scipen = 999)
print(model_dep_var); print(model_geography)
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load the bootstrapped ML model. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_id))
bootstrap_ids = '01_499' # Folder designated in directory specifying number of bootstrap iterations. 
bootstrap_by_tracts = '_tracts'
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
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007') # Cross-validated predictions are made for 2007-2020. 
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') # Post-treatment effects are assessed from 2006-2020.
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #
# Post-treatment bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 
posttr_binned_covars <- posttr_binned_covars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_bootstrap_change_in_outcome.R'))
# -------------------------------------------------------------------------------------------- #
change_in_outcomes <- bootstrap_change_in_outcomes(dta = model_preds, iter = bootstrap_id)
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_change_in_outcomes', bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'change_in_outcomes_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(change_in_outcomes, 
        here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename))
# -------------------------------------------------------------------------------------------- #

