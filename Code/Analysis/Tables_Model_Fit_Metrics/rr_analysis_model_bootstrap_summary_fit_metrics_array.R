# Script to compute bootstrap estimates of model fit using using various accuracy measures. 
# Results based on out-of-sample CV predictions and errors. 
# ----------------------------------- #
# Load packages
# ----------------------------------- #
options(scipen = 999)
pacman::p_load('future', 'furrr', 'parallel', 'tidymodels')
# -------------------------------------------------------------------------------------------- #
model_dep_var <- Sys.getenv("model_dep_var") 
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
print(model_dep_var); print(model_geography)
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Function in this script is used in the bootstrap_errors_and_preds function. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_compute_fit_metrics.R'))
# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load each bootstrapped ML model. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_iter <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_iter))
bootstrap_group = '01_499' # Folder designated in directory specifying number of bootstrap iterations. 
bootstrap_by_tracts = '_tracts' # NULL or '_tracts'
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_', bootstrap_group, bootstrap_by_tracts) # bootstrap_ids = '01_499'; bootstrap_by_tracts = NULL or '_tracts'

filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(bootstrap_iter, '.rds'), sep = '_')

model_output <- readRDS(here::here('Analysis',
                                   dir_geography,
                                   dir_dep_var, 
                                   dir_bootstrap, 
                                   filename))

untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds, pred_probs) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, pred_probs_cf) %>%
  
  rename(preds = pred_class_cf) %>%
  rename(pred_probs = pred_probs_cf) %>%
  
  filter(year >= '2006') # Counterfactual predictions from 2006 to 2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
dta_for_metrics <- model_preds %>%
  filter(rel_year < 0) %>% 
  mutate(across(.cols = c(actual, preds), 
                .fns = \(x) factor(x, levels = c('1', '0') ) ) ) %>% 
  select(rel_year, actual, pred_probs, preds)

summary_fit_metrics <- compute_metrics(dta = dta_for_metrics, 
                                       bootstrap_idx = bootstrap_iter)
# -------------------------------------------------------------------------------------------- #
directory_geography <- paste(model_geography, 'Bootstrap', sep = '_')

directory_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')

directory_boot_diag_type <- paste0('bootstrap_fit_metrics', bootstrap_by_tracts) 

filename <- paste0('bootstrap_', 
                   'fit_metrics_', 
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_iter) # e.g., 1:499

filename <- paste0(filename, '.rds'); filename

saveRDS(summary_fit_metrics, here::here('Analysis', directory_geography, directory_dep_var, directory_boot_diag_type, filename))

# -------------------------------------------------------------------------------------------- #