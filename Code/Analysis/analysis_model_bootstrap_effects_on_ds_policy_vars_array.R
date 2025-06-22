# -------------------------------------------------------------------------------------------- #
.libPaths()
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on BG bootstrap or CT bootstrap. 
model_dep_var = Sys.getenv('model_dep_var') # Used in script below. If running for low_access_pers, must change settings below.
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
print(model_geography); print(model_dep_var)
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts' # or NULL to bootstrap by block-group and stratify by relative time. 
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

# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load the bootstrapped ML model.
# -------------------------------------------------------------------------------------------- #
 bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
 bootstrap_id <- as.numeric(bootstrap_id)
 print(paste('Bootstrap model number', bootstrap_id))
 bootstrap_ids = '01_499' # Folder designated in directory specifying number of bootstrap iterations.
# # -------------------------------------------------------------------------------------------- #
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
            multiple = 'all', relationship = 'many-to-one') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%

  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year'), 
            multiple = 'all', relationship = 'many-to-one') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, market) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') # We obtain post-treatment predictions from 2006-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the binned dollar store bans and restrictions data. 
# -------------------------------------------------------------------------------------------- #
ds_bans_binned <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 
                                     paste0('posttreatment_binned_ds_policy_vars_', 
                                            str_to_lower(model_geography), '.rds') ) )
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_effects_and_ds_policy_vars.R'))
# -------------------------------------------------------------------------------------------- #

effects_on_ds_policies_df <- effects_and_ds_policies(df = model_preds, 
                                                     bootstrap_idx = bootstrap_id)

# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_effects_on_ds_policy_vars', bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'effects_on_ds_policy_vars_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(effects_on_ds_policies_df, 
        here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename))
# -------------------------------------------------------------------------------------------- #