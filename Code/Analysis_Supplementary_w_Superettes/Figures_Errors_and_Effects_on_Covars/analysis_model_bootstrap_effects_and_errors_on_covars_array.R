# Script used to estimate bootstrap estimates on average CV errors and ATTs across binned predictor values and treatment effects on standardized predictors. 
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below.
model_geography = Sys.getenv("model_geography") # Used in script below.
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts'
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #

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

model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes',
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
  
  left_join(select(dta_untreated_wfolds, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(dta_treated, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load pre-treatment and post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #
# Pre-treatment covariate bins. 
# -------------------------------------------------------------------------------------------- #
fname_pretr_binned_covars = paste0('pretreatment_binned_and_factor_covariates_w_superettes_', str_to_lower(model_geography), '.rds')

pretr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_covars))
# -------------------------------------------------------------------------------------------- #
# Remove the error calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
# -------------------------------------------------------------------------------------------- #
pretr_binned_covars <- pretr_binned_covars %>% select(-err)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Join the bootstrapped model_preds data.
# -------------------------------------------------------------------------------------------- #
pretr_preds <- model_preds %>% # Varies by bootstrap iteration. 
  
  filter(rel_year < 0) %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  select(-tau) %>% 
  
  mutate(err = actual - preds, # Bootstrap error. 
         rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #
pretr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

pretr_preds_sel <- pretr_preds %>% select(all_of(pretr_key_vars), err)
# -------------------------------------------------------------------------------------------- #
# Join the prepared binned data to the bootstrap data. 
# -------------------------------------------------------------------------------------------- #
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
# -------------------------------------------------------------------------------------------- #
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
# -------------------------------------------------------------------------------------------- #
# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
# -------------------------------------------------------------------------------------------- #
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# tidy_regression functions are used in script: 'Function_effects_and_errors_on_covars.R'
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R')) 
source(here::here('Code', 'Functions', 'Function_top_and_bottom_code_vars.R')) 
source(here::here('Code', 'Functions', 'Function_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
err_and_tau_on_covars <- effects_and_errors_on_covars(national = FALSE, 
                                                      geography_str = model_geography, 
                                                      model_preds_dta = model_preds, 
                                                      pretr_binned_covars_dta = pretr_binned_covars, 
                                                      boot_iter = bootstrap_id)
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_errors_and_effects_by_covars', bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'err_and_tau_by_covars_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(err_and_tau_on_covars, 
        here::here('Analysis_Supplementary_w_Superettes',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename))
# -------------------------------------------------------------------------------------------- #