# Script to compute average CV errors and ATTs based on random forest .
# -------------------------------------------------------------------------------------------- #
# For testing scripts. 

# model_dep_var = 'low_access'; model_geography = 'Urban'; bootstrap_id = 0

# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below. If running for low_access_pers, must change settings below.
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
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
bootstrap_by_tracts = '_tracts' # NULL or '_tracts'
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts, '_rf') # NULL or '_tracts'

filename <- paste(str_to_lower(model_geography), model_dep_var, 'rf_bootstrap', paste0(bootstrap_id, '.rds'), 
                  sep = '_')

model_output <- readRDS(here::here('Analysis',
                                   dir_geography,
                                   dir_dep_var, 
                                   dir_bootstrap, 
                                   filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
# Note: We filter >= 2007 because for the untreated/yet-to-be-treated observations, we only have 
# holdout predictions for years 2007-2020. For block groups whose year of treatment (event_year == 2007), 
# we do not obtain out-of-sample predictions for their relative time to treatment food-desert/low-access status. 
# We obtain predictions for relative time to treatment at t = -1 for the event_year == 2008 because the first
# fold corresponds to 2007-2008. As another example, for the event_year == 2009, we obtain relative time to treatment
# predictions for t = -2 and -1 in the years 2007 and 2008, respectively. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, cv_preds, cv_error) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename(err = cv_error) %>%
  
  filter(year >= '2007') # Cross-validated predictions are made for 2007-2020. 
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, cf_preds, tau) %>%
  
  rename(preds = cf_preds) %>%
  
  filter(year >= '2006') # Post-treatment effects are assessed from 2006-2020.
# -------------------------------------------------------------------------------------------- #
# Note: We do obtain counterfactual predictions from 2006, whereby for block-groups treated 
# in 2006, we obtain predictions at rel_year = 0. However, the ML model CV predictions are made from years 2007-2020. 
# Therefore, subsetting from 2007 to 2020 in the post-treatment period is consistent with out-of-sample prediction 
# periods in the pre-treatment data. Beginning in 2007 in post-treatment data, we obtain rel_year predictions for t = 0 
# for block-groups treated in 2007 and rel_year predictions t = 1 for treated block groups in 2006. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #

pretreat <- model_preds %>% 
  filter(rel_year < 0) %>%
  select(-tau) %>%
  mutate(err = actual - preds)


posttreat <- model_preds %>% 
  filter(rel_year >= 0) %>%
  mutate(rel_year = factor(rel_year) ) 
# -------------------------------------------------------------------------------------------- #
# Percentage change in the CV error by relative time and overall. 
# -------------------------------------------------------------------------------------------- #
cv_error_relyear <- pretreat %>% 
  
  group_by(rel_year) %>%
  
  summarise( across(.cols = c(err, preds, actual), 
                    .fn = list(avg = mean), 
                    .names = '{.col}_{.fn}') ) %>%
  
  mutate(pct_err = err_avg/preds_avg, 
         id = bootstrap_id, 
         percentage_type = 'Relative Time', 
         rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #
cv_error_overall <- pretreat %>% 
  
  summarise( across(.cols = c(err, preds, actual), 
                    .fn = list(avg = mean), 
                    .names = '{.col}_{.fn}') ) %>%
  
  mutate(pct_err = err_avg/preds_avg, 
         id = bootstrap_id, 
         percentage_type = 'All')

# Percentage change in the CV error by relative time and overall. 
# -------------------------------------------------------------------------------------------- #
att_rel_year <- posttreat %>% 
  
  group_by(rel_year) %>%
  
  summarise( across(.cols = c(tau, preds, actual), 
                    .fn = list(avg = mean), 
                    .names = '{.col}_{.fn}') ) %>%
  
  mutate(pct_att = tau_avg/preds_avg, 
         id = bootstrap_id, 
         percentage_type = 'Relative Time')

# -------------------------------------------------------------------------------------------- #
# Overall percentage change in the ATT.
# -------------------------------------------------------------------------------------------- #
att_overall <- posttreat %>% 
  
  summarise( across(.cols = c(tau, preds, actual), 
                    .fn = list(avg = mean), 
                    .names = '{.col}_{.fn}') ) %>%
  
  mutate(pct_att = tau_avg/preds_avg, 
         id = bootstrap_id, 
         percentage_type = 'All')


cv_and_att_summary <- bind_rows(cv_error_relyear, cv_error_overall, att_rel_year, att_overall)
cv_and_att_summary <- cv_and_att_summary %>% relocate(id)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_change_in_outcomes', bootstrap_by_tracts, '_rf') # NULL or '_tracts'

filename <- paste0('bootstrap_',
                   'change_in_outcomes_rf_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   bootstrap_id, '.rds')

saveRDS(cv_and_att_summary, 
        here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename))
# -------------------------------------------------------------------------------------------- #
