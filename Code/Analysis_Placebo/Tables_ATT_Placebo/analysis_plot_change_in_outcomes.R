# Script uses bootstrap estimates to compute SEs and join to ATT estimates using original sample. 
# For placebo ATTs. 
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
library(readr)
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Placebo', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis_Placebo', 'Model_Training', dir_dep_var, filename))
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
  
  filter(year >= '2005') # Post-treatment effects are assessed from 2005-2020.  In placebo data, first treatment year is 2005, corresponding to the cohort
                        # whose actual treatment year is 2006. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #
# Helper script to compute Placebo ATTs for comparison with main results. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_bootstrap_change_in_outcome_placebo.R'))
# -------------------------------------------------------------------------------------------- #
emp_change_in_outcomes_plac <- bootstrap_change_in_outcomes(dta = model_preds, iter = 0)
# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_change_in_outcomes', bootstrap_by_tracts) # NULL or '_tracts'

boot_data <- seq(1, 499, 1) %>%
  
  map_dfr(function(.iter){ # A single data frame so one can row-bind automatically. 
    
    filename <- paste0('bootstrap_',
                       'change_in_outcomes_',
                       str_to_lower(model_geography), '_', 
                       model_dep_var,
                       bootstrap_by_tracts, '_', 
                       .iter, '.rds')
    
    readRDS(here::here('Analysis_Placebo',
                       dir_geography,
                       dir_dep_var, 
                       dir_bootstrap, 
                       filename))    
    
  })

# Combine the empirical with the bootstrap estimates. 
boot_data <- bind_rows(emp_change_in_outcomes_plac, boot_data)

# Compute means and standard deviations/errors of bootstrap estimates. 
summary_boot_data <- boot_data %>% 
  group_by(rel_year, year, percentage_type) %>%
  summarise(across(.cols = c(tau_avg, pct_att, preds_avg, actual_avg), 
                   .fns = c(mean = mean, sd = sd), 
                   .names = '{.col}_{.fn}')) 
# -------------------------------------------------------------------------------------------- #
# Join the bootstrap SD to the empirical data. 
# -------------------------------------------------------------------------------------------- #

emp_change_in_outcomes_plac <- emp_change_in_outcomes_plac %>%
  
  left_join(summary_boot_data, by = c('rel_year', 'year', 'percentage_type')) %>%
  
  relocate(c(pct_att_sd, pct_att_mean), .after = pct_att) %>%
  
  relocate(c(tau_avg_sd, tau_avg_mean), .after = tau_avg) %>%
  
  relocate(c(preds_avg_sd, preds_avg_mean), .after = preds_avg) %>%
  
  relocate(c(actual_avg_sd, actual_avg_mean), .after = actual_avg) %>%
  
  arrange(percentage_type)
# -------------------------------------------------------------------------------------------- #
# Compute confidence intervals for alpha = 1 percent.
# -------------------------------------------------------------------------------------------- #
emp_change_in_outcomes_plac <- emp_change_in_outcomes_plac %>% 
  
  mutate(pct_att_lci_99 = pct_att - qnorm(p = 1 - 0.01/2)*pct_att_sd, 
         pct_att_hci_99 = pct_att + qnorm(p = 1 - 0.01/2)*pct_att_sd, 
         effect_type = 'Average % Change', 
         tau_avg_lci_99 = tau_avg - qnorm(p = 1 - 0.01/2)*tau_avg_sd, 
         tau_avg_hci_99 = tau_avg + qnorm(p = 1 - 0.01/2)*tau_avg_sd, 
         effect_type = 'ATT')

emp_change_in_outcomes_plac <- emp_change_in_outcomes_plac %>% 
  relocate(c(year, percentage_type), .after = rel_year)
# -------------------------------------------------------------------------------------------- #
# Save an intermediate data frame for further processing 
# -------------------------------------------------------------------------------------------- #
fname = paste0(str_to_lower(model_geography), '_' ,'average_change_in_outcomes_relative_time_plac', '.csv')
saveRDS(emp_change_in_outcomes_plac, file = here::here('Analysis_Placebo', 'Tables', 'Low_Access', model_geography, fname))
# -------------------------------------------------------------------------------------------- #

