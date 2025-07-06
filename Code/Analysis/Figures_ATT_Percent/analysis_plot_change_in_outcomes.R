# Creates tables and figures of ATTs as a percentage of average counterfactual low-access shares across relative treatment timing and years. 
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
library(readr)
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.e., Low Access
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Note: We filter >= 2007 because for the untreated/yet-to-be-treated observations, we only have 
# holdout predictions for years 2007-2020.
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

# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #
# Post-treatment bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 

posttr_binned_covars <- posttr_binned_covars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_bootstrap_change_in_outcome.R'))
# -------------------------------------------------------------------------------------------- #
emp_change_in_outcomes <- bootstrap_change_in_outcomes(dta = model_preds, iter = 0)
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
    
    readRDS(here::here('Analysis',
                       dir_geography,
                       dir_dep_var, 
                       dir_bootstrap, 
                       filename))    
    
  })

# Combine the empirical with the bootstrap estimates. 
boot_data <- bind_rows(emp_change_in_outcomes, boot_data)

# Compute means and standard deviations/errors of bootstrap estimates. 
summary_boot_data <- boot_data %>% 
  group_by(rel_year, year, percentage_type, variable) %>%
  summarise(across(.cols = c(tau_avg, pct_att, preds_avg, actual_avg), 
                   .fns = c(mean = mean, sd = sd), 
                   .names = '{.col}_{.fn}')) 
# -------------------------------------------------------------------------------------------- #
# Join the bootstrap SD to the empirical data. 
# -------------------------------------------------------------------------------------------- #

emp_change_in_outcomes <- emp_change_in_outcomes %>%
  
  left_join(summary_boot_data, by = c('rel_year', 'year', 'percentage_type', 'variable')) %>%
  
  relocate(c(pct_att_sd, pct_att_mean), .after = pct_att) %>%
  
  relocate(c(tau_avg_sd, tau_avg_mean), .after = tau_avg) %>%
  
  relocate(c(preds_avg_sd, preds_avg_mean), .after = preds_avg) %>%
  
  relocate(c(actual_avg_sd, actual_avg_mean), .after = actual_avg) 
# -------------------------------------------------------------------------------------------- #
emp_change_in_outcomes <- emp_change_in_outcomes %>% 
  
  mutate(pct_att_lci_99 = pct_att - qnorm(1 - 0.01/2)*pct_att_sd, 
         pct_att_hci_99 = pct_att + qnorm(1 - 0.01/2)*pct_att_sd, 
         effect_type = 'Average % Change', 
         tau_avg_lci_99 = tau_avg - qnorm(1 - 0.01/2)*tau_avg_sd, 
         tau_avg_hci_99 = tau_avg + qnorm(1 - 0.01/2)*tau_avg_sd, 
         effect_type = 'ATT')
# -------------------------------------------------------------------------------------------- #
table_emp_change_in_outcomes <- emp_change_in_outcomes %>% filter(is.na(variable)) 

# Save table. Commented out to avoid re-saving. 
# write_csv(table_emp_change_in_outcomes, file = here::here('Analysis', 
#                                                     'Tables', 
#                                                     dir_dep_var, 
#                                                     str_to_title(model_geography),
#                                                     paste0(str_to_lower(model_geography), '_' , 
#                                                            'average_change_in_outcomes_relative_time', '.csv')))
# -------------------------------------------------------------------------------------------- #
table_emp_change_in_outcomes_het <- emp_change_in_outcomes %>% filter(!is.na(variable)) 

# Save table. Commented out to avoid re-saving. 
# write_csv(table_emp_change_in_outcomes_het, file = here::here('Analysis', 
#                                                           'Tables', 
#                                                           dir_dep_var, 
#                                                           str_to_title(model_geography),
#                                                           paste0(str_to_lower(model_geography), '_' , 
#                                                                  'average_change_in_outcomes_het_covars', '.csv')))

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))

library(RColorBrewer)
display.brewer.pal(n = 9, name = 'Greys')
greys <- brewer.pal(n = 9, name = 'Greys')

plot_change_in_outcome_on_relyear(dta = emp_change_in_outcomes, 
                                  ci_label_str = '99% CI',
                                  ci_level = qnorm(1 - (0.01/2)), 
                                  percentage_type_str = 'Relative Time',
                                  x_value = rel_year,
                                  y_value = pct_att, 
                                  standard_error = pct_att_sd, 
                                  y_axis_title = 'Average Treatment Effects (%)', 
                                  x_axis_title = 'Time from Treatment', 
                                  plot_title = NULL, plot_subtitle = NULL,
                                  decimal_place_y = 1)
# Save Figure.
ggsave(filename = here::here('Analysis', 
                             'Figures', 
                             dir_dep_var, 
                             model_geography, 
                             paste0('errors_and_preds', bootstrap_by_tracts),
                             paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'change_in_outcomes_by_relative_time', '.pdf')), 
       width = 8, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #
# Filter out the rows w/ ATTs heterogeneity by covariate subsets. 
# -------------------------------------------------------------------------------------------- #
emp_change_in_outcomes_wo_covars <- emp_change_in_outcomes %>% filter(is.na(variable)) 

plot_change_in_outcome_on_relyear(dta = emp_change_in_outcomes_wo_covars, 
                                  ci_label_str = '99% CI',
                                  ci_level = qnorm(1 - (0.01/2)),
                                  percentage_type_str = 'Year',
                                  x_value = year,
                                  y_value = pct_att, 
                                  standard_error = pct_att_sd, 
                                  y_axis_title = 'Average Treatment Effects (%)', 
                                  x_axis_title = 'Year', 
                                  plot_title = NULL, plot_subtitle = NULL,
                                  decimal_place_y = 1)
# Save Figure.
ggsave(filename = here::here('Analysis', 
                             'Figures', 
                             dir_dep_var, 
                             model_geography, 
                             paste0('errors_and_preds', bootstrap_by_tracts),
                             paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'change_in_outcomes_by_year', '.pdf')), 
       width = 8, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #
