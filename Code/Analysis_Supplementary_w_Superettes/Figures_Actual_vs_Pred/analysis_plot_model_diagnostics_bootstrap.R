# Script creates Figures E.1 and E.2 in supplementary analyses including superettes in low-access indicator. 
# Figures show actual vs predicted share of low-access block groups across relative treatment timing. 
# Additional figures include average CV errors and treatment effects across relative time to treatment. 
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final_w_superettes', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') # We obtain counterfactual predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_load_bootstrap_errors_and_predictions.R'))
# Functions to subset the named elements 'errors' or the 'predictions', 
# compute standard errors, and join standard errors to empirical point estimates.  
# -------------------------------------------------------------------------------------------- #
# Select from the list of bootstrapped output. 
source(here::here('Code', 'Functions', 'Function_bootstrap_subset_errors_and_predictions.R')) 
# Simple function to compute SEs by some grouping variables. 
source(here::here('Code', 'Functions', 'Function_bootstrap_compute_standard_errors.R')) 
# Join the empirical estimates and the bootstrapped SEs.
source(here::here('Code', 'Functions', 'Function_bootstrap_join_ses_to_emp_estimates.R')) 
# -------------------------------------------------------------------------------------------- #
boot_data <- seq(1, 499, 1) %>%
  
  map(function(.iter){ 
    
    load_bootstrap_errors_and_predictions_array_w_superettes(model_geography_str = model_geography, 
                                                             model_dep_var_str = model_dep_var, 
                                                             bootstrap_by_tracts = '_tracts', 
                                                             bootstrap_iter = .iter)
    
  })
# -------------------------------------------------------------------------------------------- #
# Pre-treatment errors against relative time. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R'))
source(here::here('Code', 'Functions', 'Function_errors_on_relative_time.R'))
# -------------------------------------------------------------------------------------------- #
emp_errors_on_relyear <- errors_on_relative_time(model_preds_dta = model_preds, 
                                                 national = FALSE, 
                                                 geography_str = model_geography)

# Note that id = bootstrap iteration. Therefore, id = 0 implies the original point estimates in this context. 
emp_errors_on_relyear <- emp_errors_on_relyear %>% mutate(id = 0) # to join to bootstrap models. 
# -------------------------------------------------------------------------------------------- #
# Extract the element containing bootstrapped estimates from regressing errors on rel_year. 
boot_data_errors <- subset_errors_and_predictions(bootstrap_data = boot_data, 
                                                  list_element_name = 'errors')

# Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
boot_data_errors <- bind_rows(emp_errors_on_relyear, boot_data_errors)

# Compute standard error. 
boot_se_errors_on_relyear <- compute_bootstrap_standard_errors(dta = boot_data_errors, 
                                                               group_vars = c('rel_year', 'Outcome'))  

# Combine bootstrap standard errors with the empirical estimates. 
emp_errors_on_relyear <- join_bootstrap_ses_to_emp_estimates(empirical_estimates_dta = emp_errors_on_relyear, 
                                                             bootstrap_estimates_dta = boot_se_errors_on_relyear, 
                                                             join_vars = c('rel_year', 'Outcome'))
# -------------------------------------------------------------------------------------------- #
# Actual versus Counterfactuals
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_actual_vs_predicted.R'))
#--------------------------------------------------------------------------------------------#

emp_predictions_on_relyear <- actual_vs_predicted(model_preds_dta = model_preds, 
                                                  national = FALSE, 
                                                  geography_str = model_geography)

emp_predictions_on_relyear <- emp_predictions_on_relyear %>% mutate(cv_mse = model_output$min_cv_mse, 
                                                                    id = 0) # to join to bootstrap models. 
# -------------------------------------------------------------------------------------------- #
# Extract the element containing bootstrapped estimates from regressing predicted and actual low access, respectively, on rel_year. 
boot_data_predictions <- subset_errors_and_predictions(bootstrap_data = boot_data, 
                                                       list_element_name = 'predictions')

# Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
boot_data_predictions <- bind_rows(emp_predictions_on_relyear, boot_data_predictions)

# Compute standard error. 
boot_se_predictions_on_relyear <- compute_bootstrap_standard_errors(dta = boot_data_predictions, 
                                                                    group_vars = c('rel_year', 'Outcome'))  # Note that Outcome = Actual or Predicted

# Combine bootstrap standard errors with the empirical estimates. 
emp_predictions_on_relyear <- join_bootstrap_ses_to_emp_estimates(empirical_estimates_dta = emp_predictions_on_relyear, 
                                                             bootstrap_estimates_dta = boot_se_predictions_on_relyear, 
                                                             join_vars = c('rel_year', 'Outcome'))
# -------------------------------------------------------------------------------------------- #
# Check average classification error across model runs. 
# -------------------------------------------------------------------------------------------- #
boot_data_predictions %>% summarise(mean_mse = mean(cv_mse))
# -------------------------------------------------------------------------------------------- #
# Causal Effects. 
# -------------------------------------------------------------------------------------------- # 
source(here::here('Code', 'Functions', 'Function_effects_on_relative_time.R'))
# -------------------------------------------------------------------------------------------- # 
# Regress tau, the causal effect of interest, on relative time since treatment. 
# -------------------------------------------------------------------------------------------- # 
emp_tau_on_relyear <- effects_on_relative_time(model_preds_dta = model_preds, 
                                                   national = FALSE, 
                                                   geography_str = model_geography)

emp_tau_on_relyear <- emp_tau_on_relyear %>% mutate(id = 0) # to join to bootstrap estimates. 
# -------------------------------------------------------------------------------------------- # 

# Extract the element containing bootstrapped estimates from regressing tau on rel_year. 
boot_data_tau_on_relyear <- subset_errors_and_predictions(bootstrap_data = boot_data, 
                                                              list_element_name = '^effects_rel_year$') # Must use regular expression. 

# Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
boot_data_tau_on_relyear <- bind_rows(emp_tau_on_relyear, boot_data_tau_on_relyear)

# Compute standard error. 
boot_se_tau_on_relyear <- compute_bootstrap_standard_errors(dta = boot_data_tau_on_relyear, 
                                                               group_vars = c('rel_year', 'Outcome'))  

# Combine bootstrap standard errors with the empirical estimates. 
emp_tau_on_relyear <- join_bootstrap_ses_to_emp_estimates(empirical_estimates_dta = emp_tau_on_relyear, 
                                                             bootstrap_estimates_dta = boot_se_tau_on_relyear, 
                                                          join_vars = c('rel_year', 'Outcome'))
# -------------------------------------------------------------------------------------------- # 
empirical_estimates <- list('errors' = emp_errors_on_relyear, 
                            'predictions' = emp_predictions_on_relyear, 
                            'tau_on_relyear' = emp_tau_on_relyear) 
# -------------------------------------------------------------------------------------------- #
# Function to obtain the upper and lower bound for the confidence intervals for bootstrapped estimates
# and use the values to specify the scale for the y-axis in the plot.
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_bootstrap_max_and_min_plot_coords.R'))
# -------------------------------------------------------------------------------------------- #
plot_coords <- map(empirical_estimates, function(.x) bootstrap_max_and_min_plot_coords(.x))
# -------------------------------------------------------------------------------------------- #
# Function that loads multiple functions for plotting model results. 
# Also loads in a color palette for the plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #

plot_errors_on_relyear(dta = empirical_estimates$errors, 
                       ci_label_str = '99% CI',
                       ci_level = qnorm(1 - (0.01/2)), 
                       y_value = estimate, 
                       standard_error = bootstrap_sd,
                       y_axis_title = 'Average Cross-Validated Errors', 
                       x_axis_title = 'Relative Time from Treatment', 
                       plot_title = NULL, 
                       plot_subtitle = NULL,
                       decimal_place_y = 0.001, 
                       y1_lim = NULL, 
                       y2_lim = NULL)

ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_and_preds', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                   paste0('cv_errors_on_relative_time_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts, '.pdf')), 
        width = 8, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #
plot_actual_on_predicted(dta = empirical_estimates$predictions, 
                         y_value = estimate, 
                         standard_error = bootstrap_sd,
                         ci_level = qnorm(1 - 0.01/2),
                         y_axis_title = 'Share of Low-Access Block Groups', 
                         x_axis_title = 'Time from Treatment', 
                         plot_title = NULL, 
                         plot_subtitle = NULL,
                         x_intercept_val = 0,
                         decimal_place_y = 0.01, 
                         y1_lim = NULL, 
                         y2_lim = NULL)

ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_and_preds', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                  paste0('preds_vs_cfs_relative_time_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts, '.pdf')), 
       width = 8, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #
plot_effects_on_relyear(dta = empirical_estimates$tau_on_relyear, 
                        ci_label_str = '99% CI',
                        ci_level = qnorm(1 - (0.01/2)), 
                        legend_lab_str = 'Average Treatment Effect',
                        y_value = estimate, 
                        standard_error = bootstrap_sd,
                        y_axis_title = 'Average Treatment Effects', 
                        x_axis_title = 'Time from Treatment', 
                        plot_title = NULL, 
                        plot_subtitle = NULL,
                        decimal_place_y = 0.001, 
                        y1_lim = NULL, 
                        y2_lim = NULL)

ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_and_preds', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                  paste0('tau_vs_relative_time_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts, '.pdf')), 
       width = 8, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #