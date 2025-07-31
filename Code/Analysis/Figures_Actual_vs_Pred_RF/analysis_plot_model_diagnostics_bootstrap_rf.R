# Script to create event-study type plots from random forest models. 
# Results provided for R&R and available upon request. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap.
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban'
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# Random Forest model results, including the model from the original sample, 
# are located in single folders. 
# -------------------------------------------------------------------------------------------- #
filename <- paste('bootstrap_errors_and_preds', str_to_lower(model_geography), model_dep_var, 'tracts', 'rf', '0.rds', sep = '_'); filename
dir_geography = paste0(model_geography, '_', 'Bootstrap')
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' '))
emp_estimates <- readRDS(here::here('Analysis', 
                                   dir_geography, 
                                   dir_dep_var, 
                                   'bootstrap_errors_and_predictions_tracts_rf', 
                                   filename))
# -------------------------------------------------------------------------------------------- #
# Load bootstrapped estimates. 
# -------------------------------------------------------------------------------------------- #
boot_estimates <- seq(0, 499, 1) %>% 
  
  map(function(.x){ 
    
    fname <- paste('bootstrap_errors_and_preds', 
                    str_to_lower(model_geography), 
                    model_dep_var, 
                    'tracts', 
                    'rf', 
                    paste0(.x, '.rds'), sep = '_')
    
    boot_est_list <- readRDS(here::here('Analysis', 
                       dir_geography, 
                       dir_dep_var, 
                       'bootstrap_errors_and_predictions_tracts_rf', 
                       fname))
    
    return(boot_est_list)
    
    })
# -------------------------------------------------------------------------------------------- #
# Select the predictions from each list. 
boot_preds <- boot_estimates %>%
  
  map_dfr(function(.x){ 
    
    .x %>% pluck('predictions')
    
    }) %>% 
  
  select(rel_year, estimate, Outcome, id)
# -------------------------------------------------------------------------------------------- #
boot_preds_sd <- boot_preds %>%
  
  group_by(rel_year, Outcome) %>%
  
  summarise(across(.cols = estimate, 
                   .fns = list('sd' = \(x) sd(x)), 
                   .names = '{.col}_{.fn}'))
# -------------------------------------------------------------------------------------------- #

# Join the emprical estimates and the bootstrapped SEs. 

# -------------------------------------------------------------------------------------------- #
emp_preds <- emp_estimates %>% pluck('predictions')
  
emp_preds <- emp_preds %>%
  select(rel_year, estimate, Outcome) %>%
  left_join(boot_preds_sd, by = c('rel_year', 'Outcome')) %>%
  relocate(Outcome, .after = last_col())

# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Function that loads multiple functions for plotting model results. 
# Also loads in a color palette for the plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
plot_actual_on_predicted(dta = emp_preds, 
                         y_value = estimate, 
                         standard_error = estimate_sd,
                         y_axis_title = 'Low-Access (Shares)', 
                         x_axis_title = 'Time from Treatment', 
                         plot_title = NULL, 
                         plot_subtitle = NULL,
                         x_intercept_val = 0,
                         decimal_place_y = 0.001, 
                         y1_lim = NULL, 
                         y2_lim = NULL)

ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, 
                  paste0('errors_and_preds', bootstrap_by_tracts, '_rf'), # Saves to directories and subdirectories.
                  paste0('preds_vs_cfs_relative_time_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts, '_rf', '.png')),
       width = 12, height = 8, unit = 'in')
# -------------------------------------------------------------------------------------------- #
