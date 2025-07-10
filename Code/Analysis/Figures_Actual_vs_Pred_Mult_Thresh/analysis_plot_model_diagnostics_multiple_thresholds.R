# Script creates figures showing actual vs predicted low-access shares using different classification thresholds. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' '))
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
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
  
  filter(year >= '2006') # we plot counterfactual predictions from 2006 to 2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
# Function in this script is used in the bootstrap_errors_and_preds function. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R'))

source(here::here('Code', 'Functions', 'Function_bootstrap_errors_and_preds_multiple_thresholds.R'))
# -------------------------------------------------------------------------------------------- #
emp_errors_and_preds <- bootstrap_errors_and_preds(prep_bootstrap_data = FALSE, 
                                                   bootstrap_ids = bootstrap_ids, 
                                                   iter = 0, 
                                                   bootstrap_by_tracts = bootstrap_by_tracts)
# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_load_bootstrap_errors_and_predictions.R'))
# -------------------------------------------------------------------------------------------- #
boot_data <- seq(1, 499, 1) %>%
  
  map(function(.iter){ 
    
    load_bootstrap_errors_and_predictions_mult_thresholds_array(model_geography_str = model_geography, 
                                                                model_dep_var_str = model_dep_var, 
                                                                bootstrap_by_tracts = '_tracts', 
                                                                bootstrap_iter = .iter)
    
  })
# -------------------------------------------------------------------------------------------- # 
emp_predictions_on_relyear <- emp_errors_and_preds %>% 
  pluck('predictions') %>%
  select(rel_year, estimate, Outcome)
# -------------------------------------------------------------------------------------------- # 

boot_data_preds <- boot_data %>%
  
  map_dfr(function(.x){ 
    
    dta <- .x %>% pluck('predictions')  
    
  })

# Join empirical data to bootstrapped estimates to compute SD. 
boot_data_preds <- boot_data_preds %>%
  
  select(rel_year, estimate, Outcome) %>%
  
  bind_rows(emp_predictions_on_relyear) %>%
  
  group_by(rel_year, Outcome) %>%
  
  summarise(estimate_sd = sd(estimate))

# -------------------------------------------------------------------------------------------- # 

emp_predictions_on_relyear <- emp_predictions_on_relyear %>%
  left_join(boot_data_preds, by = c('Outcome', 'rel_year')) %>% 
  relocate(estimate_sd, .after = estimate)

emp_predictions_on_relyear <- emp_predictions_on_relyear %>% 
  mutate(threshold_type = Outcome)

emp_predictions_on_relyear$Outcome <- emp_predictions_on_relyear$Outcome %>%
  str_replace_all(pattern = c('actual' = 'Actual', 
                              'preds_orig' = 'Predicted (Baseline)', 
                              'preds_j_index' = 'Predicted (J-Index)', 
                              'preds_f1' = 'Predicted (F1)',
                              'preds_avg' = 'Predicted (Average)', 
                              'preds_lipton' = 'Predicted (F1/2)')) 

factor_ordering <- unique(emp_predictions_on_relyear$Outcome)[unique(emp_predictions_on_relyear$Outcome) != 'Actual']
emp_predictions_on_relyear$Outcome <- factor(emp_predictions_on_relyear$Outcome, 
                                             levels = c('Actual', factor_ordering))
#--------------------------------------------------------------------------------------------#
source(here::here('Code', 'Functions', 'Function_plot_preds_and_observed_multiple_thresholds.R'))

map(factor_ordering, function(.x){
  
  plot_dta <- emp_predictions_on_relyear %>%
    filter(Outcome == 'Actual' | Outcome == .x)
  
  plot_label <- str_subset(unique(plot_dta$threshold_type), pattern = 'actual', negate = TRUE)
  
  plot_actual_on_predicted(dta = plot_dta, 
                           y_value = estimate, 
                           standard_error = estimate_sd, 
                           alpha_level = 0.01, 
                           y_axis_title = 'Low-Access (Shares)', 
                           x_axis_title = 'Time from Treatment', 
                           plot_title = NULL, 
                           plot_subtitle = NULL,
                           x_intercept_val = 0,
                           decimal_place_y = 0.001)
  
  ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, 
                    paste0('errors_and_preds_mult_thresh', bootstrap_by_tracts), # Saves to directories and subdirectories.
                    paste0('preds_vs_cfs_relative_time_', plot_label, '_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts, '.png')),
         width = 12, height = 8, unit = 'in', dpi = 300)
  
})
# -------------------------------------------------------------------------------------------- #