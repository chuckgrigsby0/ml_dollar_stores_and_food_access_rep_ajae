# ----------------------------------- #
# Load packages
# ----------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds')
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
model_output <- readRDS(here::here('Analysis', 
                                   'Model_Training',
                                   dir_dep_var,
                                   filename))
# model_output$cv_mse_by_tuning_params %>% arrange(Score) %>% View()
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
  
  # For consistency with the out-of-sample predictions during CV, 
  # we obtain counterfactual predictions from 2007 to 2020.
  
  filter(year >= '2006') # Predict on treated from 2006 to 2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
   
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #
# Pretreatment errors against relative time. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R'))
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
source(here::here('Code', 'Functions', 'Function_errors_on_relative_time.R')) # Errors versus relative time. 
source(here::here('Code', 'Functions', 'Function_actual_vs_predicted.R')) # Actual versus Counterfactuals
source(here::here('Code', 'Functions', 'Function_preds_and_actual_on_time.R')) # Predicted and Actual vs time. 
# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #  
max_and_min_plot_coords <- function(regression_table){
  
  y_coord_upper <- max(regression_table$estimate + 1.96*regression_table$std.error)
  y_coord_lower <- min(regression_table$estimate - 1.96*regression_table$std.error)
  
  coordinate_list <- list('y_coord_lower' = y_coord_lower, 
                          'y_coord_upper' = y_coord_upper)
  
  return(coordinate_list)
  
}
# -------------------------------------------------------------------------------------------- #  
emp_errors_on_relyear <- errors_on_relative_time(model_preds_dta = model_preds, national = FALSE, geography_str = model_geography)
error_coords <- max_and_min_plot_coords(regression_table = emp_errors_on_relyear)

dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 

plot_errors_on_relyear(dta = emp_errors_on_relyear, 
                       y_value = estimate, 
                       standard_error = std.error,
                       ci_label_str = '99% CI',
                       y_axis_title = 'Cross-Validated Errors', 
                       x_axis_title = 'Relative Time to Treatment', 
                       plot_title = 'Cross-Validated Prediction Errors by Time to Treatment', 
                       plot_subtitle = paste(paste('Geography:', model_geography), paste('Outcome:', dep_var_title), sep = '\n'),
                       #breaks_y = breaks_errors, 
                       decimal_place_y = 0.001, 
                       y1_lim = error_coords$y_coord_lower, 
                       y2_lim = error_coords$y_coord_upper)

# ggsave(here::here('Analysis', 'Figures', 
#                   paste0('cv_errors_on_relative_time_', str_to_lower(model_geography), '_', model_dep_var, '_', 'test.png')), 
#        width = 12, height = 8, unit = 'in')
# -------------------------------------------------------------------------------------------- #

# Predicted (Actual) on relative time. 
emp_predictions_on_relyear <- actual_vs_predicted(model_preds_dta = model_preds, national = FALSE, geography_str = model_geography)
prediction_coords <- max_and_min_plot_coords(regression_table = emp_predictions_on_relyear)

plot_actual_on_predicted(dta = emp_predictions_on_relyear, 
                         y_value = estimate, 
                         standard_error = std.error,
                         y_axis_title = 'Low-Access (Shares)', 
                         x_axis_title = 'Time to Treatment', 
                         plot_title = 'Cross-Validated Predicted and Actual Outcomes by Time to Treatment', 
                         plot_subtitle = paste(paste('Geography:', model_geography), paste('Outcome:', dep_var_title), sep = '\n'),
                         #breaks_y = breaks_errors, 
                         x_intercept_val = 0,
                         decimal_place_y = 0.001, 
                         y1_lim = prediction_coords$y_coord_lower, 
                         y2_lim = prediction_coords$y_coord_upper)

# ggsave(here::here('Analysis', 'Figures', 
#                   paste0('preds_vs_cfs_relative_time_', str_to_lower(model_geography), '_', model_dep_var, '_', 'test.png')), 
#        width = 12, height = 8, unit = 'in')
# -------------------------------------------------------------------------------------------- #  

preds_and_actual_on_year <- preds_and_actual_on_time(model_preds_dta = model_preds, 
                                                     national = FALSE, 
                                                     geography_str = model_geography) 

preds_by_time_coords <- max_and_min_plot_coords(regression_table = preds_and_actual_on_year)

plot_pred_and_actual_by_year(dta = preds_and_actual_on_year, 
                             outcome_type = 'CV', 
                             standard_error = std.error,
                             y_axis_title = paste(str_to_title(str_replace_all(model_dep_var, '_', '-')), '(Shares)', sep = ' '), 
                             x_axis_title = 'Year', 
                             plot_title = 'Cross-Validated Predictions and Actual Low Access', 
                             plot_subtitle = paste('Geography:', str_to_title(model_geography), sep = ' '),
                             decimal_place_y = 0.001, 
                             y1_lim = preds_by_time_coords$y_coord_lower, 
                             y2_lim = preds_by_time_coords$y_coord_upper, 
                             x_intercept_val = NULL)

# ggsave(here::here('Analysis', 'Figures', 
#                   paste0('cv_preds_vs_actual_year_', str_to_lower(model_geography), '_', model_dep_var, '_', 'test.png')), 
#        width = 12, height = 8, unit = 'in')
#--------------------------------------------------------------------------------------------#
preds_by_time_coords <- max_and_min_plot_coords(regression_table = preds_and_actual_on_year)

plot_pred_and_actual_by_year(dta = preds_and_actual_on_year, 
                             outcome_type = 'Post-Treatment', 
                             standard_error = std.error,
                             y_axis_title = paste(str_to_title(str_replace_all(model_dep_var, '_', '-')), '(Shares)', sep = ' '), 
                             x_axis_title = 'Year', 
                             plot_title = 'Post-Treatment Predictions and Actual Low Access', 
                             plot_subtitle = paste('Geography:', str_to_title(model_geography), sep = ' '),
                             decimal_place_y = 0.001, 
                             y1_lim = preds_by_time_coords$y_coord_lower, 
                             y2_lim = preds_by_time_coords$y_coord_upper, 
                             x_intercept_val = NULL)

# ggsave(here::here('Analysis', 'Figures', 
#                   paste0('post_treatment_preds_vs_actual_year_', str_to_lower(model_geography), '_', model_dep_var, '_', 'test.png')), 
#        width = 12, height = 8, unit = 'in')

#--------------------------------------------------------------------------------------------#

# -------------------------------------------------------------------------------------------- #
summary_rel_year <- model_preds %>%
  mutate(err = actual-preds) %>%
  filter(rel_year < 0) %>%
  group_by(year, rel_year) %>%
  summarise(across(.cols = err, mean, na.rm=TRUE)) %>%
  arrange(err)
# -------------------------------------------------------------------------------------------- #  
summary_year <- model_preds %>%
  mutate(err = actual-preds) %>%
  filter(rel_year < 0) %>%
  group_by(year) %>%
  summarise(across(.cols = err, mean, na.rm=TRUE)) %>%
  arrange(err)
# -------------------------------------------------------------------------------------------- #  


# -------------------------------------------------------------------------------------------- #  
# Manual F-Statistic. 
# ybar = mean(pretr_preds$err)
# sst = sum( (pretr_preds$err)^2 )
# ssr = sum(model_errors$residuals^2)
# k = length(as.numeric(unique(pretr_preds$rel_year)))
# n = nrow(pretr_preds)
# num = (sst-ssr)/((n-0)-(n-k))
# denom = (ssr/(n-k))
# fstat = num/denom
# pf(fstat, df1=((n-0)-(n-k)), df2 = (n-k), lower.tail = TRUE)