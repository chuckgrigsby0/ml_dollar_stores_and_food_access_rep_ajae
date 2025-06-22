# To run this program, simply change from Urban to Rural and vice-versa to create and save a table of ATTs
# using different thresholds. 
# -------------------------------------------------------------------------------------------- #
library(tidymodels)
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
model_geography = 'Urban'; 
model_dep_var = 'low_access'; 
bootstrap_by_tracts <- '_tracts'; 
bootstrap_ids = '01_499'
# -------------------------------------------------------------------------------------------- #
# .libPaths()
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
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
  
  filter(year >= '2006') # For consistency with the out-of-sample predictions during CV, 
# we plot counterfactual predictions from 2007 to 2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

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
# Subset the ATTs
# -------------------------------------------------------------------------------------------- # 
boot_att <- boot_data %>%
  
  map_dfr(function(.x){ 
    
    att_dta <- .x %>% pluck('att')
    
    })
# -------------------------------------------------------------------------------------------- # 
# Combine the empirical ATTs with the bootstrapped ATTs
# -------------------------------------------------------------------------------------------- # 
boot_att <- bind_rows(emp_errors_and_preds$att, boot_att)
# -------------------------------------------------------------------------------------------- # 

# Obtain a string vector for the measures requiring summary statistics. 

summary_stat_vars <- str_subset(names(boot_att), '^tau|^preds|^actual|^pct_att'); summary_stat_vars

# Compute Bootstrapped SDs. 
summary_boot_att <- boot_att %>% 
  
  group_by(rel_year, year, percentage_type) %>% 
  
  summarise(across(.cols = all_of(summary_stat_vars), 
                   .fns = list('sd' = \(x) sd(x) ), 
                   .names = '{.col}_{.fn}') )

# Combine the bootstrapped standard deviations with the point estimates. 
emp_att <- emp_errors_and_preds %>% 
  pluck('att') %>% 
  left_join(summary_boot_att, by = c('rel_year', 'year', 'percentage_type') )  %>%
  select(c('rel_year', 'year', 'percentage_type'), sort(names(.)))

correct_col_order = c('actual_avg', 'preds_orig_avg', 'tau_orig_avg', 'tau_orig_avg_sd', 'pct_att_orig', 'pct_att_orig_sd', 
                      'preds_f1_avg', 'tau_f1_avg', 'tau_f1_avg_sd', 'pct_att_f1', 'pct_att_f1_sd',
                      'preds_lipton_avg', 'tau_lipton_avg', 'tau_lipton_avg_sd', 'pct_att_lipton', 'pct_att_lipton_sd',
                      'preds_j_index_avg', 'tau_j_index_avg', 'tau_j_index_avg_sd', 'pct_att_j_index', 'pct_att_j_index_sd',
                      'preds_avg_avg', 'tau_avg_avg', 'tau_avg_avg_sd', 'pct_att_avg', 'pct_att_avg_sd')

emp_att_all <- emp_att %>% 
  filter(percentage_type == 'All') %>%
  select(-c('percentage_type', 'rel_year', 'year', 'id', 'actual_avg_sd'), -matches('^preds_.*sd$') ) %>% 
  select(all_of(correct_col_order)) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Parameter', 
               values_to = 'Estimate')


groupings <- list(
  
  'Base' = c('actual_avg', 'preds_orig_avg', 
             'tau_orig_avg', 'tau_orig_avg_sd', 
             'pct_att_orig', 'pct_att_orig_sd'), 
  
  'J Index' = c('preds_j_index_avg', 'tau_j_index_avg', 'tau_j_index_avg_sd', 
                'pct_att_j_index', 'pct_att_j_index_sd'), 
  
  
  'Average' = c('preds_avg_avg', 'tau_avg_avg', 'tau_avg_avg_sd', 
                'pct_att_avg', 'pct_att_avg_sd'),
  
  
  'F1/2' = c('preds_lipton_avg', 'tau_lipton_avg', 'tau_lipton_avg_sd', 
             'pct_att_lipton', 'pct_att_lipton_sd'), 
  
  'F1' = c('preds_f1_avg', 'tau_f1_avg', 'tau_f1_avg_sd', 
           'pct_att_f1', 'pct_att_f1_sd') 
  
  )
                  
                  
# Create a function to map variable names to their types
map_variable_type <- function(var_name, groupings) {
  type <- names(groupings)[map_lgl(groupings, \(.x) var_name %in% .x)]
  if (length(type) == 0) return(NA_character_)
  return(type)
}

# Apply the mapping to your dataframe
emp_att_all <- emp_att_all %>%
  mutate(Variable_Type = map_chr(Parameter, \(.y) map_variable_type(var_name = .y, groupings)))


emp_att_all <- emp_att_all %>% 
  mutate(temp = Parameter) %>%
  separate_wider_regex(cols = temp, 
                       patterns = c(Measure = '^actual|^preds|^tau|^pct_att', 
                                    '.*', 
                                    Statistic = 'avg$|sd$'), 
                       too_few = 'align_start') %>%
  mutate(
    Measure = str_replace_all(Measure, c('preds' = 'Proportion low access, post-entry (predicted)', 
                                         'actual' = 'Proportion low access, post-entry (observed)', 
                                         'tau' = 'ATT', 
                                         'pct_att' = 'ATT (%)') ), 
    Statistic = if_else(is.na(Statistic), 'avg', Statistic)
  ) %>% 
  select(-Parameter)
  # select(-Parameter) %>%
  # pivot_wider(names_from = Variable_Type, 
  #             values_from = Estimate) 

# Compute confidence intervals based on bootstrapped SDs
alpha_levels <- c(0.01, 0.05, 0.10)
ci_thresholds <- alpha_levels %>% map(function(.x) qnorm(p = 1 - .x/2))
ci_thresholds <- set_names(ci_thresholds, nm = c('one', 'five', 'ten'))

emp_att_all_with_ci <- emp_att_all %>%
  relocate(Estimate, .after = Statistic) %>%
  group_by(Measure, Variable_Type) %>%
  mutate(
    CI_lower_01 = case_when(
      Statistic == "avg" ~ Estimate - ci_thresholds$one * lead(Estimate),
      TRUE ~ NA_real_
    ),
    CI_upper_01 = case_when(
      Statistic == "avg" ~ Estimate + ci_thresholds$one * lead(Estimate),
      TRUE ~ NA_real_
    ),
    CI_lower_05 = case_when(
      Statistic == "avg" ~ Estimate - ci_thresholds$five * lead(Estimate),
      TRUE ~ NA_real_
    ),
    CI_upper_05 = case_when(
      Statistic == "avg" ~ Estimate + ci_thresholds$five * lead(Estimate),
      TRUE ~ NA_real_
    ),
    CI_lower_10 = case_when(
      Statistic == "avg" ~ Estimate - ci_thresholds$ten * lead(Estimate),
      TRUE ~ NA_real_
    ),
    CI_upper_10 = case_when(
      Statistic == "avg" ~ Estimate + ci_thresholds$ten * lead(Estimate),
      TRUE ~ NA_real_
    ),
    # Determine significance levels with nested case_when
    Significance = case_when(
      Statistic == "avg" & (CI_lower_01 > 0 | CI_upper_01 < 0) ~ "***",
      Statistic == "avg" & (CI_lower_05 > 0 | CI_upper_05 < 0) ~ "**",
      Statistic == "avg" & (CI_lower_10 > 0 | CI_upper_10 < 0) ~ "*",
      TRUE ~ ""
    )
  ) %>% 
  mutate(across(.cols = where(is.numeric), 
                .fn = \(x) case_when(Measure == 'ATT (%)' ~ x * 100, 
                                     TRUE ~ x) ) 
  ) %>%
  mutate(
    Estimate = round(Estimate, digits = 4) 
  ) %>% 
  mutate(
    Estimate = case_when(
      Statistic == "avg" ~ paste0(format(Estimate, scientific = FALSE, trim = TRUE), Significance),
      TRUE ~ as.character(Estimate)
    )
  ) %>%
  mutate(
    Estimate = case_when(
      Statistic == "sd" ~ paste0('(', trimws(format(Estimate, scientific = FALSE, trim = TRUE)), ')'),
      TRUE ~ as.character(Estimate)
    )
  ) %>%
  select(-matches('^CI_'), -Significance) %>%
  pivot_wider(names_from = Variable_Type, 
              values_from = Estimate) %>%
  select(-Statistic)
# ----------------------------------- #

opt_thresh_boot <- boot_data %>% 
  
  map_dfr(function(.x){ 
    
    opt_thresh_dta <- .x %>% pluck('optimal_thresholds')
    
    })

opt_thresh_boot_sd <- opt_thresh_boot %>%
  
  group_by(threshold_metric) %>%
  
  summarise(across(.cols = `.threshold`, 
                   .fns = list('sd' = \(x) sd(x, na.rm=TRUE)), 
                   .names = '{.col}_{.fn}') ) %>%
  
  mutate(.threshold_sd =  paste0('(', trimws(format(round(.threshold_sd, digits = 4),
                                                    trim = TRUE,
                                                    scientific = FALSE)), ')') ) %>%
  
  mutate(threshold_metric = str_replace_all(threshold_metric, pattern = c('avg' = 'Average', 
                                                                          'f1' = 'F1', 
                                                                          'j_index' = 'J Index', 
                                                                          'lipton' = 'F1/2'))) %>% 
  pivot_wider(names_from = threshold_metric, 
              values_from = .threshold_sd); opt_thresh_boot_sd

opt_thresh <- emp_errors_and_preds$optimal_thresholds %>%
  
  select(.threshold, threshold_metric) %>% 
  
  mutate(threshold_metric = str_replace_all(threshold_metric, pattern = c('avg' = 'Average', 
                                                                          'f1' = 'F1', 
                                                                          'j_index' = 'J Index', 
                                                                          'lipton' = 'F1/2')), 
         .threshold = round(.threshold, digits = 4) ) %>% 
  
  pivot_wider(names_from = threshold_metric, 
              values_from = .threshold) %>% 
  mutate(Base = 0.5, 
         Measure = 'Threshold') %>%
  relocate(c(Measure, Base)) %>%
  mutate(across(.cols = where(is.numeric), 
                .fns = \(x) format(x, scientific = FALSE, digits = 4) ) )

table_opt_thresh <- bind_rows(opt_thresh, 
                              opt_thresh_boot_sd) %>% 
  mutate(across(.cols = everything(), 
                .fns = \(x) if_else(is.na(x), '', x) ) )

emp_att_all_with_ci <- bind_rows(table_opt_thresh, emp_att_all_with_ci)

target_dir <- paste0('Analysis/Tables/Low_Access/', model_geography); target_dir
saveRDS(emp_att_all_with_ci, file = here::here(target_dir,
                                               paste0(str_to_lower(model_geography), 
                                                      '_att_w_multiple_thresholds', '.rds')))
rm(list=ls())
gc()
