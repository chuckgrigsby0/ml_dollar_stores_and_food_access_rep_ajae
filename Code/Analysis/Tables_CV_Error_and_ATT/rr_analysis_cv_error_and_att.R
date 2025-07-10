# Script saves R tables for further processing of average cv error and atts from main results. 
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
library(readr)
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap.
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', 
                   str_to_lower(model_geography), '_', 
                   model_dep_var, '_', 
                   'final.rds')
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title 
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
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
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #
# Create pre-treatment and post-treatment data. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id = 0

pretreat <- model_preds %>% 
  filter(rel_year < 0) %>%
  mutate(rel_year = factor(rel_year), 
         err = actual - preds) %>%
  select(-tau)

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

# Percentage change in the ATT by relative time and overall. 
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
# -------------------------------------------------------------------------------------------- #
# Load bootstrapped data. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_cv_error_and_att', bootstrap_by_tracts) # NULL or '_tracts'

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

boot_data <- bind_rows(cv_and_att_summary, boot_data) %>% 
  relocate(id)
# -------------------------------------------------------------------------------------------- #
pretr_vars <- c('actual_avg', 'preds_avg', 'err_avg', 'pct_err')
posttr_vars <- c('actual_avg', 'preds_avg', 'tau_avg', 'pct_att')
# -------------------------------------------------------------------------------------------- #
pretr_sum_boot_data <- boot_data %>%
  filter(percentage_type == 'All' & !is.na(err_avg) ) %>%
  select(-rel_year) %>%
  summarise(across( 
    .cols = all_of(pretr_vars), 
    .fns = \(x) sd(x), 
    .names = '{.col}'
  )) %>%
  mutate(Statistic = 'SD')

pretr_cv_error <- cv_and_att_summary %>%
  filter(percentage_type == 'All' & !is.na(err_avg) ) %>% 
  select(all_of(pretr_vars)) %>%
  mutate(Statistic = 'Avg')

cv_errors <- bind_rows(pretr_cv_error, pretr_sum_boot_data) %>% 
  mutate(Measure = 'CV Error', 
         Geography = model_geography)

cv_errors_long <- cv_errors %>% 
  pivot_longer(
    cols = actual_avg:pct_err,
    names_to = 'variable', 
    values_to = 'estiamte'
  ) %>%
  mutate(variable = factor(variable, 
                           levels = c('actual_avg', 
                                      'preds_avg', 
                                      'err_avg', 
                                      'pct_err'))) %>%
  arrange(variable, Statistic)

# -------------------------------------------------------------------------------------------- #
posttr_sum_boot_data <- boot_data %>%
  filter(percentage_type == 'All' & !is.na(tau_avg) ) %>%
  select(-rel_year) %>%
  summarise(across( 
    .cols = all_of(posttr_vars), 
    .fns = \(x) sd(x), 
    .names = '{.col}'
  )) %>%
  mutate(Statistic = 'SD')

posttr_att <- cv_and_att_summary %>%
  filter(percentage_type == 'All' & !is.na(tau_avg) ) %>% 
  select(all_of(posttr_vars)) %>%
  mutate(Statistic = 'Avg')

atts <- bind_rows(posttr_att, posttr_sum_boot_data) %>%
  mutate(Measure = 'ATT', 
         Geography = model_geography)
# -------------------------------------------------------------------------------------------- #
atts_long <- atts %>% 
  pivot_longer(
    cols = actual_avg:pct_att,
    names_to = 'variable', 
    values_to = 'estiamte'
  ) %>%
  mutate(variable = factor(variable, 
                           levels = c('actual_avg', 
                                      'preds_avg', 
                                      'tau_avg', 
                                      'pct_att'))) %>%
  arrange(variable, Statistic)
# -------------------------------------------------------------------------------------------- #
# Combine the long-format CV errors and ATTs. 
# -------------------------------------------------------------------------------------------- #
cv_and_att_summary_long <- bind_rows(cv_errors_long, atts_long)

filename <- paste0(str_to_lower(model_geography), '_', 'average_cv_error_and_att_relative_time.rds')
saveRDS(cv_and_att_summary_long, 
        file = here::here('Analysis', 'Tables', 'Low_Access', model_geography, filename) )
# -------------------------------------------------------------------------------------------- #
