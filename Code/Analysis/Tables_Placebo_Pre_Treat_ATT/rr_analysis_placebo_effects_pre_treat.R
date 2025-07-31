# Script to obtain the pre-treatment "ATTs" by relative time to treatment, or the pre-treatment CV Errors. 
# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
library('kableExtra')
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
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
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') # Counterfactual predictions are made from 2006-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_load_bootstrap_errors_and_predictions.R'))
# -------------------------------------------------------------------------------------------- #
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
boot_data_rel_year_atts <- seq(1, 499, 1) %>%
  
  map(function(.iter){ 
    
    load_bootstrap_errors_and_predictions_array(model_geography_str = model_geography, 
                                                model_dep_var_str = model_dep_var, 
                                                bootstrap_by_tracts = '_tracts', bootstrap_iter = .iter)
    
  })


boot_data_atts <- seq(1, 499, 1) %>% 
  
  map(function(.iter){ 
    
    readRDS(here::here('Analysis', 
                       paste0(model_geography, '_', 'Bootstrap'), 
                       str_replace(str_to_title(str_replace(model_dep_var, '_', ' ')), ' ', '_' ), 
                       paste0('bootstrap_placebo_att', bootstrap_by_tracts), 
                       paste0('bootstrap_placebo_att_', 
                              str_to_lower(model_geography), 
                              '_',
                              model_dep_var, 
                              bootstrap_by_tracts, 
                              '_', 
                              .iter, 
                              '.rds')) )
    
  })
# -------------------------------------------------------------------------------------------- #
# Pretreatment errors against relative time. 
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
# Estimate the placebo ATT for entire pre-treatment period. 
# -------------------------------------------------------------------------------------------- #
emp_placebo_att <- model_preds %>% 
  filter(rel_year < 0) %>%
  summarise(estimate = mean(actual - preds, na.rm=TRUE) ) %>%
  mutate(id = 0)
# -------------------------------------------------------------------------------------------- #

# Relative time placebo ATTs

# Extract the element containing bootstrapped estimates from regressing errors on rel_year. 
boot_data_errors <- subset_errors_and_predictions(bootstrap_data = boot_data_rel_year_atts, 
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

emp_errors_on_relyear <- emp_errors_on_relyear %>% 
  select(id, rel_year, estimate, bootstrap_sd)
# -------------------------------------------------------------------------------------------- #

# Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
boot_data_atts <- bind_rows(emp_placebo_att, bind_rows(boot_data_atts))

# Compute standard error. 
boot_se_atts <- boot_data_atts %>% 
  summarise(across(.cols = estimate, 
                   .fns = c('sd' = sd, 
                            'avg' = mean), 
                   .names = 'bootstrap_{.fn}') ) 


emp_placebo_att <- bind_cols(emp_placebo_att, boot_se_atts) %>% 
  select(id, estimate, matches('sd$'))

# -------------------------------------------------------------------------------------------- #
# Add empirical estimates to bootstrapped estimates to include in computation of standard errors. 
# -------------------------------------------------------------------------------------------- #
dta_placebo_atts <- bind_rows(emp_placebo_att, emp_errors_on_relyear)

dta_placebo_atts <- dta_placebo_atts %>% 
  select(-c(id))
# -------------------------------------------------------------------------------------------- #
# Create a dataframe containing indicators for statistical significance. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_check_significance.R'))

check_significance_df <- seq(1, nrow(dta_placebo_atts), 1) %>% 
  
  map_dfr(function(.x){
    
    setNames(map(seq_along(alpha_levels), 
                 function(.y){
                   check_significance(dta_placebo_atts$estimate[.x], 
                                      dta_placebo_atts$bootstrap_sd[.x], 
                                      z_scores[.y]) }), 
             paste0('significant_', alpha_names) ) %>% 
      bind_cols() %>% 
      mutate(significance = case_when(
        significant_one == TRUE ~ "***",
        significant_five == TRUE ~ "**",
        significant_ten == TRUE ~ "*",
        TRUE ~ "") 
      ) %>% 
      select(-significant_ten, -significant_five, -significant_one)
  })
# -------------------------------------------------------------------------------------------- #

# Combine the significance indicators to the data frame containing the effects and SEs. 

# -------------------------------------------------------------------------------------------- #
dta_placebo_atts <- bind_cols(dta_placebo_atts, check_significance_df)

dta_placebo_atts <- dta_placebo_atts %>% 
  arrange(desc(rel_year)) %>%
  mutate(across(.cols = c(estimate, bootstrap_sd), 
                .fns = \(x) round(x, digits = 4))) %>%
  mutate(across(.cols = c(rel_year, estimate, bootstrap_sd), 
                .fns = \(x) as.character(x))) %>%
  mutate(bootstrap_sd = paste0('(', bootstrap_sd, ')', significance)) %>%
  select(-significance) %>% 
  mutate(rel_year = if_else(is.na(rel_year), 'ATT', rel_year)) %>%
  relocate(rel_year)

# Rearrange the ATTs so that the ATT is on the first row. 
placebo_att_only <- dta_placebo_atts %>% filter(rel_year == 'ATT')
placebo_wo_att <- dta_placebo_atts %>% filter(rel_year != 'ATT')
dta_placebo_atts <- bind_rows(placebo_att_only, placebo_wo_att)


dta_placebo_atts <- bind_cols(dta_placebo_atts, data.frame('Estimate' = rep('', nrow(dta_placebo_atts)), 
                                                           'SD' = rep('', nrow(dta_placebo_atts))))

names(dta_placebo_atts) <- str_replace_all(names(dta_placebo_atts), 
                                           c('rel_year' = 'Time-to-Treatment', 
                                             'estimate' = 'Estimate', 
                                             'bootstrap_sd' = 'SD'))
# -------------------------------------------------------------------------------------------- #
# Create the LaTeX table
latex_table <- kable(dta_placebo_atts, 
                     format = "latex", 
                     align = 'ccc', 
                     booktabs = TRUE,
                     escape = TRUE, # Note: This is the default.
                     position = '!h',
                     linesep = '', 
                     centering = TRUE, # Note: This is the default. 
                     col.names = names(dta_placebo_atts),
                     caption = paste0("Pre-Treatment Placebo ATTs for Linear and ML Models ", 
                                      '(', model_geography, ' Areas', ')') ) %>%
  footnote(general = c(paste0('Table xxx shows the estimated placebo ATTs for XGBoost models in ',
                              model_geography, ' Areas.', 
                              ' Bootstrapped standard errors are in parentheses.', 
                              ' *$p<0.10$, **$p<0.05$, ***$p<0.01$') ), 
           general_title = 'Notes: ', 
           threeparttable = TRUE,
           escape = FALSE) %>%
  add_header_above(c('', 'ML' = 2, 'Linear' = 2) ); latex_table

