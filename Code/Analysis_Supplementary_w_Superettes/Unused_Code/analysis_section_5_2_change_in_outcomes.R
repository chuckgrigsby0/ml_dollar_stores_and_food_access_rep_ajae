# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
library(readr)
model_dep_var = 'low_access'
model_geography = 'Urban' # Change for Urban/Rural results. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final_w_superettes', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.e., Low Access
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #

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
  
  filter(year >= '2006') # Post-treatment predictions are made for 2007-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #
# Post-treatment bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_w_superettes_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_covars <- posttr_binned_covars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
posttre_effects <- model_preds %>% 
  filter(rel_year >= 0) %>% 
  filter(grepl(model_geography, Geography)) %>%
  mutate(rel_year = factor(rel_year))

join_sel_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

posttre_effects <- posttre_effects %>% 
  
  left_join(select(posttr_binned_covars, all_of(join_sel_vars), 
                   poverty_rate_bins, pop_black_bins, 
                   vacant_housing_bins, public_assistance_bins, 
                   no_vehicle_bins), by = join_sel_vars, 
            multiple = 'all', relationship = 'many-to-one' )
# -------------------------------------------------------------------------------------------- #
# Percentage change in the ATT by relative time. 
# Using sums. 
# -------------------------------------------------------------------------------------------- #
outcome_pct_change_relyear <- posttre_effects %>% 
  
  group_by(rel_year) %>%
  
  summarise( across(.cols = c(tau, preds, actual), 
                    .fn = list(sum = sum), 
                    .names = '{.col}_{.fn}') ) 

# Total number of block groups. 
row_totals <- posttre_effects %>%
  group_by(rel_year) %>%
  count()

# Compute shares, which are identical to other tables, but contain totals. 
outcome_pct_change_relyear <- outcome_pct_change_relyear %>% left_join(row_totals, by = c('rel_year'))

outcome_pct_change_relyear <- outcome_pct_change_relyear %>% 
  mutate(pct_att = tau_sum/preds_sum, 
         actual_share = actual_sum/n, 
         pred_share = preds_sum/n)

# Information replicates  Section 5.2 in the main text using results from analyses including superettes in low-access definition. 
print(
  paste0(
    'By r = 1, the share of low access block groups increased by ',
    outcome_pct_change_relyear %>% filter(rel_year == 1) %>% 
      select(pct_att) %>% 
      mutate(pct_att = 100*round(pct_att, digits = 4 )), 
    '% relative to no entry' ) 
)

print(
  paste0(
    'By r = 4, the share of low access block groups increased by ',
    outcome_pct_change_relyear %>% filter(rel_year == 4) %>% 
      select(pct_att) %>% 
      mutate(pct_att = 100*round(pct_att, digits = 4 )), 
    '% relative to no entry' ) 
)

print(
  paste0(
    'By r = 12, the share of low access block groups increased by ',
outcome_pct_change_relyear %>% filter(rel_year == 12) %>% 
  select(pct_att) %>% 
  mutate(pct_att = 100*round(pct_att, digits = 3 )), 
 '% relative to no entry' ) 
)


# The counterfactual share of low access in these relative treatment years is: 
outcome_pct_change_relyear %>% 
  filter(rel_year == 1 | rel_year == 4 | rel_year == 12) %>% 
  select(pred_share) %>%
  mutate(across(.cols = pred_share, .fns = \(x) 100*round(x, digits = 4)))


# -------------------------------------------------------------------------------------------- #
# Percentage ATT by year. 
# -------------------------------------------------------------------------------------------- #
outcome_pct_change_year <- posttre_effects %>% 
  
  group_by(year) %>%
  
  summarise( across(.cols = c(tau, preds, actual), 
                    .fn = list(sum = sum), 
                    .names = '{.col}_{.fn}') ) 

row_totals <- posttre_effects %>%
  group_by(year) %>%
  count()


outcome_pct_change_year <- outcome_pct_change_year %>% left_join(row_totals, by = c('year'))

outcome_pct_change_year <- outcome_pct_change_year %>% 
  mutate(pct_att = tau_sum/preds_sum, 
         actual_share = actual_sum/n, 
         pred_share = preds_sum/n)

outcome_pct_change_year <- outcome_pct_change_year %>% ungroup() %>% arrange(desc(pct_att))
# -------------------------------------------------------------------------------------------- #

