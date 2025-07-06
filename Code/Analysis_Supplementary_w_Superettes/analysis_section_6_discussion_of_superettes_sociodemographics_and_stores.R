model_dep_var = 'low_access'
model_geography = 'Urban' # Change for Urban/Rural results. 
options(scipen=999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters specified above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final_w_superettes', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.g., Low Access
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  left_join(select(dta_untreated_wfolds, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(dta_treated, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #
# See file 'data_preparation_prepare_binned_and_factor_covars.R' and associated Functions called in script. 
# -------------------------------------------------------------------------------------------- #
# Post-treatment bins (quartiles) 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_quartile_covars_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_covars <- posttr_binned_covars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_w_superettes_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_covars <- posttr_binned_covars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_and_superette_', str_to_lower(model_geography), '.rds')

posttr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_grocery))
# -------------------------------------------------------------------------------------------- #
posttre_effects <- model_preds %>% 
  
  filter(rel_year >= 0) %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  mutate(rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #
# Prepare data for post-treatment analyses. 
# -------------------------------------------------------------------------------------------- #
posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

posttre_effects_wdsentry <- posttre_effects %>%
  select(all_of(posttr_key_vars), actual, preds, tau, inc_per_capita, poverty_rate, pop_black) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) # Joins binned dollar store and grocery store variables. 


# Select relevant covariates for analyses. 

sel_covars <- c('inc_per_capita', 'poverty_rate', 'pop_black')
sel_covars_bins <- c('poverty_rate_bins', 'pop_black_bins', 'inc_per_capita_bins', 'vacant_housing_bins', 'public_assistance_bins',
                     'no_vehicle_bins', 'educ_bins', 'pop_white_bins', 'pop_hispanic_bins')
sel_dsvars_bins <- c('gross_entry_cumsum_bins', 'entry_events_bins', 'net_entry_cumsum_bins')

# Prepare data. 

posttre_effects_winteracts <- posttre_effects_wdsentry %>% # dta
  
  select(all_of(posttr_key_vars), actual, preds, tau, 
         all_of(sel_covars), all_of(sel_dsvars_bins), matches('^Grocery_.*|^Superette_.*')) %>%
  
  left_join(select(posttr_binned_covars, all_of(posttr_key_vars), all_of(sel_covars_bins)), 
            by =  posttr_key_vars)
# -------------------------------------------------------------------------------------------- #
# Summary statistics by decile for most significant socio-demographic variables w.r.t. one SD increase and SD. 
# -------------------------------------------------------------------------------------------- #

summary_stats_by_covar <- function(dta, covar_bins){
  
  
  sum_by_covar <- dta %>% 
    group_by(.data[[covar_bins]]) %>%
    summarise(across(.cols = c(actual, preds, tau, 
                               Superette_Count_10mile_2005, Grocery_Count_10mile_2005), 
                     .fns = list('avg' = \(x) mean(x)), 
                     .names = '{.col}_{.fn}')) %>%
    mutate(pct_att = 100*tau_avg/preds_avg) %>% 
    relocate(pct_att, .after = tau_avg)
  
  total_by_covar_group <- dta %>% 
    group_by(.data[[covar_bins]]) %>%
    count(name = 'total_per_group') %>%
    ungroup() %>%
    mutate(total_obs = sum(total_per_group), 
           share_per_group = total_per_group/total_obs)
  
  sum_by_covar <- sum_by_covar %>% left_join(total_by_covar_group, by = covar_bins)
  
  sum_by_covar <- sum_by_covar %>% select(-c(total_per_group, total_obs) )
  
  sum_by_covar$percentile <- paste0('Bin_', seq(1, nrow(sum_by_covar), 1))
  
  sum_by_covar <- sum_by_covar %>% select(-all_of(covar_bins))
  
  sum_by_covar <- sum_by_covar %>%
    pivot_longer(cols = c(actual_avg:share_per_group), 
                 names_to = 'stat', 
                 values_to = 'values') %>%
    pivot_wider(names_from = 'percentile', 
                values_from = 'values' )
  
  sum_by_covar <- sum_by_covar %>% mutate(covar = covar_bins) %>% relocate(covar, .before = 1 )
  
  return(sum_by_covar)
  
}
# -------------------------------------------------------------------------------------------- #
sum_stats_list <- sel_covars_bins %>% 
  map(function(.x){
    summary_stats_by_covar(dta = posttre_effects_winteracts, covar_bins = .x)
  })

sum_stats_list <- set_names(sum_stats_list, nm = sel_covars_bins)

sum_stats_df <- sum_stats_list %>% bind_rows() %>% mutate(across(.cols = where(is.numeric), .fns = \(x) round(x, digits = 4)))
# -------------------------------------------------------------------------------------------- #
# Create a data frame of tidy variable names. 
# -------------------------------------------------------------------------------------------- #
model_covar_df_vars_tidy <- tibble::enframe(model_covar_names, name = "category", value = "varname_tidy") %>% 
  unnest(varname_tidy)

model_covar_df_vars <- tibble::enframe(model_covars_list, name = "category", value = "varname") %>% 
  unnest(varname) %>% select(-category)

model_covar_df <- bind_cols(model_covar_df_vars_tidy, model_covar_df_vars)
# -------------------------------------------------------------------------------------------- #
# Join tidy variables to the table
# -------------------------------------------------------------------------------------------- #
sum_stats_df$covar <- sum_stats_df$covar %>% str_replace_all(pattern = '_bins', replacement = '')

sum_stats_df <- sum_stats_df %>% 
  left_join(model_covar_df, by = c('covar' = 'varname')) %>% 
  relocate(c(category, varname_tidy), .after = covar) %>%
  select(-covar, -category)
# -------------------------------------------------------------------------------------------- # 
# Clean stat column. 
# -------------------------------------------------------------------------------------------- #
sum_stats_df$stat_tidy <- sum_stats_df$stat %>% str_replace_all(c('actual_avg' = 'Low Access (Actual)', 
                                                                  'preds_avg' = 'Low Access (Pred.)', 
                                                                  'tau_avg' = 'ATT', 
                                                                  'pct_att' = '% Change ATT', 
                                                                  'actual_total' = 'Total Low Access (Actual)', 
                                                                  'preds_total' = 'Total Low Access (Pred.)',
                                                                  'tau_total' = 'Total ATT', 
                                                                  'total_per_group' = 'Obs. per Group',
                                                                  'total_bg_per_group' = 'Unique BGs per Group',
                                                                  'share_per_group' = 'Share of Obs.', 
                                                                  'Grocery_Count_10mile_2005_avg' = 'Grocery Stores (2005)', 
                                                                  'Superette_Count_10mile_2005_avg' = 'Superettes (2005)') )

sum_stats_df <- sum_stats_df %>% relocate(stat_tidy, .after = stat) %>% select(-stat)
# -------------------------------------------------------------------------------------------- #