model_dep_var = 'low_access'; model_geography = 'Urban'
options(scipen=999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters specified in analysis_plot_effects_and_errors_on_covars 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Filter out urban_area and uc_area from the Rural models. 
# -------------------------------------------------------------------------------------------- #
# if (model_geography == 'Rural'){ 
#   model_covars <- model_covars[!grepl('^urban_area$|^uc_area$', model_covars)]
# }
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
  
  # For consistency with the out-of-sample predictions during CV, 
  # we obtain counterfactual predictions from 2007 to 2020.
  
  filter(year >= '2006') %>%
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #
# See file 'data_preparation_prepare_binned_and_factor_covars.R' and associated Functions called in script. 

# Note: These data.frames are from the original data so they must be joined 
# to the bootrapped data for the current bootstrap iteration. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment bins (quartiles) 
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
# Prepare data for pre-treatment analyses and post-treatment analyses. 
# Note: data preparation is already completed for pre-treatment data. See pretr_binned_covars. 
# -------------------------------------------------------------------------------------------- #
posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')
# tau and dollar store entry/counts w/ grocery stores from 2005. 

posttre_effects_wdsentry <- posttre_effects %>%
  select(all_of(posttr_key_vars), actual, preds, tau, inc_per_capita, poverty_rate, pop_black) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) # Joins binned dollar store and grocery store variables. 


# Select relevant covariates for analyses. 

sel_covars <- c('inc_per_capita', 'poverty_rate', 'pop_black')
sel_covars_bins <- c('poverty_rate_bins', 'pop_black_bins', 'vacant_housing_bins', 'public_assistance_bins',
                     'no_vehicle_bins', 'educ_bins', 'pop_white_bins', 'pop_hispanic_bins')
sel_dsvars_bins <- c('gross_entry_cumsum_bins', 'entry_events_bins', 'net_entry_cumsum_bins')
sel_grocery_bins <- c('Grocery_Count_10mile_2005_bins', 
                      'Superette_Count_10mile_2005_bins', 
                      'Grocery_and_Superette_Count_10mile_2005_bins')

# Prepare data. 
# Note: While posttre_effects_wdsentry originates with bootstrap output, posttr_binned_covars is created with 
# original data, allowing for smooth joins.

posttre_effects_winteracts <- posttre_effects_wdsentry %>% # dta
  
  select(all_of(posttr_key_vars), actual, preds, tau, 
         all_of(sel_covars), all_of(sel_dsvars_bins), all_of(sel_grocery_bins)) %>%
  
  left_join(select(posttr_binned_covars, all_of(posttr_key_vars), all_of(sel_covars_bins)), 
            by =  posttr_key_vars)

# -------------------------------------------------------------------------------------------- #
# Summary statistics by decile for most significant socio-demographic variables w.r.t. one SD increase and SD. 
# -------------------------------------------------------------------------------------------- #

summary_stats_by_covar <- function(dta, covar_bins){


  sum_by_covar <- dta %>% 
    group_by(.data[[covar_bins]]) %>%
    summarise(across(.cols = c(actual, preds, tau), 
                     .fns = list('avg' = \(x) mean(x)), 
                     .names = '{.col}_{.fn}')) %>%
    mutate(pct_att = 100*tau_avg/preds_avg)
  
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


sum_stats_list <- sel_covars_bins %>% 
  map(function(.x){
    summary_stats_by_covar(dta = posttre_effects_winteracts, covar_bins = .x)
  })

sum_stats_list <- set_names(sum_stats_list, nm = sel_covars_bins)

sum_stats_df <- sum_stats_list %>% bind_rows() %>% mutate(across(.cols = where(is.numeric), .fns = \(x) round(x, digits =3)))
