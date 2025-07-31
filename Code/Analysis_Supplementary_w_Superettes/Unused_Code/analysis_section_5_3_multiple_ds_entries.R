model_dep_var = 'low_access'
model_geography = 'Rural'# Change for Urban/Rural area results. 
options(scipen=999)
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
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
   
  filter(year >= '2006') %>%
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
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
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)


# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store and superette bins. 
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
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))

posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

# tau and dollar store entry/counts w/ grocery stores from 2005. 

posttre_effects_wdsentry <- posttre_effects %>%
  select(all_of(posttr_key_vars), actual, preds, tau, inc_per_capita, poverty_rate, pop_black) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) %>% # Joins binned dollar store and grocery store variables. 
  left_join(select(st_drop_geometry(bg_pop_centroids_10_sfp_geo), GEOID, STATE, COUNTY, market_name), by = 'GEOID')


# Select relevant covariates for analyses. 

sel_covars <- c('inc_per_capita', 'poverty_rate', 'pop_black')
sel_covars_bins <- c('inc_per_capita_bins', 'poverty_rate_bins', 'pop_black_bins', 'pop_white_bins')
sel_dsvars_bins <- c('gross_entry_cumsum_bins', 'entry_events_bins', 'net_entry_cumsum_bins')
sel_grocery_bins <- c('Grocery_Count_10mile_2005_bins', 
                      'Superette_Count_10mile_2005_bins', 
                      'Grocery_and_Superette_Count_10mile_2005_bins')
geog_vars <- c('GEOID', 'STATE', 'COUNTY', 'market_name')

# Prepare data. 

posttre_effects_winteracts <- posttre_effects_wdsentry %>% 
  
  select(all_of(posttr_key_vars), all_of(geog_vars),
         actual, preds, tau, 
         all_of(sel_covars), all_of(sel_dsvars_bins), all_of(sel_grocery_bins)) %>%
  
  left_join(select(posttr_binned_covars, all_of(posttr_key_vars), all_of(sel_covars_bins)), 
            by =  posttr_key_vars)
# -------------------------------------------------------------------------------------------- #

# Summary statistics for predicted, actual, and average treatment effects by year 2005 grocery stores and superettes and entries. 

# -------------------------------------------------------------------------------------------- #

ds_entry_by_num_grocery_2005 <- function(dta, ds_entry_var, grocery_store_var){
  
  sum_stats <- dta %>% 
    group_by(.data[[ds_entry_var]], .data[[grocery_store_var]]) %>%
    summarise(across(.cols = c(actual, preds, tau), 
                     .fns = c('avg' = mean, 
                              'total' = sum), 
                     .names = '{.col}_{.fn}')) %>%
    mutate(pct_att = 100*(tau_avg/preds_avg) ) %>%
    select(all_of(ds_entry_var), all_of(grocery_store_var), matches('*_avg$'), pct_att, matches('*_total$'))
  
  total_per_group <- dta %>% 
    group_by(.data[[grocery_store_var]], .data[[ds_entry_var]]) %>%
    count(name = 'total_per_group')
  
  total_bg_per_group <- dta %>% 
    group_by(.data[[grocery_store_var]], .data[[ds_entry_var]]) %>%
    distinct(GEOID) %>%
    count(name = 'total_bg_per_group')
  
  sum_stats <- sum_stats %>% 
    left_join(total_per_group, by = c({{grocery_store_var}}, ds_entry_var)) %>%
    left_join(total_bg_per_group, by = c({{grocery_store_var}}, ds_entry_var)) %>%
    ungroup() %>%
    mutate(total_treated = sum(total_per_group), 
           share_per_group = total_per_group/total_treated)
  
  if (ds_entry_var == 'gross_entry_cumsum_bins'){
    sum_stats <- sum_stats %>% 
      filter(gross_entry_cumsum_bins != 0) 
  }
  
  sum_stats <- sum_stats %>%
    filter(.data[[grocery_store_var]] %in% seq(0, 4, 1))
  
  current_levels <- levels(sum_stats[[ds_entry_var]])
  
  new_levels <- current_levels %>% str_replace_all("\\((\\d+),\\s*\\d+\\]", "\u003e \\1") %>% 
    str_replace_all("^\\[\\-\\d+,\\s*(\\-*\\d+)\\]", "\u2264 \\1") # Replaces the bins with negative numbers
  
  
  levels(sum_stats[[ds_entry_var]]) <- new_levels
  
  
  sum_stats_long <- sum_stats %>% 
    pivot_longer(cols = actual_avg:share_per_group, 
                 names_to = 'stat', 
                 values_to = 'values') %>%
    mutate(values = round(values, digits = 3))
  
  sum_stats_wide <- sum_stats_long %>%
    pivot_wider(names_from = all_of(grocery_store_var), 
                values_from = 'values')
  
  sum_stats_wide <- sum_stats_wide %>% filter(stat != 'total_treated')
  
  sum_stats_wide$stat_tidy <- sum_stats_wide$stat %>% str_replace_all(c('actual_avg' = 'Low Access (Actual)', 
                                                                        'preds_avg' = 'Low Access (Pred.)', 
                                                                        'tau_avg' = 'ATT', 
                                                                        'pct_att' = '% Change ATT', 
                                                                        'actual_total' = 'Total Low Access (Actual)', 
                                                                        'preds_total' = 'Total Low Access (Pred.)',
                                                                        'tau_total' = 'Total ATT', 
                                                                        'total_per_group' = 'Obs. per Group',
                                                                        'total_bg_per_group' = 'Unique BGs per Group',
                                                                        'share_per_group' = 'Share of Obs.') )
  
  sum_stats_wide <- sum_stats_wide %>% relocate(stat_tidy, .before = stat) %>% relocate(stat, .after = last_col())
}
# -------------------------------------------------------- #
grocery_var_counts <- names(posttre_effects_winteracts) %>% str_subset('Count_.*_2005')
grocery_var_counts_labels <- grocery_var_counts %>% str_replace_all('_Count.*', '') %>% str_to_lower(); grocery_var_counts_labels
# -------------------------------------------------------- #
# Map across grocery store types (grocery, grocery and superette, and superette)
# -------------------------------------------------------- #
map2(grocery_var_counts, 
     grocery_var_counts_labels, 
     function(.x, .y){

sum_stats_gross_entry_by_grocery <- ds_entry_by_num_grocery_2005(dta = posttre_effects_winteracts, 
                                                                 ds_entry_var = 'gross_entry_cumsum_bins', 
                                                                 grocery_store_var = .x)
readr::write_csv(sum_stats_gross_entry_by_grocery, 
                 here::here('Analysis_Supplementary_w_Superettes', 
                            'Tables', 
                            'Low_Access', 
                            model_geography, 
                            paste0('sum_stats_gross_entry_by_', .y ,'.csv') ) )
# -------------------------------------------------------- #
sum_stats_entry_events_by_grocery <- ds_entry_by_num_grocery_2005(dta = posttre_effects_winteracts, 
                                                                  ds_entry_var = 'entry_events_bins', 
                                                                  grocery_store_var = .x)
readr::write_csv(sum_stats_entry_events_by_grocery, 
                 here::here('Analysis_Supplementary_w_Superettes', 
                            'Tables', 
                            'Low_Access', 
                            model_geography, 
                            paste0('sum_stats_entry_events_by_', .y ,'.csv') ) )
# -------------------------------------------------------- #
sum_stats_net_entry_by_grocery <- ds_entry_by_num_grocery_2005(dta = posttre_effects_winteracts, 
                                                               ds_entry_var = 'net_entry_cumsum_bins', 
                                                               grocery_store_var = .x)
readr::write_csv(sum_stats_net_entry_by_grocery, 
                 here::here('Analysis_Supplementary_w_Superettes', 
                            'Tables', 
                            'Low_Access', 
                            model_geography, 
                            paste0('sum_stats_net_entry_by_', .y ,'.csv') ) )

})
# -------------------------------------------------------- #
