model_dep_var = 'low_access'; model_geography = 'Urban'
options(scipen=999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters specified in analysis_plot_effects_and_errors_on_covars 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
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
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.g., Low Access
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
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
model_preds <- bind_rows(untreated_preds, treated_preds) 
# -------------------------------------------------------------------------------------------- #
# USDA Low-Income
# Tract poverty rate is 20 percent or greater; or
# Tract median family income is less than or equal to 80 percent of the State-wide median family income; or
# Tract in a metropolitan area and has a median family income less than or equal to 80 percent of the metropolitan area's median family income.
# -------------------------------------------------------------------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))

bg_pop_centroids_10_sfp_geo <- st_drop_geometry(bg_pop_centroids_10_sfp_geo) %>% 
  select(GEOID, STATE, market_name)

low_income_ids <- model_preds %>% 
  select(GEOID, year, poverty_rate, inc_per_capita) %>% 
  left_join(bg_pop_centroids_10_sfp_geo, by = 'GEOID') %>%
  select(GEOID, STATE, market_name, year, poverty_rate, inc_per_capita)

low_income_ids <- low_income_ids %>% 
  mutate(poverty_rate_flag = if_else(poverty_rate >= 0.20, 1, 0))

pctl_income_by_state <- low_income_ids %>%
  group_by(STATE, year) %>%
  reframe(across(.cols = inc_per_capita, 
                 .fns = \(x) quantile(x, probs = 0.80), 
                 .names = '{.col}_80_state'))

pctl_income_by_mkt <- low_income_ids %>%
  group_by(market_name, year) %>%
  reframe(across(.cols = inc_per_capita, 
                 .fns = \(x) quantile(x, probs = 0.80), 
                 .names = '{.col}_80_market'))

low_income_ids <- low_income_ids %>% 
  left_join(select(pctl_income_by_state, STATE, year, matches('_80')), by = c('STATE', 'year')) %>%
  left_join(select(pctl_income_by_mkt, market_name, year, matches('_80')), by = c('market_name', 'year'))


low_income_ids <- low_income_ids %>% 
  mutate(low_income_flag = case_when(poverty_rate_flag == 1 | 
                                       inc_per_capita <= inc_per_capita_80_state | 
                                       inc_per_capita <= inc_per_capita_80_market ~ 1, 
                                     TRUE ~ 0))

model_preds <- model_preds %>%
  left_join(select(low_income_ids, GEOID, year, low_income_flag), by = c('GEOID', 'year') ) %>%
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
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
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

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
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

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
  select(all_of(posttr_key_vars), actual, preds, tau, low_income_flag, inc_per_capita, poverty_rate, pop_black) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) # Joins binned dollar store and grocery store variables. 


# Select relevant covariates for analyses. 

sel_covars <- c('low_income_flag', 'inc_per_capita', 'poverty_rate', 'pop_black')
sel_covars_bins <- c('poverty_rate_bins', 'pop_black_bins', 'vacant_housing_bins', 'public_assistance_bins',
                     'no_vehicle_bins', 'educ_bins', 'pop_white_bins', 'pop_hispanic_bins')
sel_dsvars_bins <- c('gross_entry_cumsum_bins', 'entry_events_bins', 'net_entry_cumsum_bins')
sel_grocery_bins <- c('Grocery_Count_10mile_2005_bins')

# Prepare data. 
# Note: While posttre_effects_wdsentry originates with bootstrap output, posttr_binned_covars is created with 
# original data, allowing for smooth joins.

posttre_effects_winteracts <- posttre_effects_wdsentry %>% # dta
  
  select(all_of(posttr_key_vars), actual, preds, tau, 
         all_of(sel_covars), all_of(sel_dsvars_bins), all_of(sel_grocery_bins)) %>%
  
  left_join(select(posttr_binned_covars, all_of(posttr_key_vars), all_of(sel_covars_bins)), 
            by =  posttr_key_vars)

# -------------------------------------------------------------------------------------------- #
sum_stats <- posttre_effects_winteracts %>%
  group_by(low_income_flag) %>%
  summarise(across(.cols = c(actual, preds, tau), 
                   .fns = c('avg' = mean, 
                            'total' = sum), 
                   .names = '{.col}_{.fn}')) %>%
  mutate(pct_att = 100*(tau_avg/preds_avg) ) 

total_per_group <- posttre_effects_winteracts %>%
  group_by(low_income_flag) %>% 
  count(name = 'total_per_group') %>%
  ungroup() %>%
  mutate(total_obs = sum(total_per_group), 
         share_per_group = total_per_group/total_obs)

sum_stats <- sum_stats %>% left_join(total_per_group, by = 'low_income_flag')

sum_stats <- sum_stats %>% select(low_income_flag, matches('_avg$'), pct_att, share_per_group, matches('total'))

sum_stats <- sum_stats %>% 
  pivot_longer(cols = c(actual_avg:total_obs), 
               names_to = 'stat',
               values_to = 'values') %>%
  pivot_wider(names_from = 'low_income_flag', 
              values_from = 'values')


sum_stats$stat_tidy <- sum_stats$stat %>% 
  str_replace_all(c('actual_avg' = 'Low Access (Actual)', 
                    'preds_avg' = 'Low Access (Pred.)', 
                    'tau_avg' = 'ATT', 
                    'pct_att' = '% Change ATT', 
                    'actual_total' = 'Total Low Access (Actual)', 
                    'preds_total' = 'Total Low Access (Pred.)',
                    'tau_total' = 'Total ATT', 
                    'share_per_group' = 'Share of Obs.',
                    'total_per_group' = 'Obs. per Group') )

sum_stats <- sum_stats %>% filter(stat != 'total_obs' & stat != 'total_per_group') 

sum_stats_tab <- sum_stats %>%
  select(-stat) %>%
  relocate(stat_tidy)

colnames(sum_stats_tab) <- c('Variable', 'Not Low Income', 'Low Income')

sum_stats_tab <- sum_stats_tab %>% mutate(across(.cols = where(is.numeric), \(x) round(x, digits = 3)))
library(knitr)
library(kableExtra)

# Generate the LaTeX table with detailed notes
latex_table <- kable(sum_stats_tab, "latex", booktabs = TRUE, 
                     caption = "Average Treatment Effects by Low-Income Status") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  footnote(general = c("\"Low Access (Actual)\" and \"Low Access (Pred.)\" indicate the actual and predicted shares of low access block groups among those affected by dollar store entry for low-income and not low-income block groups.",
                       "\"Share of Obs.\" denotes the proportion of the total dataset that each group's observations constitute."),
           general_title = "", threeparttable = TRUE, footnote_as_chunk = TRUE)

# Print the LaTeX code
print(latex_table)
# -------------------------------------------------------------------------------------------- #
