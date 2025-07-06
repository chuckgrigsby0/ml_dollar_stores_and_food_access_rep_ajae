# Script creates Figures E.3 and E.4 in supplementary analyses including superettes in low-access definition. 
# Results assess average treatment effects across dollar store entries and baseline grocery store-superette counts. 
# Used in conjunction with analysis_plot_effects_on_grocery_and_superettes_sourced.R and sbatch_figures.sh. 
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
print(model_dep_var)
print(model_geography)
bootstrap_by_tracts = '_tracts'
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
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
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
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
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store and Superette bins. 
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

# tau and dollar store entry/counts w/ grocery stores and superettes from 2005. 

posttre_effects_wdsentry <- posttre_effects %>%
  select(all_of(posttr_key_vars), actual, preds, tau) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) %>% # Joins binned dollar store and grocery store variables. 
  left_join(select(st_drop_geometry(bg_pop_centroids_10_sfp_geo), GEOID, STATE, COUNTY, market_name), by = 'GEOID')


# Select relevant covariates for analyses. 
sel_dsvars_bins <- c('gross_entry_cumsum_bins', 'entry_events_bins', 'net_entry_cumsum_bins')
sel_grocery_bins <- c('Grocery_Count_10mile_2005_bins', 
                      'Superette_Count_10mile_2005_bins', 
                      'Grocery_and_Superette_Count_10mile_2005_bins')
geog_vars <- c('GEOID', 'STATE', 'COUNTY', 'market_name')

# Prepare data. 

posttre_effects_winteracts <- posttre_effects_wdsentry %>% # dta
  
  select(all_of(posttr_key_vars), all_of(geog_vars),
         actual, preds, tau, 
         all_of(sel_dsvars_bins), all_of(sel_grocery_bins)) 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_effects_on_ds_entry_x_grocery_and_superettes.R') )
# -------------------------------------------------------------------------------------------- #

ds_entry_vars <- c("gross_entry_cumsum_bins",
                   "entry_events_bins",                    
                   "net_entry_cumsum_bins")

bootstrap_id = 0
# -------------------------------------------------------------------------------------------- #
sum_stats <- map(ds_entry_vars, function(.x){
  
  ds_entry_by_num_grocery_and_superette_2005(dta = posttre_effects_winteracts, 
                                             ds_entry_var = .x, 
                                             grocery_store_var = 'Grocery_Count_10mile_2005_bins', 
                                             superette_var = 'Superette_Count_10mile_2005_bins', 
                                             iter_id = bootstrap_id)
})
# -------------------------------------------------------------------------------------------- #
sum_stats <- set_names(sum_stats, nm = ds_entry_vars)  

sum_stats <- map2(sum_stats, ds_entry_vars, function(.x, .y){ 
  
  .x %>% 
    
    rename_with(.fn = \(x) str_replace_all(x, pattern = .y, 'ds_entry_bins') ) 
  
}) %>%
  
  bind_rows(.id = 'ds_entry')
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
emp_reg_coefs <- map_dfr(ds_entry_vars, function(.x){
  
  effects_by_ds_entry_x_grocery_x_superette(dta = posttre_effects_winteracts, 
                                            ds_entry_var = .x, 
                                            grocery_store_var = 'Grocery_Count_10mile_2005_bins', 
                                            superette_var = 'Superette_Count_10mile_2005_bins', 
                                            iter_id = bootstrap_id)
})
# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_effects_on_grocery_x_superettes') # NULL or '_tracts'

boot_data <- seq(1, 499, 1) %>%
  
  map_dfr(function(.iter){ 
    
    filename <- paste0('bootstrap_',
                       'summary_stats_grocery_x_superettes_',
                       .iter, '.rds')
    
    readRDS(here::here('Analysis_Supplementary_w_Superettes',
                       dir_geography,
                       dir_dep_var, 
                       dir_bootstrap, 
                       filename))    
    
  })
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Functions to compute standard errors, and join standard errors to empirical point estimates. 
# -------------------------------------------------------------------------------------------- #
# Simple function to compute SEs by some grouping variables. 
source(here::here('Code', 'Functions', 'Function_bootstrap_compute_standard_errors.R')) 
# -------------------------------------------------------------------------------------------- #
boot_data <- bind_rows(emp_reg_coefs, boot_data)
group_join_vars <- c('ds_entry', 'ds_entry_bins', 'grocery_stores', 'superettes', 
                     'superette_count_var', 'grocery_count_var')
boot_data_sd <- compute_bootstrap_standard_errors(dta = boot_data, group_vars = group_join_vars)
emp_reg_coefs <- left_join(emp_reg_coefs, boot_data_sd, by = group_join_vars)
emp_reg_coefs <- emp_reg_coefs %>% relocate(c(bootstrap_mean, bootstrap_sd), .after = estimate)
# -------------------------------------------------------------------------------------------- #
emp_reg_coefs <- emp_reg_coefs %>% 
  mutate(tidy_ds_entry = tidy_covar_names(ds_entry) ) %>% 
  relocate(tidy_ds_entry, .before = ds_entry)

ds_entry_vars <- unique(emp_reg_coefs$ds_entry); ds_entry_vars
tidy_ds_entry_vars <- unique(emp_reg_coefs$tidy_ds_entry)
tidy_ds_entry_vars <- str_remove_all(tidy_ds_entry_vars, pattern = ' Bins')
tidy_ds_entry_vars <- tidy_ds_entry_vars %>% str_replace_all(pattern = 'Entry$', replacement = 'Entry Events')

one_store_pairs_vec <- c(TRUE, FALSE)
pair_set = c('first', 'second')
# -------------------------------------------------------------------------------------------- #
# Loads functions for plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #
map2(ds_entry_vars, tidy_ds_entry_vars, function(.x, .y){
  
  map2(one_store_pairs_vec, pair_set, function(.z, .w){

plot_effects_on_ds_entry_grocery_and_superettes(dta = emp_reg_coefs, 
                                                filter_ds_entry_type = .x, 
                                                ci_label_str = '99% CI', 
                                                ci_level = qnorm(1 - (0.01/2)),
                                                one_store_pairs = .z,
                                                y_axis_title = 'Average Treatment Effects', 
                                                x_axis_title = .y, 
                                                legend_title_str = 'Pre-Entry Stores (2005)', 
                                                title_str = '', 
                                                subtitle_str = '', 
                                                decimal_place_y = 0.01, 
                                                nbreaks = 14)

figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_',
                  .x, '_x_', 'grocery_superette_pairs', bootstrap_by_tracts, '_', .w, '.pdf')

fig_dir_name <- paste0('effects_on_ds_entry_grocery_superettes', bootstrap_by_tracts)

ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography,
                  fig_dir_name, # Saves to directories and subdirectories.
                  figname), 
       width = 8, height = 6, unit = 'in', dpi = 600)

})
  
})
# -------------------------------------------------------------------------------------------- #