# Script creates figures of average CV errors and treatment effects w.r.t baseline counts of retailer controls and multiple dollar store entries. 
# Script is used in conjunction with `analysis_plot_errors_and_effects_on_ds_entry_x_baseline_store_counts_sourced.R` and sbatch_figures.sh
# -------------------------------------------------------------------------------------------- #
print(model_dep_var)
print(model_geography)
bootstrap_by_tracts = '_tracts' 
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Pre-treatment observations, binned and factorized covariates. 
# -------------------------------------------------------------------------------------------- #
fname_pretr_binned_covars = paste0('pretreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

pretr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_covars))
# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, binned and factorized covariates. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_and_factor_covariates_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))
# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007') %>% # We obtain CV predictions from 2007-2020
  
  left_join(select(pretr_binned_covars, GEOID, year,
                   matches('Count_.*_2005')), 
            by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- untreated_preds %>%
  
  filter(is.finite(rel_year)) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, 
            by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(select(posttr_binned_covars, GEOID, year,
                   matches('Count_.*_2005')), 
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all') %>%
  left_join(select(posttr_binned_dsvars, GEOID, year, # Join the dollar store entry information to post-treatment data.  
                   entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins),
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all' ) 
# -------------------------------------------------------------------------------------------- #
# Get the distinct combinations of entry events for each treated block group. 
# -------------------------------------------------------------------------------------------- #
entries_by_bg <- treated_preds %>% 
  
  group_by(GEOID, event_year) %>%
  
  distinct(entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins)
# -------------------------------------------------------------------------------------------- #
pretr_preds_aug <- pretr_preds %>%
  
  filter(is.finite(rel_year) ) %>% # Filter out observations with rel_year == Inf because these are never-treated observations.
  
  left_join(entries_by_bg,  by = c('GEOID', 'event_year'),
            relationship = 'many-to-many', multiple = 'all' ) %>% 
  
  drop_na(.) # A few block group observations are present in the pre-treatment data but drop out of the post-treatment data. 
            # Therefore, there is no post-treatment information on entries to join
# -------------------------------------------------------------------------------------------- #
bootstrap_id = 0 # Used in function, so need to supply to global environment. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_errors_on_baseline_store_counts_x_entry.R') )
errors_on_baseline_store_counts_x_entry <- errors_on_baseline_store_counts_x_entry() # No arguments are used because all data in the global env. are employed. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_errors_on_baseline_store_counts_x_entry_tracts') # NULL or '_tracts'

bootstrap_ids <- seq(1, 499, 1)

err_boot_data <- map(bootstrap_ids, function(.x){
  
  filename <- paste0('bootstrap_',
                     'errors_by_baseline_store_counts_x_entry_',
                     str_to_lower(model_geography), '_', # e.g., rural or urban
                     model_dep_var, # e.g., low_access
                     bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                     .x, '.rds')
  
  dta <- readRDS(here::here('Analysis',
                            dir_geography,
                            dir_dep_var, 
                            dir_bootstrap, 
                            filename) )
  
})

# Combine the bootstrapped estimates with empirical 
err_boot_data <- c(list(errors_on_baseline_store_counts_x_entry), err_boot_data) 

# Summarise the bootstrapped data to obtain bootstrapped SEs. 
summary_err_boot_data <- err_boot_data %>%
  
  bind_rows() %>%
  
  group_by(store_count, entry, reg_type, entry_var) %>%
  
  summarise(across(.cols = Estimate, 
                   .fns = c('avg' = \(x) mean(x), 
                            'sd' = \(x) sd(x) ) ) )

# To rename columns. 
replace_col_names <- c(estimate = 'Estimate', bootstrap_mean = 'Estimate_avg', bootstrap_sd = 'Estimate_sd')

errors_on_baseline_store_counts_x_entry <- errors_on_baseline_store_counts_x_entry %>%
  
  left_join(summary_err_boot_data, 
            by = c('store_count', 'entry', 'reg_type', 'entry_var') ) %>% 
  
  rename(any_of(replace_col_names) ) %>%
  
  relocate(c(bootstrap_mean, bootstrap_sd), .after = estimate) 
# -------------------------------------------------------------------------------------------- #
# Function that loads multiple functions for plotting model results. 
# Also loads in a color palette for the plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #

create_plot_error_on_store_count_x_entry_fun <- function(dta, reg_type_arg, entry_type, entry_var_names, baseline_retail_count_name){ 
  
  error_on_store_x_entry <- dta %>% filter(reg_type == reg_type_arg & entry_var == entry_type)
  
  store_counts_str <- unique(error_on_store_x_entry$store_count)
  
  # Apply replacements
  replaced_store_counts_str <- str_replace_all(
    store_counts_str,
    c("\\[-1,0\\]" = "0", "\\(0,1\\]" = "1", "\\(1,2\\]" = "2", "\\(2,3\\]" = "3", "\\(3,4\\]" = "4",
      "\\(4,5\\]" = "5", "\\(5,6\\]" = "6", "\\(6,7\\]" = "7", "\\(7,8\\]" = "8", "\\(8,9\\]" = "9", "\\(9,10\\]" = "10")
  )
  
  replaced_store_counts_str[length(replaced_store_counts_str)] <- replaced_store_counts_str[length(replaced_store_counts_str)] %>% 
    str_replace_all('^\\(', '\u003e ') %>% 
    str_replace_all('\\,\\d+\\]$', '')
  
  
  replacement_rules <- setNames( # Map the new store counts to the original 
    replaced_store_counts_str,
    store_counts_str # Names of the replacement are the original store factor labels.  
  )
  
  error_on_store_x_entry$store_count <- unname(map_chr(error_on_store_x_entry$store_count, ~replacement_rules[.x]))
  
  store_counts_str <- unique(error_on_store_x_entry$store_count)
  
  error_on_store_x_entry <- error_on_store_x_entry %>% 
    mutate(store_count = factor(store_count, store_counts_str))
  
  if (entry_type == 'gross_entry_cumsum'){
    
    error_on_store_x_entry <- error_on_store_x_entry %>%
    
    filter(entry != '0')
    
  }
  
  entry_vals <- error_on_store_x_entry$entry
  
  if (entry_type != 'net_entry_cumsum'){
    entry_vals <- entry_vals %>% 
      str_replace_all('\\(', '\u003e ') %>%
      str_remove_all('\\,.*') 
    
  } else if (entry_type == 'net_entry_cumsum'){
    entry_vals <- entry_vals %>% 
      str_replace_all('\\(', '\u003e ') %>%
      str_remove_all('\\,.*') %>%
      str_replace_all('\\[.*', '\u2264 -1') 
  }
  
  
  legend_labels_entry <- unique(entry_vals)
  
  error_on_store_x_entry <- error_on_store_x_entry %>% 
    mutate(entry = factor(entry_vals, levels = legend_labels_entry) )
  
  subset_first_five_bins <- unique(error_on_store_x_entry$store_count)[1:5] # Subset first five bins. 
  
  error_on_store_x_entry <- error_on_store_x_entry %>%
    filter(store_count %in%  subset_first_five_bins) 
  
  legend_labels_store <- unique(error_on_store_x_entry$store_count)
  
  plot_errors_on_ds_entry_x_baseline_stores(dta = error_on_store_x_entry, 
                                            ci_label_str = '99% CI',
                                            ci_level = qnorm( 1 - (0.01/2)), 
                                            y_axis_title = 'Average Cross-Validation Error', 
                                            x_axis_title = entry_var_names, 
                                            x_axis_subtitle = NULL, 
                                            legend_labels_store_str = legend_labels_store,
                                            legend_title_str = baseline_retail_count_name, 
                                            title_str = NULL, 
                                            subtitle_str = NULL,
                                            decimal_place_y = 0.001, 
                                            nbreaks = 20)
  
  
  figname <- paste0(str_to_lower(model_geography), '_', 
                    model_dep_var, '_',
                    entry_type, '_x_',
                    str_remove_all(str_to_lower(reg_type_arg), '_10mile_2005'), 
                    bootstrap_by_tracts, '.pdf')
  
  ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, 
                    paste0('errors_on_store_counts_2005_x_entry', bootstrap_by_tracts), # Saves to directories and subdirectories.
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
}
# -------------------------------------------------------------------------------------------- #
entry_var_names = c('Dollar Store Entry Events', 
                    'Dollar Store Entry (Gross)', 
                    'Dollar Store Entry (Net)')

ds_entry_vars <- unique(errors_on_baseline_store_counts_x_entry$entry_var)

baseline_retail_counts_vars <- unique(errors_on_baseline_store_counts_x_entry$reg_type)

retail_varnames_df <- data.frame(original = baseline_retail_counts_vars, 
                                 
                                 tidy_retail_varname = tidy_covar_names(covar_name_str = baseline_retail_counts_vars))
# -------------------------------------------------------------------------------------------- #
map2(ds_entry_vars, entry_var_names,  
     function(.x, .y){
       map2(retail_varnames_df$original, retail_varnames_df$tidy_retail_varname, 
            function(.w, .z){
       
              create_plot_error_on_store_count_x_entry_fun(dta = errors_on_baseline_store_counts_x_entry, 
                                                           reg_type_arg = .w, 
                                                           entry_type = .x, 
                                                           entry_var_names = .y, 
                                                           baseline_retail_count_name = .z)
       
     }) 
       })
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_effects_on_baseline_store_counts_x_entry.R') )
effects_on_baseline_store_counts_x_entry <- effects_on_baseline_store_counts_x_entry() # No arguments are used because all data in the global env. are employed. 
# -------------------------------------------------------------------------------------------- #

dir_bootstrap <- paste0('bootstrap_effects_on_baseline_store_counts_x_entry_tracts') # NULL or '_tracts'

bootstrap_ids <- seq(1, 499, 1)

efx_boot_data <- map(bootstrap_ids, function(.x){
  
  filename <- paste0('bootstrap_',
                     'effects_by_baseline_store_counts_x_entry_',
                     str_to_lower(model_geography), '_', # e.g., rural or urban
                     model_dep_var, # e.g., low_access
                     bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                     .x, '.rds')
  
  dta <- readRDS(here::here('Analysis',
                            dir_geography,
                            dir_dep_var, 
                            dir_bootstrap, 
                            filename) )
  
})
# -------------------------------------------------------------------------------------------- #
# Combine the bootstrapped estimates with empirical 
efx_boot_data <- c(list(effects_on_baseline_store_counts_x_entry), efx_boot_data) 
# -------------------------------------------------------------------------------------------- #
# Summarise the bootstrapped data to obtain bootstrapped SEs. 
summary_efx_boot_data <- efx_boot_data %>%
  
  bind_rows() %>%
  
  group_by(store_count, entry, reg_type, entry_var) %>%
  
  summarise(across(.cols = Estimate, 
                   .fns = c('avg' = \(x) mean(x), 
                            'sd' = \(x) sd(x) ) ) )
# -------------------------------------------------------------------------------------------- #
# To rename columns. 
replace_col_names <- c(estimate = 'Estimate', bootstrap_mean = 'Estimate_avg', bootstrap_sd = 'Estimate_sd')

effects_on_baseline_store_counts_x_entry <- effects_on_baseline_store_counts_x_entry %>%
  
  left_join(summary_efx_boot_data, 
            by = c('store_count', 'entry', 'reg_type', 'entry_var') ) %>% 
  
  rename(any_of(replace_col_names) ) %>%
  
  relocate(c(bootstrap_mean, bootstrap_sd), .after = estimate) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
create_plot_effects_on_store_count_x_entry_fun <- function(dta, reg_type_arg, entry_type, entry_var_names, baseline_retail_count_name){ 
  
  effects_on_store_x_entry <- dta %>% filter(reg_type == reg_type_arg & entry_var == entry_type)
  
  store_counts_str <- unique(effects_on_store_x_entry$store_count)
  
  # Apply replacements
  replaced_store_counts_str <- str_replace_all(
    store_counts_str,
    c("\\[-1,0\\]" = "0", "\\(0,1\\]" = "1", "\\(1,2\\]" = "2", "\\(2,3\\]" = "3", "\\(3,4\\]" = "4",
      "\\(4,5\\]" = "5", "\\(5,6\\]" = "6", "\\(6,7\\]" = "7", "\\(7,8\\]" = "8", "\\(8,9\\]" = "9", "\\(9,10\\]" = "10")
  )
  
  replaced_store_counts_str[length(replaced_store_counts_str)] <- replaced_store_counts_str[length(replaced_store_counts_str)] %>% 
    str_replace_all('^\\(', '\u003e ') %>% 
    str_replace_all('\\,\\d+\\]$', '')
  
  
  replacement_rules <- setNames( # Map the new store counts to the original 
    replaced_store_counts_str,
    store_counts_str # Names of the replacement are the original store factor labels.  
  )
  
  effects_on_store_x_entry$store_count <- unname(map_chr(effects_on_store_x_entry$store_count, ~replacement_rules[.x]))
  
  store_counts_str <- unique(effects_on_store_x_entry$store_count)
  
  effects_on_store_x_entry <- effects_on_store_x_entry %>% 
    mutate(store_count = factor(store_count, store_counts_str))
  
  if (entry_type == 'gross_entry_cumsum'){
    
    effects_on_store_x_entry <- effects_on_store_x_entry %>%
      
      filter(entry != '0')
    
  }
  
  entry_vals <- effects_on_store_x_entry$entry
  
  if (entry_type != 'net_entry_cumsum'){
    entry_vals <- entry_vals %>% 
      str_replace_all('\\(', '\u003e ') %>%
      str_remove_all('\\,.*') 
    
  } else if (entry_type == 'net_entry_cumsum'){
    entry_vals <- entry_vals %>% 
      str_replace_all('\\(', '\u003e ') %>%
      str_remove_all('\\,.*') %>%
      str_replace_all('\\[.*', '\u2264 -1') 
  }
  
  
  legend_labels_entry <- unique(entry_vals)
  
  effects_on_store_x_entry <- effects_on_store_x_entry %>% 
    mutate(entry = factor(entry_vals, levels = legend_labels_entry) )
  
  subset_first_five_bins <- unique(effects_on_store_x_entry$store_count)[1:5] # Subset first five bins. 
  
  effects_on_store_x_entry <- effects_on_store_x_entry %>%
    filter(store_count %in%  subset_first_five_bins) 
  
  legend_labels_store <- unique(effects_on_store_x_entry$store_count)
  
  # We can use the same function to plot the treatment effects as the we did the errors. 
  plot_errors_on_ds_entry_x_baseline_stores(dta = effects_on_store_x_entry, 
                                            ci_label_str = '99% CI',
                                            ci_level = qnorm(1 - (0.01/2)),
                                            y_axis_title = 'Average Treatment Effects', 
                                            x_axis_title = entry_var_names, 
                                            x_axis_subtitle = NULL, 
                                            legend_labels_store_str = legend_labels_store,
                                            legend_title_str = baseline_retail_count_name, 
                                            title_str = NULL, 
                                            subtitle_str = NULL,
                                            decimal_place_y = 0.001, 
                                            nbreaks = 20)
  
  
  figname <- paste0(str_to_lower(model_geography), '_', 
                    model_dep_var, '_',
                    entry_type, '_x_',
                    str_remove_all(str_to_lower(reg_type_arg), '_10mile_2005'), 
                    bootstrap_by_tracts, '.pdf')
  
  ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, 
                    paste0('effects_on_store_counts_2005_x_entry', bootstrap_by_tracts), # Saves to directories and subdirectories.
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
}
# -------------------------------------------------------------------------------------------- #
entry_var_names = c('Dollar Store Entry Events', 
                    'Dollar Store Entry (Gross)', 
                    'Dollar Store Entry (Net)')

ds_entry_vars <- unique(effects_on_baseline_store_counts_x_entry$entry_var)

baseline_retail_counts_vars <- unique(effects_on_baseline_store_counts_x_entry$reg_type)

retail_varnames_df <- data.frame(original = baseline_retail_counts_vars, 
                                 
                                 tidy_retail_varname = tidy_covar_names(covar_name_str = baseline_retail_counts_vars))
# -------------------------------------------------------------------------------------------- #
map2(ds_entry_vars, entry_var_names,  
     function(.x, .y){
       map2(retail_varnames_df$original, retail_varnames_df$tidy_retail_varname, 
            function(.w, .z){
              
              create_plot_effects_on_store_count_x_entry_fun(dta = effects_on_baseline_store_counts_x_entry, 
                                                           reg_type_arg = .w, 
                                                           entry_type = .x, 
                                                           entry_var_names = .y, 
                                                           baseline_retail_count_name = .z)
              
            }) 
     })
# -------------------------------------------------------------------------------------------- #