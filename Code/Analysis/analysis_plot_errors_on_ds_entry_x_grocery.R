# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
print(model_dep_var); print(model_geography)
.libPaths()
# -------------------------------------------------------------------------------------------- #
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts' # or NULL to bootstrap by block-group and stratify by relative time. 
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
# Filter out urban_area and uc_area from the Rural models. 
# -------------------------------------------------------------------------------------------- #
# if (model_geography == 'Rural'){ 
#   model_covars <- model_covars[!grepl('^urban_area$|^uc_area$', model_covars)]
#   }
# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #
ds_bans <- readr::read_csv(here::here('Data', 'block_groups_w_ds_policies.csv'), show_col_types = FALSE)
ds_bans <- ds_bans %>% select(GEOID, Defeated, Moratorium, Ordinance, policy_total)
source(here::here('Code', 'Functions', 'Function_prepare_binned_ds_policy_vars.R'))
ds_bans_binned <- prepare_binned_ds_policy_vars(df = ds_bans)
# -------------------------------------------------------------------------------------------- #
# Pre-treatment observations, year 2005 Grocery Store bins. 
# -------------------------------------------------------------------------------------------- #
fname_pretr_binned_grocery = paste0('pretreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

pretr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_grocery))
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
  
  left_join(bg_regs_and_divs, by = 'GEOID') %>% # Regional and divisional indicators. 
  
  left_join(select(ds_bans_binned, GEOID, 
                   Defeated_and_Restrictions_bins, Restrictions_and_Defeated_bins, 
                   Defeated_and_Restrictions, Restrictions_and_Defeated, 
                   policy_total_binary_bins, policy_total_binary, policy_total), 
            by = 'GEOID', relationship = 'many-to-one', multiple = 'all') 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- untreated_preds %>%
  
  filter(is.finite(rel_year)) %>% # Filter out observations with rel_year == Inf because these are never-treated observations. 
  
  left_join(select(pretr_binned_grocery, GEOID, year, Grocery_Count_10mile_2005_bins), by = c('GEOID', 'year') )
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year'), 
            relationship = 'many-to-one', multiple = 'all') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
# because the pretr_preds from model_preds contains the bootstrapped error. 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# Join the dollar store entry information to post-treatment data. 
treated_preds <- treated_preds %>%
  left_join(select(posttr_binned_dsvars, GEOID, year, 
                   entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins),
            by = c('GEOID', 'year'), relationship = 'many-to-one', multiple = 'all' ) %>%
  select(GEOID, event_year, matches('bins$') )
# -------------------------------------------------------------------------------------------- #
# Get the distinct combinations of entry events for each treated block group. 
# -------------------------------------------------------------------------------------------- #
treated_preds <- treated_preds %>% 
  
  group_by(GEOID, event_year) %>%
  
  distinct(entry_events_bins, gross_entry_cumsum_bins, net_entry_cumsum_bins)
# -------------------------------------------------------------------------------------------- #
pretr_preds_aug <- pretr_preds %>%
  
  filter(is.finite(rel_year) ) %>% # Filter out observations with rel_year == Inf because these are never-treated observations.
  
  left_join(treated_preds,  by = c('GEOID', 'event_year'),
            relationship = 'many-to-many', multiple = 'all' ) %>% 
  
  drop_na(.) # A few block group observations are present in the pre-treatment data but drop out of the post-treatment data. 
# Therefore, there is no post-treatment information on entries to join
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_errors_on_grocery_policy_and_geography.R') )
bootstrap_id = 0 # Used in function, so need to supply to global environment. 
errors_on_grocery_x_entry_and_policy_combined <- errors_on_grocery_x_entry_and_policy() # No arguments are used because all data in the global env. are employed. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_error_by_grocery_entry_policy_region_tracts') # NULL or '_tracts'

bootstrap_ids <- seq(1, 499, 1)

boot_data <- map(bootstrap_ids, function(.x){

filename <- paste0('bootstrap_',
                   'error_by_grocery_entry_policy_region_',
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
boot_data <- c(list(errors_on_grocery_x_entry_and_policy_combined), boot_data) 
# Run code to obtain a list of the empirical estimates with associated bootstrapped SDs. 
source(here::here('Code', 'Analysis', 'data_preparation_errors_on_ds_entry_x_grocery_emp_estimates.R'))
# -------------------------------------------------------------------------------------------- #
replace_col_names <- c(estimate = 'Estimate', bootstrap_mean = 'Estimate_mean', bootstrap_sd = 'Estimate_sd', 
                       estimate = 'err_avg', bootstrap_mean = 'err_avg_mean', bootstrap_sd = 'err_avg_sd')

emp_estimates <- emp_estimates %>% 
  
  map(function(.x){ 
    
    new_dta <- .x %>% rename(any_of(replace_col_names)) 
    
    })
# -------------------------------------------------------------------------------------------- #
# Function that loads multiple functions for plotting model results. 
# Also loads in a color palette for the plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #
# Plot: Errors on dollar store policy indicators. 
# -------------------------------------------------------------------------------------------- #
# Set legend labels for plots. 
legend_labels <- c("Defeated", "Restrictions", "No Defeat or Restriction")

error_on_ds_policy <- emp_estimates$error_on_ds_policy %>% 
  mutate(variable = factor(variable, levels = legend_labels)) 

plot_errors_on_ds_policy_vars(dta = error_on_ds_policy, 
                              ci_label_str = '99% CI', 
                              ci_level = qnorm(1 - (0.01/2)),
                              geom_col_width = 0.8,
                              decimal_place_y = 0.001, 
                              analysis_dir = 'Analysis')
# -------------------------------------------------------------------------------------------- #
error_on_region <- emp_estimates$error_on_region %>% 
  
  rename(tidy_name = 'REGION_NAME') %>%
  
  mutate(coefficient_label = 'Coefficient Estimate') %>%
  
  drop_na(tidy_name)

plot_errors_vs_region(dta = error_on_region, 
                      ci_label_str = '99% CI', 
                      ci_level = qnorm(1 - (0.01/2)),
                      reg_div_str = 'Region', decimal_place_y = 0.0001, 
                      title_str = NULL, subtitle_str = NULL)

figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'errors_by_region', bootstrap_by_tracts, '.pdf')

ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, paste0('errors_by_region', bootstrap_by_tracts), # Saves to directories and subdirectories.
                  figname), 
       width = 9, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #

error_on_grocery <- emp_estimates$error_on_grocery %>%
  
  mutate(coefficient_label = 'Grocery Stores (2005)')

legend_labels <- unique(error_on_grocery$count)

error_on_grocery$count <- factor(error_on_grocery$count, levels = legend_labels)

plot_errors_on_grocery_stores_2005(dta = error_on_grocery, 
                                   ci_label_str = '99% CI', 
                                   ci_level = qnorm(1 - (0.01/2)),
                                   y_axis_title = 'Average Cross-Validation Error', 
                                   x_axis_title = 'Grocery Stores (2005)', 
                                   title_str = NULL, subtitle_str = NULL, 
                                   decimal_place_y = 0.001)

figname <- paste0(str_to_lower(model_geography), '_', 
                  model_dep_var, '_',
                  'errors_on_grocery_stores_2005', 
                  bootstrap_by_tracts, '.pdf')

ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, 
                  paste0('errors_on_grocery_stores_2005', bootstrap_by_tracts), # Saves to directories and subdirectories.
                  figname), 
       width = 8, height = 6, unit = 'in', dpi = 600)

# -------------------------------------------------------------------------------------------- #
# Plot: Errors on dollar store entry events-x-2005 grocery stores 
# -------------------------------------------------------------------------------------------- #
entry_vars <- names(emp_estimates) %>% str_subset(pattern = '_entry.')

create_plot_error_on_grocery_x_entry_fun <- function(entry_type, entry_var_names){ 
  
  error_on_grocery_x_entry <- emp_estimates %>% pluck(entry_type)
  
  entry_vals <- as.character(error_on_grocery_x_entry$entry)
  
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
  
  error_on_grocery_x_entry <- error_on_grocery_x_entry %>% 
    mutate(entry = factor(entry_vals, levels = legend_labels_entry) )
  
  error_on_grocery_x_entry <- error_on_grocery_x_entry %>%
    filter(grocery_count %in% as.character(seq(1, 4, 1) ) ) 
  
  legend_labels_grocery <- unique(error_on_grocery_x_entry$grocery_count)
  
  error_on_grocery_x_entry <- error_on_grocery_x_entry %>%
    mutate(grocery_count = factor(grocery_count, levels = legend_labels_grocery) )
  
  plot_errors_on_ds_entry_x_grocery_stores(dta = error_on_grocery_x_entry, 
                                           ci_label_str = '99% CI', 
                                           ci_level = qnorm(1 - (0.01/2)),
                                           y_axis_title = 'Average Cross-Validation Error', 
                                           x_axis_title = entry_var_names, 
                                           x_axis_subtitle = NULL, 
                                           legend_labels_store_str = legend_labels_grocery,
                                           legend_title_str = 'Grocery Stores (2005)', 
                                           title_str = NULL, 
                                           subtitle_str = NULL,
                                           decimal_place_y = 0.001, 
                                           nbreaks = 20)
  
  
 # print(error_on_grocery_x_entry %>% filter(entry %in% c(1, 2, 3) & grocery_count %in% c(1, 2) ))
  # The point estimates for one dollar store entry event and one baseline grocery store is statistically 
  # significant, but the point estimate is small, 0.00445. For two entry events and one grocery store, the 
  # point estimate is slightly larger, 0.00864, but is less than one percentage point. 
  
  figname <- paste0(str_to_lower(model_geography), '_',
                    model_dep_var, '_',
                    str_replace_all(entry_type, '\\.', '_'),
                    bootstrap_by_tracts, '.pdf')

  ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography,
                    paste0('errors_on_grocery_stores_2005', bootstrap_by_tracts), # Saves to directories and subdirectories.
                    figname),
         width = 8, height = 6, unit = 'in', dpi = 600)
  
}
# -------------------------------------------------------------------------------------------- #
entry_var_names = c('Dollar Store Entry Events', 
                    'Dollar Store Entry (Gross)', 
                    'Dollar Store Entry (Net)')
# -------------------------------------------------------------------------------------------- #
map2(entry_vars, entry_var_names,  
     function(.x, .y){
       
       create_plot_error_on_grocery_x_entry_fun(entry_type = .x, 
                                                entry_var_names = .y)
       
     })
# -------------------------------------------------------------------------------------------- #
