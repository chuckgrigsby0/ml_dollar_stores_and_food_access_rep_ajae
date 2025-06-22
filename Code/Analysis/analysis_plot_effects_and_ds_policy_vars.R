# -------------------------------------------------------------------------------------------- #
.libPaths()
print(model_geography); print(model_dep_var)
options(scipen = 999)
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

# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year'), 
            multiple = 'all', relationship = 'one-to-one') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year'), 
            multiple = 'all', relationship = 'one-to-one') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, market) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') # We obtain post-treatment predictions from 2006-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------- #
# Load the binned dollar store bans and restrictions data. 
# -------------------------------------------------------------------------------------------- #
ds_bans_binned <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 
                                     paste0('posttreatment_binned_ds_policy_vars_', 
                                            str_to_lower(model_geography), '.rds') ) )
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_effects_and_ds_policy_vars.R'))
# -------------------------------------------------------------------------------------------- #
effects_on_ds_policies_df <- effects_and_ds_policies(df = model_preds, 
                                                     bootstrap_idx = 0)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load bootstrap data/estimates created in analysis_model_bootstrap_effects_on_ds_policy_vars_array.R 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_effects_on_ds_policy_vars', bootstrap_by_tracts) # NULL or '_tracts'

boot_data <- seq(1, 499, 1) %>%
  
  map_dfr(function(.iter){ 
    
    filename <- paste0('bootstrap_',
                       'effects_on_ds_policy_vars_',
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

boot_data <- bind_rows(effects_on_ds_policies_df, boot_data)
# -------------------------------------------------------------------------------------------- #
# Functions to subset the named elements 'errors' or the 'predictions', 
# compute standard errors, and join standard errors to empirical point estimates. 
# -------------------------------------------------------------------------------------------- #
# Simple function to compute SEs by some grouping variables. 
source(here::here('Code', 'Functions', 'Function_bootstrap_compute_standard_errors.R')) 
# -------------------------------------------------------------------------------------------- #
# Compute the bootstrapped standard errors. 
# -------------------------------------------------------------------------------------------- #
boot_data_se <- compute_bootstrap_standard_errors(dta = boot_data, group_vars = c('variable', 'reg_type'))
boot_data_se <- boot_data_se %>% arrange(reg_type)
# -------------------------------------------------------------------------------------------- #
# Join the bootstrapped SEs to the empirical estimates from the optimal model. 
# -------------------------------------------------------------------------------------------- #
boot_effects_on_ds_policies <- effects_on_ds_policies_df %>% 
  left_join(boot_data_se, by = c('variable', 'reg_type')) %>%
  relocate(c(bootstrap_sd, bootstrap_mean), .after = estimate)
# -------------------------------------------------------------------------------------------- #
# Rename the policy variables before plotting. 
# -------------------------------------------------------------------------------------------- #

boot_effects_on_ds_policies$variable <- boot_effects_on_ds_policies$variable %>% 
  str_replace_all(pattern = c('/Defeated and Restrictions'='', 
                              '/Restrictions and Defeated'=''))
# -------------------------------------------------------------------------------------------- #
# Plot estimates. 
# Loads functions for plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #

num_plots <- unique(boot_effects_on_ds_policies$reg_type)
num_plots <- str_subset(num_plots, pattern = 'year', negate = TRUE); num_plots
geom_col_width <- c(0.4, 0.8, 0.7)

if (model_geography == 'Rural'){
  
  decimals_arg = c(0.001, .001, .001)
  
} else { 

  decimals_arg <- c(0.001, .001, .005)
    
}
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
  pmap(list('filter_reg_type' = num_plots, 
            'geom_col_width' = geom_col_width, 
            'decimal_place_y' = decimals_arg), 
       plot_effects_on_ds_policy_vars, 
       dta = boot_effects_on_ds_policies, 
       analysis_dir = 'Analysis', 
       ci_level = qnorm(1 - (0.01/2)), 
       ci_label_str = '99% CI')
# -------------------------------------------------------------------------------------------- #