# -------------------------------------------------------------------------------------------- #
# This script loads all of the data (both original point estimates and bootstrapped standard errors) 
# necessary for creating the figures in the current script. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_analysis_plot_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
# Plot estimates. 
# Loads functions for plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #
# Create a covariate-name key. 
# Create a covariate-predictor category key. 
# -------------------------------------------------------------------------------------------- #
tidy_covar_name_keys <- model_covars_list %>% 
  
  map_dfr(function(.x){ 
    
    data.frame(covariate = .x) 
    
  }, .id = 'covariate_type') %>%
  
  mutate(tidy_name = unlist(model_covar_names, use.names = FALSE))
# -------------------------------------------------------------------------------------------- #
tidy_covar_categories <- model_covars_list_disag %>% # *._disag means disaggregated. 
  
  map_dfr(function(.x){ 
    
    data.frame(covariate = .x) 
    
  }, .id = 'covariate_cat')
# -------------------------------------------------------------------------------------------- #
tidy_covar_name_keys <- tidy_covar_name_keys %>% 
  
  left_join(tidy_covar_categories, by = 'covariate')
# -------------------------------------------------------------------------------------------- #
# Define covariate categories for factor levels. Note, cats => categories. 

covar_fine_cats <- c('socioeconomics', 'land_use', 'distance_to_urban_areas', 
                      'park_access', 'schools', 'roads', 'retail', 'urban_areas', 'fixed_effects')

covar_broad_cats <- c('socioeconomics', 'economic_geography', 
                      'retail', 'fixed_effects')
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'analysis_plot_effects_on_covars_and_dsvars.R'))

source(here::here('Code', 'Analysis', 'analysis_plot_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #