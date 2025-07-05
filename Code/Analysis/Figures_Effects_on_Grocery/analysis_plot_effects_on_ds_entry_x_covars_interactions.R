# Creates Figures 3 (main text) and Figures C.1, C.2, C.3, C.6, and C.7 (supplementary text)
# Used in conjunction with `analysis_plot_effects_on_ds_entry_x_covars_interactions_sourced.R`
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_analysis_plot_effects_on_interactions.R'))
# -------------------------------------------------------------------------------------------- #
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
# Regression of effects on covariate bins interacted with dollar store entries. 
# -------------------------------------------------------------------------------------------- #
effects_on_ds_x_covars <- empirical_estimates$effects_on_ds_x_covars %>%
  
  mutate(across(.cols = c(covariate, ds_entry), 
                .fn = ~str_remove_all(., '_bins'))) %>%
  
  left_join(tidy_covar_name_keys, by = 'covariate') %>%
  
  mutate(ds_entry_tidy = tidy_covar_names(covar_name_str = ds_entry) )
# -------------------------------------------------------------------------------------------- #

effects_on_ds_x_covars$entry <- droplevels(effects_on_ds_x_covars$entry)

# Filter special cases when gross entry is zero. 
filter_zeros_gross_entry <- with(effects_on_ds_x_covars, ds_entry == 'gross_entry_cumsum' & entry == '0')
effects_on_ds_x_covars <- effects_on_ds_x_covars[!filter_zeros_gross_entry, ]

# Rearrange the levels to accommodate for negative net dollar store entries. 
current_levels <- levels(effects_on_ds_x_covars$entry)

# Use gsub() to replace any "[-digit,-1]" format with "d -1"
# This regex matches the pattern: open square bracket, a hyphen, one or more digits, a comma, whitespace (optional), "-1", and close square bracket
new_levels <- gsub("\\[\\-\\d+,\\s*-1\\]", 
                   "\u2264 -1", 
                   current_levels)

# Reassign the modified levels back to the factor
levels(effects_on_ds_x_covars$entry) <- new_levels

current_levels <- levels(effects_on_ds_x_covars$entry)
current_levels <- current_levels[current_levels != "\u2264 -1"]
new_levels <- c("\u2264 -1", current_levels)

effects_on_ds_x_covars$entry <- factor(effects_on_ds_x_covars$entry, 
                                       levels = new_levels)
# -------------------------------------------------------------------------------------------- #

effects_on_ds_x_covars$ds_entry_tidy[effects_on_ds_x_covars$ds_entry_tidy == 'Dollar Store Entry'] <- 'Dollar Store Entry Events'

# -------------------------------------------------------------------------------------------- #
tidy_name_str <- unique(effects_on_ds_x_covars$tidy_name)
ds_entry_str <- unique(effects_on_ds_x_covars$ds_entry_tidy)

covariate_varname <- unique(effects_on_ds_x_covars$covariate)
ds_entry_varname <- unique(effects_on_ds_x_covars$ds_entry)

# Plot and save. 
map2(ds_entry_str, ds_entry_varname, function(.x, .y){ 
  
  map2(tidy_name_str, covariate_varname, function(.z, .w){ 
    
    plot_ds_entry_on_covar_interacts(dta = effects_on_ds_x_covars, 
                                     ci_label_str = '99% CI', 
                                     ci_level = qnorm(1 - (0.01/2)),
                                     filter_covariate_type = c(ds_entry_tidy == .x & tidy_name == .z), 
                                     y_axis_title = 'Average Treatment Effects', 
                                     x_axis_title = .x, 
                                     legend_title_str = .z,
                                     title_str = NULL, 
                                     subtitle_str = NULL, 
                                     decimal_place_y = 0.001)
    
    figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_',
                      .y, '_x_', .w, bootstrap_by_tracts, '.pdf')

    ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, paste0('effects_on_interactions', bootstrap_by_tracts), # Saves to directories and subdirectories.
                      figname), 
           width = 8, height = 6, unit = 'in', dpi = 600)

    
  })
})
# -------------------------------------------------------------------------------------------- #
# Regression of effects on grocery store bins (2005) interacted with dollar store entries. 
# -------------------------------------------------------------------------------------------- #
effects_on_ds_x_grocery <- empirical_estimates$effects_on_ds_x_grocery %>%
  
  mutate(across(.cols = c(covariate, ds_entry), 
                .fn = ~str_remove_all(., '_bins'))) %>%
  
  mutate(ds_entry_tidy = tidy_covar_names(covar_name_str = ds_entry), 
         tidy_name = tidy_covar_names(covar_name_str = covariate))
# -------------------------------------------------------------------------------------------- #

# Filter special cases when gross entry is zero. 
filter_zeros_gross_entry <- with(effects_on_ds_x_grocery, ds_entry == 'gross_entry_cumsum' & entry == '0')
effects_on_ds_x_grocery <- effects_on_ds_x_grocery[!filter_zeros_gross_entry, ]


# Use gsub() to replace any "[-digit,-1]" format with "d -1"
# This regex matches the pattern: open square bracket, a hyphen, one or more digits, a comma, whitespace (optional), "-1", and close square bracket
new_levels <- gsub("\\[\\-\\d+,\\s*-1\\]", 
                   "\u2264 -1", 
                   effects_on_ds_x_grocery$entry)

# Reassign the modified levels back to the factor
effects_on_ds_x_grocery$entry <- new_levels


new_levels <- unique(effects_on_ds_x_grocery$entry)
begin_levels <- new_levels[grepl('0|\u2264', new_levels)]
new_levels <- new_levels[!grepl('0|\u2264', new_levels)]
# Rearrange the levels to accomodate for negative net dollar store entries. 
effects_on_ds_x_grocery$entry <- factor(effects_on_ds_x_grocery$entry, 
                                       levels = c(begin_levels, new_levels))

# -------------------------------------------------------------------------------------------- #
effects_on_ds_x_grocery$ds_entry_tidy[effects_on_ds_x_grocery$ds_entry_tidy == 'Dollar Store Entry'] <- 'Dollar Store Entry Events'
# -------------------------------------------------------------------------------------------- #
tidy_name_str <- unique(effects_on_ds_x_grocery$tidy_name)
ds_entry_str <- unique(effects_on_ds_x_grocery$ds_entry_tidy)

covariate_varname <- str_remove_all(str_to_lower(unique(effects_on_ds_x_grocery$covariate)), '_(?=)10.*') # Remove everything from _10mile...
ds_entry_varname <- unique(effects_on_ds_x_grocery$ds_entry)

# -------------------------------------------------------------------------------------------- #
# Plot and save. 
# Removes the cases when year 2005 grocery stores are == 0. 
# -------------------------------------------------------------------------------------------- #
map2(ds_entry_str, ds_entry_varname, function(.x, .y){ 
    
  plot_ds_entry_on_grocery_interacts(dta = effects_on_ds_x_grocery, 
                                     ci_label_str = '99% CI', 
                                     ci_level = qnorm(1 - (0.01/2)),
                                     filter_covariate_type = c(ds_entry_tidy == .x & tidy_name == tidy_name_str), 
                                     one_or_more_grocers = TRUE,
                                     y_axis_title = 'Average Treatment Effects', 
                                     x_axis_title = paste0(.x),
                                     legend_title_str = tidy_name_str,
                                     title_str = NULL, 
                                     subtitle_str = NULL, 
                                     decimal_place_y = 0.01, 
                                     lower_ylim = NULL, 
                                     upper_ylim = NULL, 
                                     nbreaks = 14)
    
    figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_',
                      .y, '_x_', covariate_varname, '_one_or_more_grocers', bootstrap_by_tracts, '.pdf')

    ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography,
                      paste0('effects_on_interactions', bootstrap_by_tracts), # Saves to directories and subdirectories.
                      figname),
           width = 8, height = 6, unit = 'in', dpi = 600)


  })
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Plot and save. 
# Includes the cases when year 2005 grocery stores are == 0. 
# -------------------------------------------------------------------------------------------- #
map2(ds_entry_str, ds_entry_varname, function(.x, .y){ 
  
  plot_ds_entry_on_grocery_interacts(dta = effects_on_ds_x_grocery, 
                                     ci_label_str = '99% CI', 
                                     ci_level = qnorm(1 - (0.01/2)),
                                     filter_covariate_type = c(ds_entry_tidy == .x & tidy_name == tidy_name_str), 
                                     one_or_more_grocers = FALSE,
                                     y_axis_title = 'Average Treatment Effects', 
                                     x_axis_title = paste0(.x),
                                     legend_title_str = tidy_name_str,
                                     title_str = NULL, 
                                     subtitle_str = NULL, 
                                     decimal_place_y = 0.01, 
                                     lower_ylim = NULL, 
                                     upper_ylim = NULL, 
                                     nbreaks = 14)
  
  figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_',
                    .y, '_x_', covariate_varname, '_zero_or_more_grocers', bootstrap_by_tracts, '.pdf')

  ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, paste0('effects_on_interactions', bootstrap_by_tracts), # Saves to directories and subdirectories.
                    figname),
         width = 8, height = 6, unit = 'in', dpi = 600)


})