
# Errors on covariate bins and integers. 
# On average, the CV errors should be uncorrleated with predictors. 

# -------------------------------------------------------------------------------------------- #
errors_on_bins <- empirical_estimates$errors_on_bins

errors_on_bins <- errors_on_bins %>%
  
  mutate(covariate = str_remove_all(covariate, '_bins')) %>%
  
  left_join(tidy_covar_name_keys, by = 'covariate')
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
errors_on_bins <- errors_on_bins %>%
  
  mutate(covariate_cat = factor(covariate_cat, 
                                levels = covar_fine_cats), 
         covariate_type = factor(covariate_type, 
                                 levels = covar_broad_cats)) %>%
  
  group_by(covariate_type, covariate_cat) %>%
  
  arrange(covariate_cat, covariate) %>%
  
  mutate(across(.cols = c(covariate, tidy_name), 
                .fn = ~factor(., levels = unique(.))))
# -------------------------------------------------------------------------------------------- #
# Clean up the term column, which is the x-axis in this context. 
# -------------------------------------------------------------------------------------------- #
# orig_levels_sort <- sort(unique(sort(unique(errors_on_bins$term))))
# lteq = grep(paste0('\u2264'), orig_levels_sort)
# gteq = grep(paste0('\u003e'), orig_levels_sort)
# errors_on_bins$term <- factor(errors_on_bins$term, 
#                               levels = c(orig_levels_sort[lteq], 
#                                          orig_levels_sort[-c(lteq, gteq)],
#                                          orig_levels_sort[gteq]))
# -------------------------------------------------------------------------------------------- #
# Create variable chunks for specific covariates.  
# -------------------------------------------------------------------------------------------- #
# select_covars <- errors_on_bins %>% 
#   filter(!(covariate %in% land_use_sel$term) &
#            !grepl('^age_|total_population', covariate) &
#            !grepl('roads|fixed_effects', covariate_cat)) %>%
#   distinct(covariate)

# errors_on_bins_sel <- errors_on_bins %>% filter(covariate %in% select_covars$covariate)

n_covars <- length(unique(errors_on_bins$covariate)); n_covars
# chunk_size was originally set to 4. When set to one, it means we create a figure for each variables
plot_chunks <- furrr:::make_chunks(n_x = n_covars, chunk_size = 1) 
plot_chunks <- plot_chunks %>% map(function(.x) unique(errors_on_bins$covariate)[.x] )
plot_ids <- 1:length(plot_chunks)
# -------------------------------------------------------------------------------------------- #
map2(plot_chunks, plot_ids, function(.x, .y){
  
  plot_errors_on_covars_binned(dta = errors_on_bins, 
                               filter_covariate_type = c(covariate %in% .x),
                               ci_label_str = '99% CI', 
                               ci_level = qnorm( 1 - (0.01/2)), 
                               y_axis_title = 'Average Cross-Validation Error', 
                               title_str = NULL,
                               subtitle_str = NULL,
                               decimal_place_y = 0.001)
  
  figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_errors_on_covars_', 'bins_', '0', .y, bootstrap_by_tracts, '.pdf')
  
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_on_covars_bins', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #
# select_covars <- errors_on_bins %>% 
#   filter((covariate %in% land_use_sel$term) |
#          grepl('^age_|total_population', covariate) |
#          grepl('roads|fixed_effects', covariate_cat)) %>%
# distinct(covariate)

# errors_on_bins_sel <- errors_on_bins %>% filter(covariate %in% select_covars$covariate)

# n_covars <- length(unique(errors_on_bins_sel$covariate)); n_covars
# plot_chunks <- furrr:::make_chunks(n_x = n_covars, chunk_size = 4)
# plot_chunks <- plot_chunks %>% map(function(.x) unique(errors_on_bins_sel$covariate)[.x] )
# -------------------------------------------------------------------------------------------- #

# Errors on Integer covariates. 
# -------------------------------------------------------------------------------------------- #

errors_on_ints <- empirical_estimates$errors_on_ints

# Join clean variable names for plots.  

errors_on_ints <- errors_on_ints %>% 
  
  left_join(tidy_covar_name_keys, by = 'covariate')

# Convert to factor variables variable names and variable categories.
# This is done for plotting.  
errors_on_ints <- errors_on_ints %>%
  
  mutate(covariate_cat = factor(covariate_cat, 
                                levels = covar_fine_cats), 
         
         covariate_type = factor(covariate_type, 
                                 levels = covar_broad_cats)) %>%
  
  group_by(covariate_type, covariate_cat) %>%
  
  arrange(covariate, .by_group = TRUE) %>%
  
  mutate(across(.cols = c(covariate, tidy_name), 
                .fn = ~factor(., levels = unique(.))))
  
  # group_by(covariate) %>%
  
  # mutate(row_id = row_number() - 1) %>%
  
  # relocate(row_id, .after = term) %>%
  
  # mutate(across(.cols = row_id, .fn = ~factor(., levels = sort(unique(.))))) %>%
  
  # mutate(row_id = case_when(grepl('\u2265', term) ~ term, 
  #                         TRUE ~ row_id), 
#      term = row_id)

# -------------------------------------------------------------------------------------------- #
# Clean up the term column, which is the x-axis in this context. 
# -------------------------------------------------------------------------------------------- #
# errors_on_ints$term <- factor(errors_on_ints$term, levels = unique(errors_on_ints$term))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
n_covars <- length(unique(errors_on_ints$covariate)); n_covars
plot_chunks <- furrr:::make_chunks(n_x = n_covars, chunk_size = 1) # Originally 4 for facets. 
plot_chunks <- plot_chunks %>% map(function(.x) unique(errors_on_ints$covariate)[.x] )
plot_ids <- 1:length(plot_chunks)
# -------------------------------------------------------------------------------------------- #
map2(plot_chunks, plot_ids, function(.x, .y){
  
  
  plot_errors_on_int_covars(dta = errors_on_ints, 
                            filter_covariate_type = c(covariate %in% .x),
                            ci_label_str = '99% CI', 
                            ci_level = qnorm( 1 - (0.01/2)),
                            y_axis_title = 'Average Cross-Validation Error', 
                            title_str = NULL,
                            subtitle_str = NULL,
                            decimal_place_y = 0.001)
  
  figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_errors_on_covars_', 'int_', '0', .y, bootstrap_by_tracts, '.pdf')
  
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_on_covars_bins', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Regression of tau on normalized covariates. 
# -------------------------------------------------------------------------------------------- #
errors_on_covars <- empirical_estimates$errors_on_norm_covars %>%
  
  left_join(tidy_covar_name_keys, by = 'covariate') %>%
  
  mutate(across(.cols = c(covariate_type, covariate_cat), # tidy the list names. 
                .fn = ~str_to_title(str_replace_all(., '_', ' ')))) %>%
  
  arrange(estimate) %>% 
  
  mutate(across(.cols = tidy_name, .fn = ~factor(., levels = .))) 

# Force Socioeconomic variables to be shown first in the figure, followed by Economic Geography predictors. 
errors_on_covars$covariate_type <- factor(errors_on_covars$covariate_type, 
                                           levels = str_to_title(str_replace_all(covar_broad_cats, '_', ' ')))
# -------------------------------------------------------------------------------------------- #
# Land use filter. 
land_use_sel <- errors_on_covars %>% filter(covariate_cat == 'Land Use') %>% distinct(term) %>% 
  filter(!grepl('developed', term))

# -------------------------------------------------------------------------------------------- #

# Plots
map('socioeconomics', function(.x){
  
  plot_errors_on_covars_norm(dta = errors_on_covars, 
                             filter_covariate_type = c(grepl('Socioeconomics', covariate_type) & 
                                                         !grepl('^age_|total_population', covariate)),
                             ci_label_str = '99% CI', 
                             ci_level = qnorm( 1 - (0.01/2) ),
                             title_str = NULL, 
                             subtitle_str = NULL,
                             decimal_place_y = 0.0001)
  
  figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_errors_on_covars_', 'norm', '_', .x, bootstrap_by_tracts, '.pdf')
  # Saves to directories and subdirectories. 
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_on_covars_norm', bootstrap_by_tracts), 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #
map('economic_geography', function(.x){
  
  plot_errors_on_covars_norm(dta = errors_on_covars, 
                             filter_covariate_type = c(!grepl('Fixed Effects|Retail|Roads', covariate_cat) & 
                                                         !grepl('Socioeconomics', covariate_type) &
                                                         !(covariate %in% land_use_sel$term) ),
                             ci_label_str = '99% CI', 
                             ci_level = qnorm( 1 - (0.01/2) ), 
                             title_str = NULL, 
                             subtitle_str = NULL,
                             decimal_place_y = 0.0001)
  
  figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_errors_on_covars_', 'norm', '_', .x, bootstrap_by_tracts, '.pdf')
  # Saves to directories and subdirectories. 
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('errors_on_covars_norm', bootstrap_by_tracts), 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #


