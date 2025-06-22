# Regression of tau on covariate bins and integers. 
# -------------------------------------------------------------------------------------------- #
effects_on_bins <- empirical_estimates$effects_on_binned_covars

effects_on_bins <- effects_on_bins %>%
  
  mutate(covariate = str_remove_all(covariate, '_bins')) %>%
  
  left_join(tidy_covar_name_keys, by = 'covariate')
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
effects_on_bins <- effects_on_bins %>%
  
  mutate(covariate_cat = factor(covariate_cat, 
                                levels = covar_fine_cats), 
         covariate_type = factor(covariate_type, 
                                 levels = covar_broad_cats)) %>%
  
  group_by(covariate_type, covariate_cat) %>%
  
  arrange(covariate_cat, covariate) %>%
  
  mutate(across(.cols = c(covariate, tidy_name), 
                .fn = ~factor(., levels = unique(.))))
# -------------------------------------------------------------------------------------------- #
# Create variable chunks for specific covariates.  
# -------------------------------------------------------------------------------------------- #
n_covars <- length(unique(effects_on_bins$covariate)); n_covars
# chunk_size was originally set to 4. When set to one, it means we create a figure for each variables
plot_chunks <- furrr:::make_chunks(n_x = n_covars, chunk_size = 1) 
plot_chunks <- plot_chunks %>% map(function(.x) unique(effects_on_bins$covariate)[.x] )
plot_ids <- 1:length(plot_chunks)
# -------------------------------------------------------------------------------------------- #
map2(plot_chunks, plot_ids, function(.x, .y){
  
  plot_effects_on_covars_binned(dta = effects_on_bins, 
                                filter_covariate_type = c(covariate %in% .x),
                                ci_label_str = '99% CI', 
                                ci_level = qnorm(p = 1 - (0.01/2)), 
                                y_axis_title = 'Average Treatment Effects', 
                                title_str = NULL,
                                subtitle_str = NULL,
                                decimal_place_y = 0.001)
  
  figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_covars_', 'bins_', '0', .y, bootstrap_by_tracts, '.pdf')
  
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_on_covars_bins', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #
# Effects on integer covariates. 
# -------------------------------------------------------------------------------------------- #

effects_on_ints <- empirical_estimates$effects_on_int_covars

# Join clean variable names for plots.  

effects_on_ints <- effects_on_ints %>% 
  
  left_join(tidy_covar_name_keys, by = 'covariate')

# Convert to factor variables variable names and variable categories.
# This is done for plotting.  
effects_on_ints <- effects_on_ints %>%
  
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
# effects_on_ints$term <- factor(effects_on_ints$term, levels = unique(effects_on_ints$term))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
n_covars <- length(unique(effects_on_ints$covariate)); n_covars
plot_chunks <- furrr:::make_chunks(n_x = n_covars, chunk_size = 1) # Originally 4 for facets. 
plot_chunks <- plot_chunks %>% map(function(.x) unique(effects_on_ints$covariate)[.x] )
plot_ids <- 1:length(plot_chunks)
# -------------------------------------------------------------------------------------------- #
map2(plot_chunks, plot_ids, function(.x, .y){
  
  
  plot_effects_on_int_covars(dta = effects_on_ints, 
                             filter_covariate_type = c(covariate %in% .x),
                             ci_level = qnorm(p = 1- (0.01/2) ), 
                             ci_label_str = '99% CI',
                             y_axis_title = 'Average Treatment Effects', 
                             title_str = NULL,
                             subtitle_str = NULL,
                             decimal_place_y = 0.001)
  
  figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_covars_', 'int_', '0', .y, bootstrap_by_tracts, '.pdf')
  
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_on_covars_bins', bootstrap_by_tracts), # Saves to directories and subdirectories. 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #
# Regression of tau on normalized covariates. 
# -------------------------------------------------------------------------------------------- #
effects_on_covars <- empirical_estimates$effects_on_norm_covars %>%
  
  left_join(tidy_covar_name_keys, by = 'covariate') %>%
  
  mutate(across(.cols = c(covariate_type, covariate_cat), # tidy the list names. 
                .fn = ~str_to_title(str_replace_all(., '_', ' ')))) %>%
  
  arrange(estimate) %>% 
  
  mutate(across(.cols = tidy_name, .fn = ~factor(., levels = .))) 

# Force Socioeconomic variables to be shown first in the figure, followed by Economic Geography predictors. 
effects_on_covars$covariate_type <- factor(effects_on_covars$covariate_type, 
                                           levels = str_to_title(str_replace_all(covar_broad_cats, '_', ' ')))
# -------------------------------------------------------------------------------------------- #
# Land use filter. 
land_use_sel <- effects_on_covars %>% filter(covariate_cat == 'Land Use') %>% distinct(term) %>% 
  filter(!grepl('developed', term))
# -------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------- #
# Plots
map('socioeconomics', function(.x){
  
  plot_effects_on_covars_norm(dta = effects_on_covars, 
                              filter_covariate_type = c(grepl('Socioeconomics', covariate_type) & 
                                                          !grepl('^age_|total_population', covariate)),
                              ci_label_str = '99% CI', 
                              ci_level = qnorm(1 - (0.01/2)),
                              title_str = NULL, 
                              subtitle_str = NULL,
                              decimal_place_y = 0.0001)
  
  figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_covars_', 'norm', '_', .x, bootstrap_by_tracts, '.pdf')
  # Saves to directories and subdirectories. 
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_on_covars_norm', bootstrap_by_tracts), 
                    figname), 
         width = 9, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #
map('economic_geography', function(.x){
  
  plot_effects_on_covars_norm(dta = effects_on_covars, 
                              filter_covariate_type = c(!grepl('Fixed Effects|Retail|Roads', covariate_cat) & 
                                                          !grepl('Socioeconomics', covariate_type) &
                                                          !(covariate %in% land_use_sel$term) ),
                              ci_label_str = '99% CI', 
                              ci_level = qnorm(1 - (0.01/2)),
                              title_str = NULL, 
                              subtitle_str = NULL,
                              decimal_place_y = 0.0001)
  
  figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_covars_', 'norm', '_', .x, bootstrap_by_tracts, '.pdf')
  # Saves to directories and subdirectories. 
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_on_covars_norm', bootstrap_by_tracts), 
                    figname), 
         width = 9, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #

# Tau on dollar store counts and entries. 

# -------------------------------------------------------------------------------------------- #
effects_on_dsvars <- empirical_estimates$effects_on_dsvars_fact 

# Create a tidy name column for plots. 
tidy_name <- tidy_covar_names(covar_name_str = empirical_estimates$effects_on_dsvars_fact$covariate); unique(tidy_name)

# -------------------------------------------------------------------------------------------- #
# Add the tidy_name to a new column and remove Bins and Ints indicating Binned and Integer/Factor type variables.  
ds_vars_clean_str <- c('Bins' = '(Bins)', 'Int' = '', '\\(Net\\) \\(Bins\\)' = '(Net, Bins)', '\\(Gross\\) \\(Bins\\)' = '(Gross, Bins)')

effects_on_dsvars <- effects_on_dsvars %>% 
  
  mutate(tidy_name = str_trim(str_replace_all(tidy_name, ds_vars_clean_str), side = 'right'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
effects_on_dsvars$tidy_name <- factor(effects_on_dsvars$tidy_name, 
                                      levels = unique(effects_on_dsvars$tidy_name))
# -------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------- #
# Plots 
ds_outcomes_str <- as.character(unique(effects_on_dsvars$tidy_name))
ds_outcomes_type_str <- c(rep('Dollar Stores', 2), 
                          rep('Dollar Store Entries', 2), 
                          rep('Dollar Store Entries (Gross)', 2), 
                          rep('Dollar Store Entries (Net)', 2))

map2(ds_outcomes_str, ds_outcomes_type_str,
     function(.x, .y){
       
       plot_effects_on_dsvars(dta = effects_on_dsvars, 
                              filter_covariate_type = c(tidy_name == .x),
                              ci_label_str = '99% CI', 
                              ci_level = qnorm(1 - (0.01/2)),
                              x_axis_title = .y, 
                              y_axis_title = 'Average Treatment Effects', 
                              title_str = NULL, 
                              subtitle_str = NULL,
                              decimal_place_y = 0.001)
       
       bin_str <- str_to_lower(as.character(str_extract_all(.x, 'Bins', simplify = TRUE)))
       ds_outcome_type_str_replace = c('\\(' = '', 
                                       '\\)' = '', 
                                       ' ' = '_')
       
       figname = paste0(str_to_lower(model_geography), '_', 
                        model_dep_var, 
                        '_effects_on_', 
                        paste(str_replace_all(str_to_lower(.y), ds_outcome_type_str_replace), bin_str, sep = '_'), 
                        bootstrap_by_tracts, '.pdf')
       
       # Saves to directories and subdirectories. 
       ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_on_dollar_stores', bootstrap_by_tracts), 
                         figname), 
              width = 9, height = 6, unit = 'in', dpi = 600)
       
     })
# -------------------------------------------------------------------------------------------- #
# Effects by region and division. 
# -------------------------------------------------------------------------------------------- #
# Region
# -------------------------------------------------------------------------------------------- #
effects_by_region <- empirical_estimates$effects_by_region

# label_levels <- paste('Intercept:', c('West', 'Northeast', 'Midwest', 'South'))

# Replace characters until the first underscore _. This converts either REGION_NAME or DIVISION_NAME to the column name 'tidy_name'
effects_by_region <- effects_by_region %>% 
  
  rename_with(.cols = matches('_NAME$', ignore.case = FALSE), 
              .fn = ~str_to_lower(str_replace_all(., pattern = '[[A-Z]]+(?=_)',
                                                  replacement = 'tidy'))) %>%
  arrange(desc(estimate)) %>%
  
  mutate(across(.cols = tidy_name, .fn = ~factor(., unique(.)) ) )

  # Change the text of label and convert to factor using label_levels as the factor levels. 
  # mutate(label = str_trim(str_replace_all(label, c('Effects relative to' = 'Intercept:', 'Region' = '')), 
  #                         side = 'right')) %>%
  # mutate(label = factor(label, 
  #                       levels = label_levels))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
plot_effects_vs_region_division(dta = effects_by_region, 
                                ci_label_str = '99% CI', 
                                ci_level = qnorm(1 - (0.01/2)),
                                reg_div_str = 'Region', 
                                decimal_place_y = 0.001,
                                title_str = NULL, 
                                subtitle_str = NULL)

figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_by_region', bootstrap_by_tracts, '.pdf')
# Saves to directories and subdirectories. 
ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_by_region_and_division', bootstrap_by_tracts), 
                  figname), 
       width = 9, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Division
# -------------------------------------------------------------------------------------------- #
effects_by_division <- empirical_estimates$effects_by_division

# label_levels <- paste('Intercept:', rev(us_divisions_key$DIVISION_NAME))
# Replace characters until the first underscore _. This converts either REGION_NAME or DIVISION_NAME to tidy_name
effects_by_division <- effects_by_division %>% 
  
  rename_with(.cols = matches('_NAME$', ignore.case = FALSE), 
              .fn = ~str_to_lower(str_replace_all(., pattern = '[[A-Z]]+(?=_)',
                                                  replacement = 'tidy'))) %>%
  arrange(desc(estimate)) %>%
  
  mutate(across(.cols = tidy_name, .fn = ~factor(., unique(.)) ) )

  # Change the text of label and convert to factor using label_levels as the factor levels. 
  # mutate(label = str_trim(str_replace_all(label, c('Effects relative to' = 'Intercept:', 
  #                                                  'Division' = '')), 
  #                         side = 'right')) %>%
  # mutate(label = factor(label, 
  #                       levels = label_levels))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
plot_effects_vs_region_division(dta = effects_by_division, 
                                reg_div_str = 'Division', 
                                ci_label_str = '99% CI', 
                                ci_level = qnorm(1 - (0.01/2)),
                                decimal_place_y = 0.001,
                                title_str = NULL,
                                subtitle_str = NULL)

figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_by_division', bootstrap_by_tracts, '.pdf')
# Saves to directories and subdirectories. 
ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('effects_by_region_and_division', bootstrap_by_tracts), 
                  figname), 
       width = 9, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #

ds_vars_on_time <- empirical_estimates$ds_vars_on_time

tidy_name = tidy_covar_names(ds_vars_on_time$outcome) # tidy_covar_names() is found in Functions directory. 

ds_vars_on_time$tidy_name <- factor(tidy_name, levels = unique(tidy_name))

ds_vars_on_time$label <- factor(ds_vars_on_time$label, levels = unique(ds_vars_on_time$label))

ds_vars_on_time$term <- as.numeric(ds_vars_on_time$term) # Convert relative time and year to numeric vector. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #

# Plots. 
ds_type <- rep(as.character(unique(ds_vars_on_time$tidy_name)), 2)
reps_time_type <- length(unique(ds_vars_on_time$tidy_name))
time_type <- c(rep('Average by Year', reps_time_type), rep('Average by Relative Time', reps_time_type))


map2(time_type, ds_type, function(.x, .y){ 
  
  plot_ds_counts_entries_on_time(dta = ds_vars_on_time, 
                                 filter_covariate_type = c(label == .x & tidy_name == .y),
                                 ci_label_str = '99% CI', 
                                 ci_level = qnorm(1 - (0.01/2)), 
                                 x_axis_title = str_remove_all(.x, 'Average by '),
                                 y_axis_title = paste(.y),
                                 title_str = NULL, 
                                 subtitle_str = NULL,
                                 decimal_place_y = 0.01)
  
  figname = paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 
                   str_replace_all(str_to_lower(.y), c(' ' = '_', '\\(|\\)' = '')), '_', # Replaces spaces with '_' and deletes '(' ')' around cumulative.
                   str_replace_all(str_to_lower(.x), ' ', '_'), 
                   bootstrap_by_tracts, '.pdf')
  
  # Saves to directories and subdirectories. 
  ggsave(here::here('Analysis_Supplementary_w_Superettes', 'Figures', dir_dep_var, model_geography, paste0('dollar_store_growth_by_time', bootstrap_by_tracts), 
                    figname), 
         width = 8, height = 6, unit = 'in', dpi = 600)
  
})
# -------------------------------------------------------------------------------------------- #