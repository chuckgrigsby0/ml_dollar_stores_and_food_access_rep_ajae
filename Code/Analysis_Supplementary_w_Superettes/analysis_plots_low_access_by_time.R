# Load packages
# ----------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# Pre-Treatment data. 
pretr_dta_for_summary <- dta_untreated_wfolds %>%
  
  left_join(select(dta_untreated_non_model_vars, GEOID, year, rel_year, event_year, DS_Count_10mile), by = c('GEOID', 'year'))

# Post-Treatment data. 
postr_dta_for_summary <- dta_treated %>%
  
  left_join(select(dta_treated_non_model_vars, GEOID, year, rel_year, event_year, DS_Count_10mile), by = c('GEOID', 'year'))

# Combined pre- and post-treatment data. 
dta_for_summary <- bind_rows(pretr_dta_for_summary, postr_dta_for_summary)

# -------------------------------------------------------------------------------------------- #
summary_by_year <- dta_for_summary %>%
  
  mutate(treatment = case_when(event_year == 0 ~ 'Never-Treated', 
                                   event_year > 0 ~ 'Treated')) %>%
  
  group_by(year, treatment) %>%
  
  summarise(across(.cols = all_of(model_dep_var), 
                   
                   .fns = c(Total = sum, Share = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(year = as.numeric(year)) # Convert to numeric for plots. 
# -------------------------------------------------------------------------------------------- #

names(summary_by_year) <- str_to_title(str_replace_all(names(summary_by_year), '_', '-'))

# -------------------------------------------------------------------------------------------- #
summary_by_year <- summary_by_year %>% 
  
  pivot_longer(cols = c('Total-Low-Access', 'Share-Low-Access'), 
               
               names_to = c('Statistic', 'Dependent_Variable'), 
               
               names_sep = '(-)(?=L)', # Separates the first dash from anything starting with L. 
               
               values_to = 'Value') 
# -------------------------------------------------------------------------------------------- #

# Shares 
share = 'Share'
sh_x_yr_plot_coords <- filter(summary_by_year, Statistic == 'Share') %>% ungroup() %>% 
  summarise(across(.cols = Value, .fns = c(Min = min, Max = max), .names = '{.fn}'))

dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' '))

plot_treated_vs_nevertreated_by_year(dta = summary_by_year, 
                                     stat_type = share, 
                                     x_value = Year, 
                                     y_value = Value,
                                     group_var = Treatment,
                                     y_axis_title = paste(dep_var_title, paste0('(', share, ')'), sep = ' '),
                                     x_axis_title = 'Year', 
                                     plot_title = paste(dep_var_title, 'by Treated and Untreated Units', sep = ' '),
                                     plot_subtitle = paste('Geography:', model_geography, 'Block Groups', sep = ' '),
                                     decimal_place_y = 0.001,
                                     y1_lim = sh_x_yr_plot_coords$Min, 
                                     y2_lim = sh_x_yr_plot_coords$Max, 
                                     x_intercept_val = NULL)
# -------------------------------------------------------------------------------------------- #

# Totals 
total = 'Total'
tot_x_yr_plot_coords <- filter(summary_by_year, Statistic == 'Total') %>% ungroup() %>% 
  summarise(across(.cols = Value, .fns = c(Min = min, Max = max), .names = '{.fn}'))



plot_treated_vs_nevertreated_by_year(dta = summary_by_year, 
                                     stat_type = total, 
                                     x_value = Year, 
                                     y_value = Value,
                                     
                                     group_var = Treatment,
                                     y_axis_title = paste(dep_var_title, paste0('(', total, ')'), sep = ' '),
                                     x_axis_title = 'Year', 
                                     plot_title = paste(dep_var_title, 'by Treated and Untreated Units', sep = ' '),
                                     plot_subtitle = paste('Geography:', model_geography, 'Block Groups', sep = ' '),
                                     decimal_place_y = 1,
                                     y1_lim = tot_x_yr_plot_coords$Min, 
                                     y2_lim = tot_x_yr_plot_coords$Max, 
                                     x_intercept_val = NULL)
                                     
# -------------------------------------------------------------------------------------------- #
summary_by_relyear <- dta_for_summary %>%
  
  filter(year >= 2007) %>%
  
  mutate(treatment = case_when(event_year == 0 ~ 'Never-Treated', 
                               event_year > 0 ~ 'Treated')) %>%
  
  group_by(rel_year, treatment) %>%
  
  summarise(across(.cols = all_of(model_dep_var), 
                   
                   .fns = c(Total = sum, Share = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  filter(treatment != 'Never-Treated') %>% # When observing data by relative time, we are focused on Yet-to-be-treated. 
  
  mutate(rel_year = as.numeric(rel_year)) # Convert to numeric for plots. 
# -------------------------------------------------------------------------------------------- #
rel_year_filter <- names(summary_by_relyear) != 'rel_year'
names(summary_by_relyear)[rel_year_filter] <- str_to_title(str_replace_all(names(summary_by_relyear)[rel_year_filter], '_', '-'))

# -------------------------------------------------------------------------------------------- #
summary_by_relyear <- summary_by_relyear %>% 
  
  pivot_longer(cols = c('Total-Low-Access', 'Share-Low-Access'), 
               
               names_to = c('Statistic', 'Dependent_Variable'), 
               
               names_sep = '(-)(?=L)', # Separates the first dash from anything starting with L. 
               
               values_to = 'Value') 
# -------------------------------------------------------------------------------------------- #

# Shares 
share = 'Share'
sh_x_yr_plot_coords <- filter(summary_by_relyear, Statistic == 'Share') %>% ungroup() %>% 
  summarise(across(.cols = Value, .fns = c(Min = min, Max = max), .names = '{.fn}'))


plot_treated_vs_nevertreated_by_year(dta = summary_by_relyear, 
                                     stat_type = share, 
                                     x_value = rel_year, 
                                     y_value = Value,
                                     group_var = Treatment,
                                     y_axis_title = paste(dep_var_title, paste0('(', share, ')'), sep = ' '),
                                     x_axis_title = 'Time to Treatment', 
                                     plot_title = paste(dep_var_title, 'by Treated and Untreated Units', sep = ' '),
                                     plot_subtitle = paste('Geography:', model_geography, 'Block Groups', sep = ' '),
                                     decimal_place_y = 0.001,
                                     y1_lim = sh_x_yr_plot_coords$Min, 
                                     y2_lim = sh_x_yr_plot_coords$Max, 
                                     x_intercept_val = 0)


# -------------------------------------------------------------------------------------------- #

# Totals 
total = 'Total'
tot_x_yr_plot_coords <- filter(summary_by_relyear, Statistic == 'Total') %>% ungroup() %>% 
  summarise(across(.cols = Value, .fns = c(Min = min, Max = max), .names = '{.fn}'))


plot_treated_vs_nevertreated_by_year(dta = summary_by_relyear, 
                                     stat_type = total, 
                                     x_value = rel_year, 
                                     y_value = Value,
                                     group_var = Treatment,
                                     y_axis_title = paste(dep_var_title, paste0('(', total, ')'), sep = ' '),
                                     x_axis_title = 'Time to Treatment', 
                                     plot_title = paste(dep_var_title, 'by Treated and Untreated Units', sep = ' '),
                                     plot_subtitle = paste('Geography:', model_geography, 'Block Groups', sep = ' '),
                                     decimal_place_y = 1,
                                     y1_lim = tot_x_yr_plot_coords$Min, 
                                     y2_lim = tot_x_yr_plot_coords$Max, 
                                     x_intercept_val = 0)
