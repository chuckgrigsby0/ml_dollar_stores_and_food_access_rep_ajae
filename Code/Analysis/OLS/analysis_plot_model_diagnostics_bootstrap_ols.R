# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
pacman::p_load('readr', 'here', 'dplyr', 'purrr', 'stringr', 'tidyr', 'ggplot2')
# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap.
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level.
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load bootstrapped data. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_change_in_outcomes', bootstrap_by_tracts, '_ols') # NULL or '_tracts'

boot_data <- seq(0, 499, 1) %>%
  
  map_dfr(function(.iter){ # A single data frame so one can row-bind automatically. 
    
    filename <- paste0('bootstrap_',
                       'change_in_outcomes_ols_',
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

est_data <- boot_data %>% filter(id == 0)


est_data_lng <- est_data %>% 
  
  filter(percentage_type == 'Relative Time') %>%
  
  select(rel_year, actual_avg, preds_avg) %>%
  
  pivot_longer(
    cols = actual_avg:preds_avg, 
    names_to = 'statistic', 
    values_to = 'estimate'
  ) %>%
  
  separate_wider_regex(cols = statistic, 
                       patterns = c('Outcome' = '.*', 
                                    '_', 
                                    '.*'), 
                       cols_remove = FALSE) %>%
  
  mutate(
    across(
      .cols = Outcome, 
      .fns = \(x) str_to_title(x) %>%
        str_replace_all(x, c('Preds' = 'Predicted'))
    )
  ) %>%
  
  mutate(
    across(
      .cols = rel_year, 
      .fns = \(x) as.numeric(as.character(x))
    )
  )
    
# -------------------------------------------------------------------------------------------- #

boot_data_sum <- boot_data %>% 
  
  filter(percentage_type == 'Relative Time') %>%
  
  group_by(rel_year) %>%
  
  summarise(across(
    .cols = c(actual_avg, preds_avg), 
    .fn = list('sd' = \(x) sd(x, na.rm = TRUE)), 
    .names = '{.col}') 
    ) %>% 
  
  pivot_longer(
    cols = actual_avg:preds_avg, 
    names_to = 'statistic', 
    values_to = 'estimate_sd'
  ) %>%
  
  mutate(
    across(
      .cols = rel_year, 
      .fns = \(x) as.numeric(as.character(x))
    )
  )

# -------------------------------------------------------------------------------------------- #

est_data_lng <- est_data_lng %>%
  
  left_join(boot_data_sum, by = c('rel_year', 'statistic') ) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Function that loads multiple functions for plotting model results. 
# Also loads in a color palette for the plots. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_plots_for_model_diagnostics.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
plot_actual_on_predicted(dta = est_data_lng, 
                         y_value = estimate, 
                         ci_level = qnorm(1 - (0.01/2)),
                         standard_error = estimate_sd,
                         y_axis_title = 'Share of Low-Access Block Groups', 
                         x_axis_title = 'Time from Treatment', 
                         # plot_title = 'Cross-Validated Predicted and Actual Outcomes by Time from Treatment', 
                         # plot_subtitle = paste(paste('Geography:', model_geography), paste('Outcome:', dep_var_title), sep = '\n'),
                         plot_title = NULL, 
                         plot_subtitle = NULL,
                         #breaks_y = breaks_errors, 
                         x_intercept_val = 0,
                         decimal_place_y = 0.001, 
                         y1_lim = NULL, 
                         y2_lim = NULL)

dir_dep_var = 'Low_Access'

ggsave(here::here('Analysis', 'Figures', dir_dep_var, model_geography, 
                  paste0('errors_and_preds', bootstrap_by_tracts, '_ols'), # Saves to directories and subdirectories.
                  paste0('preds_vs_cfs_relative_time_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts, '_ols', '.pdf')),
       width = 8, height = 6, unit = 'in', dpi = 600)
# -------------------------------------------------------------------------------------------- #