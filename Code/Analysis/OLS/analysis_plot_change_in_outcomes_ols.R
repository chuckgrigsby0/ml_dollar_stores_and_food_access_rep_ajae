# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
library(readr)
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

# -------------------------------------------------------------------------------------------- #
pretr_vars <- c('actual_avg', 'preds_avg', 'err_avg', 'pct_err')
posttr_vars <- c('actual_avg', 'preds_avg', 'tau_avg', 'pct_att')
# -------------------------------------------------------------------------------------------- #
pretr_sum_boot_data <- boot_data %>%
  filter(percentage_type == 'All' & !is.na(err_avg) ) %>%
  select(-rel_year) %>%
  summarise(across( 
    .cols = all_of(pretr_vars), 
    .fns = \(x) sd(x), 
    .names = '{.col}'
  )) %>%
  mutate(Statistic = 'SD')

pretr_cv_error <- est_data %>%
  filter(percentage_type == 'All' & !is.na(err_avg) ) %>% 
  select(all_of(pretr_vars)) %>%
  mutate(Statistic = 'Avg')

cv_errors <- bind_rows(pretr_cv_error, pretr_sum_boot_data) %>% 
  mutate(Measure = 'CV Error', 
         Geography = model_geography)

cv_errors_long <- cv_errors %>% 
  pivot_longer(
    cols = actual_avg:pct_err,
    names_to = 'variable', 
    values_to = 'estimate'
  ) %>%
  mutate(variable = factor(variable, 
                           levels = c('actual_avg', 
                                      'preds_avg', 
                                      'err_avg', 
                                      'pct_err'))) %>%
  arrange(variable, Statistic)

# -------------------------------------------------------------------------------------------- #
posttr_sum_boot_data <- boot_data %>%
  filter(percentage_type == 'All' & !is.na(tau_avg) ) %>%
  select(-rel_year) %>%
  summarise(across( 
    .cols = all_of(posttr_vars), 
    .fns = \(x) sd(x), 
    .names = '{.col}'
  )) %>%
  mutate(Statistic = 'SD')

posttr_att <- est_data %>%
  filter(percentage_type == 'All' & !is.na(tau_avg) ) %>% 
  select(all_of(posttr_vars)) %>%
  mutate(Statistic = 'Avg')

atts <- bind_rows(posttr_att, posttr_sum_boot_data) %>%
  mutate(Measure = 'ATT', 
         Geography = model_geography)
# -------------------------------------------------------------------------------------------- #
head(atts)

atts_long <- atts %>% 
  pivot_longer(
    cols = actual_avg:pct_att,
    names_to = 'variable', 
    values_to = 'estimate'
  ) %>%
  mutate(variable = factor(variable, 
                           levels = c('actual_avg', 
                                      'preds_avg', 
                                      'tau_avg', 
                                      'pct_att'))) %>%
  arrange(variable, Statistic)

cv_and_att_summary_long <- bind_rows(cv_errors_long, atts_long)
# -------------------------------------------------------------------------------------------- #
filename <- paste0(str_to_lower(model_geography), '_', 'average_change_in_outcomes_relative_time_ols', '.rds')

saveRDS(cv_and_att_summary_long, 
        file = here::here('Analysis', 'Tables', 'Low_Access', model_geography, filename) )
# -------------------------------------------------------------------------------------------- #