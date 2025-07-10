# Script creates lookup table showing the total number of actual and predicted low-access block groups in 2020, referenced in main text. 
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
library(pacman)
pacman::p_load('dplyr', 'stringr', 'tidyr', 'purrr')
# -------------------------------------------------------------------------------------------- #
model_geography = 'Urban' # Run for urban results since null rural results in main text. 
model_dep_var = 'low_access'
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
posttr_preds <- model_output %>% pluck('data_cf_preds') %>%
  rename(preds = pred_class_cf)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# ATTs and total actual and predicted low-access by year. 
# Note that year == 2020 would contain treatment effects for all cohorts. 
# -------------------------------------------------------------------------------------------- #
boot_id = 0

avg_efx_year <- posttr_preds %>%
  
  group_by(year) %>% 
  
  summarise(across(.cols = c(tau, actual, preds), 
                   .fns = list('total' = \(x) sum(x), 
                               'avg' = \(x) mean(x)), 
                   .names = '{.col}_{.fn}') )

# -------------------------------------------------------------------------------------------- #

avg_efx_year <- avg_efx_year %>% 
  mutate(iter = boot_id) %>% 
  relocate(iter)


# Load bootstrapped estimates. 

dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

new_dir <- 'bootstrap_total_new_grocers_cf'

dir_path <- here::here('Analysis', dir_geography, dir_dep_var, new_dir)

boot_data <- seq(1, 499, 1) %>%
  
  map(function(.x){
    
    filename <- paste0(str_to_lower(model_geography), '_',
                       model_dep_var, '_',
                       new_dir, '_', 
                       .x, '.rds')
    
    return(readRDS(here::here(dir_path, filename) ) )
    
  })

boot_data <- bind_rows(avg_efx_year, boot_data)

sd_boot_data <- boot_data %>%
  
  select(-iter) %>%
  
  group_by(year) %>%
  
  summarise(across(.cols = where(is.numeric), 
                   .fns = \(x) sd(x), 
                   .names = '{.col}_sd') )


avg_efx_year <- avg_efx_year %>%
  left_join(sd_boot_data, by = 'year') %>%
  select(iter, sort(names(.) ) )


total_grocers <- avg_efx_year %>% 
  filter(year == 2020) %>%
  select(matches('^actual_total|^preds_total|tau_total|year'))

pivot_cols <- str_subset(names(total_grocers), 'year', negate = TRUE); pivot_cols

total_grocers <- total_grocers %>%
  pivot_longer(cols = all_of(pivot_cols), 
               names_to = c('variable', 'statistic'),
               names_pattern = '(.+?)(?:_(sd))?$',
               values_to = 'total') %>%
  mutate(statistic = if_else(statistic == '', 'total', statistic))


alpha_levels <- c(0.01, 0.05, 0.10)
critical_values <- qnorm(p = 1 - alpha_levels/2)

total_grocers_tidy_ci <- total_grocers %>%
  group_by(variable) %>%
  mutate(
    CI_lower_01 = case_when(
      statistic == "avg" ~ total - critical_values[1] * lead(total),
      TRUE ~ NA_real_
    ),
    CI_upper_01 = case_when(
      statistic == "avg" ~ total + critical_values[1] * lead(total),
      TRUE ~ NA_real_
    ), 
    CI_lower_05 = case_when(
      statistic == "total" ~ total - critical_values[2] * lead(total),
      TRUE ~ NA_real_
    ),
    CI_upper_05 = case_when(
      statistic == "total" ~ total + critical_values[2] * lead(total),
      TRUE ~ NA_real_
    ),
    
    CI_lower_10 = case_when(
      statistic == "total" ~ total - critical_values[3] * lead(total),
      TRUE ~ NA_real_
    ),
    CI_upper_10 = case_when(
      statistic == "total" ~ total + critical_values[3] * lead(total),
      TRUE ~ NA_real_
    ),
    
    Significance = case_when(
      statistic == "total" & variable == 'tau_total' & (CI_lower_01 > 0 | CI_upper_01 < 0) ~ "***",
      statistic == "total" & variable == 'tau_total' & (CI_lower_05 > 0 | CI_upper_05 < 0) ~ "**",
      statistic == "total" & variable == 'tau_total' & (CI_lower_10 > 0 | CI_upper_10 < 0) ~ "*",
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    total = round(total, digits = 4) 
  ) %>% 
  mutate(
    total = case_when(
      statistic == "total" ~ paste0(format(total, scientific = FALSE, trim = TRUE), Significance),
      TRUE ~ as.character(total)
    )
  ) %>%
  mutate(
    total = case_when(
      statistic == "sd" ~ paste0('(', trimws(format(total, scientific = FALSE, trim = TRUE)), ')'),
      TRUE ~ as.character(total)
    )
  ) %>%
  select(-matches('^CI_'), -Significance) 

total_grocers_tidy_ci <- total_grocers_tidy_ci %>%
  select(-year) 

total_grocers_tidy_ci <- total_grocers_tidy_ci %>%
  filter(!(statistic == "sd" & variable %in% c("preds_total", "actual_total")))

total_grocers_tidy_ci <- total_grocers_tidy_ci %>% 
  group_by(variable) %>%
  mutate(
    variable = if_else(statistic == "sd", "", variable) 
  ) %>%
  select(-statistic)


names(total_grocers_tidy_ci) <- str_replace_all(names(total_grocers_tidy_ci), 
                                                c('variable' = 'Parameter', 
                                                  'total' = 'Estimate'))

total_grocers_tidy_ci$Parameter <- str_replace_all(total_grocers_tidy_ci$Parameter, 
                                                  c('actual_total' = 'Total low access, post entry (observed)', 
                                                    'preds_total' = 'Total low access, post entry (predicted)', 
                                                    'tau_total' = 'Difference low access'))


total_grocers_tidy_ci

