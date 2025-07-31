# Helper script used to combine average cv error and att from main model with supplementary model results, including LHS and Random Forest. 
# -------------------------------------------------------------------------------------------- #
pacman::p_load('here', 'dplyr', 'purrr', 'stringr')
# -------------------------------------------------------------------------------------------- #
rootname <- 'average_cv_error_and_att_relative_time.rds'
urban_att <- readRDS(here::here('Analysis', 'Tables', 'Low_Access', 'Urban', paste0('urban_', rootname) ) )
rural_att <- readRDS(here::here('Analysis', 'Tables', 'Low_Access', 'Rural', paste0('rural_', rootname) ) )

att <- bind_rows(urban_att, rural_att) %>% rename(estimate = estiamte)

# Compute confidence intervals based on bootstrapped SDs. 
alpha_levels <- c(0.01, 0.05, 0.10)
ci_thresholds <- map(alpha_levels, function(.x) qnorm(p = 1 - .x/2) ); ci_thresholds
ci_thresholds <- set_names(ci_thresholds, nm = c('one', 'five', 'ten'))

att <- att %>%
  group_by(Measure, Geography, variable) %>%
  mutate(estimate_lead = dplyr::lead(estimate),
         # Calculate CIs using threshold list values
         CI_lower_01 = case_when(
           Statistic == "Avg" ~ estimate - ci_thresholds$one * estimate_lead,
           TRUE ~ NA_real_
         ),
         CI_upper_01 = case_when(
           Statistic == "Avg" ~ estimate + ci_thresholds$one * estimate_lead,
           TRUE ~ NA_real_
         ),
         CI_lower_05 = case_when(
           Statistic == "Avg" ~ estimate - ci_thresholds$five * estimate_lead,
           TRUE ~ NA_real_
         ),
         CI_upper_05 = case_when(
           Statistic == "Avg" ~ estimate + ci_thresholds$five * estimate_lead,
           TRUE ~ NA_real_
         ),
         CI_lower_10 = case_when(
           Statistic == "Avg" ~ estimate - ci_thresholds$ten * estimate_lead,
           TRUE ~ NA_real_
         ),
         CI_upper_10 = case_when(
           Statistic == "Avg" ~ estimate + ci_thresholds$ten * estimate_lead,
           TRUE ~ NA_real_
         ),
         # Determine significance levels with nested case_when
         Significance = case_when(
           Statistic == "Avg" & (CI_lower_01 > 0 | CI_upper_01 < 0) ~ "***",
           Statistic == "Avg" & (CI_lower_05 > 0 | CI_upper_05 < 0) ~ "**",
           Statistic == "Avg" & (CI_lower_10 > 0 | CI_upper_10 < 0) ~ "*",
           TRUE ~ ""
         )
  ) %>% 
  mutate(across(.cols = where(is.numeric), 
                .fn = \(x) case_when(str_detect(variable, pattern = '^pct') ~ x * 100, 
                                     TRUE ~ x) ) 
  ) %>%
  mutate(
    estimate = round(estimate, digits = 4) 
  ) %>% 
  mutate(
    estimate = case_when(
      Statistic == "Avg" & str_detect(variable, pattern = '^err|^tau|^pct') ~ paste0(format(estimate, scientific = FALSE, trim = TRUE), Significance),
      TRUE ~ as.character(estimate)
    )
  ) %>%
  mutate(
    estimate = case_when(
      Statistic == "SD" ~ paste0('(', trimws(format(estimate, scientific = FALSE, trim = TRUE)), ')'),
      TRUE ~ as.character(estimate)
    )
  ) %>%
  select(-starts_with("CI_"), -Significance, -estimate_lead) %>%
  pivot_wider(names_from = Geography, 
              values_from = estimate) %>%
  filter(!(Statistic == 'SD' & variable %in% c('actual_avg', 'preds_avg') ) )

# --------------------------------- # 
# Create a final table. 
# --------------------------------- # 
table_att <- att %>% 
  mutate(Parameter = case_when( 
    variable == 'actual_avg' & Measure == 'CV Error' ~ 'Proportion low access, pre-entry (observed)', 
    variable == 'preds_avg' & Measure == 'CV Error' ~ 'Proportion low access, pre-entry (predicted)',
    variable == 'err_avg' & Measure == 'CV Error' & Statistic == 'Avg' ~ 'CV Error',
    variable == 'pct_err' & Measure == 'CV Error' & Statistic == 'Avg' ~ 'CV Error (%)',
    
    variable == 'actual_avg' & Measure == 'ATT' ~ 'Proportion low access, post-entry (observed)', 
    variable == 'preds_avg' & Measure == 'ATT' ~ 'Proportion low access, post-entry (predicted)',
    variable == 'tau_avg' & Measure == 'ATT' & Statistic == 'Avg' ~ 'ATT',
    variable == 'pct_att' & Measure == 'ATT' & Statistic == 'Avg' ~ 'ATT (%)', 
    
    TRUE ~ ''
  )
  ) %>% 
  ungroup() %>%
  select(Parameter, Urban, Rural)

names(table_att) <- names(table_att) %>%
  str_replace_all(c('Urban' = 'Urban_Main', 'Rural' = 'Rural_Main'))


print('Sourced: table_att')