pacman::p_load('here', 'dplyr', 'purrr', 'stringr')

rootname <- 'average_change_in_outcomes_relative_time_lhs.rds'
urban_att_lhs <- readRDS(here::here('Analysis', 'Tables', 'Low_Access', 'Urban', paste0('urban_', rootname) ) )
rural_att_lhs <- readRDS(here::here('Analysis', 'Tables', 'Low_Access', 'Rural', paste0('rural_', rootname) ) )

att_lhs <- bind_rows(urban_att_lhs, rural_att_lhs) %>% rename(estimate = estiamte)


# Compute confidence intervals based on bootstrapped SDs. 
alpha_levels <- c(0.01, 0.05, 0.10)
ci_thresholds <- map(alpha_levels, function(.x) qnorm(p = 1 - .x/2) ); ci_thresholds
ci_thresholds <- set_names(ci_thresholds, nm = c('one', 'five', 'ten'))

att_lhs <- att_lhs %>%
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
table_att_lhs <- att_lhs %>% 
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

source(here::here('Code', 'Analysis', 'Tables_CV_Error_and_ATT', 'rr_analysis_cv_error_and_att_create_table.R'))
table_att_comb <- bind_cols(table_att, select(table_att_lhs, Urban, Rural))


# Create the LaTeX table
library(kableExtra)
kable(table_att_comb, 
      format = "latex", 
      align = 'lcccc', 
      booktabs = TRUE,
      escape = TRUE, # Note: This is the default.
      digits = 4,
      position = '!h',
      linesep = '', # Remove \addlinespace every 5 rows. 
      centering = TRUE, # Note: This is the default. 
      col.names = c('Parameter', rep(c('Urban', 'Rural'), 2) ),
      caption = paste0("Average Treatment Effects of Dollar Store Entry on Low Food Access Status with 
                       Latin Hypercube Sampling for Tuning Hyperparameters, Block Groups Receiving a Dollar Store from 2006-2020")) %>% 
  kable_styling() %>%
  add_header_above(c('', 'BO' = 2, 'LHS' = 2)) %>%
  footnote(general = paste0(
    "Notes: Bootstrapped standard errors are in parentheses. ",
    "*$p<0.10$, **$p<0.05$, ***$p<0.01$. ",
    "$\\text{ATT} (\\%)$ gives the \\text{ATT} as a percentage of ",
    "the estimated proportion of low access block groups in the absence ",
    "of dollar store entry over the observed time horizon."
  ),
  general_title = '', 
  threeparttable = TRUE, 
  escape = FALSE, 
  footnote_as_chunk = TRUE)