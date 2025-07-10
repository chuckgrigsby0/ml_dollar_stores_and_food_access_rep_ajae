# ----------------------------------- #
# Compare ATTs using different definitions of geography. 
# ----------------------------------- #
pacman::p_load('here', 'dplyr', 'stringr', 'tidyr', 'purrr')
model_dep_var = 'low_access' # Used in script below. 
model_geography = 'Urban' # Used in script below to subset by either Urban or Rural.
options(scipen = 999)

boot_dir <- here::here('Analysis', 
                       paste0(model_geography, '_Bootstrap'), 
                       'Low_Access', 
                       'bootstrap_ruca_urban_rural_comparison')
boot_files <- list.files(boot_dir)

boot_dta <- boot_files %>% 
  
  map_dfr(function(.x){
    
    readRDS(here::here(boot_dir, .x))
  })



boot_dta_wd <- boot_dta %>% 
  
  pivot_wider(names_from = statistic,
              values_from = value) 

stat_cols <- c('actual_avg', 'preds_avg', 
               'att_avg', 'pct_att', 'err_avg', 'pct_err')

sd_boot <- boot_dta_wd %>% 
  
  group_by(Geography, Geography_Lopez, Parameter) %>%
  
  summarise(
    across(
      .cols = all_of(stat_cols), 
      .fns = list('sd' = \(x) sd(x, na.rm = TRUE)), 
      .names = '{.col}'
    )
  )

sd_boot_lng <- sd_boot %>%
  
  pivot_longer(cols = all_of(stat_cols), 
               names_to = 'statistic', 
               values_to = 'sd') %>%
  filter(!is.na(sd))



emp_err_att <- boot_dta %>% 
  filter(boot_id == 0) 


emp_err_att <- emp_err_att %>%
  
  left_join(
    
    sd_boot_lng, by = c('Geography', 'Geography_Lopez', 'Parameter', 'statistic')
    
  )

emp_err_att <- emp_err_att %>%
  
  group_by(Geography, Geography_Lopez, Parameter) %>%
  
  mutate(ci = case_when(
    (value - sd * qnorm(0.995) > 0 | value + sd * qnorm(0.995) < 0) ~ '***' , 
    (value - sd * qnorm(0.975) > 0 | value + sd * qnorm(0.975) < 0) ~ '**', 
    (value - sd * qnorm(0.95) > 0 | value + sd * qnorm(0.95) < 0) ~ '*', 
    TRUE ~ '')
  ) %>% 
  
  # Then format value column based on statistic type
  mutate(formatted_value = case_when(
  
    # For att/pct_att and err/pct_err averages, add CI asterisks
    grepl("^(att|err)_avg$", statistic) ~ paste0(sprintf("%.3f", value), ci),
    grepl("^(pct_att|pct_err)$", statistic) ~ paste0(sprintf("%.3f", value * 100), ci), 
    
    # For all other values, just round
    TRUE ~ sprintf("%.3f", value)
  ), 
  formatted_sd = case_when( 
    
    # For att/pct_att and err/pct_err averages, add CI asterisks
    grepl("^(att|err)_avg$", statistic) ~ paste0("(", sprintf("%.3f", sd), ")"), 
    grepl("^(pct_att|pct_err)$", statistic) ~ paste0("(", sprintf("%.3f", sd * 100), ")"), 
    # For all other values, just round
    TRUE ~ paste0("(", sprintf("%.3f", sd), ")")
    )) %>%
  select(-value, -sd, -ci) %>%
  rename_with(.cols = matches('^formatted'), 
              .fn = ~str_replace_all(., c('formatted_' = '')))
  
emp_err_att_lng <- emp_err_att %>%
  
  pivot_wider(names_from = 'statistic', 
              values_from = c('value', 'sd')) %>% 
  
  pivot_longer(cols = value_actual_avg:sd_pct_err, 
               names_to = 'statistic', 
               values_to = 'estimate') %>% 
  
  filter(!is.na(estimate) & !is.na(Geography_Lopez)) %>%
  
  select(-boot_id) %>%
  
  separate_wider_regex(cols = statistic, 
                       patterns = c(stat = 'value|sd', '_', 
                                    outcome = 'actual_avg|preds_avg|att_avg|pct_att|pct_err|err_avg' 
                                    ),
                       cols_remove = FALSE)

emp_err_att_lng <- emp_err_att_lng %>%
  
  mutate(
    stat = str_replace_all(stat, c('value' = 'avg')), 
    Parameter = factor(Parameter, levels = c('CV Error', 'ATT')), 
    outcome = factor(outcome, 
                     levels = c("actual_avg",  "preds_avg", 
                                "err_avg", "pct_err", 
                                "att_avg", "pct_att")
                     )
  ) %>%
  mutate(stat = factor(stat, levels = c('avg', 'sd'))) 
  

emp_err_att_lng %>%
  
  arrange(Geography, Geography_Lopez, Parameter, outcome) %>% 
  
  select(-statistic) %>% 
  
  mutate(outcome = as.character(outcome)) %>%
  
  mutate(outcome = str_replace_all(outcome, 
                                   c('err_avg' = 'avg', 
                                     'att_avg' = 'avg', 
                                     'pct_err' = 'pct', 
                                     'pct_att' = 'pct'))
         ) %>%
  
  pivot_wider(
    names_from = Parameter, 
    values_from = estimate, 
    values_fill = '-'
  ) %>% 
  
  View()