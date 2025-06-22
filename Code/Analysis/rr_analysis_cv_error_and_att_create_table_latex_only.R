pacman::p_load('here', 'dplyr', 'purrr', 'stringr', 'tidyr')
options(scipen=999)

# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap.
model_geography_vec <- c('Urban', 'Rural') # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level.
# -------------------------------------------------------------------------------------------- #

# Load the optimal estimated model following tuning/training. 

# -------------------------------------------------------------------------------------------- #

nobs <- model_geography_vec %>%
  
  map(function(.x){
    
    filename <- paste0('xgboost_10m_', 
                       str_to_lower(.x), '_', 
                       model_dep_var, '_', 
                       'final.rds')
    dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
    dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title # For plot titles (below). e.e., Low Access
    model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
    col_name <- .x
    df <- data.frame(format(nrow(model_output$data_cf_preds), big.mark = ',' ))
    colnames(df) <- col_name
    
    return(df)
    
    })

nobs <- list_cbind(nobs)
nobs <- bind_cols(
  data.frame('Parameter' = 'Observations'), 
  nobs)
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


table_att <- bind_rows(table_att, nobs)


# Create the LaTeX table
library(kableExtra)
kable(table_att, 
      format = "latex", 
      align = 'lcc', 
      booktabs = TRUE,
      escape = TRUE, # Note: This is the default.
      digits = 4,
      position = '!h',
      linesep = '', # Remove \addlinespace every 5 rows. 
      centering = TRUE, # Note: This is the default. 
      col.names = colnames(table_att),
      caption = paste0("Average Treatment Effects of Dollar Store Entry on Low-Access Status, 
                       Block Groups Receiving a Dollar Store from 2006-2020")) %>% 
  kable_styling() %>%
  footnote(general = paste0(
    "Notes: Bootstrapped standard errors are in parentheses. ",
    "*$p<0.10$, **$p<0.05$, ***$p<0.01$. ",
    "$\\text{ATT} (\\%)$ gives the \\text{ATT} as a percentage of ",
    "the estimated proportion of low access block groups in the absence ",
    "of dollar store entry over the observed time horizon. ", 
    "See main text for distinction between urban and rural block groups. ",
    "Observation counts give the number of treated block-group-year observations in the urban and rural samples."
  ),
  general_title = '', 
  threeparttable = TRUE, 
  escape = FALSE, 
  footnote_as_chunk = TRUE)
