# ----------------------------------- #
# Compare ATTs using different definitions of geography. 
# ----------------------------------- #
pacman::p_load('dplyr', 'purrr', 'tidyr', 'stringr', 'readxl', 'here')
model_dep_var = Sys.getenv('model_dep_var') 
model_geography = Sys.getenv("model_geography")
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_id))
options(scipen = 999)

if (bootstrap_id == 0){
  # -------------------------------------------------------------------------------------------- #
  # Load the optimal estimated model following tuning/training. 
  # -------------------------------------------------------------------------------------------- #
  filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
  dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title 
  # -------------------------------------------------------------------------------------------- #
  model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
  # -------------------------------------------------------------------------------------------- #
  
} else if (bootstrap_id > 0){
  
  model_dir = paste0(str_to_lower(model_geography), '_', model_dep_var, '_bootstrap_', bootstrap_id, '.rds')
  
  model_output <- readRDS(here::here('Analysis', 
                                     paste0(model_geography, '_Bootstrap'), 
                                     'Low_Access', 
                                     'bootstrap_01_499_tracts', 
                                     model_dir)
                          )

}
# ----------------------------------- #
# Compute ATTs using Rural-Urban definitions from Lopez et al. 
# ----------------------------------- #
cf_preds <- model_output %>% 
  pluck('data_cf_preds') %>%
  mutate(GEOID_TRACT = str_sub(GEOID, end = 11) ) %>%
  rename(preds = pred_class_cf)
# ----------------------------------- #
# Load RUCA Codes for 2010 from USDA. 
# ----------------------------------- #
ruca <- readxl::read_xlsx(path = here::here('Data', 'ruca2010revised.xlsx'), sheet = 'Data', skip = 1)

colnames(ruca) <- c('county_fips', 'state_name', 'county_name', 'GEOID_TRACT', 
                    'ruca_primary', 'ruca_secondary', 'population', 'land_area', 'pop_density')
# ----------------------------------- #

cf_preds <- cf_preds %>%
  
  left_join(
    select(ruca, GEOID_TRACT, ruca_primary, ruca_secondary), 
    by = 'GEOID_TRACT', 
    relationship = 'many-to-one', 
    multiple = 'first'
  ) %>%
    # 4-10 is defined as rural in Lopez et al. However, some of these tracts
    # belong to urban clusters, where we place urban clusters in urban areas. 
  mutate(
    Geography_Lopez = case_when(
      ruca_primary %in% seq(1, 3, 1) ~ 'Urban', 
      ruca_primary %in% seq(4, 10, 1) ~ 'Rural', 
      TRUE ~ NA
    )
  )
# ----------------------------------- #
# Summarize ATTs by urban-rural definitions from Lopez et al. 
# ----------------------------------- #
att_summary_urban_rural <- cf_preds %>%
  
  group_by(Geography_Lopez) %>%
  
  summarise(across( 
    .cols = c(actual, preds, tau), 
    .fns = list('avg' = \(x) mean(x, na.rm = TRUE)), 
    .names = "{str_replace_all(.col, c('tau' = 'att'))}_{.fn}"
    )) %>%
  
  ungroup() %>%
  
  mutate(pct_att = att_avg/preds_avg, 
         Geography = model_geography, 
         boot_id = bootstrap_id, 
         Parameter = 'ATT') %>%
  relocate(Geography, .before = Geography_Lopez) %>%
  
  pivot_longer(cols = c(actual_avg:pct_att), 
               names_to = 'statistic', 
               values_to = 'value')

# ----------------------------------- #

# Repeat for CV Errors

# ----------------------------------- #

cv_preds <- model_output %>% 
  pluck('cv_errors_opt') %>%
  rename(actual = low_access, 
         preds = cv_preds) %>%
  filter(!is.na(preds)) %>%
  mutate(GEOID_TRACT = str_sub(GEOID, end = 11), 
         err = actual - preds)
  

cv_preds <- cv_preds %>%
  
  left_join(
    select(ruca, GEOID_TRACT, ruca_primary, ruca_secondary), 
    by = 'GEOID_TRACT', 
    relationship = 'many-to-one', 
    multiple = 'first'
  ) %>%
  # 4-10 is defined as rural in Lopez et al. However, some of these tracts
  # belong to urban clusters, where we place urban clusters in urban areas. 
  mutate(
    Geography_Lopez = case_when(
      ruca_primary %in% seq(1, 3, 1) ~ 'Urban', 
      ruca_primary %in% seq(4, 10, 1) ~ 'Rural', 
      TRUE ~ NA
    )
  )

# Summarize CV Errors. 
err_summary_urban_rural <- cv_preds %>%
  
  group_by(Geography_Lopez) %>%
  
  summarise(across( 
    .cols = c(actual, preds, err), 
    .fns = list('avg' = \(x) mean(x, na.rm = TRUE)), 
    .names = "{.col}_{.fn}"
  )) %>%
  
  ungroup() %>%
  
  mutate(pct_err = err_avg/preds_avg, 
         Geography = model_geography, 
         boot_id = bootstrap_id, 
         Parameter = 'CV Error') %>%
  relocate(Geography, .before = Geography_Lopez) %>%
  
  pivot_longer(cols = c(actual_avg:pct_err), 
               names_to = 'statistic', 
               values_to = 'value')


err_and_att_summary <- bind_rows(att_summary_urban_rural, err_summary_urban_rural)

fname <- paste0('bootstrap_ruca_comparison_', str_to_lower(model_geography), '_', bootstrap_id, '.rds')
saveRDS(err_and_att_summary, here::here('Analysis', 
                                        paste0(model_geography, '_Bootstrap'), 
                                        'Low_Access', 
                                        'bootstrap_ruca_urban_rural_comparison', 
                                        fname)
)
