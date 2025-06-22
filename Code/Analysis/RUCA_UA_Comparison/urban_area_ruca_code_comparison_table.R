library('pacman')
library('here')
pacman::p_load('dplyr', 'stringr', 'purrr', 'tidyr', 'sf')
# Load and prepare data. 
model_dep_var = 'low_access' # Used in script below. If running for low_access_pers, must change settings below.
# model_geography = 'Urban' # Used in script below to subset by either Urban or Rural.
print(model_dep_var)
options(scipen=999)
# Load data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
dta_untreated <- list(panel_df_ds_entry_2_and_10mile$untreated, # Untreated/Treated dollar store entry data with relative treatment timing. 
               panel_df_access_inds_2_and_10mile, # Low-access indicators
               land_use, 
               covar_df_sel_jn) %>% # Covariates from census/acs. 
  reduce(left_join, by = c('GEOID', 'year')) %>%
  ungroup() %>%
  left_join(retail_counts_2005_2_and_10mile, by = 'GEOID') %>% 
  left_join(park_access, by = 'GEOID') %>%
  left_join(schools, by = 'GEOID') %>%
  left_join(dist_to_urb, by = 'GEOID') %>% 
  left_join(roads, by = 'GEOID') %>%
  left_join(geog_data, by = 'GEOID') %>%
  # --------------------- #
  left_join(fes_state_by_time, by = c('year', 'STATE')) %>%
  # --------------------- #
  mutate(urban_area = if_else(Geography == 'Urbanized', 1, 0), 
         uc_area = if_else(Geography == 'Urban Cluster', 1, 0)) %>%
  mutate(across(.cols = matches('^fe_'), 
                .fns = ~if_else(is.na(.), 0, .)))  

# Columns not needed for imputation models. 
non_model_vars <- c('DS_Count_10mile', 'DS_Count_10mile_diff',
                    'entry', 'entry_events', 'event_year', 'net_entry_cumsum', 'rel_year', 'treat', 
                    'Grocery_Count_10mile', 'Grocery_Count_10mile_diff', 'Grocery_Count_10mile_2005', 'total_low_access', 
                    'STATE', 'market', 'market_name_full', 'Geography')
# Modeling variables (Columns needed for imputation models). 
model_vars <- names(dta_untreated)[!(names(dta_untreated) %in% non_model_vars)]; model_vars
# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates or are located in Puerto Rico, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_ut <- dta_untreated[!complete.cases(dta_untreated), ]  
dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]
# -------------------------------------------------------------------------------------------- #
dta_untreated_non_model_vars <- dta_untreated %>% select(GEOID, year, all_of(non_model_vars))
dta_untreated <- dta_untreated %>% select(all_of(model_vars))
dta_untreated <- dta_untreated %>% left_join(select(dta_untreated_non_model_vars, GEOID, year, event_year), by = c('GEOID', 'year'))
# -------------------------------------------------------------------------------------------- #
# Create the data set of treated observations, as we did for the untreated. 
# -------------------------------------------------------------------------------------------- #
dta_treated <- list(panel_df_ds_entry_2_and_10mile$treated, # Untreated/Treated dollar store entry data with relative treatment timing. 
                    panel_df_access_inds_2_and_10mile, # Low-access indicators
                    land_use, 
                    covar_df_sel_jn) %>% # Covariates from census/acs. 
  reduce(left_join, by = c('GEOID', 'year')) %>%
  ungroup() %>%
  left_join(retail_counts_2005_2_and_10mile, by = 'GEOID') %>% 
  left_join(park_access, by = 'GEOID') %>%
  left_join(schools, by = 'GEOID') %>%
  left_join(dist_to_urb, by = 'GEOID') %>% 
  left_join(roads, by = 'GEOID') %>%
  left_join(geog_data, by = 'GEOID') %>%
  # --------------------- #
  left_join(fes_state_by_time, by = c('year', 'STATE')) %>%
  # --------------------- #
  mutate(urban_area = if_else(Geography == 'Urbanized', 1, 0), 
         uc_area = if_else(Geography == 'Urban Cluster', 1, 0)) %>%
  mutate(across(.cols = matches('^fe_'), 
                .fns = ~if_else(is.na(.), 0, .)))  
# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  
dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# -------------------------------------------------------------------------------------------- #
dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars), ends_with('bins'))
dta_treated <- dta_treated %>% select(all_of(model_vars))
dta_treated <- dta_treated %>% left_join(select(dta_treated_non_model_vars, GEOID, year, event_year), by = c('GEOID', 'year'))
# -------------------------------------------------------------------------------------------- #

dta_all <- bind_rows(dta_untreated, dta_treated) %>%
  
  filter(event_year != 0) %>%
  
  arrange(GEOID, year)

dta_all <- dta_all %>% 
  
  mutate(GEOID_TRACT = str_sub(GEOID, end = 11) )

ruca <- readxl::read_xlsx(path = here::here('Data', 'ruca2010revised.xlsx'), sheet = 'Data', skip = 1)

colnames(ruca) <- c('county_fips', 'state_name', 'county_name', 'GEOID_TRACT', 
                    'ruca_primary', 'ruca_secondary', 'population', 'land_area', 'pop_density')


dta_ua_ruca <- dta_all %>%
  
  select(
    year, GEOID, GEOID_TRACT, urban_area, uc_area
  ) %>%
  
  left_join(
    select(ruca, GEOID_TRACT, ruca_primary, ruca_secondary), 
    by = 'GEOID_TRACT', 
    relationship = 'many-to-one', 
    multiple = 'first'
    ) %>%
  mutate(
    Geography = case_when(
      urban_area == 1 ~ 'Urban', 
      uc_area == 1 ~ 'Urban', 
      urban_area == 0 & uc_area == 0 ~ 'Rural', 
      TRUE ~ NA
  ), 
  # 4-10 is defined as rural in Lopez et al. However, some of these tracts
  # belong to urban clusters, where we place urban clusters in urban areas. 
  Geography_Lopez = case_when(
    ruca_primary %in% seq(1, 3, 1) ~ 'Urban', 
    ruca_primary %in% seq(4, 10, 1) ~ 'Rural', 
    TRUE ~ NA
  ))
  

ua_ruca_summary <- dta_ua_ruca %>% 
  
  group_by(Geography, Geography_Lopez) %>%
  
  summarise(count = n()) %>% 
  
  group_by(Geography) %>%
  
  mutate(total = sum(count), 
         share = count/total) %>%
  
  mutate(
    across(
      .cols = where(is.numeric), 
      .fns = \(x) round(x, digits = 4)
    )
  )
  

# Alternative by RUCA Code: 
dta_ua_ruca %>% 
  
  group_by(Geography, ruca_primary) %>%
  
  summarise(count = n()) %>% 
  
  group_by(Geography) %>%
  
  mutate(total = sum(count), 
         share = count/total) %>%
  
  mutate(
    across(
      .cols = where(is.numeric), 
      .fns = \(x) round(x, digits = 4)
    )
  ) %>% 
  
  ungroup() %>%
  
  filter(Geography == 'Urban' & between(ruca_primary, 4, 9)) %>%
  
  summarise(total = sum(share))
