# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on BG bootstrap or CT bootstrap. 
model_dep_var = 'low_access' # Used in script below. If running for low_access_pers, must change settings below.
model_geography = 'Urban' # Used in script below to subset by either Urban or Rural.
print(model_dep_var); print(model_geography)
options(scipen = 999)
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 
       'recipes', 'rsample', 'fixest', 'sf', 'tictoc', 'glmnet', 
       'future', 'furrr', 'parallel', 'doParallel', 'effectsize')
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Combine the dollar store entry data of untreated observations, the food access indicators, and all model covariates. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Combine_Treated_or_Untreated_Data.R'))
dta_untreated <- Combine_Data_Function(dta = panel_df_ds_entry_2_and_10mile$untreated, 
                                       panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                       retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile, 
                                       national = FALSE, 
                                       geography_str = model_geography)

# Columns not needed for imputation models. 
non_model_vars <- c('DS_Count_10mile_diff', # 'DS_Count_10mile',
                    'entry', 'entry_events', 'event_year', 'net_entry_cumsum', 'rel_year', 'treat', 
                    'Grocery_Count_10mile_diff', 'total_low_access', # 'Grocery_Count_10mile', 'Grocery_Count_10mile_2005',
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
# -------------------------------------------------------------------------------------------- #
# Create the data set of treated observations, as we did for the untreated. 
# -------------------------------------------------------------------------------------------- #
dta_treated <- Combine_Data_Function(dta = panel_df_ds_entry_2_and_10mile$treated, 
                                     panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                     retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile, 
                                     national = FALSE, 
                                     geography_str = model_geography)
# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  
dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# -------------------------------------------------------------------------------------------- #
dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars), ends_with('bins'))
dta_treated <- dta_treated %>% select(all_of(model_vars))
# -------------------------------------------------------------------------------------------- #
dta_tr_and_ut <- bind_rows(dta_untreated, dta_treated)
# -------------------------------------------------------------------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 'acs_covars', 'econ_geog_vars',
                           'dta_tr_and_ut', 'dta_untreated', 'dta_treated', 'dta_treated_w_folds',
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars', 
                           'model_geography', 'model_dep_var', 'ncores'))])
gc()
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the dollar store bans and restrictions data. 
# -------------------------------------------------------------------------------------------- #
ds_bans <- readr::read_csv(here::here('Data', 'block_groups_w_ds_policies.csv'), show_col_types = FALSE )
# Use to select relvant variables below. 
policy_vars = names(ds_bans) %>% str_subset(string=., pattern='^Def|^Mor|^Ord|^policy_*')
# -------------------------------------------------------------------------------------------- #
# Load the block-group level data with geographic information. 
# -------------------------------------------------------------------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))
bg_pop_centroids_10_sfp_geo <- bg_pop_centroids_10_sfp_geo %>% select(GEOID, Geography) %>% st_drop_geometry()
# -------------------------------------------------------------------------------------------- #
# Join geographic information to dollar store policy data. 
# -------------------------------------------------------------------------------------------- #
ds_bans <- ds_bans %>% left_join(bg_pop_centroids_10_sfp_geo, by = 'GEOID')
# -------------------------------------------------------------------------------------------- #
ds_bans_modeling <- ds_bans %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  mutate(Restrictions = Moratorium + Ordinance) %>% 
  mutate(
    Defeated_and_Restrictions = case_when(Defeated >= 1 & Restrictions == 0 ~ Defeated + Restrictions, 
                                          Defeated >= 1 & Restrictions >= 1 ~ Defeated + Restrictions, 
                                          Defeated == 0 & Restrictions >= 1 ~ 0, 
                                          Defeated == 0 & Restrictions == 0 ~ 0),
    Restrictions_and_Defeated = case_when(Restrictions >= 1 & Defeated == 0 ~ Defeated + Restrictions, 
                                          Restrictions >= 1 & Defeated >= 1 ~ Defeated + Restrictions, 
                                          Restrictions == 0 & Defeated >= 1 ~ 0, 
                                          Restrictions == 0 & Defeated == 0 ~ 0),
    policy_total_binary = if_else(policy_total > 0, 1, 0) ) %>%
  
  relocate(c(Restrictions, contains('_and_'), policy_total_binary), .before = policy_total) %>%
  select(GEOID, zip_code_id, policy_total_binary, Restrictions_and_Defeated, Defeated_and_Restrictions, 
         Restrictions, Moratorium, Ordinance, Defeated) %>% 
  mutate(across(.cols = c(Restrictions_and_Defeated, Defeated_and_Restrictions), 
                .fns = \(x) if_else(x > 0, 1, 0), 
                .names = '{.col}_binary' ) )
# -------------------------------------------------------------------------------------------- #
dta_model <- dta_treated %>% 
  left_join(ds_bans_modeling, by = 'GEOID', multiple = 'all', relationship = 'many-to-one') %>%
  select(GEOID, zip_code_id, year, 
         ends_with('binary'), contains('Defeated'), contains('Restrictions'), Ordinance, Moratorium)
# -------------------------------------------------------------------------------------------- #

ds_bans_counts <- dta_model %>% 
  
  group_by(GEOID) %>%
  
  filter(row_number() == 1) 

ds_bans_counts <- ds_bans_counts %>% ungroup()

# -------------------------------------------------------------------------------------------- #
# Unique treated block group counts w.r.t. policy (defeats and restrictions) variables. 
# -------------------------------------------------------------------------------------------- #
paste0('There are ', ds_bans_counts %>% filter(Defeated >= 1) %>% nrow(), 
       ' unique block groups with at least one defeat.')
paste0('There are ', ds_bans_counts %>% filter(Defeated >= 1 & Restrictions == 0) %>% nrow(), 
       ' unique block groups with at least one defeat only.')
paste0('There are ', ds_bans_counts %>% filter(Defeated >= 1 & Restrictions >= 1) %>% nrow(), 
       ' unique block groups with at least one defeat and at least one restriction policy.')


paste0('There are ', ds_bans_counts %>% filter(Restrictions >= 1) %>% nrow(), 
       ' unique block groups with at least one restriction policy.')
paste0('There are ', ds_bans_counts %>% filter(Restrictions >= 1 & Defeated == 0) %>% nrow(), 
       ' unique block groups with at least one restriction policy only.')
paste0('There are ', ds_bans_counts %>% filter(Restrictions >= 1 & Defeated >= 1) %>% nrow(), 
       ' unique block groups with at least one defeat and at least one restriction policy.')

paste0('The share of treated block groups experiencing any dollar store defeat or restriction policy ',
round(
  nrow(filter(ds_bans_counts, policy_total_binary == 1))/nrow(filter(ds_bans_counts, Restrictions == 0 & Defeated == 0)), 
  digits=3
  ) 
)
# -------------------------------------------------------------------------------------------- #




