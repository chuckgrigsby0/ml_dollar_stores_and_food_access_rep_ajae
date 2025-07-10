# Script that creates data for conventional DID methods. 
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 
       'recipes', 'rsample', 'fixest', 'sf', 'tictoc', 'xgboost', 
       'haven')
# -------------------------------------------------------------------------------------------- #
model_geography = 'Urban' # Change according to Rural/Urban data needs. 
model_dep_var = 'low_access'
# -------------------------------------------------------------------------------------------- #
# Food Desert indicators for a given drive time or distance. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'food_access_indicators_block_groups_2_and_10mile.RData'))
# -------------------------------------------------------------------------------------------- #
# Dollar store entries - This dataset is needed to separate the treated from the yet-to-be-treated and never-treated. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.RData'))
# -------------------------------------------------------------------------------------------- #
# Pre-Entry Retail Store Counts
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'retail_store_counts_2_and_10mile_2005_feature.RData'))
# -------------------------------------------------------------------------------------------- #
# Demographic and socioeconomic data. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'census_and_acs_list.RData'))

covar_df <- bind_rows(census_and_acs_list, .id = 'year')
# -------------------------------------------------------------------------------------------- #
covar_df_sel <- covar_df %>% 
  select(GEOID, year, starts_with('age'), starts_with('commute'), 
         starts_with('educ'), inc_per_capita, no_vehicle, 
         pop_black, pop_hispanic, pop_asian, pop_white, 
         poverty_rate, public_assistance, 
         total_population, unemployed, vacant_housing) %>%
  mutate(inc_per_capita = inc_per_capita/10000, # In $10,000s
         total_population = total_population/1000)  # In 1,000s

acs_covars <- names(covar_df_sel)
acs_covars <- acs_covars[!grepl('GEOID|year', acs_covars)]; acs_covars
# -------------------------------------------------------------------------------------------- #
# Create year-by-year expansions in order to join the 
# block-group census data to each year to which it corresponds. 
# -------------------------------------------------------------------------------------------- #
year_crosswalk <- bind_rows(data.frame(acs_year = 2005, year = '2000'), 
                            expand.grid(acs_year = seq(2006, 2010, 1), year = '2010'), 
                            expand.grid(acs_year = seq(2011, 2015, 1), year = '2015'),
                            expand.grid(acs_year = seq(2016, 2020, 1), year = '2020'))

# Convert to character to allow join with feat_eng_vars. 
year_crosswalk$acs_year <- as.character(year_crosswalk$acs_year) 

covar_df_sel_jn <- year_crosswalk %>% 
  left_join(covar_df_sel, by = 'year', multiple = 'all', relationship = 'many-to-many') %>% 
  select(-year) %>% 
  rename('year' = 'acs_year')

# -------------------------------------------------------------------------------------------- #
# Parks
# -------------------------------------------------------------------------------------------- #
park_access <- readRDS(here::here('Data', 'block_group_park_access_allnn.rds'))
park_access <- park_access %>% select(GEOID, park_access_7nn)
park_access_vars <- names(park_access)[!grepl('GEOID|year', names(park_access))]
# -------------------------------------------------------------------------------------------- #
# Schools 
# -------------------------------------------------------------------------------------------- #
schools <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'block_group_school_counts_2_and_10mile.rds'))
schools <- schools %>% select(GEOID, school_count_pub_10mile, school_count_priv_10mile)
schools_vars <- names(schools)[!grepl('GEOID|year', names(schools))]
# -------------------------------------------------------------------------------------------- #
# Land Use/Development
# -------------------------------------------------------------------------------------------- #
land_use <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'nlcd_data_block_groups_by_year_list_2_and_10mile.rds'))

land_use <- bind_rows(land_use, .id = 'year')

# Use to create repeat years for land-use data. 
year_crosswalk <- readRDS(here::here('Data', 'year_combine_dataframe.rds'))

land_use <- year_crosswalk %>%
  
  left_join(land_use, by = c('year_nlcd'='year'), multiple = 'all', relationship = 'many-to-many') %>% 
  
  mutate(frac_44_forest = rowSums(across(.cols = matches('forest$')), na.rm=TRUE), 
         frac_83_planted_cultivated = rowSums(across(.cols = matches('hay_pasture$|cultivated_crops$')), na.rm=TRUE), 
         frac_96_wetlands = rowSums(across(.cols = matches('wetlands$') ), na.rm=TRUE ) ) %>%
  
  select(-year_nlcd, -ends_with('unclassified'), -ends_with('perennial_snow_ice'), 
         -c(frac_41_deciduous_forest, frac_42_evergreen_forest, frac_43_mixed_forest, 
            frac_81_hay_pasture, frac_82_cultivated_crops, 
            frac_90_woody_wetlands, frac_95_emergent_herbaceous_wetlands)) %>% 
  
  filter(year != '2004') %>%
  
  mutate(across(.cols = where(is.numeric), 
                .fn = ~if_else(is.na(.), 0, .) ) )

land_use_vars <- names(land_use)[!grepl('GEOID|year', names(land_use))]
# -------------------------------------------------------------------------------------------- #
# Distance to Urban Cores
# -------------------------------------------------------------------------------------------- #
dist_to_urb <- readRDS(here::here('Data', 'block_group_distance_to_urban_areas.rds'))
dist_to_urb <- dist_to_urb %>% select(GEOID, distance_mi) %>% rename(distance_to_urban_area = distance_mi)
dist_to_urb_vars <- names(dist_to_urb)[!grepl('GEOID|year', names(dist_to_urb))]
# -------------------------------------------------------------------------------------------- #
# Roads
# -------------------------------------------------------------------------------------------- #
roads <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'block_group_road_network_miles_2010_2_and_10mile.rds'))

# Combine the common roads and other roads to other roads. 
roads <- roads %>% 
  mutate(other_tot_road_length_mi = common_tot_road_length_mi + other_tot_road_length_mi) %>%
  select(-common_tot_road_length_mi)

roads_vars <- names(roads)[!grepl('GEOID|year', names(roads))]
# -------------------------------------------------------------------------------------------- #
# Geographic data. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))

geog_data <- st_drop_geometry(bg_pop_centroids_10_sfp_geo) %>% 
  select(GEOID, STATE, market_name, market_name_full, Geography) %>%
  rename(market = market_name)
# -------------------------------------------------------------------------------------------- #
# List of string vars for economic geography data. 
# -------------------------------------------------------------------------------------------- #
econ_geog_vars <- list('distance_to_urban_areas' = dist_to_urb_vars, 
                       'land_use' = land_use_vars, 
                       'park_access' = park_access_vars, 
                       'roads' = roads_vars, 
                       'schools' = schools_vars)
# -------------------------------------------------------------------------------------------- #
gc()
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Combine the dollar store entry data of untreated observations, the food access indicators, and all model covariates. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Combine_Data_for_DID_Regs.R'))
dta_untreated <- Combine_Data_for_DID_Regs_Function(dta = panel_df_ds_entry_2_and_10mile$untreated, 
                                       panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                       retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile, 
                                       national = FALSE, 
                                       geography_str = model_geography)

# Columns not needed for imputation models. 
non_model_vars <- c('DS_Count_10mile', 'DS_Count_10mile_diff',
                    'entry', 'entry_events', 'event_year', 'net_entry_cumsum', 'rel_year', 'treat', 
                    'Grocery_Count_10mile', 'Grocery_Count_10mile_diff', 'Grocery_Count_10mile_2005', 'total_low_access', 
                    'market', 'market_name_full', 'Geography')
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

# Re-Join the entry variables

entry_vars <- c('entry', 'entry_events', 'event_year', 'rel_year', 'treat')

if (model_geography == 'Urban'){ 

  dta_untreated <- dta_untreated %>% 
    left_join(select(dta_untreated_non_model_vars, GEOID, year, all_of(entry_vars) ), 
              by = c('GEOID', 'year') ) %>% 
    relocate(all_of(entry_vars), .after = 'year') %>%
    relocate(c(STATE, urban_area), .after = GEOID) %>%
    select(-c(low_access_perm, low_access_pers))  
  
} else { 
    
  dta_untreated <- dta_untreated %>% 
    left_join(select(dta_untreated_non_model_vars, GEOID, year, all_of(entry_vars) ), 
              by = c('GEOID', 'year') ) %>% 
    relocate(all_of(entry_vars), .after = 'year') %>%
    relocate(c(STATE), .after = GEOID) %>%
    select(-c(low_access_perm, low_access_pers))
  
  
  }
# -------------------------------------------------------------------------------------------- #
# Create the data set of treated observations, as we did for the untreated. 
# -------------------------------------------------------------------------------------------- #
dta_treated <- Combine_Data_for_DID_Regs_Function(dta = panel_df_ds_entry_2_and_10mile$treated, 
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

# Re-Join the entry variables
if (model_geography == 'Urban'){ 
  
  dta_treated <- dta_treated %>% 
    left_join(select(dta_treated_non_model_vars, GEOID, year, all_of(entry_vars) ), 
              by = c('GEOID', 'year') ) %>% 
    relocate(all_of(entry_vars), .after = 'year') %>%
    relocate(c(STATE, urban_area), .after = GEOID) %>%
    select(-c(low_access_perm, low_access_pers))
  
} else { 
  
  dta_treated <- dta_treated %>% 
    left_join(select(dta_treated_non_model_vars, GEOID, year, all_of(entry_vars) ), 
              by = c('GEOID', 'year') ) %>% 
    relocate(all_of(entry_vars), .after = 'year') %>%
    relocate(c(STATE), .after = GEOID) %>%
    select(-c(low_access_perm, low_access_pers))
  
  
}

# -------------------------------------------------------------------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 'acs_covars', 'econ_geog_vars',
                           'dta_untreated', 'dta_treated', 
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars', 
                           'entry_vars', 'model_geography', 'model_dep_var', 'ncores', 'bootstrap_by_tracts', 
                           'ds_bans_stats_all_rural', 'ds_bans_stats_all_urban'))])
gc()
# -------------------------------------------------------------------------------------------- #
dta_cb <- bind_rows(dta_untreated, dta_treated) %>% arrange(GEOID)

names(dta_cb) <- names(dta_cb) %>% str_replace_all("^frac_\\d+_", ""); names(dta_cb)

write_dta(dta_cb, 
          path = here::here('Data', 'Data_DID_Robustness_Check', 
                            paste0('dollar_stores_', 
                                   model_dep_var, '_', 
                                   str_to_lower(model_geography), 
                                   '.dta') ) )
