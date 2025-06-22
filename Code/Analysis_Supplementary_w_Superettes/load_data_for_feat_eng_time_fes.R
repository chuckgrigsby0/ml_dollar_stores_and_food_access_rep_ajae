# Script that loads the data for statistical models.  
# -------------------------------------------------------------------------------------------- #
library(here)
# -------------------------------------------------------------------------------------------- #
# Food Desert indicators for a given drive time or distance. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'food_access_indicators_block_groups_2_and_10mile.RData')) # 10min, 3mile, 5mile 
# -------------------------------------------------------------------------------------------- #
# Dollar store entries - This dataset is needed to separate the treated from the yet-to-be-treated and never-treated. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.RData')) #10min, 3mile
# -------------------------------------------------------------------------------------------- #
# Pre-Entry Retail Store Counts
# -------------------------------------------------------------------------------------------- #
# load(here::here('Data', 'dollar_store_counts_3mile_2005_feature.RData')) #10min, 3mile Same variable in retail_store.*
load(here::here('Data', 'Data_2_and_10_Miles', 'retail_store_counts_2_and_10mile_2005_feature.RData')) #10min, 3mile
# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
# Demographic and socioeconomic data. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'census_and_acs_list.RData'))

covar_df <- bind_rows(census_and_acs_list, .id = 'year')

covar_df_sel <- covar_df %>% 
  select(GEOID, year, 
         starts_with('age'), starts_with('commute'), 
         starts_with('educ'), inc_per_capita, no_vehicle, 
         pop_black, pop_hispanic, pop_asian, pop_white, 
         poverty_rate, public_assistance, 
         total_population, unemployed, vacant_housing) 
# -------------------------------------------------------------------------------------------- #


# Parks
# -------------------------------------------------------------------------------------------- #
park_access <- readRDS(here::here('Data', 'block_group_park_access_allnn.rds'))
park_access <- park_access %>% select(GEOID, park_access_7nn)
# -------------------------------------------------------------------------------------------- #
# Schools 
# -------------------------------------------------------------------------------------------- #
schools <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'block_group_school_counts_2_and_10mile.rds'))
schools <- schools %>% select(GEOID, school_count_pub_10mile, school_count_priv_10mile)
# -------------------------------------------------------------------------------------------- #
# Land Use/Development
# -------------------------------------------------------------------------------------------- #
land_use <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'nlcd_data_block_groups_by_year_list_2_and_10mile.rds'))

land_use <- bind_rows(land_use, .id = 'year')

# Use to create repeat years for land-use data. 
year_crosswalk <- readRDS(here::here('Data', 'year_combine_dataframe.rds'))

land_use <- year_crosswalk %>%
  
  left_join(land_use, by = c('year_nlcd'='year')) %>%
  
  mutate(frac_44_forest = frac_41_deciduous_forest + frac_42_evergreen_forest + frac_43_mixed_forest, 
         frac_83_planted_cultivated = frac_81_hay_pasture + frac_82_cultivated_crops, 
         frac_96_wetlands = frac_90_woody_wetlands + frac_95_emergent_herbaceous_wetlands) %>%
  
  select(-year_nlcd, -starts_with('frac_0'), -starts_with('frac_12'), 
         -c(frac_41_deciduous_forest, frac_42_evergreen_forest, frac_43_mixed_forest, 
            frac_81_hay_pasture, frac_82_cultivated_crops, frac_90_woody_wetlands, 
            frac_95_emergent_herbaceous_wetlands)) %>% 
  
  filter(year != '2004') %>%
  
  mutate(across(.cols = where(is.numeric), 
                .fn = ~if_else(is.na(.), 0, .)))
# -------------------------------------------------------------------------------------------- #
# Distance to Urban Cores
# -------------------------------------------------------------------------------------------- #
dist_to_urb <- readRDS(here::here('Data', 'block_group_distance_to_urban_areas.rds'))
dist_to_urb <- dist_to_urb %>% select(GEOID, distance_mi) %>% rename(distance_to_urban_area = distance_mi)
# -------------------------------------------------------------------------------------------- #
# Roads
# -------------------------------------------------------------------------------------------- #
roads <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'block_group_road_network_miles_2010_2_and_10mile.rds'))

# Combine the common roads and other roads to other roads. 

roads <- roads %>% 
  mutate(other_tot_road_length_mi = common_tot_road_length_mi + other_tot_road_length_mi) %>%
  select(-common_tot_road_length_mi)
# -------------------------------------------------------------------------------------------- #
# State Dummy Variables
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'state_dummies_for_untreated_data_2_and_10mile.RData'))
state_dummy_baked <- state_dummy_baked %>% mutate(GEOID = as.character(GEOID))
# -------------------------------------------------------------------------------------------- #
# Urban-Rural Indicators
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))
# -------------------------------------------------------------------------------------------- #

gc()