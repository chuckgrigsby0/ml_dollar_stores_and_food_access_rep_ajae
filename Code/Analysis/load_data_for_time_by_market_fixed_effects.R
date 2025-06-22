# -------------------------------------------------------------------------------------------- #
#.libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
.libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'sf', 'purrr', 'tidyr', 'stringr', 'recipes', 'rsample', 
       'glmnet', 'broom', 'tidymodels', 'fixest', 'doParallel', 'furrr', 'doMC')
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
# Create interaction terms and one-hot encoded dummy variables for the year fixed effects. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'census_and_acs_list.RData'))
covar_df <- bind_rows(census_and_acs_list, .id = 'year')
# -------------------------------------------------------------------------------------------- #
covar_df_sel <- covar_df %>% 
  select(GEOID, year, 
         starts_with('age'), starts_with('commute'), 
         starts_with('educ'), inc_per_capita, no_vehicle, 
         pop_black, pop_hispanic, pop_asian, pop_white, 
         poverty_rate, public_assistance, 
         total_population, unemployed, vacant_housing) 
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
# Urban-Rural Indicators
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))
# -------------------------------------------------------------------------------------------- #

gc()

# -------------------------------------------------------------------------------------------- #
# Create year-by-year expansions in order to join the 
# feature-engineered data to each year to which it corresponds. 
# -------------------------------------------------------------------------------------------- #
year_crosswalk <- bind_rows(data.frame(acs_year = 2005, year = '2000'), 
                            expand.grid(acs_year = seq(2006, 2010, 1), year = '2010'), 
                            expand.grid(acs_year = seq(2011, 2015, 1), year = '2015'),
                            expand.grid(acs_year = seq(2016, 2020, 1), year = '2020'))
# Convert to character to allow join with feat_eng_vars. 
year_crosswalk$acs_year <- as.character(year_crosswalk$acs_year) 

covar_df_sel <- year_crosswalk %>% 
  left_join(covar_df_sel, by = 'year') %>% 
  select(-year) %>% 
  rename('year' = 'acs_year')
# -------------------------------------------------------------------------------------------- #
# feat_eng_vars_list <- as.character(unique(feat_eng_vars$year)) %>%
#   map(function(.x){
#     feat_eng_vars %>% filter(year == .x)
#   })
# -------------------------------------------------------------------------------------------- #
# feat_eng_vars_list <- set_names(feat_eng_vars_list, nm = unique(feat_eng_vars$year))
# -------------------------------------------------------------------------------------------- #
dta_untreated <- list(panel_df_ds_entry_2_and_10mile$untreated, 
                      panel_df_access_inds_2_and_10mile, 
                      covar_df_sel, 
                      land_use) %>%
  reduce(left_join, by = c('GEOID', 'year')) %>%
  ungroup() %>%
  left_join(retail_counts_2005_2_and_10mile, by = 'GEOID') %>% 
  left_join(park_access, by = 'GEOID') %>%
  left_join(schools, by = 'GEOID') %>%
  left_join(dist_to_urb, by = 'GEOID') %>%
  left_join(roads, by = 'GEOID') %>%
  select(-c('DS_Count_10mile':'Grocery_Count_10mile_diff', total_low_access)) #10min
# -------------------------------------------------------------------------------------------- #
# Create market-level data. 
# -------------------------------------------------------------------------------------------- #
markets <- st_drop_geometry(bg_pop_centroids_10_sfp_geo ) %>% 
  select(GEOID, STATE, STATEFP, COUNTYFP, COUNTY, market_name) %>%
  rename(STATEA = STATEFP) %>%
  rename(market = market_name)

unique_markets <- markets %>%
  group_by(GEOID, market) %>%
  ungroup() %>%
  distinct(market)
# Create market IDs for each market. 
unique_markets$market_id = 1:nrow(unique_markets)

rm(bg_pop_centroids_10_sfp_geo)
# Join the market IDs to the full set of market data. 
markets <- markets %>% left_join(unique_markets, by = 'market')
# Join the markets data to the untreated data. 
dta_untreated <- dta_untreated %>% left_join(select(markets, GEOID, market, market_id), by = 'GEOID')
# Clean up untreated data. Note that NAs exist in Puerto Rico and a set of block groups for which we do not have census data. 
# Therefore when we estimate FEs for markets, there are approximately 93 fewer markets. 
nas <- dta_untreated[!complete.cases(dta_untreated), ]
nas <- nas %>% filter(!grepl('^72', GEOID)) 
sum(is.na(nas$age_18_34))==nrow(nas)

dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]
#--------------------------------------------------------------------------------------------#
gc()
#--------------------------------------------------------------------------------------------#