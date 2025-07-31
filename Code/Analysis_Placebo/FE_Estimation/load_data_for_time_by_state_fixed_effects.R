# Script that loads the data for statistical models.  
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Low-access indicators for a given drive time or distance. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'food_access_indicators_block_groups_2_and_10mile.RData'))
# -------------------------------------------------------------------------------------------- #
# Dollar store entries - This dataset is needed to separate the treated from the yet-to-be-treated and never-treated. 
# Specific to placebo data. 
# -------------------------------------------------------------------------------------------- #
panel_df_ds_entry_2_and_10mile <- readRDS(here::here('Data', 
                                                     'Data_2_and_10_Miles', 
                                                     'pla_ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.rds'))
# -------------------------------------------------------------------------------------------- #
# Pre-Entry Retail Store Counts
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'Data_2_and_10_Miles', 'retail_store_counts_2_and_10mile_2005_feature.RData'))
# -------------------------------------------------------------------------------------------- #
# Demographic and socioeconomic data. 
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'census_and_acs_list.RData'))
# -------------------------------------------------------------------------------------------- #
covar_df <- bind_rows(census_and_acs_list, .id = 'year')
# -------------------------------------------------------------------------------------------- #
covar_df_sel <- covar_df %>% 
  select(GEOID, year, 
         starts_with('age'), starts_with('commute'), 
         starts_with('educ'), inc_per_capita, no_vehicle, 
         pop_black, pop_hispanic, pop_asian, pop_white, 
         poverty_rate, public_assistance, 
         total_population, unemployed, vacant_housing) %>%
  mutate(inc_per_capita = inc_per_capita/10000, # In $10,000s
         total_population = total_population/1000)  # In 1,000s
# -------------------------------------------------------------------------------------------- #
# Create year-by-year expansions in order to join the 
# feature-engineered data to each year to which it corresponds. 
# -------------------------------------------------------------------------------------------- #
year_crosswalk <- bind_rows(data.frame(acs_year = 2005, year = '2000'), 
                            expand.grid(acs_year = seq(2006, 2010, 1), year = '2010'), 
                            expand.grid(acs_year = seq(2011, 2015, 1), year = '2015'),
                            expand.grid(acs_year = seq(2016, 2020, 1), year = '2020'))
# -------------------------------------------------------------------------------------------- #
# Convert to character to allow join with covar_df_sel.  
# -------------------------------------------------------------------------------------------- #
year_crosswalk$acs_year <- as.character(year_crosswalk$acs_year) 

covar_df_sel <- year_crosswalk %>% 
  left_join(covar_df_sel, by = 'year', multiple = 'all', relationship = 'many-to-many') %>% 
  select(-year) %>% 
  rename('year' = 'acs_year')
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
                .fn = ~if_else(is.na(.), 0, .)))
# -------------------------------------------------------------------------------------------- #
# Distance to Urban Cores
# -------------------------------------------------------------------------------------------- #
dist_to_urb <- readRDS(here::here('Data', 'block_group_distance_to_urban_areas.rds') )
dist_to_urb <- dist_to_urb %>% select(GEOID, distance_mi) %>% rename(distance_to_urban_area = distance_mi)
# -------------------------------------------------------------------------------------------- #
# Roads
# -------------------------------------------------------------------------------------------- #
roads <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'block_group_road_network_miles_2010_2_and_10mile.rds'))

# Combine the common roads and other roads to other roads. 

roads <- roads %>% 
  mutate(other_tot_road_length_mi = common_tot_road_length_mi + other_tot_road_length_mi) %>%
  select(-common_tot_road_length_mi)

# Urban-Rural Indicators
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))
# -------------------------------------------------------------------------------------------- #
gc()
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

# Add the geography variables to identify areas by STATE, urban, urban-cluster, and rural. 

dta_untreated <- dta_untreated %>%
  left_join(select(st_drop_geometry(bg_pop_centroids_10_sfp_geo), GEOID, Geography, STATE), by = 'GEOID') %>%
  mutate(urban_area = if_else(Geography == 'Urbanized', 1, 0), 
         uc_area = if_else(Geography == 'Urban Cluster', 1, 0))

dta_untreated <- dta_untreated %>% filter(STATE != 'Puerto Rico')
# -------------------------------------------------------------------------------------------- #
# Clean up untreated data. Note that NAs exist in Puerto Rico and a set of block groups for which we do not have census data.
# -------------------------------------------------------------------------------------------- #
nas <- dta_untreated[!complete.cases(dta_untreated), ]
nas <- nas %>% filter(!grepl('^72', GEOID)) 
sum(is.na(nas$age_18_34))==nrow(nas)

dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]
names(dta_untreated)
#--------------------------------------------------------------------------------------------#
gc()
#--------------------------------------------------------------------------------------------#