# -------------------------------------------------------------------------------------------- #
#.libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
.libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'sf', 'tmap', 'tmaptools', 'didimputation', 
       'ggplot2', 'nngeo', 'purrr', 'tidyr', 'stringr', 'recipes', 'rsample', 
       'glmnet')
# -------------------------------------------------------------------------------------------- #
# Load data. 
load_data <- function(data_string){
  data_file <- load(here::here('Data', data_string)) 
  
  return(get(data_file))
}
# -------------------------------------------------------------------------------------------- #
load(here::here('Data', 'census_and_acs_list.RData'))

ds_entries_panel_dta <- load_data(data_string = 'Data_2_and_10_Miles/ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.RData')

load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))
# -------------------------------------------------------------------------------------------- #
# Obtain the unique GEOIDs from the untreated data. 
# -------------------------------------------------------------------------------------------- #
unique_geoids_ut <- ds_entries_panel_dta$untreated %>% distinct(GEOID)
# -------------------------------------------------------------------------------------------- #
markets <- st_drop_geometry(bg_pop_centroids_10_sfp_geo ) %>% 
  select(GEOID, STATE, STATEFP, COUNTYFP, COUNTY, market_name) %>%
  rename(STATEA = STATEFP)

rm(bg_pop_centroids_10_sfp_geo)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Acquire the untreated data. 
# -------------------------------------------------------------------------------------------- #
panel_untreated_dta <- ds_entries_panel_dta$untreated %>% 
  
  select(GEOID, year)
# -------------------------------------------------------------------------------------------- #
# Join the unique GEOIDs containing the state names to the untreated data. 
# Note that the join is safe because it is one-to-many. 
# -------------------------------------------------------------------------------------------- #
panel_untreated_dta <- panel_untreated_dta %>%
  
  left_join(markets, by = 'GEOID') %>%
  
  rename(market = market_name)
# -------------------------------------------------------------------------------------------- #
# Create the dummy variables using the market names. 
# Note that I used the untreated data to create the dummy variables. 
length(unique(panel_untreated_dta$market))

unique_markets <- panel_untreated_dta %>%
  group_by(GEOID, market) %>%
  ungroup() %>%
  distinct(market)

unique_markets$Market_ID = 1:nrow(unique_markets)

panel_untreated_dta <- panel_untreated_dta %>% left_join(unique_markets, by = 'market')

panel_untreated_dta$Market_ID <- factor(panel_untreated_dta$Market_ID)

# -------------------------------------------------------------------------------------------- #
# Create the recipe with steps. 
market_dummy_recipe <- panel_untreated_dta %>% 
  
  recipe(~. , data = .) %>%
  #step_rm(GEOID) %>%
  
  step_dummy(Market_ID, 
             one_hot = TRUE, 
             naming = function(var, lvl, ordinal = FALSE){ paste0(var, '_', lvl)}, 
             keep_original_cols = TRUE)
# Prepare the recipe and bake. 
market_dummy_baked <- market_dummy_recipe %>%  
  
  prep(training = NULL) %>% 
  
  bake(., GEOID, year, contains('market_'), new_data = NULL) 
# -------------------------------------------------------------------------------------------- #
# save(market_dummy_baked, file = here::here('Data', 'market_dummies_for_untreated_data_2_and_10mile.RData')) #_2_and_5mile
# -------------------------------------------------------------------------------------------- #