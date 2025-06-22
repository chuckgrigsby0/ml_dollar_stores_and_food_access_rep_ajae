# -------------------------------------------------------------------------------------------- #
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
ds_entries_panel_dta <- load_data(data_string = 'ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.RData')
# -------------------------------------------------------------------------------------------- #
# load(here::here('Data', 'ds_entries_panel_treated_wbins.RData'))
# -------------------------------------------------------------------------------------------- #
# Create interaction terms and one-hot encoded dummy variables for the year fixed effects. 
# -------------------------------------------------------------------------------------------- #
covar_df <- bind_rows(census_and_acs_list, .id = 'year')
# -------------------------------------------------------------------------------------------- #
covar_df_sel <- covar_df %>% 
  select(GEOID, year, STATE, STATEA) 

# -------------------------------------------------------------------------------------------- #
# Create unique State and State FIPS pairs. 
# -------------------------------------------------------------------------------------------- #

state_pairs <- covar_df_sel %>% 
  group_by(STATE, STATEA) %>%
  distinct(STATE, STATEA)

# -------------------------------------------------------------------------------------------- #
# The data have some NAs in the STATEA column. 
# We replace the NAs FIPs with the first two digits of the GEOID. 
# To get the corresponding state names, remove the original STATE variable, and join the state_pairs
# data using the FIPS ids in the STATEA column. 
# -------------------------------------------------------------------------------------------- #

covar_df_sel <- covar_df_sel %>% 
  mutate(STATEA = if_else(is.na(STATEA), str_sub(GEOID, start = 1L, end = 2L), STATEA)) %>%
  select(-STATE) %>%
  left_join(state_pairs, by = 'STATEA') %>% 
  relocate(STATE, .before = STATEA)
# -------------------------------------------------------------------------------------------- #
# Obtain the unique GEOIDs from the untreated data. 
# -------------------------------------------------------------------------------------------- #
unique_geoids_ut <- ds_entries_panel_dta$untreated %>% distinct(GEOID)
# -------------------------------------------------------------------------------------------- #
# Obtain the unique GEOID-State pairs from the census data. 
# -------------------------------------------------------------------------------------------- #
states_ut <-  covar_df_sel %>%
  group_by(GEOID, STATE, STATEA) %>%
  distinct(GEOID, STATE, STATEA)
# -------------------------------------------------------------------------------------------- #
# Join the state names to the unique GEOIDs in the untreated data. 
# -------------------------------------------------------------------------------------------- #
unique_geoids_ut <- unique_geoids_ut %>%
  left_join(states_ut, by = 'GEOID')
# -------------------------------------------------------------------------------------------- #
# There are some GEOIDs in the untreated data that were not in the census data. 
# We fill in the NAs produced from the join by following the same steps outlined above. 
# -------------------------------------------------------------------------------------------- #
unique_geoids_ut <- unique_geoids_ut %>% 
  mutate(STATEA = if_else(is.na(STATEA), str_sub(GEOID, start = 1L, end = 2L), STATEA)) %>%
  select(-STATE) %>%
  left_join(state_pairs, by = 'STATEA') %>% 
  relocate(STATE, .before = STATEA)
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
  left_join(unique_geoids_ut, by = 'GEOID') %>%
  mutate(STATE = str_replace_all(STATE, ' ', '_'))
# -------------------------------------------------------------------------------------------- #
# Create the dummy variables using the state names. 
# Note that I used the untreated data to create the dummy variables. 
unique(panel_untreated_dta$STATE)
# -------------------------------------------------------------------------------------------- #
# Create the recipe with steps. 
state_dummy_recipe <- panel_untreated_dta %>% 
  
  recipe(~. , data = .) %>%
  #step_rm(GEOID) %>%
  
  step_dummy(STATE, 
             one_hot = TRUE, 
             naming = function(var, lvl, ordinal = FALSE){ paste0(var, '_', lvl)}, 
             keep_original_cols = TRUE)
# Prepare the recipe and bake. 
state_dummy_baked <- state_dummy_recipe %>%  
  
  prep(training = NULL) %>% 
  
  bake(., GEOID, year, contains('STATE_'), new_data = NULL) 
# -------------------------------------------------------------------------------------------- #
save(state_dummy_baked, file = here::here('Data', 'state_dummies_for_untreated_data_2_and_10mile.RData')) #_2_and_5mile
# -------------------------------------------------------------------------------------------- #