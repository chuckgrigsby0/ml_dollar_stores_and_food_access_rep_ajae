# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'purrr', 'stringr', 'tidyr')
# ----------------------------------- #

load(here::here('Data', 'Data_2_and_10_Miles', 
                'ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.RData'))

ds_entry <- bind_rows(panel_df_ds_entry_2_and_10mile) %>% 
  select(GEOID, year, DS_Count_10mile) %>% 
  arrange(GEOID, year)

pre_ds_entry <- ds_entry %>% filter(year == 2005 | year == 2006)

pos_ds_entry <- ds_entry %>% filter(year >= 2006)

year_1 = sort(unique(pos_ds_entry$year))
year_2 = year_1[(year_1 != '2006')]
year_1 = year_1[year_1 != '2020']

source(here::here('Code', 'Dollar_Store_Entry', 'Functions', 'Function_Change_in_Counts.R'))

pos_ds_entry_list <- map2(year_1, year_2, 
                          function(.x, .y){
                            
                            Change_in_Counts_Function(store_data_yr = pos_ds_entry, 
                                                      year_1 = .x, 
                                                      year_2 = .y, 
                                                      column_name = 'DS_Count_10mile')
                            
                          })

pos_ds_entry_list <- set_names(pos_ds_entry_list, nm = year_2)
pos_ds_entry_dif <- bind_rows(pos_ds_entry_list)

pos_ds_entry_dif <- pos_ds_entry_dif %>% arrange(GEOID, year)
source(here::here('Code', 'Dollar_Store_Entry', 'Functions', 'Function_Make_Entry_Events.R'))

pos_ds_entry_dif <- Make_Entry_Events(entry_dta = pos_ds_entry_dif, 
                                      treat_var = 'DS_Count_10mile_diff')

saveRDS(here::here('Data'))


