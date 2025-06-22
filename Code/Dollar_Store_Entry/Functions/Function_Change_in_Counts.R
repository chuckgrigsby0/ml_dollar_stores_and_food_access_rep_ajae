# Script used to flexibly compute changes in store counts from two consecutive periods. 
print('Sourced: Change_in_Counts_Function')
# -------------------------------------------------------------------------------------------- #
Change_in_Counts_Function <- function(store_data_yr, year_1, year_2, column_name){
  
  dta_year_1 <- store_data_yr %>% filter(year == year_1) %>% select(-year)
  dta_year_2 <- store_data_yr %>% filter(year == year_2) %>% select(-year)
  
  diff_dta <- dta_year_1 %>% select(GEOID)
  
  joined_dta <- dta_year_1 %>%
    left_join(dta_year_2, by = 'GEOID', 
              suffix = c(paste0('_', year_1), paste0('_', year_2)))
  
  column_name_year_1 = paste(column_name, year_1, sep = '_')
  column_name_year_2 = paste(column_name, year_2, sep = '_')
  
  mutated_dta_temp <- joined_dta %>% 
    select(GEOID, starts_with({{column_name}})) %>%
    mutate('{column_name}_diff' := .data[[column_name_year_2]] - .data[[column_name_year_1]]) %>%
    select(GEOID, ends_with('diff'))
  
  diff_dta <- diff_dta %>% left_join(mutated_dta_temp, by = 'GEOID')
  
  diff_dta$year <- year_2
  
  diff_dta <- diff_dta %>% relocate(year, .after = GEOID)
  
  return(diff_dta)
  
}
# -------------------------------------------------------------------------------------------- #