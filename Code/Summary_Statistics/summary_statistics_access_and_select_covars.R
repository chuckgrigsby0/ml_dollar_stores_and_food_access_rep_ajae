# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
#.libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
.libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample', 'tictoc', 'xgboost')
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
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
  left_join(covar_df_sel, by = 'year') %>% 
  select(-year) %>% 
  rename('year' = 'acs_year')
# -------------------------------------------------------------------------------------------- #
geog_data <- st_drop_geometry(bg_pop_centroids_10_sfp_geo) %>% 
  select(GEOID, STATE, market_name, market_name_full, Geography) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Combine the dollar store entry data of untreated observations, the food access indicators, and all model covariates. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Combine_Treated_or_Untreated_Data.R'))
dta_untreated <- Combine_Data_Function(dta = panel_df_ds_entry_2_and_10mile$untreated, 
                                       panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                       retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile)

# Columns not needed for imputation models. 
non_model_vars <- c('DS_Count_10mile', 'DS_Count_10mile_diff',
                    'entry', 'entry_events', 'event_year', 'net_entry_cumsum', 
                    'rel_year', 'treat', 'Grocery_Count_10mile', 'Grocery_Count_10mile_diff', 
                    'Grocery_Count_10mile_2005', 'total_low_access', 
                    'STATE', 'market_name', 'market_name_full', 'Geography')
# Modeling variables (Columns needed for imputation models). 
model_vars <- names(dta_untreated)[!(names(dta_untreated) %in% non_model_vars)]; model_vars

dta_untreated_non_model_vars <- dta_untreated %>% select(GEOID, year, all_of(non_model_vars))
dta_untreated <- dta_untreated %>% select(all_of(model_vars))
# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates or are located in Puerto Rico, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_ut <- dta_untreated[!complete.cases(dta_untreated), ]  
dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]

# Keep only the rows from the clean untreated data.
dta_untreated_non_model_vars <- select(dta_untreated, GEOID, year) %>% 
  inner_join(dta_untreated_non_model_vars, by = c('GEOID', 'year'))
# -------------------------------------------------------------------------------------------- #
# Create the data set of treated observations, as we did for the untreated. 
# -------------------------------------------------------------------------------------------- #
dta_treated <- Combine_Data_Function(dta = panel_df_ds_entry_2_and_10mile$treated, 
                                     panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                     retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile)

dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars))
dta_treated <- dta_treated %>% select(all_of(model_vars))

# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  
dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# -------------------------------------------------------------------------------------------- #

# Obtain separate data sets for never-treated and yet-to-be treated observations. 
# We have coded the never-treated observations with zeros. 
# -------------------------------------------------------------------------------------------- #
dta_nevertreated <- dta_untreated %>% 
  left_join(select(dta_untreated_non_model_vars, GEOID, year, event_year), 
            by = c('GEOID', 'year')) %>% 
  filter(event_year == 0) %>%
  select(-event_year)

dta_ytbt <- dta_untreated %>% 
  left_join(select(dta_untreated_non_model_vars, GEOID, year, event_year), 
            by = c('GEOID', 'year')) %>% 
  filter(event_year != 0) %>%
  select(-event_year)

nrow(dta_nevertreated)+nrow(dta_ytbt)==nrow(dta_untreated)
# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 
                           'dta_untreated', 'dta_treated', 
                           'dta_nevertreated', 'dta_ytbt', 
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars'))])
gc()
# -------------------------------------------------------------------------------------------- #

la_summary_function <- function(dta, dta_non_model_vars, value){
  
la_summary <- dta %>%
  
  left_join(select(dta_non_model_vars, GEOID, year, Geography), by = c('GEOID', 'year')) %>%
  
  mutate(Geography = case_when(Geography == 'Urban Cluster' ~ 'Urban',
                               Geography == 'Urbanized' ~ 'Urban',
                               TRUE ~ Geography)) %>%
  
  group_by(Geography) %>%
  
  summarise(across(.cols = matches('^low_access'), 
                   .fns = list(Total = sum, Share = mean), 
                   .names = '{.col}_{.fn}')) %>% 
  
  pivot_longer(cols = c('low_access_Total':'low_access_pers_Share'), 
               names_to = c('Access_Type', 'Statistic'), 
               names_pattern = '(.*)_(.*)', 
               values_to = value) %>% 
  
  mutate({{value}} := round(.data[[value]], digits = 3))
# -------------------------------------------------------------------------------------------- #
la_replacement_str <- c('^low_access$' = 'Low Access', 
                        'low_access_perm' = 'Low Access (Permanent)', 
                        'low_access_pers' = 'Low Access (Persistent)')
# -------------------------------------------------------------------------------------------- #
la_summary$Access_Type <- str_replace_all(la_summary$Access_Type, pattern = la_replacement_str)
# -------------------------------------------------------------------------------------------- #

return(la_summary)

}

la_summary_by_group <- pmap(list(dta = list(dta_untreated, dta_nevertreated, dta_ytbt, dta_treated), 
                                 dta_non_model_vars = list(dta_untreated_non_model_vars, dta_untreated_non_model_vars, 
                                                           dta_untreated_non_model_vars, dta_treated_non_model_vars), 
                                 value = list('Untreated', 'Never-Treated', 'Yet-to-be-Treated', 'Treated')), 
                            .f = la_summary_function)

la_summary_by_group <- la_summary_by_group %>% 
  reduce(.f = left_join, by = c('Geography', 'Access_Type', 'Statistic'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Select Socioeconomic and Demographic Variables. 
# -------------------------------------------------------------------------------------------- #
sociodemographic_vars <- c('inc_per_capita', 'poverty_rate', 'unemployed', 'total_population',
                           'pop_black', 'pop_hispanic', 'pop_asian', 'pop_white')
# -------------------------------------------------------------------------------------------- #
sociodemographic_summary_function <- function(dta, dta_non_model_vars, value){ 
  
 sociodem_summary <-dta %>%
  
  left_join(select(dta_non_model_vars, GEOID, year, Geography), by = c('GEOID', 'year')) %>%
  
  mutate(Geography = case_when(Geography == 'Urban Cluster' ~ 'Urban',
                               Geography == 'Urbanized' ~ 'Urban',
                               TRUE ~ Geography)) %>%
  
  group_by(Geography) %>%
  
  summarise(across(.cols = all_of(sociodemographic_vars), 
                   .fns = list(Mean = mean, SD = sd), 
                   .names = '{.col}_{.fn}')) %>% 
  
  pivot_longer(cols = c('inc_per_capita_Mean':'pop_white_SD'), 
               names_to = c('Variable', 'Statistic'), 
               names_pattern = '(.*)_(.*)', 
               values_to = value) %>% 
  
  mutate({{value}} := round(.data[[value]], digits = 3))

 # -------------------------------------------------------------------------------------------- #
 sociodemog_replacement_str <- c('pop_' = 'share ', 
                                 'total_' = '',
                                 '_' = ' ', 
                                 'inc' = 'income')
 # -------------------------------------------------------------------------------------------- #
 sociodem_summary$Variable <- str_to_title(str_replace_all(sociodem_summary$Variable, 
                                                        pattern = sociodemog_replacement_str))
 # -------------------------------------------------------------------------------------------- #
 return(sociodem_summary)
 
}
# -------------------------------------------------------------------------------------------- #
sociodemog_summary_by_group <- pmap(list(dta = list(dta_untreated, dta_nevertreated, dta_ytbt, dta_treated), 
                                         dta_non_model_vars = list(dta_untreated_non_model_vars, dta_untreated_non_model_vars, 
                                                                   dta_untreated_non_model_vars, dta_treated_non_model_vars), 
                                         value = list('Untreated', 'Never-Treated', 'Yet-to-be-Treated', 'Treated')),  
                                    .f = sociodemographic_summary_function)

sociodemog_summary_by_group <- sociodemog_summary_by_group %>% 
  reduce(.f = left_join, by = c('Geography', 'Variable', 'Statistic'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Retail Variables. 

# -------------------------------------------------------------------------------------------- #
retail_vars <- c("DS_Count_10mile_2005", "Convenience_Count_10mile_2005", "Drug_Count_10mile_2005",
                 "Superette_Count_10mile_2005", "Wholesale_Club_Count_10mile_2005",
                 "Gen_Merch_Count_10mile_2005", "Mass_Merch_Count_10mile_2005")
# -------------------------------------------------------------------------------------------- #
retail_summary_function <- function(dta, dta_non_model_vars, value){ 
  
  retail_summary <-dta %>%
    
    left_join(select(dta_non_model_vars, GEOID, year, Geography), by = c('GEOID', 'year')) %>%
    
    mutate(Geography = case_when(Geography == 'Urban Cluster' ~ 'Urban',
                                 Geography == 'Urbanized' ~ 'Urban',
                                 TRUE ~ Geography)) %>%
    
    group_by(Geography) %>%
    
    summarise(across(.cols = all_of(retail_vars), 
                     .fns = list(Mean = mean, SD = sd), 
                     .names = '{.col}_{.fn}')) %>% 
    
    pivot_longer(cols = c('DS_Count_10mile_2005_Mean':'Mass_Merch_Count_10mile_2005_SD'), 
                 names_to = c('Variable', 'Statistic'), 
                 names_pattern = '(.*)_(.*)', 
                 values_to = value) %>% 
    
    mutate({{value}} := round(.data[[value]], digits = 3))
  
  # -------------------------------------------------------------------------------------------- #
  retail_replacement_str <- c('_Count_10mile_2005' = '', 
                              'DS' = 'Dollar Stores', 
                              'Drug' = 'Drug Stores', 
                              'Convenience' = 'Convenience Stores', 
                              'Wholesale_Club' = 'Wholesale Club',
                              'Mass_Merch' = 'Mass Merchandiser', 
                              'Gen_Merch' = 'General Merchandiser')
  # -------------------------------------------------------------------------------------------- #
  retail_summary$Variable <- str_to_title(str_replace_all(retail_summary$Variable, 
                                                             pattern = retail_replacement_str))
  # -------------------------------------------------------------------------------------------- #
  return(retail_summary)
  
}
# -------------------------------------------------------------------------------------------- #
retail_summary_by_group <- pmap(list(dta = list(dta_untreated, dta_nevertreated, dta_ytbt, dta_treated), 
                                         dta_non_model_vars = list(dta_untreated_non_model_vars, dta_untreated_non_model_vars, 
                                                                   dta_untreated_non_model_vars, dta_treated_non_model_vars), 
                                         value = list('Untreated', 'Never-Treated', 'Yet-to-be-Treated', 'Treated')),  
                                    .f = retail_summary_function)

retail_summary_by_group <- retail_summary_by_group %>% 
  reduce(.f = left_join, by = c('Geography', 'Variable', 'Statistic'))
# -------------------------------------------------------------------------------------------- #