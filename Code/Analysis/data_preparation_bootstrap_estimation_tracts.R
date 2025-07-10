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
non_model_vars <- c('DS_Count_10mile', 'DS_Count_10mile_diff',
                    'entry', 'entry_events', 'event_year', 'net_entry_cumsum', 'rel_year', 'treat', 
                    'Grocery_Count_10mile', 'Grocery_Count_10mile_diff', 'Grocery_Count_10mile_2005', 'total_low_access', 
                    'STATE', 'market', 'market_name_full', 'Geography')
# Modeling variables (Columns needed for imputation models). 
model_vars <- names(dta_untreated)[!(names(dta_untreated) %in% non_model_vars)]
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates or are located in Puerto Rico, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_ut <- dta_untreated[!complete.cases(dta_untreated), ]  
dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Create bootstrap sample at census-tract level.
# -------------------------------------------------------------------------------------------- #
geoids <- dta_untreated %>% 
  mutate(GEOID_TR = str_sub(GEOID, start = 1, end = -2)) %>%
  select(GEOID, GEOID_TR, year, event_year)

geoids <- geoids %>% 
  group_by(GEOID_TR) %>%
  distinct(GEOID_TR) %>%
  ungroup()

bootstrap_sample <- bootstraps(geoids, times = 1)

geoids_btst <- analysis(bootstrap_sample$splits[[1]]) # btst = bootstrap

print(length(unique(geoids_btst$GEOID_TR))/nrow(geoids_btst)) # Share of unique tracts to total number of tracts.
# -------------------------------------------------------------------------------------------- #
dta_untreated %>% group_by(event_year) %>% arrange(event_year) %>% 
  count() %>% ungroup() %>% mutate(total = sum(n), share = n/total)

# Create Census Tract GEOID. 
dta_untreated <- dta_untreated %>% mutate(GEOID_TR = str_sub(GEOID, start = 1, end = -2))

dta_untreated <- select(geoids_btst, GEOID_TR) %>% left_join(dta_untreated, 
                                                             by = 'GEOID_TR', 
                                                             multiple = 'all', 
                                                             relationship = 'many-to-many')

dta_untreated %>% group_by(event_year) %>% arrange(event_year) %>% 
  count() %>% ungroup() %>% mutate(total = sum(n), share = n/total)
# -------------------------------------------------------------------------------------------- #

# Separate the modeling data from the non-modeling data. 

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
# Subset the treated data based on the bootstrap sample. 
# -------------------------------------------------------------------------------------------- #

dta_treated <- dta_treated %>% mutate(GEOID_TR = str_sub(GEOID, start = 1, end = -2))

dta_treated <- select(geoids_btst, GEOID_TR) %>% left_join(dta_treated, 
                                                           by = 'GEOID_TR', 
                                                           multiple = 'all', 
                                                           relationship = 'many-to-many')

# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  

dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# -------------------------------------------------------------------------------------------- #

dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars), ends_with('bins'))

dta_treated <- dta_treated %>% select(all_of(model_vars))

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Cross_Validation_Folds.R'))
# -------------------------------------------------------------------------------------------- #
dta_untreated_wfolds <- CV_Function(untreated_dta = dta_untreated,
                                    cv_type = 'horizontal equal-sized-block-cv', 
                                    k = 7)

for (i in 1:7) print( dta_untreated_wfolds %>% filter(fold_id == i) %>% distinct(year) ) 
for (i in 1:7) print( dta_untreated_wfolds %>% filter(fold_id == i) %>% nrow() ) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 'acs_covars', 'econ_geog_vars',
                           'dta_untreated_wfolds', 'dta_treated', 
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars', 
                           'model_geography', 'model_dep_var', 'ncores', 'bootstrap_id', 'bootstrap_by_tracts'))])
gc()
# -------------------------------------------------------------------------------------------- #