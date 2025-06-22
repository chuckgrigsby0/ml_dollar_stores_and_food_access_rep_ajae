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
# Create bootstrap sample of block groups stratified by cohort treatment time. 
# -------------------------------------------------------------------------------------------- #
event_year_levels <- sort(unique(dta_untreated$event_year))

geoids <- dta_untreated %>% select(GEOID, year, event_year) %>% 
  group_by(GEOID, event_year) %>%
  distinct(GEOID, event_year) %>%
  mutate(event_year = factor(event_year, levels = event_year_levels)) %>%
  ungroup()

bootstrap_sample <- bootstraps(geoids, strata = event_year, times = 1)

geoids_btst <- analysis(bootstrap_sample$splits[[1]]) # btst = bootstrap

#geoids_btst_tract <- geoids_btst %>% mutate(GEOID_TR = str_sub(GEOID, start = 1, end = -2))
# -------------------------------------------------------------------------------------------- #
# One to many join. 
# If block-group, g, is selected x times, then copies of all of its years of data are made x times. 
# -------------------------------------------------------------------------------------------- #
#geoids_btst %>% group_by(event_year) %>% count() %>% arrange(desc(n)) %>% ungroup() %>% mutate(total = sum(n), share = n/total)
dta_untreated %>% group_by(event_year) %>% arrange(event_year) %>% 
  count() %>% ungroup() %>% mutate(total = sum(n), share = n/total)

dta_untreated <- select(geoids_btst, GEOID) %>% left_join(dta_untreated, by = 'GEOID')

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

geoids_btst_tr <- geoids_btst %>% filter(event_year != 0) # Filter out never-treated. 

dta_treated <- select(geoids_btst_tr, GEOID) %>% left_join(dta_treated, by = 'GEOID')

# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  

dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# -------------------------------------------------------------------------------------------- #
# There are xx more GEOIDs in the untreated data than in the treated data because while the observations were 
# complete in the untreated data, by the time of the treated data, there were incomplete rows. 
# Checks
# -------------------------------------------------------------------------------------------- #
geoids_btst %>% filter(event_year != 0) %>% distinct(GEOID) %>% nrow()
length(unique(geoids_btst_tr$GEOID))
length(unique(dta_treated$GEOID))
# -------------------------------------------------------------------------------------------- #
#dta_untreated_non_model_vars %>% dplyr::filter(event_year != 0) %>% distinct(GEOID) %>% nrow()
#dta_treated_non_model_vars %>% dplyr::filter(event_year != 0) %>% distinct(GEOID) %>% nrow()

# -------------------------------------------------------------------------------------------- #

dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars), ends_with('bins'))

dta_treated <- dta_treated %>% select(all_of(model_vars))


# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Cross_Validation_Folds.R'))
# -------------------------------------------------------------------------------------------- #
dta_untreated_wfolds <- CV_Function(untreated_dta = dta_untreated,
                                    cv_type = 'horizontal equal-sized-block-cv', #  'horizontal rolling-origin-block-cv', 'horizontal equal-sized-block-cv'
                                    k = 7) # For horizontal CV try 6 or 8 to obtain 5 or 7 folds.  
# Use 5 when cv_type = 'vertical'. 
#'vertical' implies stratified sampling by GEOID. 

for (i in 1:7) print( dta_untreated_wfolds %>% filter(fold_id == i) %>% distinct(year) ) 
for (i in 1:7) print( dta_untreated_wfolds %>% filter(fold_id == i) %>% nrow() ) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 'acs_covars', 'econ_geog_vars',
                           'dta_untreated_wfolds', 'dta_treated', 
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars', 
                           'model_geography', 'model_dep_var', 'ncores', 'bootstrap_id'))])
gc()
# -------------------------------------------------------------------------------------------- #