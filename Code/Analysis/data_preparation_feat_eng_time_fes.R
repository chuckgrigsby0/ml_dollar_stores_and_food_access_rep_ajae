# -------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------- #
#.libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
.libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'sf', 'purrr', 'tidyr', 'stringr', 'recipes', 'rsample', 
       'glmnet', 'broom', 'tidymodels', 'fixest', 'doParallel', 'doMC', 'furrr', 'tictoc')
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_feat_eng_time_fes.R'))
# -------------------------------------------------------------------------------------------- #
# Create interaction terms and one-hot encoded dummy variables for the year fixed effects. 
# -------------------------------------------------------------------------------------------- #
# Create untreated data for FEs estimation of time effects
# interactions = TRUE or FALSE to include interactions/polynomials
Untreated_Data_W_Interactions_Function <- function(panel_untreated_dta,
                                                   panel_df_access_inds_dist_band,
                                                   retail_counts_2005_dist_band,
                                                   DS_Count_dist_band, 
                                                   Grocery_Count_dist_band_diff, 
                                                   state_dummy_data,
                                                   interactions){ 
# -------------------------------------------------------------------------------------------- #
# Interactions b/w covariates. 
# -------------------------------------------------------------------------------------------- #
# Create the recipe with steps. 
interactions_recipe <- covar_df_sel %>% 
  
  recipe(~ . , data = .) %>%
  
  #step_rm(GEOID, year) %>%
  
  step_interact(terms = ~where(is.numeric):where(is.numeric))
# Prepare the recipe and bake. 
interactions_baked <- interactions_recipe %>%  
  
  prep(training = NULL) %>% 
  
  bake(., GEOID, year, contains('_x_'), new_data = NULL) 
# -------------------------------------------------------------------------------------------- #
# Polynomials to 2nd degree of covariates. 
# -------------------------------------------------------------------------------------------- #
# Create the recipe with steps. 
poly_recipe <- covar_df_sel %>% 
  
  recipe(~ . , data = .) %>%
  #step_rm(GEOID, year) %>%
  
  step_poly(where(is.numeric), degree = 2, options = list(raw = TRUE)) # raw = TRUE means that we want un-transformed polys.
# Prepare the recipe and bake. 
poly_baked <- poly_recipe %>%  
  
  prep(training = NULL) %>% 
  
  bake(., GEOID, year, contains('poly_2'), new_data = NULL) 
# -------------------------------------------------------------------------------------------- #
# Dummy Variables for the years. 
# Note that I used the untreated data to create the dummy variables. 
# -------------------------------------------------------------------------------------------- #
# Create the recipe with steps. 
year_dummy_recipe <- panel_untreated_dta %>% 
  
  recipe(~. , data = .) %>%
  #step_rm(GEOID) %>%
  
  step_dummy(year, 
             one_hot = TRUE, 
             naming = function(var, lvl, ordinal = FALSE){ paste0(var, '_', lvl)}, 
             keep_original_cols = TRUE)
# Prepare the recipe and bake. 
year_dummy_baked <- year_dummy_recipe %>%  
  
  prep(training = NULL) %>% 
  
  bake(., GEOID, year, contains('year_2'), new_data = NULL) # %>%
  
  # select(-year_2005)

year_dummy_baked <- year_dummy_baked %>% mutate(GEOID = as.character(GEOID))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
if (interactions == TRUE){
  feat_eng_vars <- list(covar_df_sel,
                        interactions_baked, 
                        poly_baked) %>%
    reduce(left_join, by = c('GEOID', 'year')) %>%
    select(GEOID, year, sort(names(.)))
}
else if (interactions == FALSE){
  feat_eng_vars <- list(covar_df_sel) %>%
    reduce(left_join, by = c('GEOID', 'year')) %>%
    select(GEOID, year, sort(names(.)))
}
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

feat_eng_vars_join <- year_crosswalk %>% 
  left_join(feat_eng_vars, by = 'year') %>% 
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
dta_untreated <- list(panel_untreated_dta, 
                      panel_df_access_inds_dist_band, 
                      feat_eng_vars_join, 
                      land_use, 
                      year_dummy_baked, 
                      state_dummy_data) %>%
  reduce(left_join, by = c('GEOID', 'year')) %>%
  ungroup() %>%
  left_join(retail_counts_2005_dist_band, by = 'GEOID') %>% 
  left_join(park_access, by = 'GEOID') %>%
  left_join(schools, by = 'GEOID') %>%
  left_join(dist_to_urb, by = 'GEOID') %>%
  left_join(roads, by = 'GEOID') %>%
  select(-c({{DS_Count_dist_band}}:{{Grocery_Count_dist_band_diff}}, total_low_access)) #10min

# Add the geography variables to identify areas by urban, urban-cluster, and rural. 
dta_untreated <- dta_untreated %>%
  left_join(select(st_drop_geometry(bg_pop_centroids_10_sfp_geo), GEOID, Geography), by = 'GEOID') %>%
  mutate(urban_area = if_else(Geography == 'Urbanized', 1, 0), 
         uc_area = if_else(Geography == 'Urban Cluster', 1, 0))

dta_untreated <- dta_untreated %>% select(-STATE_Puerto_Rico)

return(dta_untreated)
}
# -------------------------------------------------------------------------------------------- #
#dta_untreated <- Untreated_Data_W_Interactions_Function(panel_untreated_dta = panel_df_ds_entry_10min$untreated,
 #                                                       panel_df_access_inds_dist_band = panel_df_access_inds_10min,
  #                                                      ds_counts_2005_dist_band = ds_counts_2005_10min,
   #                                                     DS_Count_dist_band = 'DS_Count_10min', 
    #                                                    Grocery_Count_dist_band_diff = 'Grocery_Count_10min_diff', 
     #                                                   interactions = FALSE)
# -------------------------------------------------------------------------------------------- #
# dta_untreated <- Untreated_Data_W_Interactions_Function(panel_untreated_dta = panel_df_ds_entry_2_and_5mile$untreated,
#                                        panel_df_access_inds_dist_band = panel_df_access_inds_2_and_5mile,
#                                      retail_counts_2005_dist_band = retail_counts_2005_2_and_5mile, # We use year 2005 store counts as a baseline.
#                                      DS_Count_dist_band = 'DS_Count_5mile', 
#                                      Grocery_Count_dist_band_diff = 'Grocery_Count_5mile_diff',
#                                      state_dummy_data = state_dummy_baked,
#                                      interactions = FALSE)
# -------------------------------------------------------------------------------------------- #
dta_untreated <- Untreated_Data_W_Interactions_Function(panel_untreated_dta = panel_df_ds_entry_2_and_10mile$untreated,
                                                        panel_df_access_inds_dist_band = panel_df_access_inds_2_and_10mile,
                                                        retail_counts_2005_dist_band = retail_counts_2005_2_and_10mile, # We use year 2005 store counts as a baseline.
                                                        DS_Count_dist_band = 'DS_Count_10mile', 
                                                        Grocery_Count_dist_band_diff = 'Grocery_Count_10mile_diff',
                                                        state_dummy_data = state_dummy_baked,
                                                        interactions = FALSE)
#--------------------------------------------------------------------------------------------#

# To create state by time fixed effects. 

# source(here::here('Code', 'Functions', 'Function_state_by_time_interactions.R'))
# state_time_interactions <- state_by_time_interactions(dta_untreated)


gc()
#--------------------------------------------------------------------------------------------#
# dta_untreated <- dta_untreated %>%
#   left_join(state_time_interactions, by = c('GEOID', 'year'))
#--------------------------------------------------------------------------------------------#
#Create model.matrix objects for the training and test data. 
#--------------------------------------------------------------------------------------------#
#Training. 
dta <- dta_untreated 
nas <- dta[!complete.cases(dta), ]
dta <- dta[complete.cases(dta), ]
# -------------------------------------------------------------------------------------------- #
data_for_folds <- dta %>% select(GEOID, year)
# Obtain cross-validation fold IDs for the lasso regression model to obtain FE estimates. 
# -------------------------------------------------------------------------------------------- #
# dta$GEOID_fact <- factor(dta$GEOID) # Optional for use in group_vfold_cv
n_folds = 5

set.seed(243444)
geoid_folds <- group_vfold_cv(data_for_folds, 
                              group = GEOID, 
                              v = n_folds, 
                              balance = 'groups')
# -------------------------------------------------------------------------------------------- #
# Check the share of observations in the assessment data frames. 
# -------------------------------------------------------------------------------------------- #
for (i in 1:n_folds){
print(i)
  anal <- analysis(geoid_folds$splits[[i]])
  asses <- assessment(geoid_folds$splits[[i]])
  print(sum(anal$GEOID %in% asses$GEOID))
  print(sum(asses$GEOID %in% anal$GEOID))
  print(nrow(asses)/nrow(anal))
  print(nrow(asses)/nrow(dta))
  print(nrow(anal)/nrow(dta))
}
# -------------------------------------------------------------------------------------------- #
holdout_ids <- list(NULL)
for(i in 1:n_folds){
  print(i)
  holdout_dta <- data.frame()
  holdout_dta <- assessment(geoid_folds$splits[[i]]) %>%
    add_resample_id(geoid_folds$splits[[i]]) 
  
  holdout_dta <- holdout_dta %>%
    select(GEOID, id) %>%
    mutate(fold_id = as.numeric(str_remove(id, 'Resample'))) %>%
    select(-id)
  
  holdout_ids[[i]] <- holdout_dta
  
}
# -------------------------------------------------------------------------------------------- #
holdout_ids <- bind_rows(holdout_ids)

# Either of the following work and are identical. 
holdout_ids <- holdout_ids %>% 
  group_by(GEOID) %>%
  filter(row_number() == 1L) %>%
  arrange(GEOID)%>% ungroup()

#holdout_ids <- holdout_ids %>% 
  #group_by(GEOID, fold_id) %>%
  #distinct(GEOID, fold_id) 
# saveRDS(holdout_ids, here::here('Data', 'holdout_ids_for_fixed_effects.rds'))
# -------------------------------------------------------------------------------------------- #
dta <- dta %>%
  left_join(holdout_ids, by = 'GEOID')# %>%
  #select(-GEOID_fact)
# -------------------------------------------------------------------------------------------- #
fold_id <- dta$fold_id # Needed to supply to glmnet. 
# -------------------------------------------------------------------------------------------- #
# 5 miles
# -------------------------------------------------------------------------------------------- #
# dta <- dta %>% select(-c(GEOID, year, Grocery_Count_5mile_2005, Geography, fold_id))
# -------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------- #
# 10 miles
# -------------------------------------------------------------------------------------------- #
dta <- dta %>% select(-c(GEOID, year, Grocery_Count_10mile_2005, Geography, fold_id))
# -------------------------------------------------------------------------------------------- #
# Use the fixest package functions of ..ctrl and xpd to conveniently create
# formula and regression variable macros. 
# -------------------------------------------------------------------------------------------- #
#library(future)
#--------------------------------------------------------------------------------------------#
#Register parallel backend
#--------------------------------------------------------------------------------------------#
#availableWorkers()
#availableCores()
#plan(cluster, workers = availableWorkers()) # or availableCores()
#--------------------------------------------------------------------------------------------#
# Source Function that will compute time fixed effects using the Lasso. 
# Note that the function estimates models without intercepts because I include 
# all years except for the year 2005 as dummy variables to obtain a vector of time fixed effects
# and state fixed effects. 
# Also note that I have commented out the section with interactions and polynomials. 
# I determined that model fit improved minimally with the interactions, so the faster model 
# is to include only the base covariates. 
#--------------------------------------------------------------------------------------------#
source(here::here('Code', 'Functions', 'Function_Lasso_FE_Estimates.R'))
# Helper function to cleanly and efficiently obtain the non-zero glmnet coefficients. 
source(here::here('Code', 'Functions', 'Function_tidy_glmnet.R'))
#--------------------------------------------------------------------------------------------#
# Create exclusion variables for the function. 
#--------------------------------------------------------------------------------------------#
la_names <- names(dta)[grepl('low_access', names(dta))]
la_names_exclude <- map(seq(1, 3, by = 1), function(.x) la_names[-.x])
la_names_exclude <- set_names(la_names_exclude, la_names)
#--------------------------------------------------------------------------------------------#
rm(list=ls()[!(ls() %in% c('dta', 'la_names_exclude', 'Lasso_FE_Estimates_Function', 'tidy_glmnet', 
                           'fold_id', 'n_folds', 'la_names'))])
gc()
options(scipen=999)
#--------------------------------------------------------------------------------------------#
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# getDoParWorkers()
# plan(cluster, workers = no_cores)
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
# Change the names according to the distance band. 
#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
# 5-mile
#--------------------------------------------------------------------------------------------#
# start.time <- Sys.time()
# fe_estimates_5mile <- la_names_exclude %>% map(function(.x){
# Lasso_FE_Estimates_Function(model_data = dta, la_vars_exclude1 = .x[1], la_vars_exclude2 = .x[2])
# })
#--------------------------------------------------------------------------------------------#
# fe_estimates_5mile <- set_names(fe_estimates_5mile, nm = la_names)
# saveRDS(fe_estimates_5mile, file = here::here('Data', 'fe_estimates_5mile.rds'))
#--------------------------------------------------------------------------------------------#
# end.time <- Sys.time()
# end.time-start.time
#--------------------------------------------------------------------------------------------#
# 10-mile
#--------------------------------------------------------------------------------------------#
ncores = parallelly::availableCores()
tic()
fe_estimates_10mile <- la_names_exclude %>% map(function(.x){
Lasso_FE_Estimates_Function(model_data = dta, la_vars_exclude1 = .x[1], la_vars_exclude2 = .x[2], post_selection_output = TRUE)
})
#--------------------------------------------------------------------------------------------#
fe_estimates_10mile <- set_names(fe_estimates_10mile, nm = la_names)
saveRDS(fe_estimates_10mile, file = here::here('Data', 'fe_estimates_10mile_lasso.rds'))
#--------------------------------------------------------------------------------------------#
toc()