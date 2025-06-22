# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on BG bootstrap or CT bootstrap. 
model_dep_var = Sys.getenv('model_dep_var') # Used in script below. If running for low_access_pers, must change settings below.
model_geography = Sys.getenv("model_geography") # Used in script below to subset by either Urban or Rural.
print(model_dep_var); print(model_geography)
options(scipen = 999)
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 
       'recipes', 'rsample', 'fixest', 'sf', 'tictoc', 'glmnet', 
       'future', 'furrr', 'parallel', 'doParallel')
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
model_vars <- names(dta_untreated)[!(names(dta_untreated) %in% non_model_vars)]; model_vars
# -------------------------------------------------------------------------------------------- #
# The NA observations are completely missing in covariates or are located in Puerto Rico, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_ut <- dta_untreated[!complete.cases(dta_untreated), ]  
dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]
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
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# -------------------------------------------------------------------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  
dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# -------------------------------------------------------------------------------------------- #
dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars), ends_with('bins'))
dta_treated <- dta_treated %>% select(all_of(model_vars))
# -------------------------------------------------------------------------------------------- #
dta_tr_and_ut <- bind_rows(dta_untreated, dta_treated)
 
dta_tr_and_ut <- dta_tr_and_ut %>% filter(year == '2020')

n_folds = 5 # Parameter specified in the function when cv_type == 'vertical'
set.seed(243444)
geoid_folds <- group_vfold_cv(dta_tr_and_ut, 
                              group = 'GEOID', 
                              v = n_folds, 
                              balance = 'groups')

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

holdout_ids <- bind_rows(holdout_ids)
holdout_ids <- holdout_ids %>%
  group_by(GEOID) %>%
  filter(row_number() == 1L)

dta_tr_and_ut_w_folds <- dta_tr_and_ut %>% 
  left_join(holdout_ids, by = c('GEOID')) %>% 
  relocate(fold_id)

# Check the share of observations in the assessment data frames. 
for (i in 1:n_folds){
  anal <- analysis(geoid_folds$splits[[i]])
  asses <- assessment(geoid_folds$splits[[i]])
  print(anal %>% inner_join(asses, by = c('GEOID', 'year')))
  print(nrow(asses)/nrow(anal))
  print(nrow(anal)/nrow(dta_tr_and_ut_w_folds))
}
# -------------------------------------------------------------------------------------------- #
for (i in 1:5) print( dta_tr_and_ut_w_folds %>% filter(fold_id == i) %>% nrow() ) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 'acs_covars', 'econ_geog_vars',
                           'dta_tr_and_ut_w_folds', 'dta_untreated', 'dta_treated', 'dta_treated_w_folds',
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars', 
                           'model_geography', 'model_dep_var', 'ncores'))])
gc()
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the dollar store bans and restrictions data. 
# -------------------------------------------------------------------------------------------- #
ds_bans <- readr::read_csv(here::here('Data', 'block_groups_w_ds_policies.csv'), show_col_types = FALSE )
# Use to select relvant variables below. 
policy_vars = names(ds_bans) %>% str_subset(string=., pattern='^Def|^Mor|^Ord|^policy_*')
# -------------------------------------------------------------------------------------------- #
# Load the block-group level data with geographic information. 
# -------------------------------------------------------------------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))
bg_pop_centroids_10_sfp_geo <- bg_pop_centroids_10_sfp_geo %>% select(GEOID, Geography) %>% st_drop_geometry()
# -------------------------------------------------------------------------------------------- #
# Join geographic information to dollar store policy data. 
# -------------------------------------------------------------------------------------------- #
ds_bans <- ds_bans %>% left_join(bg_pop_centroids_10_sfp_geo, by = 'GEOID')
# -------------------------------------------------------------------------------------------- #
ds_bans_modeling <- ds_bans %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  mutate(Restrictions = Moratorium + Ordinance) %>% 
  mutate(
    Defeated_and_Restrictions = case_when(Defeated >= 1 & Restrictions == 0 ~ Defeated + Restrictions, 
                                          Defeated >= 1 & Restrictions >= 1 ~ Defeated + Restrictions, 
                                          Defeated == 0 & Restrictions >= 1 ~ 0, 
                                          Defeated == 0 & Restrictions == 0 ~ 0),
    Restrictions_and_Defeated = case_when(Restrictions >= 1 & Defeated == 0 ~ Defeated + Restrictions, 
                                          Restrictions >= 1 & Defeated >= 1 ~ Defeated + Restrictions, 
                                          Restrictions == 0 & Defeated >= 1 ~ 0, 
                                          Restrictions == 0 & Defeated == 0 ~ 0),
    policy_total_binary = if_else(policy_total > 0, 1, 0) ) %>%
  
  relocate(c(Restrictions, contains('_and_'), policy_total_binary), .before = policy_total) %>%
  select(GEOID, policy_total_binary, Restrictions_and_Defeated, Defeated_and_Restrictions) %>% 
  mutate(across(.cols = c(Restrictions_and_Defeated, Defeated_and_Restrictions), 
                .fns = \(x) if_else(x > 0, 1, 0), 
                .names = '{.col}_binary' ) )

sel_ds_modeling_vars <- names(ds_bans_modeling) %>% str_subset(pattern = 'binary')

# -------------------------------------------------------------------------------------------- #
sel_modeling_vars <- names(dta_tr_and_ut_w_folds) %>% 
  str_subset(pattern = 'GEOID|^year$|^low_access|_perm$|_pers$|_x_', negate = TRUE)
# -------------------------------------------------------------------------------------------- #
dta_model <- dta_tr_and_ut_w_folds %>% 
  left_join(ds_bans_modeling, by = 'GEOID', multiple = 'all', relationship = 'many-to-one') %>%
  select(all_of(sel_ds_modeling_vars), all_of(sel_modeling_vars))
# -------------------------------------------------------------------------------------------- #
dep_vars <- str_subset(names(dta_model), pattern = '_binary$')
indep_vars <- str_subset(names(dta_model), pattern = '_binary$|fold_id', negate = TRUE)
fold_idx <- (dta_model$fold_id)
# -------------------------------------------------------------------------------------------- #


#Set seed for replication. 
set.seed(123)
# Function to run lasso with CV blocked by block-group and select the optimal coefficients w.r.t. predicting ds policy vars. 
ds_policy_on_covars_lasso <- function(dta_arg, dep_var_arg){ 
  
  reg_formula <- xpd(.[dep_var_arg] ~ ..ctrl - 1, ..ctrl = indep_vars, data = dta_arg)
  # -------------------------------------------------------------------------------------------- #
  # Create model matrices. 
  #--------------------------------------------------------------------------------------------#
  x_train <- model.matrix(reg_formula, data = dta_arg)
  y_train <- dta_arg %>% select(matches(dep_var_arg)) %>% pull()
  
  
#Run 5 fold CV using the training data, using the unique values of alpha. 
output <- cv.glmnet(x_train, y_train, 
                    alpha = 1,
                    standardize = TRUE,
                    family = 'binomial', 
                    type.measure = 'class', 
                    foldid = fold_idx, 
                    trace.it = FALSE, 
                    parallel = TRUE)

opt_lambda <- output$lambda.min

#Get the significant coefficients from the optimal model to re-run them in a final model. 
tidy_enet_coef_train <- (coef(output, s = opt_lambda)) %>% 
  as.matrix() %>% 
  data.frame() %>%
  rename('Estimate' = 's1')

tidy_enet_coef_train <- tibble::rownames_to_column(tidy_enet_coef_train, 'Variable')

tidy_enet_coef_train <- tidy_enet_coef_train %>%

   filter(Estimate != 0) %>%

  mutate(dep_var = dep_var_arg)

return(tidy_enet_coef_train)

}
# -------------------------------------------------------------------------------------------- #

# For Parallel
ncores = parallelly::availableCores()
#plan('multicore', workers = ncores)
#options(future.globals.maxSize= 8388608000)
cl <- makeCluster(ncores)
registerDoParallel(cl)
getDoParWorkers()

# -------------------------------------------------------------------------------------------- #
# tic()
# lasso_coefs <- map_dfr(dep_vars, function(.x){
#    ds_policy_on_covars_lasso(dta_arg = dta_model, dep_var_arg = .x) 
#  } ) 
# toc()
lasso_coefs <- ds_policy_on_covars_lasso(dta_arg = dta_model, dep_var_arg = 'policy_total_binary') 
# -------------------------------------------------------------------------------------------- #
# Save as table. 
directory_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
readr::write_csv(lasso_coefs, 
                 file = here::here('Analysis', 
                                   'Tables', 
                                   directory_dep_var, 
                                   str_to_title(model_geography),
                                   paste0(str_to_lower(model_geography), '_' , 
                                          'lasso_coefs_ds_policy_on_covars', '.csv')))

saveRDS(lasso_coefs, here::here('Analysis', 
                                'Tables', 
                                directory_dep_var, 
                                str_to_title(model_geography),
                                paste0(str_to_lower(model_geography), '_' , 
                                       'lasso_coefs_ds_policy_on_covars', '.rds')))
# -------------------------------------------------------------------------------------------- #