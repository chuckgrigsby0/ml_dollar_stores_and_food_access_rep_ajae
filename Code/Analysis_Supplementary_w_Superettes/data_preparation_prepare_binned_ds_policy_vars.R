# -------------------------------------------------------------------------------------------- #
.libPaths()
# Load empirical data and point estimates. 
model_dep_var = 'low_access'; model_geography = 'Rural' # Change arguments from Urban to Rural 
print(model_geography); print(model_dep_var)
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts' # or NULL to bootstrap by block-group and stratify by relative time. 
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
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
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%
  
  left_join(select(ds_bans, GEOID, zip_code_id, City, all_of(policy_vars)),
            by = 'GEOID',
            multiple = 'all', relationship = 'many-to-one') %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, market) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(ds_bans, GEOID, zip_code_id, City, all_of(policy_vars)),
            by = 'GEOID',
            multiple = 'all', relationship = 'many-to-one') %>%
  
  filter(year >= '2006') # We obtain post-treatment predictions from 2006-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
 filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_prepare_binned_ds_policy_vars_posttreatment.R'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
binned_ds_policy_vars <- prepare_binned_ds_policy_vars(df = model_preds)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
filename = paste0('posttreatment_binned_ds_policy_vars_', str_to_lower(model_geography), '.rds')
saveRDS(binned_ds_policy_vars, file = here::here('Data', 'Data_2_and_10_Miles', filename))
# -------------------------------------------------------------------------------------------- #
