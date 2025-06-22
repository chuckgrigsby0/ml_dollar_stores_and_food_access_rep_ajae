# Script that, when sourced, will obtain a list of covariate names, separated by predictor category (model_covars_list), and 
# a list of "tidy" covariate names to be used for plots (model_covar_names). 
#--------------------------------------------------------------------------------------------#
Get_Vars_Function <- function(grepl_string, dta){
  covars = names(dta)[!grepl(grepl_string, names(dta))]; return(covars)
}
# -------------------------------------------------------------------------------------------- #
covars_string <- c('perm$|pers$|^low_access$|GEOID|^year$|fold_id$',
                   'low_access$|pers$|^low_access_perm$|GEOID|^year$|fold_id$', 
                   'low_access$|perm$|^low_access_pers$|GEOID|^year$|fold_id$')
# -------------------------------------------------------------------------------------------- #

covars_list <- map(covars_string, function(.x) Get_Vars_Function(.x, dta_untreated_wfolds) )

dep_var_vector <- c('low_access', 'low_access_perm', 'low_access_pers')

covars_list <- set_names(covars_list, nm = dep_var_vector)

# -------------------------------------------------------------------------------------------- #
# Select only the variable set pertaining to the dependent variable. 
# -------------------------------------------------------------------------------------------- #
covars_list <- covars_list[model_dep_var]
# -------------------------------------------------------------------------------------------- #
# ACS covars.
# -------------------------------------------------------------------------------------------- #
acs_covars_list <- covars_list %>% map(function(.x) .x[ (.x %in% acs_covars) ] )
# -------------------------------------------------------------------------------------------- #
# Rename lists. 
# -------------------------------------------------------------------------------------------- #
names(acs_covars_list) <- 'socioeconomics' # Fix name.

store_covars_list <- covars_list %>% map(function(.x) .x[grepl('Count_[[:digit:]]+mile', .x) ] )

names(store_covars_list) <- 'retail' # Fix name.
# -------------------------------------------------------------------------------------------- #
# Subset econ_geog_vars based on outcome variable. 
# -------------------------------------------------------------------------------------------- #
econ_geog_vars <- econ_geog_vars %>% map(function(.x) .x[(.x %in% unlist(covars_list))])  
# -------------------------------------------------------------------------------------------- #
urban_area_dummies <- unlist(covars_list)[(unlist(covars_list) %in% c('urban_area', 'uc_area'))]
urban_areas_name = urban_area_dummies %>% 
  str_replace_all(pattern = c('urban_area' = 'urbanized', 
                              'uc_area' = 'urban_cluster'))
urban_area_dummies <- set_names(urban_area_dummies, nm = urban_areas_name)
# -------------------------------------------------------------------------------------------- #
# Combine covariate lists. 
# -------------------------------------------------------------------------------------------- #
model_covars_list <- c(acs_covars_list, econ_geog_vars, urban_area_dummies, store_covars_list)
# -------------------------------------------------------------------------------------------- #
# For disaggregated covariate lists. 
model_covars_list_disag <- model_covars_list

# Tidy aggregated covariate lists. 

# For aggregated covariate lists, combine lists into more concise variable lists. 
fes_list <- model_covars_list[grepl('^fes_', names(model_covars_list))] %>% reduce(.f = c)
econ_geog_list <- model_covars_list[grepl('land_use|park_access|schools|^distance_to|roads|^urban', 
                                          names(model_covars_list))] %>% 
  reduce(.f = c)
# Remove the previously saved and combined lists. 
model_covars_list <- model_covars_list[!grepl('^fes_|land_use|park_access|schools|^distance_to|roads|^urban', names(model_covars_list))]
# Recombine lists. 
model_covars_list <- c(model_covars_list,
                       list('economic_geography' = econ_geog_list), 
                       list('fixed_effects' = fes_list))

# -------------------------------------------------------------------------------------------- #
if (model_geography == 'Urban'){
# Tidy disaggregated covariate lists. 
urban_areas_list <- model_covars_list_disag[grepl('^urban', names(model_covars_list_disag))] %>% reduce(.f = c)
# Remove the previously saved and combined lists. 
model_covars_list_disag <- model_covars_list_disag[!grepl('^fes_|^urban', names(model_covars_list_disag))]
# Recombine lists. 
model_covars_list_disag <- c(model_covars_list_disag,
                       list('urban_areas' = urban_areas_list), 
                       list('fixed_effects' = fes_list))
} else if (model_geography == 'Rural'){ 
  # Tidy disaggregated covariate lists. 
  # Remove the previously saved and combined lists. 
  model_covars_list_disag <- model_covars_list_disag[!grepl('^fes_', names(model_covars_list_disag))]
  # Recombine lists. 
  model_covars_list_disag <- c(model_covars_list_disag,
                               list('fixed_effects' = fes_list))
  }
# -------------------------------------------------------------------------------------------- #
# Tidy the covariate names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_covar_names.R'))
model_covar_names <- model_covars_list %>% map(function(.x) tidy_covar_names(covar_name_str = .x))
model_covar_names_disag <- model_covars_list_disag %>% map(function(.x) tidy_covar_names(covar_name_str = .x))
# -------------------------------------------------------------------------------------------- #
# US Region and Division key-value pairs. 
us_regions_key <- data.frame(REGION = as.character(seq(1, 4, 1)), 
                             REGION_NAME = c('Northeast', 'Midwest', 'South', 'West'))

us_divisions_key <- data.frame(DIVISION = as.character(seq(1, 9, 1)), 
                               DIVISION_NAME = c('New England', 'Middle Atlantic', 'East North Central', 'West North Central', 
                                                 'South Atlantic', 'East South Central', 'West South Central', 'Mountain', 'Pacific'))
# -------------------------------------------------------------------------------------------- #
cat('Loaded: \n model_covars_list \n model_covar_names \n model_covars_list_disag \n model_covar_names_disag \n us_regions_key \n us_divisions_key \n')
# -------------------------------------------------------------------------------------------- #
      