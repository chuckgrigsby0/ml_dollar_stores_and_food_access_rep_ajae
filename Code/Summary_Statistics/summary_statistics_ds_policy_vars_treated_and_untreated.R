# -------------------------------------------------------------------------------------------- #
.libPaths()
# Load empirical data and point estimates. 
model_dep_var = 'low_access'; model_geography = 'Urban' # Change arguments from Urban to Rural 
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
# -------------------------------------------------------------------------------------------- #
# Load the block-group level data with geographic information. 
# -------------------------------------------------------------------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))
bg_pop_centroids_10_sfp_geo <- bg_pop_centroids_10_sfp_geo %>% select(GEOID, Geography) %>% st_drop_geometry()
# -------------------------------------------------------------------------------------------- #
# Join geographic information to dollar store policy data. 
# -------------------------------------------------------------------------------------------- #
ds_bans_geo <- ds_bans %>% left_join(bg_pop_centroids_10_sfp_geo, by = 'GEOID')
# -------------------------------------------------------------------------------------------- #
# Join geographic information to dollar store policy data. 
# -------------------------------------------------------------------------------------------- #

ds_bans_stats_tr_and_ut <- function(ds_bans_arg, dta_tr_and_ut_arg, event_year_arg, geog_arg, tr_type){

  condition_sym <- dplyr::enquos(event_year_arg)
  
  ds_bans_arg <- select(dta_tr_and_ut_arg, GEOID, event_year) %>% 
    left_join(ds_bans_arg, by = 'GEOID', 
              multiple = 'all', relationship = 'many-to-one')
  
  ds_bans_arg <- ds_bans_arg %>% group_by(GEOID) %>% 
    filter(row_number() == 1) %>% ungroup() %>% 
    filter(!!!condition_sym)
  
  ds_bans_arg <- ds_bans_arg %>% 
    mutate(Restrictions = Moratorium + Ordinance) %>% 
    relocate(c(Restrictions), .after = Ordinance)
  
  # Use to select relevant variables below. 
  policy_vars = names(ds_bans_arg) %>% str_subset(string=., pattern='^Def|^Mor|^Ord|Rest|^policy_*')
  
  sel_policy_vars <- str_subset(policy_vars, pattern = 'year', negate=TRUE); sel_policy_vars
  
  ds_bans_arg <- ds_bans_arg %>% 
    
    mutate(across(.cols = all_of(sel_policy_vars), 
                  .fns = \(x) if_else(x > 0, 1, 0), 
                  .names = '{.col}_binary') )
  
  ds_bans_stats_any <- ds_bans_arg %>% 
    
    summarise(across(.cols = matches('_binary$'), 
                     .fns = list('total' = sum), 
                     .names = '{.fn}_{.col}'))
  
  first_and_last_cols <- c(names(ds_bans_stats_any)[1], 
                           names(ds_bans_stats_any)[length(names(ds_bans_stats_any))])  
  
  ds_bans_stats_any <- ds_bans_stats_any %>%
    
    pivot_longer(cols = first_and_last_cols[1]:first_and_last_cols[2], 
                 names_to = c('statistic', 'variable'), 
                 names_pattern = '(mean|total)_(.*)$') 
  
  ds_bans_stats_any <- ds_bans_stats_any %>% 
    mutate(across(.cols = 'variable', 
                  .fns = \(x) str_replace_all(x, pattern = '_binary', replacement = '') ) )
  
  ds_bans_stats_any$type <- tr_type
  
  return(ds_bans_stats_any)
  
}

ds_bans_stats_tr <- ds_bans_stats_tr_and_ut(ds_bans_arg = ds_bans_geo, 
                                            dta_tr_and_ut_arg = dta_treated_non_model_vars, 
                                            event_year_arg = c(event_year > 0), 
                                            geog_arg = model_geography, tr_type = 'Treated')

ds_bans_stats_ut <- ds_bans_stats_tr_and_ut(ds_bans_arg = ds_bans_geo, 
                                            dta_tr_and_ut_arg = dta_untreated_non_model_vars, 
                                            event_year_arg = c(event_year == 0), 
                                            geog_arg = model_geography, tr_type = 'Untreated')

ds_bans_stats_all <- bind_rows(ds_bans_stats_tr, ds_bans_stats_ut)
# -------------------------------------------------------------------------------------------- #

print(paste0('There are ', filter(ds_bans_stats_all, variable == 'policy_total' & type == 'Untreated') %>% select(value) , 
             ' defeats and restrictions in the Untreated data.') )

print(paste0('There are ', filter(ds_bans_stats_all, variable == 'policy_total' & type == 'Treated') %>% select(value) , 
             ' defeats and restrictions in the Treated data.') )
# -------------------------------------------------------------------------------------------- #
ds_bans_stats_all_urban <- ds_bans_stats_all

# Re-run for Rural

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
# -------------------------------------------------------------------------------------------- #
# Load the block-group level data with geographic information. 
# -------------------------------------------------------------------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))
bg_pop_centroids_10_sfp_geo <- bg_pop_centroids_10_sfp_geo %>% select(GEOID, Geography) %>% st_drop_geometry()
# -------------------------------------------------------------------------------------------- #
# Join geographic information to dollar store policy data. 
# -------------------------------------------------------------------------------------------- #
ds_bans_geo <- ds_bans %>% left_join(bg_pop_centroids_10_sfp_geo, by = 'GEOID')
# -------------------------------------------------------------------------------------------- #
# Join geographic information to dollar store policy data. 
# -------------------------------------------------------------------------------------------- #

ds_bans_stats_tr_and_ut <- function(ds_bans_arg, dta_tr_and_ut_arg, event_year_arg, geog_arg, tr_type){
  
  condition_sym <- dplyr::enquos(event_year_arg)
  
  ds_bans_arg <- select(dta_tr_and_ut_arg, GEOID, event_year) %>% 
    left_join(ds_bans_arg, by = 'GEOID', 
              multiple = 'all', relationship = 'many-to-one')
  
  ds_bans_arg <- ds_bans_arg %>% group_by(GEOID) %>% 
    filter(row_number() == 1) %>% ungroup() %>% 
    filter(!!!condition_sym)
  
  ds_bans_arg <- ds_bans_arg %>% 
    mutate(Restrictions = Moratorium + Ordinance) %>% 
    relocate(c(Restrictions), .after = Ordinance)
  
  # Use to select relevant variables below. 
  policy_vars = names(ds_bans_arg) %>% str_subset(string=., pattern='^Def|^Mor|^Ord|Rest|^policy_*')
  
  sel_policy_vars <- str_subset(policy_vars, pattern = 'year', negate=TRUE); sel_policy_vars
  
  ds_bans_arg <- ds_bans_arg %>% 
    
    mutate(across(.cols = all_of(sel_policy_vars), 
                  .fns = \(x) if_else(x > 0, 1, 0), 
                  .names = '{.col}_binary') )
  
  ds_bans_stats_any <- ds_bans_arg %>% 
    
    summarise(across(.cols = matches('_binary$'), 
                     .fns = list('total' = sum), 
                     .names = '{.fn}_{.col}'))
  
  first_and_last_cols <- c(names(ds_bans_stats_any)[1], 
                           names(ds_bans_stats_any)[length(names(ds_bans_stats_any))])  
  
  ds_bans_stats_any <- ds_bans_stats_any %>%
    
    pivot_longer(cols = first_and_last_cols[1]:first_and_last_cols[2], 
                 names_to = c('statistic', 'variable'), 
                 names_pattern = '(mean|total)_(.*)$') 
  
  ds_bans_stats_any <- ds_bans_stats_any %>% 
    mutate(across(.cols = 'variable', 
                  .fns = \(x) str_replace_all(x, pattern = '_binary', replacement = '') ) )
  
  ds_bans_stats_any$type <- tr_type
  
  return(ds_bans_stats_any)
  
}

ds_bans_stats_tr <- ds_bans_stats_tr_and_ut(ds_bans_arg = ds_bans_geo, 
                                            dta_tr_and_ut_arg = dta_treated_non_model_vars, 
                                            event_year_arg = c(event_year > 0), 
                                            geog_arg = model_geography, tr_type = 'Treated')

ds_bans_stats_ut <- ds_bans_stats_tr_and_ut(ds_bans_arg = ds_bans_geo, 
                                            dta_tr_and_ut_arg = dta_untreated_non_model_vars, 
                                            event_year_arg = c(event_year == 0), 
                                            geog_arg = model_geography, tr_type = 'Untreated')

ds_bans_stats_all <- bind_rows(ds_bans_stats_tr, ds_bans_stats_ut)
# -------------------------------------------------------------------------------------------- #

print(paste0('There are ', filter(ds_bans_stats_all, variable == 'policy_total' & type == 'Untreated') %>% select(value) , 
             ' defeats and restrictions in the Untreated data.') )

print(paste0('There are ', filter(ds_bans_stats_all, variable == 'policy_total' & type == 'Treated') %>% select(value) , 
             ' defeats and restrictions in the Treated data.') )
# -------------------------------------------------------------------------------------------- #
ds_bans_stats_all_rural <- ds_bans_stats_all
# -------------------------------------------------------------------------------------------- #
ds_bans_stats_all_urban$Geography = 'Urban'
ds_bans_stats_all_rural$Geography = 'Rural'
# -------------------------------------------------------------------------------------------- #
ds_bans_stats_comb <- bind_rows(ds_bans_stats_all_urban, ds_bans_stats_all_rural)


ds_bans_stats_comb %>% filter(variable == 'policy_total') %>% summarise(sum(value))
# 1765
ds_bans_stats_comb %>% filter(variable == 'Defeated') %>% summarise(sum(value))
# 1105
ds_bans_stats_comb %>% filter(variable == 'Restrictions') %>% summarise(sum(value))
# 715

print(paste0('There are ', 1765, ' block groups with at least one defeat or restriction. 
             There are ', (1105+715), ' defeats plus restrictionss. ', 
             'Thus, there are ', (1105+715)-1765, ' block groups with both defeats and restrictions'))

paste0('The share of pure defeats is ', round((1105-55)/1765, digits = 3))
paste0('The share of pure restrictions is ', round((715-55)/1765, digits = 3))
paste0('The share of defeats/restrictions is ', round((55)/1765, digits = 3))
round((1105-55)/1765, digits = 3)+round((715-55)/1765, digits = 3)+round(55/1765, digits = 3)


ds_bans_stats_comb %>% filter(Geography == 'Urban' & variable == 'policy_total') %>% summarise(sum(value))
ds_bans_stats_comb %>% filter(Geography == 'Rural' & variable == 'policy_total') %>% summarise(sum(value))
paste0('Urban areas account for approximately , ', round(1419/1765, digits = 3), ' of policies')

# Number of total 
ds_bans_stats_comb %>% filter(variable == 'policy_total' & Geography == 'Urban') 
ds_bans_stats_comb %>% filter(variable == 'policy_total' & Geography == 'Rural') 
(1377+388)
1377/(1377+388)
