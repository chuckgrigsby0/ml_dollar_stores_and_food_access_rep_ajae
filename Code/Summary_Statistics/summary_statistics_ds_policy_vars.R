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

ds_bans_geo <- ds_bans_geo %>% 
  mutate(Restrictions = Moratorium + Ordinance) %>% 
  relocate(c(Restrictions), .after = Ordinance)

# Use to select relevant variables below. 
policy_vars = names(ds_bans_geo) %>% str_subset(string=., pattern='^Def|^Mor|^Ord|Rest|^policy_*')

sel_policy_vars <- str_subset(policy_vars, pattern = 'year', negate=TRUE); sel_policy_vars

ds_bans_geo <- ds_bans_geo %>% 
  
  mutate(across(.cols = all_of(sel_policy_vars), 
                .fns = \(x) if_else(x > 0, 1, 0), 
                .names = '{.col}_binary') )

ds_bans_stats_national <- ds_bans_geo %>% 
  
  summarise(across(.cols = matches('_binary$'), 
                   .fns = list('total' = sum), 
                   .names = '{.fn}_{.col}'))

first_and_last_cols <- c(names(ds_bans_stats_national)[1], 
                         names(ds_bans_stats_national)[length(names(ds_bans_stats_national))])  

ds_bans_stats_national <- ds_bans_stats_national %>%
  
  pivot_longer(cols = first_and_last_cols[1]:first_and_last_cols[2], 
               names_to = c('statistic', 'variable'), 
               names_pattern = '(mean|total)_(.*)$') 
  
ds_bans_stats_national <- ds_bans_stats_national %>% 
  mutate(across(.cols = 'variable', 
                .fns = \(x) str_replace_all(x, pattern = '_binary', replacement = '') ) )

# -------------------------------------------------------------------------------------------- #
ds_bans_stats_by_geography <- function(geog_arg, dta){

  ds_bans_stats_urban_rural <- dta %>% 
  
  filter(grepl(geog_arg, Geography)) %>% 
  
  summarise(across(.cols = matches('_binary$'), 
                   .fns = list('total' = sum), 
                   .names = '{.fn}_{.col}'))

first_and_last_colsfun <- c(names(ds_bans_stats_urban_rural)[1], 
                         names(ds_bans_stats_urban_rural)[length(names(ds_bans_stats_urban_rural))])  

ds_bans_stats_urban_rural <- ds_bans_stats_urban_rural %>%
  
  pivot_longer(cols = first_and_last_colsfun[1]:first_and_last_colsfun[2], 
               names_to = c('statistic', 'variable'), 
               names_pattern = '(mean|total)_(.*)$') 

ds_bans_stats_urban_rural <- ds_bans_stats_urban_rural %>% 
  mutate(across(.cols = 'variable', 
                .fns = \(x) str_replace_all(x, pattern = '_binary', replacement = '') ) )

ds_bans_stats_urban_rural$Geography <- geog_arg

return(ds_bans_stats_urban_rural)

}
# -------------------------------------------------------------------------------------------- #
ds_bans_stats_rural <- ds_bans_stats_by_geography(geog_arg = 'Rural', dta = ds_bans_geo)

ds_bans_stats_urban <- ds_bans_stats_by_geography(geog_arg = 'Urban', dta = ds_bans_geo)

ds_bans_stats_national$Geography <- 'National'

ds_bans_stats_all <- bind_rows(ds_bans_stats_national, 
                               ds_bans_stats_urban, 
                               ds_bans_stats_rural)
# -------------------------------------------------------------------------------------------- #
total_val <- ds_bans_stats_all %>% filter(variable == 'policy_total' & Geography == 'National') %>% select(value)
print(paste0('The share of block groups with at least one defeat or restriction is ', 
             round(total_val/nrow(ds_bans_geo), digits = 2)) )


print(paste0('Block groups with only at least one defeat ', 
             ds_bans_geo %>% filter(Defeated > 0 & Moratorium == 0 & Ordinance == 0) %>% nrow() ))

print(paste0('Block groups with only at least one restriction ', 
             ds_bans_geo %>% filter(Defeated == 0 & Restrictions > 0) %>% nrow() ))

print(paste0('Block groups with at least one restriction and defeat ', 
             ds_bans_geo %>% filter(Defeated > 0 & Restrictions > 0) %>% nrow() ))

print(
  paste0('The total number of block groups with polices is ', 
         ds_bans_geo %>% filter(Defeated > 0 & Moratorium == 0 & Ordinance == 0) %>% nrow() +
           ds_bans_geo %>% filter(Defeated == 0 & Restrictions > 0) %>% nrow() +
           ds_bans_geo %>% filter(Defeated > 0 & Restrictions > 0) %>% nrow()
) 
)
