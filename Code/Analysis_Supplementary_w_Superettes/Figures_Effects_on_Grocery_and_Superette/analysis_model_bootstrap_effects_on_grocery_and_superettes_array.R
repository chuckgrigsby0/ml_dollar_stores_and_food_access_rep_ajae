# Bootstrapped average treatment effects across dollar store entries and baseline grocery stores and superettes interactions. 
# (As opposed to total baseline grocery and superettes combined). 
# -------------------------------------------------------------------------------------------- #
# Load data.
# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below.
model_geography = Sys.getenv("model_geography") # Used in script below.
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts'
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
# Load regional and divisional labels. 
# -------------------------------------------------------------------------------------------- #
bg_regs_and_divs <- readRDS(here::here('Data', 'block_group_regions_and_division.rds'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load the bootstrapped ML model. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_id))
bootstrap_ids = '01_499' # Folder designated in directory specifying number of bootstrap iterations. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(bootstrap_id, '.rds'), sep = '_')

model_output <- readRDS(here::here('Analysis_Supplementary_w_Superettes',
                                   dir_geography,
                                   dir_dep_var, 
                                   dir_bootstrap, 
                                   filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(bg_regs_and_divs, by = 'GEOID') # Regional and divisional indicators. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load post-treatment binned and factor data. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 

posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_and_superette_', str_to_lower(model_geography), '.rds')

posttr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_grocery))
# -------------------------------------------------------------------------------------------- #
posttre_effects <- model_preds %>% 
  
  filter(rel_year >= 0) %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  mutate(rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #
# Prepare data for pre-treatment analyses and post-treatment analyses. 
# Note: data preparation is already completed for pre-treatment data. See pretr_binned_covars. 
# -------------------------------------------------------------------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))

posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')

# effects and dollar store entry/counts w/ grocery stores from 2005. 

posttre_effects_wdsentry <- posttre_effects %>%
  select(all_of(posttr_key_vars), actual, preds, tau) %>%
  left_join(posttr_binned_dsvars, by = posttr_key_vars) %>% 
  left_join(posttr_binned_grocery, by = posttr_key_vars) %>% # Joins binned dollar store and grocery store variables. 
  left_join(select(st_drop_geometry(bg_pop_centroids_10_sfp_geo), GEOID, STATE, COUNTY, market_name), by = 'GEOID')


# Select relevant covariates for analyses. 
sel_dsvars_bins <- c('gross_entry_cumsum_bins', 'entry_events_bins', 'net_entry_cumsum_bins')
sel_grocery_bins <- c('Grocery_Count_10mile_2005_bins', 
                      'Superette_Count_10mile_2005_bins', 
                      'Grocery_and_Superette_Count_10mile_2005_bins')
geog_vars <- c('GEOID', 'STATE', 'COUNTY', 'market_name')

# Prepare data. 

posttre_effects_winteracts <- posttre_effects_wdsentry %>% # dta
  
  select(all_of(posttr_key_vars), all_of(geog_vars),
         actual, preds, tau, 
         all_of(sel_dsvars_bins), all_of(sel_grocery_bins)) 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_effects_on_ds_entry_x_grocery_and_superettes.R') )
# -------------------------------------------------------------------------------------------- #

ds_entry_vars <- c("gross_entry_cumsum_bins",
                   "entry_events_bins",                    
                   "net_entry_cumsum_bins")

# -------------------------------------------------------------------------------------------- #
sum_stats <- map(ds_entry_vars, function(.x){
  
  ds_entry_by_num_grocery_and_superette_2005(dta = posttre_effects_winteracts, 
                                             ds_entry_var = .x, 
                                             grocery_store_var = 'Grocery_Count_10mile_2005_bins', 
                                             superette_var = 'Superette_Count_10mile_2005_bins', 
                                             iter_id = bootstrap_id)
})
# -------------------------------------------------------------------------------------------- #
sum_stats <- set_names(sum_stats, nm = ds_entry_vars)  

sum_stats <- map2(sum_stats, ds_entry_vars, function(.x, .y){ 
  
  .x %>% 
   
     rename_with(.fn = \(x) str_replace_all(x, pattern = .y, 'ds_entry_bins') ) 
                
                }) %>%
    
  bind_rows(.id = 'ds_entry')
# -------------------------------------------------------------------------------------------- #
saveRDS(sum_stats, 
        here::here('Analysis_Supplementary_w_Superettes', 
                   paste0(model_geography, '_', 'Bootstrap'),
                   str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'),
                   'bootstrap_effects_on_grocery_x_superettes',
                   'summary_stats', 
                   paste0('bootstrap_summary_stats_grocery_x_superettes_', bootstrap_id, '.rds') ) )
# -------------------------------------------------------------------------------------------- #
reg_coefs <- map_dfr(ds_entry_vars, function(.x){
  
  effects_by_ds_entry_x_grocery_x_superette(dta = posttre_effects_winteracts, 
                                            ds_entry_var = .x, 
                                            grocery_store_var = 'Grocery_Count_10mile_2005_bins', 
                                            superette_var = 'Superette_Count_10mile_2005_bins', 
                                            iter_id = bootstrap_id)
})
# -------------------------------------------------------------------------------------------- #
saveRDS(reg_coefs, 
        here::here('Analysis_Supplementary_w_Superettes', 
                   paste0(model_geography, '_', 'Bootstrap'),
                   str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'),
                   'bootstrap_effects_on_grocery_x_superettes',
                   paste0('bootstrap_summary_stats_grocery_x_superettes_', bootstrap_id, '.rds') ) )
# -------------------------------------------------------------------------------------------- #