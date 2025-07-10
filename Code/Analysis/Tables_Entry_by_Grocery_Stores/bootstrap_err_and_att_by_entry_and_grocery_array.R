# Script computes bootstrapped estimates of average CV errors and treatment effects across 
# multiple dollar store entries and basline grocery store counts (2005). 
# ----------------------------- #
# Load and prepare data. 
# ----------------------------- #
library('pacman')
library('here') # To route directories. 
library('readr') # To save tables as .csv files. 
# ----------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below.
model_geography = Sys.getenv("model_geography") # Used in script below.
bootstrap_by_tracts <- '_tracts' 
options(scipen = 999)
print(model_dep_var); print(model_geography)
# ----------------------------- #  
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# ----------------------------- #

# ----------------------------- #
# Pre-treatment observations, year 2005 Grocery Store bins. 
# ----------------------------- #
fname_pretr_binned_grocery = paste0('pretreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

pretr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_pretr_binned_grocery))
# ----------------------------- #


# ----------------------------- #
# Post-treatment observations, year 2005 Grocery Store bins. 
# ----------------------------- #
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

posttr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_grocery))
# ----------------------------- #

# ----------------------------- #
# Post-treatment dollar store bins and factors. 
# ----------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))
# ----------------------------- #

# Remove tau calculated from the original/empirical data 

posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# ----------------------------- #

# ----------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# ----------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# ----------------------------- #
# Filter out urban_area and uc_area from the Rural models. 
# ----------------------------- #
if (model_geography == 'Rural'){ 
  model_covars <- model_covars[!grepl('^urban_area$|^uc_area$', model_covars)]
}
# ----------------------------- #  
join_key_vars <- c('GEOID', 'year')
# ----------------------------- #  

# From the SLURM sbatch script save/store the job array ID number, which is used to load the bootstrapped ML model. 
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_id))

if (bootstrap_id == 0){ 
  # ----------------------------- #
  # Load the optimal estimated model following tuning/training. 
  # ----------------------------- #
  filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
  dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) 
  model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
  # ----------------------------- #
} else if (bootstrap_id != 0){
  # ----------------------------- #
  bootstrap_ids = '01_499' # Folder designated in directory specifying number of bootstrap iterations. 
  dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access
  
  dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) # NULL or '_tracts'
  
  filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(bootstrap_id, '.rds'), sep = '_')
  
  model_output <- readRDS(here::here('Analysis',
                                     dir_geography,
                                     dir_dep_var, 
                                     dir_bootstrap, 
                                     filename))
}
# ----------------------------- #
join_key_vars <- c('GEOID', 'year')
non_model_vars <- c('entry', 'entry_events', 'event_year', 'rel_year', 'treat', 
                    'Grocery_Count_10mile_2005', 'Geography')
# ----------------------------- #
pretr_preds <- model_output$cv_errors_opt %>% 
  
  left_join(select(dta_untreated_non_model_vars, 
                   all_of(join_key_vars), 
                   all_of(non_model_vars)), by = join_key_vars) %>%
  
  left_join(
    select(pretr_binned_grocery, 
           all_of(join_key_vars), 
           Grocery_Count_10mile_2005_bins), 
    by = join_key_vars 
    ) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  filter(year >= '2007') %>% # We obtain CV predictions from 2007-2020
  
  filter(is.finite(rel_year))
# ----------------------------- #
posttr_preds <- model_output$data_cf_preds %>%
  
  left_join(
    select(dta_treated_non_model_vars, 
           all_of(join_key_vars), 
           all_of(non_model_vars)), 
    by = join_key_vars
  ) %>%
  
  left_join(
    select(posttr_binned_grocery, 
           all_of(join_key_vars), 
           Grocery_Count_10mile_2005_bins),
    by = join_key_vars
  ) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') %>% # We obtain post-treatment predictions from 2006-2020. 
  
  left_join(
    select(posttr_binned_dsvars, 
           all_of(join_key_vars), # Join the dollar store entry information to post-treatment data.
           gross_entry_cumsum, gross_entry_cumsum_bins),
    by = join_key_vars
  )
# ----------------------------- #

# ----------------------------- #
if (model_geography == 'Rural'){ 
  ee_breaks = c(-Inf, 1, 2, 3, 4, Inf)
  ee_labels = c("1", "2", "3", "4", "> 4")
  
  ge_breaks = c(-Inf, 1, 2, 3, 5, Inf)
  ge_labels = c("1", "2", "3", "(3, 5]" , "> 5")
  
} else if (model_geography == 'Urban'){ 
  ee_breaks = c(-Inf, 1, 2, 3, Inf)
  ee_labels = c("1", "2", "3", "> 3")
  
  ge_breaks = c(-Inf, 1, 2, 3, 4, Inf)
  ge_labels = c("1", "2", "3", '4', "> 4")
}
# ----------------------------- #
posttr_preds <- posttr_preds %>% 
  mutate(
    entry_events_bins = cut(
      entry_events,
      breaks = ee_breaks,
      labels = ee_labels,
      right = TRUE
    ), 
    gross_entry_cumsum_bins = cut(
      gross_entry_cumsum,
      breaks = ge_breaks,
      labels = ge_labels,
      right = TRUE
    )
  ) %>% 
  
  relocate(c("entry_events_bins", 
             "gross_entry_cumsum",         
             "gross_entry_cumsum_bins"), 
           .after = entry_events)
# ----------------------------- #
# Get the distinct combinations of entry events for each treated block group. 
# ----------------------------- #
entry_vars <- c('entry_events_bins', 'gross_entry_cumsum_bins')

posttr_ds_entries <-  posttr_preds %>% 
      
      group_by(GEOID, event_year) %>%
      
      distinct(across(.cols = all_of(entry_vars)))
# ----------------------------- #
pretr_preds_aug <- pretr_preds %>%
  
  filter(is.finite(rel_year) ) %>% # Filter out observations with rel_year == Inf because these are never-treated observations.
  
  left_join(posttr_ds_entries,  
            by = c('GEOID', 'event_year'),
            relationship = 'many-to-many', 
            multiple = 'all' ) %>% 
  
  drop_na(.) # A few block group observations are present in the pre-treatment data but drop out of the post-treatment data. 
              # Therefore, there is no post-treatment information on entries to join


# Load function to compute summary statistic tables. 
source(here::here('Code', 
                  'Analysis', 
                  'Entry_by_Grocery_Stores_Tables', 
                  'Functions', 
                  'Function_Summary_by_Entry_and_Grocery.R'))

err_and_att_by_entry_and_grocery <- entry_vars %>% 
  
  map(function(.x){ 
    
    res <- summary_by_entry_and_grocery(df_post = posttr_preds, 
                                        df_pre = pretr_preds_aug,
                                        entry_var_type = .x, 
                                        bootstrap_id_num = bootstrap_id)
    
  })

err_and_att_by_entry_and_grocery <- set_names(err_and_att_by_entry_and_grocery, nm = entry_vars)

fname = paste0('bootstrap_err_and_att_entry_x_grocery_', 
               str_to_lower(model_geography), '_', 'tracts_', model_dep_var,
               '_', bootstrap_id, '.rds')

saveRDS(err_and_att_by_entry_and_grocery, here::here('Analysis', 
                   paste0(model_geography, '_', 'Bootstrap'), 
                   'Low_Access', 
                   'bootstrap_errors_and_effects_by_entry_x_grocery', 
                   fname))
