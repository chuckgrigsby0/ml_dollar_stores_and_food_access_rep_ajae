# Functions to efficiently load bootstrap data for a give model_geography and dependent variable. 
# Functions exist for either parallel or array processed bootstrap data. The former yields a single list the B total elements
# of bootstrapped data. The latter loads in 1:B bootstrap results from a specific director contatining the B bootstrap results. 
# -------------------------------------------------------------------------------------------- #
load_bootstrap_errors_and_predictions_parallel <- function(model_geography_str, model_dep_var_str, bootstrap_by_tracts){ 
  
  dir_geography <- paste(model_geography_str, 'Bootstrap', sep = '_')
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var_str, '_', ' ')), ' ', '_')
  
  
  filename <- paste('bootstrap_errors_and_predictions', str_to_lower(model_geography_str), 
                    paste0(model_dep_var_str, bootstrap_by_tracts, '.rds'), sep = '_')
  
  bootstrap_data <- readRDS(here::here('Analysis',
                                       dir_geography,
                                       dir_dep_var, 
                                       filename))  
  
  
  return(bootstrap_data)
  
}
cat(paste('Sourced: load_bootstrap_errors_and_predictions_parallel <-', 
          'function(model_geography_str, model_dep_var_str, bootstrap_by_tracts)', sep = '\n'))
# -------------------------------------------------------------------------------------------- #
load_bootstrap_errors_and_predictions_array <- function(model_geography_str, model_dep_var_str, 
                                                        bootstrap_by_tracts, bootstrap_iter){ 
  
  dir_geography <- paste(model_geography_str, 'Bootstrap', sep = '_') # e.g., Rural/Urban Bootstrap
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var_str, '_', ' ')), ' ', '_') # e.g., Low_Access, etc.
  
  dir_bootstrap <- paste0('bootstrap_errors_and_predictions', bootstrap_by_tracts) # NULL or '_tracts'
  
  filename <- paste0('bootstrap_', 
                     'errors_and_preds_', 
                     str_to_lower(model_geography_str), '_', # e.g., rural or urban
                     model_dep_var, # e.g., low_access
                     bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                     bootstrap_iter, '.rds') # e.g., 1:499
  
  bootstrap_data <- readRDS(here::here('Analysis',
                                       dir_geography,
                                       dir_dep_var,
                                       dir_bootstrap,
                                       filename))  
  
  
  return(bootstrap_data)
  
}
cat(paste('Sourced: load_bootstrap_errors_and_predictions_array <-', 
          'function(model_geography_str, model_dep_var_str, bootstrap_by_tracts, bootstrap_iter)', sep = '\n'))
# -------------------------------------------------------------------------------------------- #
load_bootstrap_errors_and_predictions_array_w_superettes <- function(model_geography_str, model_dep_var_str, 
                                                        bootstrap_by_tracts, bootstrap_iter){ 
  
  dir_geography <- paste(model_geography_str, 'Bootstrap', sep = '_') # e.g., Rural/Urban Bootstrap
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var_str, '_', ' ')), ' ', '_') # e.g., Low_Access, etc.
  
  dir_bootstrap <- paste0('bootstrap_errors_and_predictions', bootstrap_by_tracts) # NULL or '_tracts'
  
  filename <- paste0('bootstrap_', 
                     'errors_and_preds_', 
                     str_to_lower(model_geography_str), '_', # e.g., rural or urban
                     model_dep_var, # e.g., low_access
                     bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                     bootstrap_iter, '.rds') # e.g., 1:499
  
  bootstrap_data <- readRDS(here::here('Analysis_Supplementary_w_Superettes',
                                       dir_geography,
                                       dir_dep_var,
                                       dir_bootstrap,
                                       filename))  
  
  
  return(bootstrap_data)
  
}
cat(paste('Sourced: load_bootstrap_errors_and_predictions_array_w_superettes <-', 
          'function(model_geography_str, model_dep_var_str, bootstrap_by_tracts, bootstrap_iter)', sep = '\n'))
# -------------------------------------------------------------------------------------------- #
load_bootstrap_errors_and_predictions_array_placebo <- function(model_geography_str, model_dep_var_str, 
                                                        bootstrap_by_tracts, bootstrap_iter){ 
  
  dir_geography <- paste(model_geography_str, 'Bootstrap', sep = '_') # e.g., Rural/Urban Bootstrap
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var_str, '_', ' ')), ' ', '_') # e.g., Low_Access, etc.
  
  dir_bootstrap <- paste0('bootstrap_errors_and_predictions', bootstrap_by_tracts) # NULL or '_tracts'
  
  filename <- paste0('bootstrap_', 
                     'errors_and_preds_', 
                     str_to_lower(model_geography_str), '_', # e.g., rural or urban
                     model_dep_var, # e.g., low_access
                     bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                     bootstrap_iter, '.rds') # e.g., 1:499
  
  bootstrap_data <- readRDS(here::here('Analysis_Placebo',
                                       dir_geography,
                                       dir_dep_var,
                                       dir_bootstrap,
                                       filename))  
  
  
  return(bootstrap_data)
  
}
cat(paste('Sourced: load_bootstrap_errors_and_predictions_array_placebo <-', 
          'function(model_geography_str, model_dep_var_str, bootstrap_by_tracts, bootstrap_iter)', sep = '\n'))
# -------------------------------------------------------------------------------------------- #

load_bootstrap_errors_and_predictions_mult_thresholds_array <- function(model_geography_str, model_dep_var_str, 
                                                                        bootstrap_by_tracts, bootstrap_iter){ 
  
  dir_geography <- paste(model_geography_str, 'Bootstrap', sep = '_') # e.g., Rural/Urban Bootstrap
  
  dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var_str, '_', ' ')), ' ', '_') # e.g., Low_Access, etc.
  
  dir_bootstrap <- paste0('bootstrap_errors_and_predictions_mult_thresh', bootstrap_by_tracts) # NULL or '_tracts'
  
  filename <- paste0('bootstrap_', 
                     'errors_and_preds_mult_thresh_', 
                     str_to_lower(model_geography_str), '_', # e.g., rural or urban
                     model_dep_var, # e.g., low_access
                     bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                     bootstrap_iter, '.rds') # e.g., 1:499
  
  bootstrap_data <- readRDS(here::here('Analysis',
                                       dir_geography,
                                       dir_dep_var,
                                       dir_bootstrap,
                                       filename))  
  
  
  return(bootstrap_data)
  
}
cat(paste('Sourced: load_bootstrap_errors_and_predictions_mult_thresholds_array <-', 
          'function(model_geography_str, model_dep_var_str, bootstrap_by_tracts, bootstrap_iter)', sep = '\n'))
# -------------------------------------------------------------------------------------------- #
