# Random Forest results. 
# Script to compute bootstrap average CV errors and treatment effects by relative time from treatment, and actual and predicted low-access shares in pre- and post-treatment periods. 
# ----------------------------------- #
# Load packages
# ----------------------------------- #
options(scipen = 999)
pacman::p_load('future', 'furrr', 'parallel')
# -------------------------------------------------------------------------------------------- #
model_dep_var <- Sys.getenv("model_dep_var") # Used in script below. 
model_geography = Sys.getenv("model_geography") # Used in script below.
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Function in this script is used in the bootstrap_errors_and_preds function. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R'))

source(here::here('Code', 'Functions', 'Function_bootstrap_errors_and_preds_rf.R'))
# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load each bootstrapped ML model. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_iter <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_iter))
bootstrap_group = '01_499' # Folder designated in directory specifying number of bootstrap iterations. 
bootstrap_by_tracts = '_tracts'
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
boot_errors_and_preds <- bootstrap_errors_and_preds(bootstrap_ids = bootstrap_group, 
                                                    iter = bootstrap_iter, 
                                                    bootstrap_by_tracts = bootstrap_by_tracts) 
# -------------------------------------------------------------------------------------------- #
directory_geography <- paste(model_geography, 'Bootstrap', sep = '_')

directory_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')

directory_boot_diag_type <- paste0('bootstrap_errors_and_predictions', bootstrap_by_tracts, '_rf') 

filename <- paste0('bootstrap_', 
                   'errors_and_preds_', 
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts, '_', # e.g., '_tracts' or NULL
                   'rf', '_', 
                   bootstrap_iter) # e.g., 1:499

filename <- paste0(filename, '.rds'); filename

saveRDS(boot_errors_and_preds, here::here('Analysis', directory_geography, directory_dep_var, directory_boot_diag_type, filename))

# -------------------------------------------------------------------------------------------- #
