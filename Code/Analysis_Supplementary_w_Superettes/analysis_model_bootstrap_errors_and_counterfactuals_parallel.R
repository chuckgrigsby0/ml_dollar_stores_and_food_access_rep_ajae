# ----------------------------------- #
# Load packages
# ----------------------------------- #
options(scipen = 999)
pacman::p_load('future', 'furrr', 'parallel')
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Function in this script is used in the bootstrap_errors_and_preds function. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_tidy_regression.R'))

source(here::here('Code', 'Functions', 'Function_bootstrap_errors_and_preds.R'))
# -------------------------------------------------------------------------------------------- #
iterations = seq(1, 499, 1)
bootstrap_group = '01_499'
bootstrap_by_tracts = '_tracts' # NULL or '_tracts'
ncores = parallelly::availableCores()
plan(multicore, workers = ncores)
# -------------------------------------------------------------------------------------------- #
 tic()
 
 boot_errors_and_preds <- future_map(iterations, function(.x){ 
  
   bootstrap_errors_and_preds(bootstrap_ids = bootstrap_group, iter = .x, bootstrap_by_tracts = bootstrap_by_tracts) 
  
   })
# , .options = furrr_options(chunk_size = 25) 
toc()
# -------------------------------------------------------------------------------------------- #
# tic()
# boot_errors_and_preds <- mclapply(iterations, function(.i){
    
#    bootstrap_errors_and_preds(bootstrap_ids = bootstrap_group, iter = .i)
    
#   }, mc.cores = ncores)
# toc()
# -------------------------------------------------------------------------------------------- #
  
directory_geography <- paste(model_geography, 'Bootstrap', sep = '_')

directory_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')

filename <- paste0('bootstrap_', 'errors_and_predictions_', str_to_lower(model_geography), '_', model_dep_var, bootstrap_by_tracts)

filename <- paste0(filename, '.rds'); filename

saveRDS(boot_errors_and_preds, here::here('Analysis', directory_geography, directory_dep_var, filename))

# -------------------------------------------------------------------------------------------- #