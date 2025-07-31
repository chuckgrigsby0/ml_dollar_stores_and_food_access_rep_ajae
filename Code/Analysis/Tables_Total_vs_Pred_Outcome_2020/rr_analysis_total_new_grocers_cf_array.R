# Script to compute total number of grocery stores lost as a result of dollar store entry. 
# -------------------------------------------------------------------------------------------- #
library(pacman)
pacman::p_load('dplyr', 'stringr', 'tidyr', 'purrr')
# -------------------------------------------------------------------------------------------- #
model_dep_var = Sys.getenv('model_dep_var') # Used in script below. 
model_geography = Sys.getenv("model_geography") # Used in script below.
options(scipen = 999)
print(model_dep_var); print(model_geography)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# From the SLURM sbatch script save/store the job array ID number, which is used to load the bootstrapped ML model. 
# -------------------------------------------------------------------------------------------- #
bootstrap_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
bootstrap_id <- as.numeric(bootstrap_id)
print(paste('Bootstrap model number', bootstrap_id))
bootstrap_ids = '01_499' 
bootstrap_by_tracts = '_tracts'
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_', bootstrap_ids, bootstrap_by_tracts) # NULL or '_tracts'

filename <- paste(str_to_lower(model_geography), model_dep_var, 'bootstrap', paste0(bootstrap_id, '.rds'), sep = '_')

model_output <- readRDS(here::here('Analysis',
                                   dir_geography,
                                   dir_dep_var, 
                                   dir_bootstrap, 
                                   filename))

# Select only post-treatment predictions and actual outcomes. 
posttr_preds <- model_output %>% 
  pluck('data_cf_preds') %>%
  rename(preds = pred_class_cf)

# Compute totals and ATTs by calendar year. 
avg_efx_year <- posttr_preds %>%
  
  group_by(year) %>% 
  
  summarise(across(.cols = c(tau, actual, preds), 
                   .fns = list('total' = \(x) sum(x), 
                               'avg' = \(x) mean(x)), 
                   .names = '{.col}_{.fn}') )

avg_efx_year <- avg_efx_year %>% 
  mutate(iter = bootstrap_id) %>% 
  relocate(iter)    

# -------------------------------------------------------------------------------------------- #
new_dir <- 'bootstrap_total_new_grocers_cf'

dir_path <- here::here('Analysis', dir_geography, dir_dep_var, new_dir)

dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

filename <- paste0(str_to_lower(model_geography), '_',
                  model_dep_var, '_',
                  new_dir, '_', 
                  bootstrap_id, '.rds')
                  
saveRDS(avg_efx_year, file.path(dir_path, filename) )
# -------------------------------------------------------------------------------------------- #