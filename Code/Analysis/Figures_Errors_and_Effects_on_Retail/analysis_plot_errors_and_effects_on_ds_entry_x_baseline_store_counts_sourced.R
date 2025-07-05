# Script used in sbatch_figures.sh
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
options(scipen = 999)
source(here::here('Code', 'Analysis', 'analysis_plot_errors_and_effects_on_ds_entry_x_baseline_store_counts.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
options(scipen = 999)
source(here::here('Code', 'Analysis', 'analysis_plot_errors_and_effects_on_ds_entry_x_baseline_store_counts.R'))
# # -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #