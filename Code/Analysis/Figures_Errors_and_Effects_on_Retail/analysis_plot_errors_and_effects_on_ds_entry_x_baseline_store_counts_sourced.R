# Script used in sbatch_figures.sh

# Script creates figures of average CV errors and treatment effects w.r.t baseline counts of retailer controls and multiple dollar store entries. 
# Script is used in conjunction with `analysis_plot_errors_and_effects_on_ds_entry_x_baseline_store_counts.R`

# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
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
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
options(scipen = 999)
source(here::here('Code', 'Analysis', 'analysis_plot_errors_and_effects_on_ds_entry_x_baseline_store_counts.R'))
# # -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #