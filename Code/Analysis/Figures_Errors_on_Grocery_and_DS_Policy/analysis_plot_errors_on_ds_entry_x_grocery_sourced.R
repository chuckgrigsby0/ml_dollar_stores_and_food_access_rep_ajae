# Run with sbatch_figures.sh

# Script to create figures showing average CV errors w.r.t. dollar store policy presence, geographic region, 
# and baseline grocery stores and future dollar store entries. 
# Use in conjunction with analysis_plot_errors_on_ds_entry_x_grocery.R
# See Figures D.4, D.5, D.7, and D.8 of supplementary materials. 

# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
options(scipen = 999)
source(here::here('Code', 'Analysis', 'analysis_plot_errors_on_ds_entry_x_grocery.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
options(scipen = 999)
source(here::here('Code', 'Analysis', 'analysis_plot_errors_on_ds_entry_x_grocery.R'))
# # -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #