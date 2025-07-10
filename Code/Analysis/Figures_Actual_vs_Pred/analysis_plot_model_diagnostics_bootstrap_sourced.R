# Used in conjunction with 'analysis_plot_model_diagnostics_bootstrap.R'
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis', 'analysis_plot_model_diagnostics_bootstrap.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #