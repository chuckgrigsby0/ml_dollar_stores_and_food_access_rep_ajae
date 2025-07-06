# Executes scripts to create figures showing average CV errors and treatment effects across binned and normalized covariates. 
# See Figures E.5 and E.6 of supplementary analyses that include superettes in low-access indicator. 
# -------------------------------------------------------------------------------------------- #
# Create figures for rural area models. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis', 'analysis_plot_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
# Create figures for urban area models. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis', 'analysis_plot_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #