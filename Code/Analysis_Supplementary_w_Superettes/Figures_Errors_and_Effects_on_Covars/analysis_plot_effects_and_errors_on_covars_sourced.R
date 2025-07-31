# Executes scripts that create figures showing average CV errors and treatment effects across binned and standardized covariates, respectively. 
# See Figures E.5 and E.6 in supplementary analyses including superettes in low-access definition.  
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap.
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban'
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_effects_and_errors_on_covars.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #