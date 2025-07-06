# Script creates Figures E.1 and E.2 in supplementary analyses including superettes in low-access indicator. 
# Figures show actual vs predicted share of low-access block groups across relative treatment timing. 
# Additional figures include average CV errors and treatment effects across relative time to treatment.  
# Run with sbatch_figures.sh
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_model_diagnostics_bootstrap.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #