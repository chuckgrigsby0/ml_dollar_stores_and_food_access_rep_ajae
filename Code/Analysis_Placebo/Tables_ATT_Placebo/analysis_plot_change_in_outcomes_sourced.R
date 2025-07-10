# Script calls analysis_plot_change_in_outcomes.R for urban and rural model results, 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap.
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural'
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis_Placebo', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis_Placebo', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #