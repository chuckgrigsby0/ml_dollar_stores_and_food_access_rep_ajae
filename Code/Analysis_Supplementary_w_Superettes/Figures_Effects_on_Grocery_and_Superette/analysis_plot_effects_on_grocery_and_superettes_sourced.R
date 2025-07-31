# Scripts create Figures E.3 and E.4 in supplementary analyses including superettes in low-access definition. 
# Results assess average treatment effects across dollar store entries and baseline grocery store-superette counts. 
# Run with sbatch_figures.sh
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_effects_on_grocery_and_superettes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_effects_on_grocery_and_superettes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #