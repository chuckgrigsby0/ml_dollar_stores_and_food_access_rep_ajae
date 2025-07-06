# Executes scripts that create figures showing average treatment effects by dollar store entries and baseline grocery stores and superettes, combined. 
# Additional figures output include treatment effects across dollar store entries and binned covariates. 

# Run with sbatch_figures.sh

# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_effects_on_ds_entry_x_covars_interactions.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_effects_on_ds_entry_x_covars_interactions.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #