# Run script using sbatch_figures.sh
# Creates figures showing average treatment effects as a percentage of average counterfactual low-access shares across relative time from treatment. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'  
source(here::here('Code', 'Analysis', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' 
source(here::here('Code', 'Analysis', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #