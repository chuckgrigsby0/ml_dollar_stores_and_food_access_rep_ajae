# -------------------------------------------------------------------------------------------- #
# Used to produce plots of ATTs as a % of counterfactual low-access shares. Also, produces table of ATTs with bootstrapped SEs.  
# Run script using sbatch_figures.sh
# -------------------------------------------------------------------------------------------- #
# For rural results. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
# For urban results
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' 
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #