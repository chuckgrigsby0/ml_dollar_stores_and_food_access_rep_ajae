
# Run script using sbatch_figures.sh
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts'  # '_tracts' for bootstrap at census-tract level.
source(here::here('Code', 'Analysis', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' # '_tracts' for bootstrap at census-tract level.
source(here::here('Code', 'Analysis', 'analysis_plot_change_in_outcomes.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #