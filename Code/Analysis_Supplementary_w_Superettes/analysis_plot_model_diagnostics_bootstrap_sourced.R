# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# # Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap. 
model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level.
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_model_diagnostics_bootstrap.R'))
# -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap. 
model_geography <- 'Urban' # Used in script below to subset by either Urban or Rural.
model_dep_var <- 'low_access'
bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level.
source(here::here('Code', 'Analysis_Supplementary_w_Superettes', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # -------------------------------------------------------------------------------------------- #
rm(list = ls())
gc()
Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load empirical data and point estimates. 
# -------------------------------------------------------------------------------------------- #
# # Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap. 
# model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
# model_dep_var <- 'low_access_pers'
# bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level.
# source(here::here('Code', 'Analysis', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # -------------------------------------------------------------------------------------------- #
# rm(list = ls())
# gc()
# Sys.sleep(time = 5.0)
# # -------------------------------------------------------------------------------------------- #
# # Load empirical data and point estimates. 
# # -------------------------------------------------------------------------------------------- #
# # Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap. 
# model_geography <- 'Urban' # Used in script below to subset by either Urban or Rural.
# model_dep_var <- 'low_access_pers'
# bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level.
# source(here::here('Code', 'Analysis', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # # -------------------------------------------------------------------------------------------- #
# rm(list = ls())
# gc()
# Sys.sleep(time = 5.0)
# -------------------------------------------------------------------------------------------- #


# # -------------------------------------------------------------------------------------------- #
# # Load empirical data and point estimates. 
# # -------------------------------------------------------------------------------------------- #
# # Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap. 
# model_geography <- 'Rural' # Used in script below to subset by either Urban or Rural.
# model_dep_var <- 'low_access'
# bootstrap_by_tracts <- NULL # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level. 
# source(here::here('Code', 'Analysis', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # -------------------------------------------------------------------------------------------- #
# rm(list = ls())
# gc()
# Sys.sleep(time = 5.0)
# # -------------------------------------------------------------------------------------------- #
# # Load empirical data and point estimates. 
# # -------------------------------------------------------------------------------------------- #
# # Specify Urban/Rural, dependent variable, and results based on block-group bootstrap or census-tract bootstrap. 
# model_geography <- 'Urban' # Used in script below to subset by either Urban or Rural.
# model_dep_var <- 'low_access'
# bootstrap_by_tracts <- NULL # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level. 
# source(here::here('Code', 'Analysis', 'analysis_plot_model_diagnostics_bootstrap.R'))
# # -------------------------------------------------------------------------------------------- #
# rm(list = ls())
# gc()
# Sys.sleep(time = 5.0)