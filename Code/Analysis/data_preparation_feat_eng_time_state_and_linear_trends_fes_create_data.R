# -------------------------------------------------------------------------------------------- #
# Create time and state and state trend fixed effects data frame objects for use in models. 
# Function to compile FEs by FE type. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Compile_Market_Time_FE.R')) 
# -------------------------------------------------------------------------------------------- #
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample')
# -------------------------------------------------------------------------------------------- #
# Load the output containing the state and year FEs from the regression of food-desert status of the untreated
# on the covariates. 
# -------------------------------------------------------------------------------------------- #
fe_estimates <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'time_state_trend_fixed_effects.rds'))
# -------------------------------------------------------------------------------------------- #
la_type_names <- names(fe_estimates); la_type_names
# -------------------------------------------------------------------------------------------- #
# Using the sourced function compile_fes from above. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Year FEs
fes_year <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^year$', ls_name = 'year')

# State FEs
fes_state <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^STATE$', ls_name = 'STATE')

# State-Trend FEs. 
fes_trend <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = 'trend$', ls_name = 'STATE_trend')
fes_trend <- fes_trend %>% 
  rename('STATE' = 'STATE_trend')
# -------------------------------------------------------------------------------------------- #