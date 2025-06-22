# -------------------------------------------------------------------------------------------- #
# Create time and state fixed effects data frame objects for use in models. 
# Note: Must source the Compile_State_Time_FE_Function
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Compile_State_Time_FE.R'))
# -------------------------------------------------------------------------------------------- #
#.libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
.libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample')
# -------------------------------------------------------------------------------------------- #
# Load the output containing the state and year FEs from the regression of food-desert status of the untreated
# on the covariates. 
# -------------------------------------------------------------------------------------------- #
# fe_estimates <- readRDS(here::here('Data', 'fe_estimates_5mile.rds'))
fe_estimates <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'fe_estimates_10mile.rds'))
# -------------------------------------------------------------------------------------------- #
la_type_names <- names(fe_estimates); la_type_names
# -------------------------------------------------------------------------------------------- #
state_time_fixed_effects <- map(la_type_names, 
                                function(.x){ 
                                  Compile_State_Time_FE_Function(fe_estimates_dta = fe_estimates, 
                                                                 la_type = .x, 
                                                                 estimate_column_type = 'estimates_sel')
                                })

state_time_fixed_effects <- set_names(state_time_fixed_effects, nm = la_type_names)
# -------------------------------------------------------------------------------------------- #
# Convert the state and time fixed effect estimates to data.frame friendly formats. 
# -------------------------------------------------------------------------------------------- #
state_fixed_effects <- state_time_fixed_effects %>% 
  map(function(.x){ 
    keep(.x, .p = grepl('state', names(.x))) # For each sublist, keep the lists with the names 'state_fes'
  }) %>%
  flatten() # Remove the outer lists to have individual lists. 

time_fixed_effects <- state_time_fixed_effects %>% 
  map(function(.x){ 
    keep(.x, .p = grepl('time', names(.x))) # For each sublist, keep the lists with the names 'time_fes'
  }) %>%
  flatten() # Remove the outer lists to have individual lists. 
# -------------------------------------------------------------------------------------------- #
# Given the list of output, left-join by STATE and row (variable names) // year and row columns. 
# -------------------------------------------------------------------------------------------- #
state_fixed_effects <- state_fixed_effects %>%
  reduce(left_join, by = c('STATE', 'row')) %>%
  select(-row)

time_fixed_effects <- time_fixed_effects %>%
  reduce(left_join, by = c('year', 'row')) %>%
  select(-row)
# -------------------------------------------------------------------------------------------- #