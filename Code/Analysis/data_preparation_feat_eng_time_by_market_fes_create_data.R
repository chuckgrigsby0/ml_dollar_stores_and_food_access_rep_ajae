# -------------------------------------------------------------------------------------------- #
# Create time and market and market by time fixed effects data frame objects for use in models. 
# Function to compile FEs by FE type. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_Compile_Market_Time_FE.R'))
# -------------------------------------------------------------------------------------------- #
#.libPaths(c("K:/Home/grigsby-charles/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.4/library"))
.libPaths()
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample')
# -------------------------------------------------------------------------------------------- #
# Load the output containing the market and year FEs from the regression of food-desert status of the untreated
# on the covariates. 
# -------------------------------------------------------------------------------------------- #
fe_estimates <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'time_by_market_fixed_effects.rds'))
# -------------------------------------------------------------------------------------------- #
la_type_names <- names(fe_estimates); la_type_names
# -------------------------------------------------------------------------------------------- #
# Using the sourced function compile_fes from above. 
# -------------------------------------------------------------------------------------------- #

# Year FEs
fes_year <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^year$', ls_name = 'year')

# Market FEs
fes_market <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^market$', ls_name = 'market')

# Market by Year FEs
fes_market_by_time <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^year\\^market$', ls_name = 'year^market')
fes_market_by_time <- fes_market_by_time %>% rename_with(.cols = contains('year^market'), 
                                                 .fn = ~ str_replace_all(., 'year\\^market', 'year_x_market'))

fes_market_by_time <- fes_market_by_time %>% 
  mutate(market = str_replace_all(year_x_market, "\\d{4}_*", ''), # Remove the e.g., '2005_' portion of the string
         year = str_extract(year_x_market, '\\d+')) %>% # Keep the '2005' portion of the string. 
  relocate(year, market) %>%
  select(-year_x_market)
# -------------------------------------------------------------------------------------------- #