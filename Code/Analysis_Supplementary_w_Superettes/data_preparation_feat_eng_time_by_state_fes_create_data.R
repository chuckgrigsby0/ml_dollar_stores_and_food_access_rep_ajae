# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 'recipes', 'sf', 'rsample')
# -------------------------------------------------------------------------------------------- #
# Load the output containing the state and year FEs from the regression of low-access status of the untreated
# on the covariates. 
# -------------------------------------------------------------------------------------------- #
fe_estimates <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 'time_by_state_fixed_effects_w_superettes.rds'))
# -------------------------------------------------------------------------------------------- #
la_type_names <- names(fe_estimates); la_type_names
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Uncomment the Year and State FEs if estimating State and Year FEs, respectively. 
# -------------------------------------------------------------------------------------------- #
# Year FEs
# fes_year <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^year$', ls_name = 'year')

# State FEs
# fes_state <- compile_fes(fe_estimates_list = fe_estimates, grepl_ls_name = '^STATE$', ls_name = 'STATE')
# -------------------------------------------------------------------------------------------- #


# State by Year FEs
# -------------------------------------------------------------------------------------------- #
fes_state_by_time <- fe_estimates$low_access

fes_state_by_time <- fes_state_by_time %>% 
  rename_with(.cols = contains('year^STATE'), 
              .fn = ~str_replace_all(., 'year\\^STATE', 'year_x_STATE'))

fes_state_by_time <- fes_state_by_time %>% 
  mutate(STATE = str_replace_all(year_x_STATE, "\\d{4}_*", ''), # Remove the e.g., '2005_' portion of the string
         year = str_extract(year_x_STATE, '\\d+')) %>% # Keep the '2005' portion of the string. 
  relocate(year, STATE) %>%
  select(-year_x_STATE)
# -------------------------------------------------------------------------------------------- #