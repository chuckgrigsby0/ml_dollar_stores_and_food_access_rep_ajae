# ----------------------------------- #
# Load packages
# ----------------------------------- #
library('pacman')
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 
       'recipes', 'rsample', 'fixest', 'sf', 'tictoc', 'xgboost')
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
placebo_dta <- bind_rows(panel_df_ds_entry_2_and_10mile$untreated, 
                         panel_df_ds_entry_2_and_10mile$treated)


placebo_dta <- placebo_dta %>% 
  mutate(rel_year = if_else(is.finite(rel_year), rel_year+1, rel_year), 
         treat = case_when(rel_year < 0 ~ FALSE, 
                           is.infinite(rel_year) ~ FALSE, 
                           rel_year >= 0 ~ TRUE) ) %>% 
  arrange(GEOID)

panel_df_ds_entry_2_and_10mile <- list('untreated' = filter(placebo_dta, treat == FALSE), 
                                       'treated' = filter(placebo_dta, treat == TRUE))
# -------------------------------------------------------------------------------------------- #
saveRDS(panel_df_ds_entry_2_and_10mile, 
        file = here::here('Data', 'Data_2_and_10_Miles', 
                          'pla_ds_entries_panel_treated_wbins_and_untreated_2_and_10mile.rds'))
# -------------------------------------------------------------------------------------------- #