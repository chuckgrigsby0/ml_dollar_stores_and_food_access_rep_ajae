# -------------------------------------------------------------------------------------------- #
# Combine the time FEs first from the lasso and double lasso, then from the OLS 
# Retain only the fixed effects from the models. 
# Note tha:
# estimates_sel = First lasso model; 
# estimates_double_sel = double lasso model. 
# estimates_post_sel = post-selection model. 
# One can choose the estimated FEs by using estimate_column_type. 
print('Sourced: Compile_State_Time_FE_Function')
# -------------------------------------------------------------------------------------------- #
Compile_State_Time_FE_Function <- function(fe_estimates_dta, la_type, estimate_column_type){
  
  
  fe_estimates <- list(fe_estimates_dta[[la_type]]$coef_sel, 
                       fe_estimates_dta[[la_type]]$coef_double_sel) %>%
    reduce(left_join, by = 'row') %>%
    rename(estimates_sel = value.x) %>%
    rename(estimates_doublesel = value.y) %>%
    #select(-column) %>% # Remove extraneous column containing s0. 
    left_join(fe_estimates_dta[[la_type]]$coef_post_sel, by = c('row' = 'term')) %>%
    rename(estimates_postsel = estimate) %>%
    filter(grepl('year|STATE_', row)) %>%
    select(row, {{ estimate_column_type }}) 
  # Note that we only retain a single column of estimated fixed effects from either 
  # LASSO, double-LASSO, or post-selection models. 
  
  
  state_replacement_pattern <- c('STATE_' = '', '_' = ' ') # Use to create STATE column. 
  
  state_fes <- fe_estimates %>%
    filter(grepl('STATE', row)) %>% 
    mutate(STATE = str_replace_all(row, state_replacement_pattern)) %>%
    relocate(STATE) %>%
    rename_with(.cols = {{ estimate_column_type }}, 
                .fn = ~paste0('fe_state_', la_type))
  
  
  time_fes <- fe_estimates %>%
    filter(grepl('year', row)) %>% 
    mutate(year = str_remove(row, 'year_')) %>%
    relocate(year) %>%
    rename_with(.cols = {{ estimate_column_type }}, 
                .fn = ~paste0('fe_time_', la_type))
  
  
  state_time_fixed_effects <- list('state_fes' = state_fes, 
                                   'time_fes' = time_fes)
  
  return(state_time_fixed_effects)
  
}
# -------------------------------------------------------------------------------------------- #