#--------------------------------------------------------------------------------------------#
source(here::here('Code', 'Analysis', 'FE_Estimation', 'load_data_for_time_by_state_fixed_effects.R'))
#--------------------------------------------------------------------------------------------#
model_data <- dta_untreated %>% select(-c(GEOID, Grocery_Count_10mile_2005, Geography))

vars = names(model_data)[!grepl('^low_access|year|STATE', names(model_data))]; vars
#--------------------------------------------------------------------------------------------#
# Create exclusion variables for the function. 
#--------------------------------------------------------------------------------------------#
la_names <- names(model_data)[grepl('low_access', names(model_data))]; la_names
la_names_exclude <- map(seq(1, 3, by = 1), function(.x) la_names[-.x])
la_names_exclude <- set_names(la_names_exclude, la_names); la_names_exclude
#--------------------------------------------------------------------------------------------#

estimate_fixed_effects <- function(dta, la_var_include, la_vars_exclude1, la_vars_exclude2){
  
  model_dta <- dta %>% select(-all_of(c(la_vars_exclude1[1], la_vars_exclude2[2]))) 
  
  # Alternatively: year + STATE 
  
  la_formula <- xpd(regex('low_access') ~ ..ctrl - 1 | year^STATE, ..ctrl = vars, data = model_dta) 
  
  #print(la_formula)
  gc()
  # combine.quick = FALSE means that fixed effects using names when combining interaction FEs, e.g., 2005^California. 
  fixed_effects_reg <- feols(la_formula, data = model_dta, combine.quick = FALSE) 
  
  fixed_effects_est <- fixef(fixed_effects_reg) # Extract fixed effects using fixef(). 
  
  fe_names <- names(fixed_effects_est)
  #--------------------------------------------------------------------------------------------#
  fixed_effects_to_df <- function(.x, fe_name_str, low_access_str){
    
    fe_estimates <- data.frame(fe = fixed_effects_est[[.x]]) %>% 
      rownames_to_column(var = fe_name_str) %>%
      rename_with(.cols = fe, .fn = ~paste0(., '_', fe_name_str, '_', low_access_str))
  }
  #--------------------------------------------------------------------------------------------#
  fe_estimates_list <- pmap(list(.x = seq_along(fixed_effects_est), 
                                 fe_name_str = fe_names),
                            fixed_effects_to_df, 
                            low_access_str = la_var_include)
  
  fe_estimates_list <- set_names(fe_estimates_list, nm = fe_names)
  
  return(fe_estimates_list)
  
}

time_by_state_fixed_effects <- pmap(list(la_var_include = la_names, 
                                          la_vars_exclude1 = la_names_exclude, 
                                          la_vars_exclude2 = la_names_exclude), 
                                     estimate_fixed_effects, 
                                     dta = model_data)

time_by_state_fixed_effects <- set_names(time_by_state_fixed_effects, nm = la_names)

saveRDS(time_by_state_fixed_effects, 
        here::here('Data', 'Data_2_and_10_Miles', 'time_by_state_fixed_effects.rds') )
#--------------------------------------------------------------------------------------------#