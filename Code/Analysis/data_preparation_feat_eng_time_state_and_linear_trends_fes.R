#--------------------------------------------------------------------------------------------#
source(here::here('Code', 'Analysis', 'load_data_for_time_by_state_fixed_effects.R'))
#--------------------------------------------------------------------------------------------#
model_data <- dta_untreated %>% select(-c(GEOID, Grocery_Count_10mile_2005, Geography))

vars = names(model_data)[!grepl('^low_access|year|STATE', names(model_data))]; vars
# #--------------------------------------------------------------------------------------------#
# model_geography = 'Urban'
# if (model_geography == 'Urban'){ 
#   vars <- str_subset(vars, pattern = 'uc_area', negate = TRUE)
# } else if (model_geography == 'Rural'){ 
#   vars <- str_subset(vars, pattern = 'urban_area|uc_area', negate = TRUE)
#   }
# Create exclusion variables for the function. 
#--------------------------------------------------------------------------------------------#
la_names <- names(model_data)[grepl('low_access', names(model_data))]; la_names
la_names_exclude <- map(seq(1, 3, by = 1), function(.x) la_names[-.x])
la_names_exclude <- set_names(la_names_exclude, la_names); la_names_exclude
#--------------------------------------------------------------------------------------------#
model_data <- model_data %>% 
  mutate(
    trend = as.numeric(year) - as.numeric(min(year))
    ) %>% 
  relocate(trend, .after = year)

estimate_fixed_effects <- function(dta, la_var_include){
  
  model_dta <- dta %>% select(-all_of(str_subset(la_names, 
                                                 pattern = paste0('^', la_var_include, '$'), 
                                                 negate = TRUE) ) ) 
  
  # FEs: year + STATE + STATE-level trends. 
  
  la_formula <- xpd(
    regex('low_access') ~ ..ctrl - 1 | year + STATE + STATE[trend], 
    ..ctrl = vars, 
    data = model_dta
    ) 
  
  #print(la_formula)
  gc()
  # combine.quick = FALSE means that fixed effects using names when combining interaction FEs, e.g., 2005^California. 
  fixed_effects_reg <- feols(la_formula, data = model_dta, combine.quick = FALSE) 
  
  fixed_effects_est <- fixef(fixed_effects_reg) # Extract fixed effects using fixef(). 
  
  fe_names <- str_replace_all(names(fixed_effects_est), c('STATE\\[\\[trend\\]\\]' = 'STATE_trend')) 
  
  fixed_effects_est <- set_names(fixed_effects_est, nm = fe_names)
  
  fe_names <- names(fixed_effects_est)
  #--------------------------------------------------------------------------------------------#
  fixed_effects_to_df <- function(fe_name_str, low_access_str){
    
    fe_estimates <- data.frame(fe = fixed_effects_est[[fe_name_str]]) %>% 
      rownames_to_column(var = fe_name_str) %>%
      rename_with(.cols = fe, .fn = ~paste0(., '_', fe_name_str, '_', low_access_str))
  }
  #--------------------------------------------------------------------------------------------#
  fe_estimates_list <- map(fe_names, 
                           function(.x){
                             fixed_effects_to_df(fe_name_str = .x,
                                                 low_access_str = la_var_include)
                           } )
  
  fe_estimates_list <- set_names(fe_estimates_list, nm = fe_names)
  
  return(fe_estimates_list)
  
}

time_state_trend_fixed_effects <- map(la_names, 
                                   function(.x){
                                     estimate_fixed_effects(dta = model_data, la_var_include = .x) 
                                     } )

time_state_trend_fixed_effects <- set_names(time_state_trend_fixed_effects, nm = la_names)

saveRDS(time_state_trend_fixed_effects, 
        here::here('Data', 'Data_2_and_10_Miles', 'time_state_trend_fixed_effects.rds') )
#--------------------------------------------------------------------------------------------#