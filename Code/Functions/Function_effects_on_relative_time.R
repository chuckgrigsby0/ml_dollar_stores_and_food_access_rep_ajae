print('Sourced: effects_on_relative_time <- function(model_preds_dta, national, geography_str)')

effects_on_relative_time <- function(model_preds_dta, national, geography_str){
  
  # -------------------------------------------------------------------------------------------- #
  if (isTRUE(national)){
    
    posttre_effects <- model_preds_dta %>% 
      filter(rel_year >= 0) %>% 
      mutate(rel_year = factor(rel_year))
    
  } else {
    
    posttre_effects <- model_preds_dta %>% 
      filter(rel_year >= 0) %>% 
      filter(grepl(geography_str, Geography)) %>%
      mutate(rel_year = factor(rel_year))
  }
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_coefs <- lm(tau ~ rel_year - 1, data = posttre_effects)
  
  posttre_effects_coefs <- tidy_regression(lm_model = posttre_effects_coefs, 
                                          dep_var_string = 'Tau')
  
  # For plots, convert rel_year to numeric. 
  posttre_effects_coefs$rel_year <- as.numeric(posttre_effects_coefs$rel_year)
  
  return(posttre_effects_coefs)
 
}

# print('Sourced: effects_on_relative_time_x_entry_bin <- function(model_preds_dta, national, geography_str)')
# 
# effects_on_relative_time_x_entry_bin <- function(model_preds_dta, national, geography_str){
#   
#   # -------------------------------------------------------------------------------------------- #
#   if (isTRUE(national)){
#     
#     posttre_effects <- model_preds_dta %>% 
#       filter(rel_year >= 0) %>% 
#       mutate(rel_year = factor(rel_year))
#     
#   } else {
#     
#     posttre_effects <- model_preds_dta %>% 
#       filter(rel_year >= 0) %>% 
#       filter(grepl(geography_str, Geography)) %>%
#       mutate(rel_year = factor(rel_year))
#   }
#   # -------------------------------------------------------------------------------------------- #
# 
#   # Effects on relative time by entry-bin  
#   #--------------------------------------------------------------------------------------------#
#   # combine.quick = FALSE means that fixed effects using names when combining interaction FEs, 
#   #--------------------------------------------------------------------------------------------#
#   effects_reg <- feols(tau ~ -1 | rel_year^entry_events_bins, data = posttre_effects, combine.quick = FALSE) 
#   
#   effects_est <- fixef(effects_reg) # Extract fixed effects using fixef(). 
#   
#   #--------------------------------------------------------------------------------------------#
#   estimates <- data.frame(estimate = effects_est[[1]]) %>% 
#     tibble::rownames_to_column('term') 
#   
#   # Split up the term column and add to the estimates data.frame(). 
#   new_cols <- data.frame((str_split_fixed(estimates$term, '_', n = 2))) 
#   names(new_cols) <- c('rel_year', 'entry_events')
#   estimates <- bind_cols(new_cols, estimates) 
#   
#   
#   effects_rel_year_x_entry_bin <- estimates %>% mutate(rel_year = as.numeric(rel_year)) %>%
#     arrange(rel_year) %>%
#     select(-term) %>%
#     mutate(entry_events = factor(entry_events, levels = unique(entry_events)))
#   
#   effects_rel_year_x_entry_bin$Outcome <- 'Tau'
# 
# return(effects_rel_year_x_entry_bin)
# 
# }

