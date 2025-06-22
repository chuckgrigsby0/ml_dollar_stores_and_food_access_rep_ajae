print('Sourced: actual_vs_predicted <- function(model_preds_dta, national, geography_str)')
# -------------------------------------------------------------------------------------------- #
actual_vs_predicted <- function(model_preds_dta, national, geography_str){
  # -------------------------------------------------------------------------------------------- #
  if (isTRUE(national)){
    
    posttr_preds <- model_preds_dta %>% 
      
      mutate(rel_year = factor(rel_year)) %>%
      
      filter(year >= '2007')
    
  } else {
    
    posttr_preds <- model_preds_dta %>% 
      
      mutate(rel_year = factor(rel_year)) %>%
      
      filter(grepl(geography_str, Geography)) %>%
      
      filter(year >= '2006')
    
  }
  # -------------------------------------------------------------------------------------------- #
  # Average predicted Counterfactuals in relative time. 
  
  cf_preds_coefs <- lm(preds ~ rel_year - 1, data = posttr_preds)
  cf_preds_coefs <- tidy_regression(cf_preds_coefs, 'Predicted')
  
  # Average actuals in relative time. 
  actual_on_relyear <- paste('actual', '~', 'rel_year - 1')
  actual_on_relyear <- as.formula(actual_on_relyear)
  
  actual_coefs <- lm(actual_on_relyear, data = posttr_preds)
  actual_coefs <- tidy_regression(actual_coefs, 'Actual')
  
  # Combine the tables. 
  # convert rel_year to numeric for plots
  
  preds_vs_actual <- bind_rows(actual_coefs, cf_preds_coefs) %>% mutate(rel_year = as.numeric(rel_year)) 
  
  return(preds_vs_actual)
}
  # -------------------------------------------------------------------------------------------- #
  # breaks_outcome <- range(unique(round(sort(preds_vs_actual$estimate), digits = 2))); breaks_outcome
  # breaks_outcome <- seq(breaks_outcome[1], breaks_outcome[2], 0.02)
  
  #   plot_actual_on_predicted(dta=preds_vs_actual, 
  #                            y_value = estimate, 
  #                        standard_error = std.error,
  #                        y_axis_title = 'Low-Access (Shares)', 
  #                        x_axis_title = 'Time to Treatment', 
  #                        #breaks_y = breaks_outcome, 
  #                        x_intercept_val = 0, 
  #                        decimal_place_y = 0.01, 
  #                        y1_lim = min(preds_vs_actual$estimate), y2_lim = max(preds_vs_actual$estimate))
  
  # ggsave(here::here('Analysis', 'Figures', 
  #                   paste0('preds_vs_cfs_relative_time_', stringr::str_to_lower(geography_str), '.png')), 
  #      width = 12, height = 8, unit = 'in')
# -------------------------------------------------------------------------------------------- #