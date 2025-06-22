print('Sourced: errors_on_relative_time <- function(model_preds_dta, national, geography_str)')

errors_on_relative_time <- function(model_preds_dta, national, geography_str){

# -------------------------------------------------------------------------------------------- #
  if (isTRUE(national)){
    
pretr_preds <- model_preds_dta %>% 
  filter(rel_year < 0) %>% 
  select(-tau) %>% 
  mutate(err = actual - preds, 
         rel_year = factor(rel_year))

} else {
  
  pretr_preds <- model_preds_dta %>% 
    filter(rel_year < 0) %>% 
    filter(grepl(geography_str, Geography)) %>%
    select(-tau) %>% 
    mutate(err = actual - preds, 
           rel_year = factor(rel_year))
}
# -------------------------------------------------------------------------------------------- #
pretr_model_errors <- lm(err ~ rel_year - 1, data = pretr_preds)

model_errors_coefs <- tidy_regression(lm_model = pretr_model_errors, 
                                      dep_var_string = 'CV Error')

# For plots, convert rel_year to numeric. 
model_errors_coefs$rel_year <- as.numeric(model_errors_coefs$rel_year)

return(model_errors_coefs)
}
# plot_errors_on_relyear(dta=model_errors_coefs, 
#                        y_value = estimate, 
#                      standard_error = std.error,
#                      y_axis_title = 'Cross-Validated Errors', 
#                      x_axis_title = 'Relative Time to Treatment', 
#                      #breaks_y = breaks_errors, 
#                      decimal_place_y = 0.01, 
#                      y1_lim = -0.07, 
#                      y2_lim = 0.07)

# ggsave(here::here('Analysis', 'Figures', 
#                   paste0('cv_errors_on_relative_time_', stringr::str_to_lower(geography_str), '.png')), 
#        width = 12, height = 8, unit = 'in')