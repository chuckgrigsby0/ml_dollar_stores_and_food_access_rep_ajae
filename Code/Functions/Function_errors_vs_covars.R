print('Sourced: errors_on_covars_function <- function(national, geography_str, model_preds_dta, dta_untreated_wfolds_dta, covariate, x_axis_title')
#--------------------------------------------------------------------------------------------#

errors_on_covars_function <- function(national, geography_str, 
                                      model_preds_dta, dta_untreated_wfolds_dta, 
                                      covariate, x_axis_title){
  
  
  if (isTRUE(national)){
    
    pretr_preds <- model_preds_dta %>% 
      filter(rel_year < 0) %>% 
      select(-tau) %>% 
      mutate(err = low_access - preds, 
             rel_year = factor(rel_year))
    
  } else {
    
    pretr_preds <- model_preds_dta %>% 
      filter(rel_year < 0) %>% 
      filter(grepl(geography_str, Geography)) %>%
      select(-tau) %>% 
      mutate(err = low_access - preds, 
             rel_year = factor(rel_year))
  }
  

pretr_preds_wcovars <- pretr_preds %>%
  left_join(select(dta_untreated_wfolds_dta, GEOID, year, all_of(covariate)), by = c('GEOID', 'year')) 
# -------------------------------------------------------------------------------------------- #
custom_breaks = pretr_preds_wcovars %>%

    summarise(across(.cols = all_of(covariate), 
                     
                     .fns = ~quantile(., probs = seq(0, 1, 0.05))))

custom_breaks = unique(as.numeric(custom_breaks[[1]]))

# -------------------------------------------------------------------------------------------- #
# Create a recipe. 
# -------------------------------------------------------------------------------------------- #
my_form = paste0('err ~', covariate)

my_form  = as.formula(my_form)

rec <- pretr_preds_wcovars %>% recipe(my_form, data = .)

custom_bins <- rec %>% step_cut(all_of(covariate), breaks = custom_breaks)

# Prepare the recipe. 
prep_bins <- custom_bins %>% prep(training = pretr_preds_wcovars)

# Bake the recipe
baked_bins <- prep_bins %>% bake(new_data = NULL)

# Clean up the newly created data frame prior to merging with the treatment data. 
baked_bins <- baked_bins %>% rename_with(.cols = all_of(covariate), 
                                         .fn = ~paste0(covariate, '_bins'))

pretr_preds_wcovars <- bind_cols(pretr_preds_wcovars, select(baked_bins, ends_with('bins')))
# -------------------------------------------------------------------------------------------- #
reg_form <- paste0('err ~ ', covariate, '_bins', ' - 1')

reg_form <- as.formula(reg_form)

model_errors_vs_covars <- lm(reg_form, data = pretr_preds_wcovars)

model_errors_vs_covars <- tidy_regression_err_vs_covar(model_errors_vs_covars)

model_errors_vs_covars <- model_errors_vs_covars %>% mutate(Label = 'Average CV-Error in Bin')

# -------------------------------------------------------------------------------------------- #
plot_error_vs_covars(dta = model_errors_vs_covars, 
                     
                     y_axis = 'Cross-Validated Errors', 
                     
                     x_axis = paste0(x_axis_title), 
                     
                     #breaks_y = breaks_y, 
                     
                     decimal_place_y = 0.001, 
                     
                     y1_lim = -max(model_errors_vs_covars$estimate)-0.01, 
                     
                     y2_lim = max(model_errors_vs_covars$estimate)+0.01)

ggsave(here::here('Analysis', 'Figures', 
                  paste0('cv_errors_on_', covariate, '_', str_to_lower(geography_str), '.png')), 
       width = 12, height = 8, units = 'in')

}
# -------------------------------------------------------------------------------------------- #