print('Sourced: prepare_binned_quartile_covars_post <- function(national, geography_str, model_preds_dta)')
# -------------------------------------------------------------------------------------------- #
# Function is run for Urban and Rural posttreatment data to obtain binned and factor formatted covariates
# to assess relationships between CV errors and covariates. Integers/Counts are converted to factors. 
# Numeric data are converted to bins. 
# -------------------------------------------------------------------------------------------- #
prepare_binned_quartile_covars_post <- function(national, geography_str, model_preds_dta){
  
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
  
  # tau and predictors
  
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_wcovars <- posttre_effects %>%
    select(GEOID, year, event_year, rel_year, tau, all_of(model_covars)) 
  
  # -------------------------------------------------------------------------------------------- #
  # Convert counts to integers to separate variables by types (numeric vs integer). 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_wcovars <- posttre_effects_wcovars %>% 
    mutate(across(.cols = matches('^Wholesale_Club|^Gen_Merch|^Mass_Merch|^urban_area$|^uc_area$', ignore.case = TRUE), 
                  .fns = ~as.integer(.))) 
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  # Separate key, numeric and integer covariates. 
  # -------------------------------------------------------------------------------------------- #
  posttr_key_vars <- setdiff(names(posttre_effects_wcovars), model_covars); posttr_key_vars
  
  integer_vars <- names(select(posttre_effects_wcovars, where(is.integer)))
  integer_vars <- integer_vars[!grepl('tau', integer_vars)]; integer_vars # Remove integer var. "tau" if integer.
  
  numeric_covars <- model_covars[!(model_covars %in% integer_vars)]
  
  # Round the numeric variables so that the discretized bins do not contain too many decimal places. 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_wcovars <- posttre_effects_wcovars %>% 
    mutate(across(.cols = all_of(numeric_covars), 
                  .fn = ~round(., digits = 3)))
  # -------------------------------------------------------------------------------------------- #
  # Create character vector for continuous covariates. 
  continuous_covars <- numeric_covars[numeric_covars %in% c(model_covars_list_disag$socioeconomics)]
  # -------------------------------------------------------------------------------------------- #
  # Continuous variable breaks. 
  custom_breaks_contin_vars = posttre_effects_wcovars %>%
    
    reframe(across(.cols = all_of(continuous_covars), 
                   
                   .fns = ~quantile(., probs = c(0.25, 0.75))) )
  
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  # Function to create binned covariates one-by-one. 
  # -------------------------------------------------------------------------------------------- #
  binned_covars <- function(dta, continuous_vartype, custom_breaks_vartype, covariates, id){
    
    if (isTRUE(continuous_vartype)){ 
      
      cut_points <- unique(unlist(custom_breaks_vartype[ , id]))  
      
    } else { # For the discrete_vars, include a -1 in the first break so that 0 has a unique break. 
      
      cut_points <- unique(unlist(custom_breaks_vartype[ , id]))  
      
      cut_points <- c(-1, cut_points)
      
    }
    # -------------------------------------------------------------------------------------------- #
    # Create a recipe. 
    # -------------------------------------------------------------------------------------------- #
    rec <- recipe(tau ~ ., data = dta)
    
    custom_bins <- rec %>% step_cut(all_of(covariates[id]), breaks = cut_points)
    
    # Prepare the recipe. 
    prep_bins <- custom_bins %>% prep(training = dta)
    
    # Bake the recipe
    baked_bins <- prep_bins %>% bake(new_data = NULL, all_of(posttr_key_vars), all_of(covariates[id]))
    
    # Clean up the newly created data frame prior to merging with the treatment data. 
    posttre_effects_wcovars_binned <- baked_bins %>% rename_with(.cols = all_of(covariates[id]), 
                                                                 .fn = ~paste0(covariates[id], '_bins'))
    
    return(posttre_effects_wcovars_binned)
    
  }
  
  # -------------------------------------------------------------------------------------------- #
  # Creates Lists of data frames each of which contains the pretr_key_vars and a covariate specified by id.  
  # -------------------------------------------------------------------------------------------- #
  binned_covars_contin <- seq_along(continuous_covars) %>% 
    
    map(function(.x){ 
      
      binned_covars(dta = posttre_effects_wcovars, 
                    continuous_vartype = TRUE,
                    custom_breaks_vartype = custom_breaks_contin_vars, 
                    covariates = continuous_covars,
                    id = .x)
      
    })
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  posttr_binned_covars <- binned_covars_contin %>%
    
    reduce(.f = left_join, by = c('GEOID', 'year', 'event_year', 'rel_year', 'tau'))
  
  return(posttr_binned_covars)
  
}
