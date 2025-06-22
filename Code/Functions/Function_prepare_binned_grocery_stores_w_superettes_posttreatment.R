print('Sourced: prepare_binned_grocery_stores_post <- function(national, geography_str, model_preds_dta)')
# -------------------------------------------------------------------------------------------- #
# Function is run for Urban and Rural posttreatment data to obtain binned and factor formatted dollar store 
# counts and entries to assess relationships between causal effects and dollar store entries/counts. 
# Integers/Counts are converted to factors. Numeric data are converted to bins. 
# -------------------------------------------------------------------------------------------- #
prepare_binned_grocery_stores_post <- function(national, geography_str, model_preds_dta){
  
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
  # Create binned year 2005 Grocery Store Counts. 
  # -------------------------------------------------------------------------------------------- #
  posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')
  grocery_str <- c('Grocery_Count_10mile_2005', 'Superette_Count_10mile_2005', 'Grocery_and_Superette_Count_10mile_2005')
  cut_points <- posttre_effects %>% 
    reframe(across(.cols = c(Grocery_Count_10mile_2005, Superette_Count_10mile_2005, Grocery_and_Superette_Count_10mile_2005), 
                   .fns = \(x) quantile(x, probs = seq(0, 1, 0.05) ) ) )
  
  end_bin <- set_names(rep(list(-1), length(names(cut_points))), nm = names(cut_points) )
  end_bin <- as.data.frame(end_bin)
  # Add a -1 as a cut point to locate 0 into its own bin. 
  cut_points <- bind_rows(end_bin, cut_points)
  # Create recipe for creating binned Grocery Store data. 
  rec <- recipe(tau~., data = posttre_effects)
  # -------------------------------------------------------------------------------------------- #
  discretize_store_counts <- function(store_var_2005){
  # step_cut into bins > prep > bake > clean. 
  grocery_store_bins <- rec %>% step_cut(.data[[store_var_2005]], breaks = unique(cut_points[[store_var_2005]]) ) %>%
    prep(training = posttre_effects) %>% 
    bake(new_data = NULL, all_of(posttr_key_vars), all_of(store_var_2005)) %>% 
    rename_with(.cols = .data[[store_var_2005]], 
                .fn = ~paste0(., '_bins')) %>%
    mutate(across(.cols = ends_with('_bins'), 
                  .fns = \(x) case_when(x == '[-1,0]' ~ '0', # applies to DS_Count_10mile_bins.
                                    x == '(0,1]' ~ '1', 
                                    x == '(1,2]' ~ '2', 
                                    x == '(2,3]' ~ '3', 
                                    x == '(3,4]' ~ '4', 
                                    x == '(4,5]' ~ '5', 
                                    x == '(5,6]' ~ '6', 
                                    x == '(6,7]' ~ '7',
                                    x == '(7,8]' ~ '8',
                                    x == '(8,9]' ~ '9', 
                                    TRUE ~ x) ) ) %>%
    
    left_join(select(posttre_effects, all_of(posttr_key_vars), all_of(store_var_2005)), 
              by = posttr_key_vars) %>% 
    
    arrange(.data[[store_var_2005]]) %>%
    
    mutate(across(.cols = matches('_bins$'), .fn = \(x) factor(x, levels = unique(x))))
  
  store_var_2005_bins <- paste0(store_var_2005, '_bins')
  # Replace the last factor with > # notation. 
  old_levels <- levels(grocery_store_bins[[store_var_2005_bins]])
  
  last_factor <- length(old_levels)
  last_factor_str <- old_levels[last_factor]
  
  last_factor_replace <- str_remove_all(last_factor_str, ',[[:digit:]]+\\]') %>%
    str_replace_all(., '\\(', '\u003e ')
  
  old_levels[last_factor] <- last_factor_replace
  
  grocery_store_bins[[store_var_2005_bins]] <- forcats::fct_recode(grocery_store_bins[[store_var_2005_bins]], 
                                                                   !!last_factor_replace := last_factor_str)
  
  # levels(grocery_store_bins[[store_var_2005]])[last_factor] <- last_factor_replace
  
  # -------------------------------------------------------------------------------------------- #
  return(grocery_store_bins)
  # -------------------------------------------------------------------------------------------- #
  }
  # -------------------------------------------------------------------------------------------- #
  
  grocery_store_bins_df <- names(cut_points) %>% map(function(.x) discretize_store_counts(store_var_2005 = .x))
  grocery_store_bins_df <- grocery_store_bins_df %>% reduce(., .f = left_join, by = posttr_key_vars)
  return(grocery_store_bins_df)
  }
  