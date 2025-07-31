print('Sourced: prepare_binned_grocery_stores_pre <- function(model_preds_dta)')
# -------------------------------------------------------------------------------------------- #
# Function is run for Urban and Rural pre-treatment data to obtain binned and factor formatted dollar store 
# counts and entries to assess relationships between cv errors and dollar store entries/counts conditional 
# on pre-entry/baseline grocery store counts. 
# -------------------------------------------------------------------------------------------- #
prepare_binned_grocery_stores_pre <- function(model_preds_dta){
  
    pretre_errors <- model_preds_dta %>% 
      
      mutate(rel_year = factor(rel_year) )
  # -------------------------------------------------------------------------------------------- #
  # Create binned year 2005 Grocery Store Counts. 
  # -------------------------------------------------------------------------------------------- #
  posttr_key_vars <- c('GEOID', 'year', 'event_year', 'rel_year')
  
  cut_points <- quantile(pretre_errors$Grocery_Count_10mile_2005, probs = seq(0, 1, 0.10))
  # Add a -1 as a cut point to locate 0 into its own bin. 
  cut_points <- c(-1, unique(cut_points))
  # Create recipe for creating binned Grocery Store data. 
  rec <- recipe( ~., data = pretre_errors)
  # step_cut into bins > prep > bake > clean. 
  grocery_store_bins <- rec %>% step_cut(Grocery_Count_10mile_2005, breaks = cut_points) %>%
    prep(training = pretre_errors) %>% 
    bake(new_data = NULL, all_of(posttr_key_vars), Grocery_Count_10mile_2005) %>% 
    rename_with(.cols = Grocery_Count_10mile_2005, 
                .fn = ~paste0(., '_bins')) %>%
    mutate(across(.cols = ends_with('_bins'), 
                  .fns = ~case_when(. == '[-1,0]' ~ '0', # applies to DS_Count_10mile_bins.
                                    . == '(0,1]' ~ '1', 
                                    . == '(1,2]' ~ '2', 
                                    . == '(2,3]' ~ '3', 
                                    . == '(3,4]' ~ '4', 
                                    . == '(4,5]' ~ '5', 
                                    . == '(5,6]' ~ '6', 
                                    . == '(6,7]' ~ '7',
                                    . == '(7,8]' ~ '8',
                                    . == '(8,9]' ~ '9', 
                                    TRUE ~ .) ) ) %>%
    
    left_join(select(pretre_errors, all_of(posttr_key_vars), Grocery_Count_10mile_2005), 
              by = posttr_key_vars) %>% 
    
    arrange(Grocery_Count_10mile_2005) %>%
    
    mutate(across(.cols = Grocery_Count_10mile_2005_bins, .fn = ~factor(., levels = unique(.))))
  
  # Replace the last factor with > # notation. 
  last_factor <- length(levels(grocery_store_bins$Grocery_Count_10mile_2005_bins))
  
  last_factor_replace <- str_remove_all(levels(grocery_store_bins$Grocery_Count_10mile_2005_bins)[last_factor], ',[[:digit:]]+\\]') %>%
    str_replace_all(., '\\(', '\u003e ')
  
  levels(grocery_store_bins$Grocery_Count_10mile_2005_bins)[last_factor] <- last_factor_replace
  
  # -------------------------------------------------------------------------------------------- #
  return(grocery_store_bins)
  # -------------------------------------------------------------------------------------------- #
}
