print('Sourced: prepare_binned_and_factor_dsvars_post <- function(national, geography_str, model_preds_dta)')
# -------------------------------------------------------------------------------------------- #
# Function is run for Urban and Rural posttreatment data to obtain binned and factor formatted dollar store 
# counts and entries to assess relationships between causal effects and dollar store entries/counts. 
# Integers/Counts are converted to factors. Numeric data are converted to bins. 
# -------------------------------------------------------------------------------------------- #
prepare_binned_and_factor_dsvars_post <- function(national, geography_str, model_preds_dta){
  
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
  
  # tau and dollar store entry/counts variables. 
  
  # -------------------------------------------------------------------------------------------- #
  
  ds_entry_counts_vars <- c('DS_Count_10mile', 'DS_Count_10mile_diff', 'net_entry_cumsum', 'entry_events')
  
  posttre_effects_wdsvars <- posttre_effects %>%
    select(GEOID, year, event_year, rel_year, tau, all_of(ds_entry_counts_vars)) 
  
  # Compute cumulative gross entry for treated block-groups. 
  # Up until treatment DS_Count_10mile_diff is either zero or some negative integer. 
  # Therefore, setting DS_Count_10mile_diff to zero for negative integers and cumulatively summing implies 
  # that we track gross entry over time. 
  posttre_effects_wdsvars <- posttre_effects_wdsvars %>% 
    mutate(gross_entry = case_when(DS_Count_10mile_diff < 0 ~ 0, 
                                          TRUE ~ DS_Count_10mile_diff)) %>%
    group_by(GEOID) %>%
    mutate(gross_entry_cumsum = cumsum(gross_entry)) %>%
    select(-DS_Count_10mile_diff) %>%
    ungroup()
  
  ds_entry_counts_vars <- c('DS_Count_10mile', 'net_entry_cumsum', 'entry_events', 'gross_entry_cumsum')
  # -------------------------------------------------------------------------------------------- #
  # Convert counts to integers to separate variables by types (numeric vs integer). 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_wdsvars <- posttre_effects_wdsvars %>% 
    mutate(across(.cols = all_of(ds_entry_counts_vars), 
                  .fns = ~as.integer(.), 
                  .names = '{col}_int')) 
  # -------------------------------------------------------------------------------------------- #
  
  # -------------------------------------------------------------------------------------------- #
  # Separate key, numeric and integer covariates. 
  # -------------------------------------------------------------------------------------------- #
  posttr_key_vars <- setdiff(names(posttre_effects_wdsvars), c(ds_entry_counts_vars, paste0(ds_entry_counts_vars, '_int'))); posttr_key_vars
  
  integer_vars <- names(select(posttre_effects_wdsvars, where(is.integer)))
  integer_vars <- integer_vars[!grepl('tau', integer_vars)]; integer_vars # Remove integer var. "tau" if integer.
  
  # -------------------------------------------------------------------------------------------- #
  # Non-integer variable breaks. 
  custom_breaks_ds_vars = posttre_effects_wdsvars %>%
    
    reframe(across(.cols = all_of(ds_entry_counts_vars), 
                   
                   .fns = ~quantile(., probs = seq(0, 1, 0.10))))
  # -------------------------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------------------------- #
  # Function to create binned dollar store counts/entry variables one-by-one. 
  # -------------------------------------------------------------------------------------------- #
  binned_dsvars <- function(dta, custom_breaks_vartype, covariates, id){
    
    if ( ( sum(custom_breaks_vartype[, id] < 0 ) > 0 ) ){ # If sum of negative values is greater than one, then... 
      
      cut_points <- unique(unlist(custom_breaks_vartype[ , id]))  
      cut_points <- sort(c(-1, cut_points)) } # Include -1 in vector of deciles to ensure that negative values are not included with 0 bin.
    
    else if ( ( sum(custom_breaks_vartype[, id] < 0 ) == 0 ) ){ # else if sum of negative values is equal to zero, then...
      
      cut_points <- unique(unlist(custom_breaks_vartype[ , id]))  # Include -1 in vector to ensure that last bin is 0. 
      
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
    posttre_effects_wdsvars_binned <- baked_bins %>% rename_with(.cols = all_of(covariates[id]), 
                                                                 .fn = ~paste0(covariates[id], '_bins')) %>%
      
    
      mutate(across(.cols = ends_with('_bins'), 
                    .fns = ~case_when(. == '[-1,0]' ~ '0', # applies to DS_Count_10mile_bins.
                                      . == '[-1,1]' ~ '1', # applies to entry_events_bins. 
                                      . == '(-1,0]' ~ '0', # applies to net_entry_cumsum_bins.
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
      # Join the original variable, sort in ascending order, and convert the binned variable to a factor. 
      left_join(select(dta, all_of(posttr_key_vars), all_of(covariates[id]) ), by = posttr_key_vars) %>%
      
      arrange(.data[[ covariates[id] ]]) %>% 
      
      mutate(across(.cols = ends_with('_bins'), 
                    .fn = ~factor(., levels = unique(.)) ) ) %>%
      
      select(-all_of(covariates[id]))
    
    return(posttre_effects_wdsvars_binned)
    
  }
  
  # -------------------------------------------------------------------------------------------- #
  # Creates Lists of data frames each of which contains the pretr_key_vars and a covariate specified by id.  
  # -------------------------------------------------------------------------------------------- #
  binned_dsvars_df <- seq_along(ds_entry_counts_vars) %>% 
    
    map(function(.x){ 
      
      binned_dsvars(dta = posttre_effects_wdsvars, 
                    custom_breaks_vartype = custom_breaks_ds_vars, 
                    covariates = ds_entry_counts_vars,
                    id = .x)
      
    })
  
  binned_dsvars_df <- binned_dsvars_df %>% reduce(left_join, by = posttr_key_vars)
  # -------------------------------------------------------------------------------------------- #
  # Integer variables. 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects_wdsvars_integers <- posttre_effects_wdsvars %>% select(all_of(posttr_key_vars), all_of(integer_vars)) 
  
  posttre_effects_wdsvars_integers_simple <- posttre_effects_wdsvars_integers %>%
    mutate(across(.cols = c(all_of(integer_vars) ), 
                  .fns = ~factor(., levels = sort(unique(.) ) ) ) ) 
  # -------------------------------------------------------------------------------------------- #
  # Merge together the original dollar store count and entry vars not ending with '_int', the binned and the factor variables.
  # -------------------------------------------------------------------------------------------- #
  posttr_binned_dsvars <- list(select(posttre_effects_wdsvars, -ends_with('_int') ),  
                               binned_dsvars_df, 
                               posttre_effects_wdsvars_integers_simple) %>%
    
    reduce(left_join, by = posttr_key_vars)
  # -------------------------------------------------------------------------------------------- #
  return(posttr_binned_dsvars)
  
}
