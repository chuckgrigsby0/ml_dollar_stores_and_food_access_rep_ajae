print('Sourced: prepare_binned_ds_policy_vars <- function(df)')

prepare_binned_ds_policy_vars <- function(df){

  # Subset the post-treatment data. 
  posttre_effects <- df %>% 
    
    filter(rel_year >= 0) %>% 
    
    filter(grepl(model_geography, Geography)) %>%
    
    mutate(rel_year = factor(rel_year), 
           Restrictions = Moratorium + Ordinance) %>% 
    mutate(
           Defeated_and_Restrictions = case_when(Defeated >= 1 & Restrictions == 0 ~ Defeated + Restrictions, 
                                                 Defeated >= 1 & Restrictions >= 1 ~ Defeated + Restrictions, 
                                                 Defeated == 0 & Restrictions >= 1 ~ 0, 
                                                 Defeated == 0 & Restrictions == 0 ~ 0),
           Restrictions_and_Defeated = case_when(Restrictions >= 1 & Defeated == 0 ~ Defeated + Restrictions, 
                                                 Restrictions >= 1 & Defeated >= 1 ~ Defeated + Restrictions, 
                                                 Restrictions == 0 & Defeated >= 1 ~ 0, 
                                                 Restrictions == 0 & Defeated == 0 ~ 0),
           policy_total_binary = if_else(policy_total > 0, 1, 0) ) %>%
    
    relocate(c(Restrictions, contains('_and_'), policy_total_binary), .before = policy_total)
  
  
  # Define the dollar store policy variables. 
  policy_vars_by_type = names(posttre_effects) %>% 
    str_subset(., pattern = '^(Def|Restr|Mor|Ord|policy_total|policy_year)'); policy_vars_by_type
  
  # Define cut points for bins. 
  ds_policy_cutpoints <- seq(-1, 3, 1); ds_policy_cutpoints
  
  # Define join variables. 
  posttr_key_vars = c('GEOID', 'year', 'event_year', 'rel_year')
  # -------------------------------------------------------------------------------------------- #
  # Create a recipe. 
  # -------------------------------------------------------------------------------------------- #
  rec <- recipe(tau ~ ., data = posttre_effects)
  
  custom_bins <- rec %>% step_cut(all_of(policy_vars_by_type), breaks = ds_policy_cutpoints)
  
  # Prepare the recipe. 
  prep_bins <- custom_bins %>% prep(training = posttre_effects)
  
  # Bake the recipe --> Creates bins. 
  baked_bins <- prep_bins %>% bake(new_data = NULL, all_of(policy_vars_by_type))
  
  baked_bins <- bind_cols(select(posttre_effects, all_of(posttr_key_vars)), baked_bins)
  # -------------------------------------------------------------------------------------------- #
  # Functions to re-format bins and get new factor levels. 
  # -------------------------------------------------------------------------------------------- #
  re_format_bins <- function(factor_var, var_name){
    
    if (var_name != 'policy_total'){ # If Defeated or Restrictions or policy_year
      
      string_var = as.character(factor_var) %>%
        str_remove_all(pattern = '[ \\[\\(]-?\\d+,') %>%
        str_replace_all(pattern = '\\]', replacement='') %>% 
        str_replace_all(pattern = '2|3', replacement = '1')  
      
    } else { # if policy_total
      
      string_var = as.character(factor_var) %>%
        str_remove_all(pattern = '[ \\[\\(]-?\\d+,') %>%
        str_replace_all(pattern = '\\]', replacement='') %>% 
        str_replace_all(pattern = '2|3', replacement = '> 1')
      
    }
    
    
    return(string_var)
    
  }
  
  clean_policy_vars <- function(df, var){ 
    
    df <- df %>% arrange({{var}}) %>% 
      
      mutate(across(.cols = all_of(var), 
                    .fn = \(x) re_format_bins(factor_var = x, var_name = {{var}}))) %>%
      mutate(across(.cols = all_of(var), 
                    .fn = \(x) as.factor(x))) %>%
      
      select(all_of(posttr_key_vars), all_of(var),)
    
    return(df)
    
  }
  # -------------------------------------------------------------------------------------------- #
  # Use above functions. 
  
  baked_bins_list <- map(.x = policy_vars_by_type, 
                         function(.x){ 
                           
                           clean_policy_vars(df = baked_bins, var = .x)
                           
                         })
  
  baked_bins_list_to_df <- baked_bins_list %>% reduce(.f = left_join, by = posttr_key_vars)
  
  baked_bins_list_to_df <- baked_bins_list_to_df %>% 
    mutate(across(.cols = 'policy_total', 
                  .fns = \(x) forcats::fct_relevel(x, c('0', '1', '> 1') ) ) )
  # -------------------------------------------------------------------------------------------- #
  # Append the word '_bins' to newly created factor columns. 
  # -------------------------------------------------------------------------------------------- #
  baked_bins_list_to_df <- baked_bins_list_to_df %>% 
    rename_with(.cols = all_of(policy_vars_by_type), 
                .fn = \(x) paste0(x, '_bins'))
  
  baked_bins_list_to_df <- baked_bins_list_to_df %>% 
    
    left_join(select(posttre_effects, 
                     all_of(posttr_key_vars), 
                     policy_total, policy_total_binary,
                     Defeated, Ordinance, Moratorium, Restrictions, contains('_and_')), 
              by = posttr_key_vars)
  
  return(baked_bins_list_to_df)

}
