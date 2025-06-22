print('Sourced: summary_by_entry_and_grocery(df_post, df_pre, entry_var_type, bootstrap_id_num)')

summary_by_entry_and_grocery <- function(df_post, df_pre, entry_var_type, bootstrap_id_num){ 
  
  grouping_vars <- c(entry_var_type, 'Grocery_Count_10mile_2005_bins')
  
  att_summary <- df_post %>% 
    
    group_by(
      across(.cols = all_of(grouping_vars) )
    ) %>% 
    
    summarise(across( 
      
      .cols = c(actual, preds, tau), 
      .fn = list('avg' = \(x) mean(x), 
                 'total' = \(x) sum(x) ), 
      .names = "{str_replace_all(.col, c('tau' = 'att'))}_{.fn}_post") 
    ) %>% 
    
    ungroup() %>%
    
    mutate(pct_att_avg_post = att_avg_post/preds_avg_post) %>%
    
    select(-att_total_post)
  
  # ----------------------------- #
  
  err_summary <- df_pre %>% 
    
    mutate(err = actual - preds) %>%
    
    group_by(
      across(.cols = all_of(grouping_vars) )
    ) %>% 
    
    summarise(across( 
      
      .cols = c(actual, preds, err), 
      .fn = list('avg' = \(x) mean(x)), 
      .names = "{.col}_{.fn}_pre") 
    ) %>% 
    
    ungroup() %>%
    
    mutate(pct_err_avg_pre = err_avg_pre/preds_avg_pre, 
           id = bootstrap_id_num) 
  
  # ----------------------------- #
  # Obtain total rows per group. 
  obs_summary <- df_post %>% 
    
    group_by(
      across(.cols = all_of(grouping_vars) )
    ) %>% 
    
    summarise(total_per_group_post = n() ) %>% 
    
    ungroup() 
  
  # ----------------------------- #
  # Unique counts of entry events for each block group, then sum across block groups
  # for total events by dollar store entry and grocery store count combination. 
  unique_bg_summary <- df_post %>% 
    
    group_by(GEOID) %>% 
    
    distinct(across(.cols = all_of(grouping_vars) )) %>% 
    
    group_by(across(.cols = all_of(grouping_vars) )) %>%
    
    summarise(unique_bg_per_group_post = n() ) %>%
    
    ungroup() 
  # ----------------------------- #
  
  att_summary <- list(att_summary, obs_summary, unique_bg_summary) %>%
    
    reduce(
      .f = left_join, 
      by = all_of(grouping_vars)
    ) %>%
    
    relocate(matches('total_post$'), .before = 'total_per_group_post') %>%
    
    ungroup() %>%
    
    mutate(total_obs_post = sum(total_per_group_post, na.rm=TRUE), 
           share = total_per_group_post/total_obs_post,
           id = bootstrap_id_num)
  
  err_att_summary <- list('err_by_entry_grocery' = err_summary, 
                          'att_by_entry_grocery' = att_summary)
  
  return(err_att_summary)
  
}
