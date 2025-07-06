print('Sourced: ds_entry_by_num_grocery_and_superette_2005 <- functionfunction(dta, ds_entry_var, grocery_store_var, superette_var, iter_id)')
#--------------------------------------------------------------------------------------------#
# Effects by DS entry x 2005 Grocery stores X 2005 Superettes
#--------------------------------------------------------------------------------------------#
ds_entry_by_num_grocery_and_superette_2005 <- function(dta, ds_entry_var, grocery_store_var, superette_var, iter_id){
  
  sum_stats <- dta %>% 
    group_by(.data[[ds_entry_var]], 
             .data[[grocery_store_var]], 
             .data[[superette_var]]) %>%
    summarise(across(.cols = c(actual, preds, tau), 
                     .fns = c('avg' = mean, 
                              'total' = sum), 
                     .names = '{.col}_{.fn}')) %>%
    mutate(pct_att = 100*(tau_avg/preds_avg) ) %>%
    select(all_of(ds_entry_var), all_of(grocery_store_var), all_of(superette_var), 
           matches('*_avg$'), pct_att, matches('*_total$'))
  
  total_per_group <- dta %>% 
    group_by(.data[[ds_entry_var]], 
             .data[[grocery_store_var]], 
             .data[[superette_var]]) %>%
    count(name = 'total_per_group')
  
  total_bg_per_group <- dta %>% 
    group_by(.data[[ds_entry_var]], 
             .data[[grocery_store_var]], 
             .data[[superette_var]]) %>%
    distinct(GEOID) %>%
    count(name = 'total_bg_per_group')
  
  sum_stats <- sum_stats %>% 
    left_join(total_per_group, by = c(ds_entry_var, 
                                      superette_var, 
                                      grocery_store_var) ) %>%
    left_join(total_bg_per_group, by = c(ds_entry_var, 
                                         superette_var, 
                                         grocery_store_var) ) %>%
    ungroup() %>%
    mutate(total_treated = sum(total_per_group), 
           share_per_group = total_per_group/total_treated)
  
  if (ds_entry_var == 'gross_entry_cumsum_bins'){
    sum_stats <- sum_stats %>% 
      filter(gross_entry_cumsum_bins != 0) 
  }
  
  sum_stats_long <- sum_stats %>% 
    pivot_longer(cols = actual_avg:share_per_group, 
                 names_to = 'stat', 
                 values_to = 'values') %>%
    mutate(values = round(values, digits = 3))
  
  sum_stats_long$stat_tidy <- sum_stats_long$stat %>% str_replace_all(c('actual_avg' = 'Low Access (Actual)', 
                                                                        'preds_avg' = 'Low Access (Pred.)', 
                                                                        'tau_avg' = 'ATT', 
                                                                        'pct_att' = '% Change ATT', 
                                                                        'actual_total' = 'Total Low Access (Actual)', 
                                                                        'preds_total' = 'Total Low Access (Pred.)',
                                                                        'tau_total' = 'Total ATT', 
                                                                        'total_per_group' = 'Obs. per Group',
                                                                        'total_bg_per_group' = 'Unique BGs per Group',
                                                                        'share_per_group' = 'Share of Obs.') )
  
  sum_stats_long <- sum_stats_long %>% relocate(stat_tidy, .before = stat) %>% relocate(stat, .after = last_col())
  sum_stats_long <- sum_stats_long %>% mutate(boot_iteration = iter_id)
  
  return(sum_stats_long)
}


print('Sourced: effects_by_ds_entry_x_grocery_x_superette <- function(dta, ds_entry_var, grocery_store_var, superette_var, iter_id)')
#--------------------------------------------------------------------------------------------#
# Effects on DS entry x 2005 Grocery stores X 2005 Superettes
#--------------------------------------------------------------------------------------------#
effects_by_ds_entry_x_grocery_x_superette <- function(dta, ds_entry_var, grocery_store_var, superette_var, iter_id){  
  
  reg_formula <- xpd(tau ~ -1 + .[ds_entry_var]:.[grocery_store_var]:.[superette_var],
                     data = dta)
  
  ds_entry_x_grocery_reg <- feols(reg_formula, data = dta)
  
  tidy_reg <- broom::tidy(ds_entry_x_grocery_reg)
  
  
  remove_string_regex <- paste(ds_entry_var, grocery_store_var, superette_var, sep='|')
  
  tidy_reg <- tidy_reg %>% 
    
    separate_wider_delim(cols = term, 
                         delim = ":",
                         names = c("ds_entry_bins", "grocery_stores", "superettes"),
                         cols_remove = TRUE, 
                         too_many = "merge") %>%
    mutate(across(.cols = c("ds_entry_bins", "grocery_stores", "superettes"), 
                  .fn = \(x) 
                  str_remove_all(x, remove_string_regex) 
    ) )
  
  tidy_reg <- tidy_reg %>%
    mutate(label = 'Average Effect in Bin', 
           outcome = 'tau', 
           superette_count_var = superette_var, 
           grocery_count_var = grocery_store_var,
           ds_entry = ds_entry_var,
           boot_iteration = iter_id) %>%
    relocate(ds_entry)
  
  return(tidy_reg)
  
}
