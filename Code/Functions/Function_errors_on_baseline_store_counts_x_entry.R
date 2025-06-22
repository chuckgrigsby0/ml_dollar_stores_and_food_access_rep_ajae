print('Sourced: errors_on_baseline_store_counts_x_entry <- function()')

errors_on_baseline_store_counts_x_entry <- function(){
  # -------------------------------------------------------------------------------------------- #
 
  # -------------------------------------------------------------------------------------------- #
  
  # Regression of cv errors on binned retail stores at baseline (2005) interacted with entry events. 
  
  # -------------------------------------------------------------------------------------------- #
  pretr_preds_aug <- pretr_preds_aug %>% # Varies by bootstrap iteration. 
    
    mutate(err = actual - preds, # Bootstrap error. 
           rel_year = factor(rel_year))
  # -------------------------------------------------------------------------------------------- #
  
  err_on_store_x_entry_reg <- pretr_preds_aug %>% 
    select(err, matches('entry_'), matches('Count')) %>%
    rename_with(.fn = \(x) paste0(x, '_bins'), 
                .cols = matches('_2005$'))
  
  entry_vars <- str_subset(names(err_on_store_x_entry_reg), pattern = 'entry_'); entry_vars
  store_vars <- str_subset(names(err_on_store_x_entry_reg), pattern = 'Count'); store_vars
  
  err_on_store_x_entry_reg_fun <- function(dta, store_var_arg, entry_var_arg){ 
    
    err_on_store_reg_formula <- xpd(err ~ -1 + .[store_var_arg]:.[entry_var_arg], 
                                      data = dta)
    
    err_on_store_mod <- feols(err_on_store_reg_formula, data = dta)
    
    err_on_store_res <- err_on_store_mod$coeftable
    
    err_on_store_res <- tibble::rownames_to_column(err_on_store_res, 'variable')
    
    store_match_regex <- str_remove_all(store_var_arg, pattern = '_bins')
    
    err_on_store_res <- separate_wider_regex(
      data = err_on_store_res,
      cols = variable,
      patterns = c(
        reg_type = paste0("^", store_match_regex),  # Match start of the string until the first underscore
        '_bins',
        store_count = "(?<=_bins)\\[?[-\\d\\(\\)> \\d+]+[\\d,]*\\]?",  # Look-behind for '_bins' followed by digits
        ':',
        entry_var = "(?<=:).*",# Look-behind for ':' followed by non-underscore characters
        '_bins', 
        entry = "\\[?[-\\d\\(\\)>\\,]+\\]?"  # Match digits at the end of the string
      ),
      names_repair = "check_unique",
      cols_remove = TRUE
    )
    
    err_on_store_res <- err_on_store_res %>% 
      mutate(bootstrap_id = bootstrap_id, 
             outcome = model_dep_var, 
             Geography = model_geography) %>%
      relocate(reg_type, .after = last_col()) %>%
      relocate(entry_var, .after = last_col() )
    
    return(err_on_store_res)
    
  }
  # -------------------------------------------------------------------------------------------- #
  err_on_store_x_entry_table <- map_dfr(store_vars, function(.x){
    
    map_dfr(entry_vars, function(.y){
    
    err_on_store_x_entry_reg_fun(dta = err_on_store_x_entry_reg, 
                               store_var_arg = .x,
                               entry_var_arg = .y)
  }) 
    })
  
  # -------------------------------------------------------------------------------------------- #

  return(err_on_store_x_entry_table)
  
}
