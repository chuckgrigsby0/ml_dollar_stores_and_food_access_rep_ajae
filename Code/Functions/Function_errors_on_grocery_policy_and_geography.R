print('Sourced: errors_on_grocery_x_entry_and_policy <- function()')

errors_on_grocery_x_entry_and_policy <- function(){
# -------------------------------------------------------------------------------------------- #
pretr_preds <- pretr_preds %>% # Varies by bootstrap iteration. 
  
  mutate(err = actual - preds, # Bootstrap error. 
         rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #

# Regression of cv error on dollar store policy variables. 

# -------------------------------------------------------------------------------------------- #
# Create binary no-policy and restriction/restriction+defeated and defeated/defeated+restriction variables. 
ds_policy_reg <- pretr_preds %>%
  select(err, policy_total, Restrictions_and_Defeated, Defeated_and_Restrictions) %>%
  mutate(No_Policy_binary = if_else(policy_total == 0, 1, 0)) %>% 
  mutate(across(.cols = c(Restrictions_and_Defeated, Defeated_and_Restrictions), 
                .fns = \(x) if_else(x > 0, 1, 0), 
                .names = '{.col}_binary' ) )
# -------------------------------------------------------------------------------------------- #
# Regress treatment effects on policy type. 
# -------------------------------------------------------------------------------------------- #
policy_vars <- c('Defeated_and_Restrictions_binary', 'Restrictions_and_Defeated_binary')

reg_policy_type_function <- function(df, ind_var, bootstrap_idx){ 
  
  df_subset <- df %>% 
    
    filter(No_Policy_binary == 1 | .data[[ind_var]] == 1)
  
  reg_policy_type <- feols(xpd(err ~ No_Policy_binary + .[ind_var] - 1), data = df_subset)
  
  reg_policy_type_table <- reg_policy_type$coeftable
  
  reg_policy_type_table <- tibble::rownames_to_column(reg_policy_type_table, 'variable') 
  
  reg_policy_type_table$variable <- reg_policy_type_table$variable %>% 
    str_replace_all(pattern = c('_' = ' ', 'binary' = '')) %>% 
    str_trim(., side = 'right') %>% 
    str_replace_all(pattern = c('Defeated and Restrictions' = 'Defeated', 
                                'Restrictions and Defeated' = 'Restrictions', 
                                'No Policy' = 'No Defeat or Restriction'))
  
  reg_policy_type_table <- reg_policy_type_table %>%
    mutate(
      bootstrap_id = bootstrap_id, 
      outcome = model_dep_var, 
      Geography = model_geography, 
      reg_type = 'policy_type'
    )
  
  return(reg_policy_type_table)
  
}
# -------------------------------------------------------------------------------------------- #
reg_policy_type_table <- map_dfr(policy_vars, function(.x) { 
  
  reg_policy_type_function(df = ds_policy_reg, ind_var = .x)
  
})
# This will keep only one of the No Defeat or Restriction estimates, 
# since they are the same across both regressions.
reg_policy_type_table <- reg_policy_type_table %>% group_by(variable) %>% filter(row_number() == 1)
# -------------------------------------------------------------------------------------------- #

error_on_region <- pretr_preds %>%
  
  group_by(REGION_NAME) %>%
  
  summarise(across(.cols = err, 
                   .fns = list('avg' = mean, 
                               'sd' = sd), 
                   .names = '{.col}_{.fn}') ) %>% 
  
  mutate(bootstrap_id = bootstrap_id, 
         outcome = model_dep_var, 
         Geography = model_geography, 
         reg_type = 'Region')

# -------------------------------------------------------------------------------------------- #

# Regression of cv errors on binned grocery stores at baseline (2005)

# -------------------------------------------------------------------------------------------- #
err_on_grocery_reg <- pretr_preds %>% 
  select(err, Grocery_Count_10mile_2005_bins)

err_on_grocery_reg_formula <- xpd(err ~ -1 + Grocery_Count_10mile_2005_bins, data = err_on_grocery_reg)

err_on_grocery_mod <- feols(err_on_grocery_reg_formula, data = err_on_grocery_reg)

err_on_grocery_res <- err_on_grocery_mod$coeftable

err_on_grocery_res <- tibble::rownames_to_column(err_on_grocery_res, 'variable')

err_on_grocery_res <- err_on_grocery_res %>% 
  separate_wider_delim(cols = variable, delim = '_bins', names = c('reg_type', 'count'))

err_on_grocery_res_table <- err_on_grocery_res %>% 
  mutate(bootstrap_id = bootstrap_id, 
         outcome = model_dep_var, 
         Geography = model_geography) %>%
  relocate(reg_type, .after = last_col())

# -------------------------------------------------------------------------------------------- #

# Regression of cv errors on binned grocery stores at baseline (2005) interacted with entry events. 

# -------------------------------------------------------------------------------------------- #
pretr_preds_aug <- pretr_preds_aug %>% # Varies by bootstrap iteration. 
  
  mutate(err = actual - preds, # Bootstrap error. 
         rel_year = factor(rel_year))
# -------------------------------------------------------------------------------------------- #

err_on_grocery_x_entry_reg <- pretr_preds_aug %>% 
  select(err, Grocery_Count_10mile_2005_bins, matches('entry_'))

entry_vars <- str_subset(names(err_on_grocery_x_entry_reg), pattern = 'entry_'); entry_vars

err_on_grocery_x_entry_fun <- function(dta, entry_var_arg){ 
  
  err_on_grocery_reg_formula <- xpd(err ~ -1 + Grocery_Count_10mile_2005_bins:.[entry_var_arg], 
                                    data = dta)
  
  err_on_grocery_mod <- feols(err_on_grocery_reg_formula, data = dta)
  
  err_on_grocery_res <- err_on_grocery_mod$coeftable
  
  err_on_grocery_res <- tibble::rownames_to_column(err_on_grocery_res, 'variable')
  
  err_on_grocery_res <- separate_wider_regex(
    data = err_on_grocery_res,
    cols = variable,
    patterns = c(
      reg_type = "^Grocery_Count_10mile_2005",  # Match start of the string until the first underscore
      '_bins',
      grocery_count = "(?<=_bins)[\\d\\(\\)> \\d+]+[\\d,]*\\]?",  # Look-behind for '_bins' followed by digits
      ':',
      entry_var = "(?<=:).*",# Look-behind for ':' followed by non-underscore characters
      '_bins', 
      entry = "\\[?[-\\d\\(\\)>\\,]+\\]?"  # Match digits at the end of the string
    ),
    names_repair = "check_unique",
    cols_remove = TRUE
  )
 
  err_on_grocery_res <- err_on_grocery_res %>% 
    mutate(bootstrap_id = bootstrap_id, 
           outcome = model_dep_var, 
           Geography = model_geography) %>%
    relocate(reg_type, .after = last_col()) %>%
    relocate(entry_var, .after = last_col() )
  
  return(err_on_grocery_res)
  
}
# -------------------------------------------------------------------------------------------- #
err_on_grocery_x_entry_table <- map_dfr(entry_vars, function(.x){ 
  
  err_on_grocery_x_entry_fun(dta = err_on_grocery_x_entry_reg, 
                             entry_var_arg = .x)
  })  

# -------------------------------------------------------------------------------------------- #
results <- list('err_on_ds_policy' = reg_policy_type_table, 
                'error_on_region' = error_on_region,
                'err_on_grocery_res' = err_on_grocery_res_table, 
                'err_on_grocery_x_entry' = err_on_grocery_x_entry_table)

# -------------------------------------------------------------------------------------------- #
return(results)

}
