err_on_ds_policy_boot <- boot_data %>% 
  map(function(.x){ 
    .x %>% 
      pluck('err_on_ds_policy') 
  }) %>% 
  bind_rows()


error_on_ds_policy_summary_boot <- err_on_ds_policy_boot %>%
  group_by(variable) %>%
  summarise(across(.cols = Estimate, 
                   .fns = c('mean' = mean, 
                            'sd' = sd) ) )

err_on_ds_policy_emp <- errors_on_grocery_x_entry_and_policy_combined %>% 
  pluck('err_on_ds_policy') %>%
  select(variable, Estimate, Geography) %>%
  left_join(error_on_ds_policy_summary_boot, by  = 'variable') %>%
  relocate(Geography, .after = last_col())

# -------------------------------------------------------------------------------------------- #
error_on_region_boot <- boot_data %>% 
  map(function(.x){ 
    .x %>% 
      pluck('error_on_region') 
  }) %>% 
  bind_rows()

error_on_region_summary_boot <- error_on_region_boot %>%
  group_by(REGION_NAME) %>%
  summarise(across(.cols = err_avg, 
                   .fns = c('mean' = mean, 
                            'sd' = sd) ) )


err_on_region_emp <- errors_on_grocery_x_entry_and_policy_combined %>% 
  pluck('error_on_region') %>%
  select(REGION_NAME, err_avg, Geography) %>%
  left_join(error_on_region_summary_boot, by  = 'REGION_NAME') %>%
  relocate(Geography, .after = last_col())
# -------------------------------------------------------------------------------------------- #

err_on_grocery_boot <- boot_data %>% 
  map(function(.x){ 
    .x %>% 
      pluck('err_on_grocery_res') 
  }) %>% 
  bind_rows()

grocery_count_levels <- unique(err_on_grocery_boot$count)
err_on_grocery_boot <- err_on_grocery_boot %>%
  mutate(count = factor(count, levels = grocery_count_levels))

err_on_grocery_summary_boot <- err_on_grocery_boot %>%
  group_by(count) %>%
  summarise(across(.cols = Estimate, 
                   .fns = c('mean' = mean, 
                            'sd' = sd) ) )

err_on_grocery_emp <- errors_on_grocery_x_entry_and_policy_combined %>% 
  pluck('err_on_grocery_res') %>%
  select(count, Estimate, Geography, reg_type) %>%
  left_join(err_on_grocery_summary_boot, by  = 'count') %>%
  relocate(Geography, .after = last_col()) %>%
  select(count, matches('^Estimate'), everything())

# -------------------------------------------------------------------------------------------- #
err_on_grocery_x_entry_boot <- boot_data %>% 
  map(function(.x){ 
    .x %>% 
      pluck('err_on_grocery_x_entry') 
  }) %>% 
  bind_rows()
# -------------------------------------------------------------------------------------------- #
err_on_grocery_x_entry_fun <- function(entry_var_arg){
  
  err_on_grocery_x_entry_boot <- err_on_grocery_x_entry_boot %>% 
    filter(entry_var == {{entry_var_arg}})
  
  if (entry_var_arg == 'gross_entry_cumsum'){
    
    err_on_grocery_x_entry_boot <- err_on_grocery_x_entry_boot %>% filter(entry != '0')
  }
  
  entry_levels <- unique(err_on_grocery_x_entry_boot$entry)
  grocery_count_levels <- unique(err_on_grocery_x_entry_boot$grocery_count)
  
  
  err_on_grocery_x_entry_boot <- err_on_grocery_x_entry_boot %>%
    mutate(count = factor(grocery_count, levels = grocery_count_levels), 
           entry = factor(entry, levels = entry_levels) ) 
  
  err_on_grocery_x_entry_summary_boot <- err_on_grocery_x_entry_boot %>%
    group_by(grocery_count, entry) %>%
    summarise(across(.cols = Estimate, 
                     .fns = c('mean' = mean, 
                              'sd' = sd) ) )
  
  
  err_on_grocery_x_entry_emp <- errors_on_grocery_x_entry_and_policy_combined %>% 
    pluck('err_on_grocery_x_entry') %>%
    select(grocery_count, entry, Estimate, Geography, reg_type, entry_var) %>%
    filter(entry_var == {{entry_var_arg}})
  
  if (entry_var_arg == 'gross_entry_cumsum'){
    
    err_on_grocery_x_entry_emp <- err_on_grocery_x_entry_emp %>% filter(entry != '0')
    
  }
  
  entry_levels <- unique(err_on_grocery_x_entry_emp$entry)
  grocery_count_levels <- unique(err_on_grocery_x_entry_emp$grocery_count)
  
  err_on_grocery_x_entry_emp <- err_on_grocery_x_entry_emp %>%
    mutate(grocery_count = factor(grocery_count, levels = grocery_count_levels), 
           entry = factor(entry, levels = entry_levels) ) 
  
  err_on_grocery_x_entry_emp <- err_on_grocery_x_entry_emp %>%
    left_join(err_on_grocery_x_entry_summary_boot, by  = c('grocery_count', 'entry')) %>%
    relocate(Geography, .after = last_col()) %>%
    select(grocery_count, entry, matches('^Estimate'), everything())
  
  return(err_on_grocery_x_entry_emp)
  
}
# -------------------------------------------------------------------------------------------- #
entry_vars <- unique(err_on_grocery_x_entry_boot$entry_var)

err_on_grocery_x_entry_emp_ls <- entry_vars %>% 
  map(function(.x) err_on_grocery_x_entry_fun(entry_var_arg = .x ) )

err_on_grocery_x_entry_emp_ls <- set_names(err_on_grocery_x_entry_emp_ls, nm = entry_vars)

emp_estimates <- c(list('error_on_ds_policy' = err_on_ds_policy_emp),
                   list('error_on_region' = err_on_region_emp),  
                   list('error_on_grocery' = err_on_grocery_emp), 
                   'error_on_grocery_x_entry' = err_on_grocery_x_entry_emp_ls)
# -------------------------------------------------------------------------------------------- #
print('Loaded: `emp_estimates` in global environment.')
