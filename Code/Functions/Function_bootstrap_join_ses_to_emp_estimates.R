# -------------------------------------------------------------------------------------------- #
# Convenience function to combine the empirical estimates with the bootstrapped standard errors. 
# -------------------------------------------------------------------------------------------- #
cat('Sourced: join_bootstrap_ses_to_emp_estimates <- function(empirical_estimates_dta, bootstrap_estimates_dta, join_vars) \n
    Note: join_vars can be a string of characters')

# Note that contains(entry_events) will apply to post-treatment data. 
# label to DIVISION_NAME corresponds to the errors and effects on covariate bootstrap analyses. 
# -------------------------------------------------------------------------------------------- #
join_bootstrap_ses_to_emp_estimates <- function(empirical_estimates_dta, bootstrap_estimates_dta, join_vars){ 
  
  
  empirical_estimates_dta <- empirical_estimates_dta %>%
    
    left_join(select(bootstrap_estimates_dta, 
                     matches('^rel_year$'),
                     matches('^term$'), 
                     starts_with('bootstrap'), 
                     matches('Outcome', ignore.case = TRUE), # will match Outcome or outcome. 
                     contains('entry_events'), 
                     matches('^label$'), 
                     matches('^covariate$'), 
                     matches('^REGION_NAME$'), 
                     matches('^DIVISION_NAME$'), 
                     matches('^ds_entry$'), 
                     matches('^entry$'), 
                     matches('^quartile$'), 
                     matches('^grocery_stores$')), 
              by = {{join_vars}}) %>% 
    
    # Also works:
    #left_join(select(bootstrap_estimates_dta, rel_year, starts_with('bootstrap'), Outcome), 
    #         by = names(select(bootstrap_estimates_dta, all_of(join_vars)))) %>% 
    
    relocate(bootstrap_sd, .after = estimate) # %>% 
    
  return(empirical_estimates_dta)
  
}
# -------------------------------------------------------------------------------------------- #

