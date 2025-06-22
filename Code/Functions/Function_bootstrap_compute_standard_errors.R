# -------------------------------------------------------------------------------------------- #
# Compute the bootstrapped mean and standard errors of the relative-time terms from 
# regressing the prediction error on relative time. 
# -------------------------------------------------------------------------------------------- #
# Note:
# Positive error means that the block group is low access but we predict not low access.
# Negative error mean that the block group is not low access but we predict low access. 
# -------------------------------------------------------------------------------------------- #
cat('Sourced: compute_bootstrap_standard_errors <- function(dta, group_vars) \n 
      Note: group_vars can be string of characters')
# -------------------------------------------------------------------------------------------- #
compute_bootstrap_standard_errors <- function(dta, group_vars){ 
  
  group_vars <- syms(group_vars)
  
  bootstrap_summary <- dta %>% 
    
    group_by(!!!(group_vars)) %>% # There is only one Outcome in the errors bootstraps (CV Error).
    
    # Note: Also works. 
    # group_by(across(all_of(group_vars))) %>%
    
    summarise(across(.cols = estimate, .fns = list(mean = mean, sd = sd), .names = 'bootstrap_{.fn}')) 
  
  return(bootstrap_summary)
  
}
# -------------------------------------------------------------------------------------------- #