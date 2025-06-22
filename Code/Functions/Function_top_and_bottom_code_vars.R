cat('Sourced: ', paste('top_and_bottom_code_vars <- ', 'function(dta,', '\n', 
                       'covariate, # data frame and covariate string (column name in dta)', '\n',
                       'lpctl, hpctl, # percentile index to slice; 2 indicates 1 percentile; 100 indicates 99 percentile', '\n',
                       'lpctl_str, hpctl_str){ # percentile strings.'))
# -------------------------------------------------------------------------------------------- #
top_and_bottom_code_vars_pctls <- function(dta, covariate, # data frame and covariate string (column name in dta)
                                     lpctl, hpctl, # percentile index to slice; 2 indicates 1 percentile; 100 indicates 99 percentile
                                     lpctl_str, hpctl_str){ # percentile strings. 
  
  covar_pctl <- dta %>% 
    
    reframe(across(.cols = all_of(covariate), .fns = ~quantile(., probs = seq(0, 1, 0.01)) ) ) %>% 
    
    dplyr::slice(c(lpctl, hpctl)) # Obtains e.g., first and 99th percentile values.
  
  # Obtain numeric values for the percentiles. 
  
    pctl_low <- as.numeric(covar_pctl[[covariate]][names(covar_pctl[[covariate]]) == lpctl_str]) # e.g., '1%'
    
    pctl_high <- as.numeric(covar_pctl[[covariate]][names(covar_pctl[[covariate]]) == hpctl_str]) # e.g., '99%'
    
    # Bottom and Top code values below and above pctl_low and pctl_high. 
    
    dta <- dta %>% mutate(across(.cols = all_of(covariate), 
                                 .fn = ~case_when(. <= pctl_low ~ pctl_low, 
                                                  . >= pctl_high ~ pctl_high, 
                                                  TRUE ~ .) ) ) %>%
      select(all_of(covariate))
    
    return(dta)
  
}
# -------------------------------------------------------------------------------------------- #

cat('Sourced: ', paste('top_code_vars <- ', 'function(', '\n', 
                       'dta, # data frame', '\n', 
                       'covariate) # covariate string (column name in dta)') )

# -------------------------------------------------------------------------------------------- #
top_code_vars_iqr <- function(dta, covariate){ # percentile strings. 
  
  # Computes IQR and multiplies by 1.5. 
  iqr <- dta %>% 
    reframe(across(.cols = all_of(covariate), .fns = ~IQR(.) ) ) %>%
    mutate(across(.cols = where(is.numeric), ~.*1.5))
  
  # Finds 3rd Quartile. 
  third_quartile <- dta %>% 
    reframe(across(.cols = all_of(covariate), 
                   .fns = ~quantile(., probs = c(0.75) ) ) )
  
  # Adds IQR*1.5 to third_quartile. 
  iqr_threshold <- third_quartile + iqr
  
  # Converts to numeric scalar. 
  iqr_threshold <- iqr_threshold %>% select(all_of(covariate)) %>% as.numeric
  
  
  # Top code values above iqr_threshold
  
  dta <- dta %>% mutate(across(.cols = all_of(covariate), 
                               .fn = ~case_when(. >= iqr_threshold ~ iqr_threshold, 
                                                TRUE ~ .) ) ) %>%
    select(all_of(covariate))
  
  return(dta)
  
}
# -------------------------------------------------------------------------------------------- #
