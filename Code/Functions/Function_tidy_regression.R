# This function is used to post-process regression models to assess predictions and prediction errors. 
# -------------------------------------------------------------------------------------------- #
tidy_regression <- function(lm_model, dep_var_string){
  
    coef_table <- broom::tidy(lm_model)
    coef_table <- coef_table %>% mutate(rel_year = str_remove_all(term, pattern = 'rel_year')) %>% relocate(rel_year)
    coef_table$Outcome <- dep_var_string
    
    return(coef_table)
}

print('Sourced: tidy_regression <- function(lm_model, dep_var_string)')

# -------------------------------------------------------------------------------------------- #
tidy_regression_err_vs_covar_binned <- function(lm_model){
  
  coef_table <- broom::tidy(lm_model)
  coef_table$term <- str_replace_all(coef_table$term, '.*bins', '')
  
  #first_bin <- which(stringr::str_detect(coef_table$term, '^\\[0,')) # Set the first bin to zero for plots. 
  first_bin <- coef_table$term[1]
  first_bin <- str_remove_all(first_bin, '\\[0,') # Remove everything just after [0, in the bin. 
  first_bin <- paste0('\u2264 ', str_remove_all(first_bin, '\\]')) # Unicode for less than or equal to. 
  
  coef_table$term[1] <- first_bin
  last_bin <- nrow(coef_table)
  coef_table$term[last_bin] <- paste0( '> ', str_remove(str_remove(coef_table$term[last_bin] , '\\('), ',.*\\]')) # Remove the '(' and keep the number after the ','
  
  # Clean up brackes with negative numbers. 
  grepl_lteq <- grepl('\u2264', coef_table$term)
  
  coef_table$term[grepl_lteq] <- str_remove_all(coef_table$term[grepl_lteq], '\\[.*,') 
  
  coef_table <- coef_table %>% 
    
    mutate(term = case_when(term == '\u2264 0' ~ '0', 
                            term == '(0,1]' ~ '1', 
                            term == '(1,2]' ~ '2', 
                            term == '(2,3]' ~ '3', 
                            term == '(3,4]' ~ '4', 
                            term == '(4,5]' ~ '5', 
                            term == '(5,6]' ~ '6', 
                            term == '(6,7]' ~ '7',
                            term == '(7,8]' ~ '8',
                            term == '(8,9]' ~ '9', 
                            TRUE ~ term) ) # %>%
    
    #mutate(across(.cols = term, 
     #             .fns = ~factor(., levels = unique(.))))
  
  return(coef_table)
}

print('Sourced: tidy_regression_err_vs_covar_binned <- function(lm_model)')
# -------------------------------------------------------------------------------------------- #