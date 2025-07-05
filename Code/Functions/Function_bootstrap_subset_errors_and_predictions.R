# -------------------------------------------------------------------------------------------- #
# Convenience function to subset the named elements 'errors' or the 'predictions'. 
# -------------------------------------------------------------------------------------------- #
print('Sourced: subset_errors_and_predictions <- function(bootstrap_data, list_element_name)')
# -------------------------------------------------------------------------------------------- #
subset_errors_and_predictions <- function(bootstrap_data, list_element_name){
  
  subset_boot_data <- bootstrap_data %>% 
    
    map(function(.i){ 
      
      keep(.x = .i, .p = grepl(list_element_name, names(.i))) 
      
    })  %>% 
    
    flatten() %>% # un-nests the inner list. 
    
    bind_rows() # combine all bootstraps into single data.frame()
  
  return(subset_boot_data)
  
}
# -------------------------------------------------------------------------------------------- #