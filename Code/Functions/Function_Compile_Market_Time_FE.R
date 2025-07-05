# Code to compile each sublist of the year, market/state, and market/state^year FEs estimated for the tree-based ML algorithms. 
# -------------------------------------------------------------------------------------------- #
compile_fes <- function(fe_estimates_list, grepl_ls_name, ls_name){

fe_est <- fe_estimates_list %>% 
  map(function(.x){
    keep(.x, .p = grepl(grepl_ls_name, names(.x))) # For each sublist, keep the lists with the names 'year'
  }) %>%
  flatten() %>% # Remove the outer lists to have individual lists. 
  reduce(.f = left_join, by = ls_name)


return(fe_est)

}
# -------------------------------------------------------------------------------------------- #
print('Sourced: compile_fes <- function(fe_estimates_list, grepl_ls_name, ls_name)')
