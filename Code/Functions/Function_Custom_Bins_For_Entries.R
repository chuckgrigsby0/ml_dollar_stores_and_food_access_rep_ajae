# Custom function for creating decile bins of net entry. 
print('Sourced: Custom_Bins_Function')
Custom_Bins_Function <- function(df_treated, entry_var, entry_var_str){
  
quintiles <-  df_treated %>% 
  ungroup() %>% 
  summarise(across(.cols = {{ entry_var }}, 
                   ~unique(quantile(., prob = seq(0, 0.90, 0.10)))))

quintiles <- as.numeric(quintiles[[1]])

cond1 <- sum(quintiles<0) > 0
cond2 <- sum(quintiles<0) == 0
if(cond1 == TRUE){
  custom_breaks <- vector(mode = 'numeric')
  custom_breaks <- sort(unique(c(-1, 0, quintiles)))
} else if(cond2 == TRUE){
  custom_breaks <- vector(mode = 'numeric')
  custom_breaks <- unique(quintiles)
}
# -------------------------------------------------------------------------------------------- #
# Create a recipe. 
my_form = paste0('~', entry_var_str)
my_form  = as.formula(my_form)
rec <- df_treated %>% recipe(my_form, data = .)
# Select predictors for recipe. 
# Either quintiles or discrete bins. 
# --------------------------------------------------------------- #
#rec <- rec %>% step_discretize('net_entry_cumsum', breaks = quintiles, min_unique = 10, 
#                              options = list(prefix = 'quintile_'))
# --------------------------------------------------------------- #
# See ?step_cut() on how the function handles the lowest values for the bins. 
# Effectively, step_cut() with breaks = c(0, quintiles) will create a lowest bin
# in which negative numbers are grouped with 0. 
# --------------------------------------------------------------- #
custom_bins <- rec %>% step_cut(all_of(entry_var_str), breaks = custom_breaks)
# Prepare the recipe. 
prep_bins <- custom_bins %>% prep(training = df_treated)
# Bake the recipe
baked_bins <- prep_bins %>% bake(new_data = NULL)
# Clean up the newly created data frame prior to merging with the treatment data. 
baked_bins <- baked_bins %>% rename_with(.cols = all_of(entry_var_str), 
                                         .fn = ~paste0(entry_var_str, '_bins'))
# Combine with treated data. 
df_treated <- bind_cols(df_treated, baked_bins)

return(df_treated)

}
