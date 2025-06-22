# -------------------------------------------------------------------------------------------- #

# Function to obtain the upper and lower bound for the confidence intervals for bootstrapped estimates
# and use the values to specify the scale for the y-axis in the plot. 

print('Sourced: bootstrap_max_and_min_plot_coords <- function(regression_table)')
# -------------------------------------------------------------------------------------------- #
bootstrap_max_and_min_plot_coords <- function(regression_table){
  
  y_coord_upper <- max(regression_table$estimate + 1.96*regression_table$bootstrap_sd)
  y_coord_lower <- min(regression_table$estimate - 1.96*regression_table$bootstrap_sd)
  
  coordinate_list <- list('y_coord_lower' = y_coord_lower, 
                          'y_coord_upper' = y_coord_upper)
  
  return(coordinate_list)
  
}
# -------------------------------------------------------------------------------------------- #