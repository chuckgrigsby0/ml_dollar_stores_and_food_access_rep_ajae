print('Sourced: check_significance <- function(estimate, se, z_score)')

# Calculate z-scores for the given alpha levels
alpha_levels = c(0.10, 0.05, 0.01)
alpha_names = c('ten', 'five', 'one')
z_scores <- qnorm(1 - alpha_levels / 2)

# Function to check significance
check_significance <- function(estimate, se, z_score) {
  lower_ci <- estimate - z_score*se
  upper_ci <- estimate + z_score*se
  significant <- (lower_ci > 0 & upper_ci > 0) | (lower_ci < 0 & upper_ci < 0)
  return(significant)
}




