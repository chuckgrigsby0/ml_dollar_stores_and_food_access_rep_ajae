print('Sourced: state_by_time_interactions <- function(dta)')

state_by_time_interactions <- function(dta){
# Interactions b/w state and year dummies. 
# -------------------------------------------------------------------------------------------- #
# Create the recipe with steps. 
interactions_recipe <- dta %>% 
  
  recipe(~ . , data = .) %>%
  
  step_rm(state_tot_road_length_mi) %>%
  
  step_interact(terms = ~starts_with('year_'):starts_with('STATE_'))
# Prepare the recipe and bake. 
interactions_baked <- interactions_recipe %>%  
  
  prep(training = NULL) %>% 
  
  bake(., GEOID, year, contains('_x_'), new_data = NULL) 
# -------------------------------------------------------------------------------------------- #
return(interactions_baked)
}
