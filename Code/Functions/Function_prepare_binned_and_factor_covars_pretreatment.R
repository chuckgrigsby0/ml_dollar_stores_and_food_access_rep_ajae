print('Sourced: prepare_binned_and_factor_covars_pre <- function(national, geography_str, model_preds_dta)')
# -------------------------------------------------------------------------------------------- #
# Function is run for Urban and Rural pretreatment data to obtain binned and factor formatted covariates
# to assess relationships between CV errors and covariates. Integers/Counts are converted to factors. 
# Numeric data are converted to bins. 
# -------------------------------------------------------------------------------------------- #
prepare_binned_and_factor_covars_pre <- function(national, geography_str, model_preds_dta){

if (isTRUE(national)){ 
  
  pretr_preds <- model_preds_dta %>% 
    filter(rel_year < 0) %>% 
    select(-tau) %>% 
    mutate(err = actual - preds, 
           rel_year = factor(rel_year))
  
} else { 
  
  pretr_preds <- model_preds_dta %>% 
    
    filter(rel_year < 0) %>% 
    
    filter(grepl(geography_str, Geography)) %>%
    
    select(-tau) %>% 
    
    mutate(err = actual - preds, 
           rel_year = factor(rel_year))
  
}
# -------------------------------------------------------------------------------------------- #
pretr_preds <- pretr_preds %>%
  select(GEOID, year, event_year, rel_year, err, all_of(model_covars))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Convert counts to integers to separate variables by types (numeric vs integer). 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- pretr_preds %>% mutate(across(.cols = matches('^Wholesale_Club|^Gen_Merch|^Mass_Merch|^urban_area$|^uc_area$', 
                                                             ignore.case = TRUE), 
                                             .fns = ~as.integer(.))) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Separate key, numeric and integer covariates. 
# -------------------------------------------------------------------------------------------- #
pretr_key_vars <- setdiff(names(pretr_preds), model_covars); pretr_key_vars

integer_vars <- names(select(pretr_preds, where(is.integer)))
integer_vars <- integer_vars[!grepl('err', integer_vars)]; integer_vars # Remove integer var. "err"

numeric_covars <- model_covars[!(model_covars %in% integer_vars)]

# Round the numeric variables so that the discretized bins do not contain too many decimal places. 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- pretr_preds %>% 
  mutate(across(.cols = all_of(numeric_covars), 
                .fn = ~round(., digits = 3)))
# -------------------------------------------------------------------------------------------- #
# Create character vector for continuous covariates. 
continuous_covars <- numeric_covars[numeric_covars %in% c(model_covars_list_disag$socioeconomics, 
                                                          model_covars_list_disag$distance_to_urban_areas, 
                                                          model_covars_list_disag$land_use, 
                                                          model_covars_list_disag$park_access, 
                                                          model_covars_list_disag$roads, 
                                                          model_covars_list_disag$fixed_effects)]

# Create character vector for non-integer, discrete-value covariates. 
# Essentially, retail and schools variables. 
discrete_covars <- numeric_covars[!(numeric_covars %in% continuous_covars)]
# -------------------------------------------------------------------------------------------- #
# Continuous variable breaks. 
custom_breaks_contin_vars = pretr_preds %>%
  
  reframe(across(.cols = all_of(continuous_covars), 
                   
                   .fns = ~quantile(., probs = seq(0, 1, 0.10))))

# Discrete variable breaks. 
custom_breaks_disc_vars = pretr_preds %>%
  
  reframe(across(.cols = all_of(discrete_covars), 
                 
                 .fns = ~quantile(., probs = seq(0, 1, 0.10))))
# -------------------------------------------------------------------------------------------- #
# Function to create binned covariates one-by-one. 
# -------------------------------------------------------------------------------------------- #
binned_covars <- function(dta, continuous_vartype, custom_breaks_vartype, covariates, id){
  
  if (isTRUE(continuous_vartype)){ 
  
    cut_points <- unique(unlist(custom_breaks_vartype[ , id]))  
    
  } else { # For the discrete_vars, include a -1 in the first break so that 0 has a unique break. 
    
    cut_points <- unique(unlist(custom_breaks_vartype[ , id]))  
    
    cut_points <- c(-1, cut_points)
    
    }
  # -------------------------------------------------------------------------------------------- #
  # Create a recipe. 
  # -------------------------------------------------------------------------------------------- #
  rec <- recipe(err ~ ., data = dta)
  
  custom_bins <- rec %>% step_cut(all_of(covariates[id]), breaks = cut_points)
  
  # Prepare the recipe. 
  prep_bins <- custom_bins %>% prep(training = dta)
  
  # Bake the recipe
  baked_bins <- prep_bins %>% bake(new_data = NULL, all_of(pretr_key_vars), all_of(covariates[id]))
  
  # Clean up the newly created data frame prior to merging with the treatment data. 
  pretr_preds_binned <- baked_bins %>% rename_with(.cols = all_of(covariates[id]), 
                                                   .fn = ~paste0(covariates[id], '_bins'))
  
  return(pretr_preds_binned)
  
}

# -------------------------------------------------------------------------------------------- #
# Creates Lists of data frames each of which contains the pretr_key_vars and a covariate specified by id.  
# -------------------------------------------------------------------------------------------- #

binned_covars_contin <- seq_along(continuous_covars) %>% 
  
  map(function(.x){ 
    
    binned_covars(dta = pretr_preds, 
                  continuous_vartype = TRUE,
                  custom_breaks_vartype = custom_breaks_contin_vars, 
                  covariates = continuous_covars,
                  id = .x)
    
  })
# -------------------------------------------------------------------------------------------- #
# Creates Lists of data frames each of which contains the pretr_key_vars and a covariate specified by id.  
# -------------------------------------------------------------------------------------------- #
binned_covars_discrete <- seq_along(discrete_covars) %>% 
  
  map(function(.x){ 
    
    binned_covars(dta = pretr_preds, 
                  continuous_vartype = FALSE,
                  custom_breaks_vartype = custom_breaks_disc_vars, 
                  covariates = discrete_covars, 
                  id = .x)
    
  })
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Integer variables. 
# -------------------------------------------------------------------------------------------- #
pretr_preds_integers <- pretr_preds %>% select(all_of(pretr_key_vars), 
                                               all_of(integer_vars)) 

big_box_stores <- c("Wholesale_Club_Count_10mile_2005", "Gen_Merch_Count_10mile_2005", "Mass_Merch_Count_10mile_2005")
if (geography_str == 'Urban'){ 
  urban_area_vars <- c('urban_area')  
} else { 
  urban_area_vars <- NULL  
  }
pretr_preds_integers_simple <- pretr_preds_integers %>%
  mutate(across(.cols = c(all_of(big_box_stores), any_of(urban_area_vars)), 
                .fns = ~factor(., levels = sort(unique(.) ) ) ) ) 
# -------------------------------------------------------------------------------------------- #
# For the remaining variables, we group the values greater than 
# the value in the 90th percentile for that variable into a single factor. 
# -------------------------------------------------------------------------------------------- #
# custom_breaks_int <- pretr_preds_integers %>% 
#   reframe(across(.cols = where(is.integer), 
#                .fns = ~quantile(., probs = seq(0, 1, 0.10)))) %>%
# select(-err, -all_of(big_box_stores), -all_of(urban_area_vars))
# 
# custom_breaks_int <- custom_breaks_int[10, ] # 90th percentile. 
# -------------------------------------------------------------------------------------------- #
# Function to dynamically specify which values should belong to the final bin and create the factor vars.
# -------------------------------------------------------------------------------------------- #
# levels_int_var <- function(int_dta, int_var){
  
#   threshold_indicator <- as.numeric(custom_breaks_int[[int_var]])
  
# int_dta <- int_dta %>%
#   mutate(threshold = as.numeric(custom_breaks_int[[int_var]]), 
#          {{int_var}} := case_when(.data[[int_var]] >= threshold ~ threshold,
#                                       TRUE ~ .data[[int_var]])) %>% 
#   select(all_of(pretr_key_vars), all_of(int_var))
  
# levels_int_var <- as.character(sort(unique(int_dta[[int_var]])))
  
# int_dta <- int_dta %>%
#   mutate(across(.cols = all_of(int_var), 
#                 .fn = ~as.character(.))) %>%
#   mutate(across(.cols = all_of(int_var), 
#                 .fn = ~factor(., levels = levels_int_var, ordered = FALSE) )) 
  
  
# last_row <- length(levels(int_dta[[int_var]]))
# levels(int_dta[[int_var]])[last_row] <- paste0('\u2265 ', levels(int_dta[[int_var]])[last_row])
  
# return(int_dta)
# }
# -------------------------------------------------------------------------------------------- #
# pretr_preds_integers_sel <- names(custom_breaks_int) %>%
# 
# map(function(.x){ 
#   
#   int_dta_wlevels <- levels_int_var(int_dta = pretr_preds_integers, 
#                                     int_var = .x)    
    
# })
# Combine into single data frame. 
# pretr_preds_integers_sel <- pretr_preds_integers_sel %>% reduce(left_join, by = pretr_key_vars)
# -------------------------------------------------------------------------------------------- #
# pretr_preds_integers_comb <- pretr_preds_integers_sel %>%
# 
# left_join(select(pretr_preds_integers_simple, 
#                  all_of(pretr_key_vars), 
#                  all_of(urban_area_vars), 
#                  all_of(big_box_stores) ), 
#                  by = pretr_key_vars)
# 

# -------------------------------------------------------------------------------------------- #
pretr_binned_covars <- c(binned_covars_contin, 
                         binned_covars_discrete, 
                         list(pretr_preds_integers_simple)) %>%
                         # list(pretr_preds_integers_comb)) %>% # Combine lists of data.frames using left_join and join variables. 
  
  reduce(.f = left_join, by = c('GEOID', 'year', 'event_year', 'rel_year', 'err'))
  
return(pretr_binned_covars)

}
