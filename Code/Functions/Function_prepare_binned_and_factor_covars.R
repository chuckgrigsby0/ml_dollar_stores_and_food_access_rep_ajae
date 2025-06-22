print('Sourced: prepare_binned_and_factor_covars <- function(national, geography_str, model_preds_dta)')
# -------------------------------------------------------------------------------------------- #
# Function is run for Urban and Rural pretreatment data to obtain binned and factor formatted covariates
# to assess relationships between CV errors and covariates. Integers/Counts are converted to factors. 
# Numeric data are converted to bins. 
# -------------------------------------------------------------------------------------------- #
prepare_binned_and_factor_covars <- function(national, geography_str, model_preds_dta){

if (isTRUE(national)){ 
  
  pretr_preds <- model_preds_dta %>% 
    filter(rel_year < 0) %>% 
    select(-tau) %>% 
    mutate(err = low_access - preds, 
           rel_year = factor(rel_year))
  
} else { 
  
  pretr_preds <- model_preds_dta %>% 
    
    filter(rel_year < 0) %>% 
    
    filter(grepl(geography_str, Geography)) %>%
    
    select(-tau) %>% 
    
    mutate(err = low_access - preds, 
           rel_year = factor(rel_year))
  
}
# -------------------------------------------------------------------------------------------- #
pretr_preds <- pretr_preds %>%
  select(GEOID, year, event_year, rel_year, err, all_of(model_covars))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Convert counts to integers to separate variables by types (numeric vs integer). 
# -------------------------------------------------------------------------------------------- #
pretr_preds <- pretr_preds %>% mutate(across(.cols = matches('_count_|^urban_area$|^uc_area$', ignore.case = TRUE), 
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
numeric_covars <- numeric_covars[!grepl('_road_', numeric_covars)]; numeric_covars # Separate road vars. See below.  

# Create a separate roads variable string due to data structure of variables (too few categories).
road_covars <- model_covars[grepl('_road_', model_covars)]; road_covars
# -------------------------------------------------------------------------------------------- #
# Numeric variable breaks. 
custom_breaks_num_vars = pretr_preds %>%
  
  reframe(across(.cols = all_of(numeric_covars), 
                   
                   .fns = ~quantile(., probs = seq(0, 1, 0.05))))

# Road numeric variable breaks. 
custom_breaks_road_vars = pretr_preds %>%
  
  reframe(across(.cols = all_of(road_covars), 
                   
                   .fns = ~quantile(., probs = seq(0, 1, 0.05))))

potential_bins = nrow(custom_breaks_road_vars)

# The distribution is too skewed for specific road variables, so recreate bins for any 
# roads covariate with at least 60% of percentiles equal to zero. 
percent_breaks_eq_zero <- custom_breaks_road_vars %>% 
  summarise(across(.cols = where(is.numeric), ~sum(.x == 0) ) ) %>% 
  mutate(across(.cols = where(is.numeric), ~.x/potential_bins)) 

select_vars <- which(as.numeric(percent_breaks_eq_zero[1, ]) >= 0.60); select_vars

road_covars_num <- road_covars[-select_vars]; road_covars_num
road_covars_sel <- road_covars[select_vars]; road_covars_sel

# Select only the roads variables that are below the 60% threshold. 
custom_breaks_road_vars <- custom_breaks_road_vars %>% select(all_of(road_covars_num))
# -------------------------------------------------------------------------------------------- #
# Recreate the bins for the special-case covariates. 
custom_breaks_road_vars_sel = pretr_preds %>%
  
  reframe(across(.cols = all_of(road_covars_sel), 
                   
                   .fns = ~quantile(., probs = seq(0, 1, 0.001))))
# -------------------------------------------------------------------------------------------- #
# Function to create binned covariates one-by-one. 
# -------------------------------------------------------------------------------------------- #
binned_covars <- function(dta, custom_breaks_vartype, covariates, id){
  
  cut_points <- unique(unlist(custom_breaks_vartype[ , id]))
  
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

binned_covars_num <- seq_along(numeric_covars) %>% 
  
  map(function(.x){ 
    
    binned_covars(dta = pretr_preds, 
                  custom_breaks_vartype = custom_breaks_num_vars, 
                  covariates = numeric_covars,
                  id = .x)
    
  })
# -------------------------------------------------------------------------------------------- #
# Creates Lists of data frames each of which contains the pretr_key_vars and a covariate specified by id.  
# -------------------------------------------------------------------------------------------- #
binned_covars_roads <- seq_along(road_covars_num) %>% 
  
  map(function(.x){ 
    
    binned_covars(dta = pretr_preds, 
                  custom_breaks_vartype = custom_breaks_road_vars, 
                  covariates = road_covars_num, 
                  id = .x)
    
  })
# -------------------------------------------------------------------------------------------- #
# Creates Lists of data frames each of which contains the pretr_key_vars and a covariate specified by id.  
# -------------------------------------------------------------------------------------------- #
binned_covars_roads_sel <- seq_along(road_covars_sel) %>% 
  
  map(function(.x){ 
    
    binned_covars(dta = pretr_preds, 
                  custom_breaks_vartype = custom_breaks_road_vars_sel, 
                  covariates = road_covars_sel, 
                  id = .x)
    
  })
# -------------------------------------------------------------------------------------------- #
# Integer variables. 
# -------------------------------------------------------------------------------------------- #
pretr_preds_integers <- pretr_preds %>% select(all_of(pretr_key_vars), all_of(integer_vars)) 

big_box_stores <- c("Wholesale_Club_Count_10mile_2005", "Gen_Merch_Count_10mile_2005", "Mass_Merch_Count_10mile_2005")
urban_area_vars <- c('urban_area', 'uc_area')

pretr_preds_integers_simple <- pretr_preds_integers %>%
   mutate(across(.cols = c(all_of(big_box_stores), all_of(urban_area_vars)), 
                 .fns = ~factor(., levels = sort(unique(.))))) 
# -------------------------------------------------------------------------------------------- #
# For the remaining variables, we group the values greater than 
# the value in the 90th percentile for that variable into a single factor. 
# -------------------------------------------------------------------------------------------- #
custom_breaks_int <- pretr_preds_integers %>% 
  reframe(across(.cols = where(is.integer), 
                 .fns = ~quantile(., probs = seq(0, 1, 0.10)))) %>%
  select(-err, -all_of(big_box_stores), -all_of(urban_area_vars))

custom_breaks_int <- custom_breaks_int[10, ] # 90th percentile. 
# -------------------------------------------------------------------------------------------- #
# Function to dynamically specify which values should belong to the final bin and create the factor vars.
# -------------------------------------------------------------------------------------------- #
levels_int_var <- function(int_dta, int_var){
  
  threshold_indicator <- as.numeric(custom_breaks_int[[int_var]])
  
  int_dta <- int_dta %>%
    mutate(threshold = as.numeric(custom_breaks_int[[int_var]]), 
           {{int_var}} := case_when(.data[[int_var]] >= threshold ~ threshold,
                                        TRUE ~ .data[[int_var]])) %>% 
    select(all_of(pretr_key_vars), all_of(int_var))
  
  levels_int_var <- as.character(sort(unique(int_dta[[int_var]])))
  
  int_dta <- int_dta %>%
    mutate(across(.cols = all_of(int_var), 
                  .fn = ~as.character(.))) %>%
    mutate(across(.cols = all_of(int_var), 
                  .fn = ~factor(., levels = levels_int_var, ordered = FALSE) )) 
  
  
  last_row <- length(levels(int_dta[[int_var]]))
  levels(int_dta[[int_var]])[last_row] <- paste0('\u2265 ', levels(int_dta[[int_var]])[last_row])
  
  return(int_dta)
}
# -------------------------------------------------------------------------------------------- #
pretr_preds_integers_sel <- names(custom_breaks_int) %>%
  
  map(function(.x){ 
    
    int_dta_wlevels <- levels_int_var(int_dta = pretr_preds_integers, 
                                      int_var = .x)    
    
  })
# Combine into single data frame. 
pretr_preds_integers_sel <- pretr_preds_integers_sel %>% reduce(left_join, by = pretr_key_vars)
# -------------------------------------------------------------------------------------------- #
pretr_preds_integers_comb <- pretr_preds_integers_sel %>%
  
  left_join(select(pretr_preds_integers_simple, 
                   all_of(pretr_key_vars), 
                   all_of(urban_area_vars), 
                   all_of(big_box_stores) ), 
                   by = pretr_key_vars)

# -------------------------------------------------------------------------------------------- #
pretr_binned_covars <- c(binned_covars_num, 
                         binned_covars_roads, 
                         binned_covars_roads_sel, 
                         list(pretr_preds_integers_comb)) %>% # Combine lists of data.frames using left_join and join variables. 
  
  reduce(.f = left_join, by = c('GEOID', 'year', 'event_year', 'rel_year', 'err'))
  
return(pretr_binned_covars)

}
