# -------------------------------------------------- #
pacman::p_load('here', 'dplyr', 'stringr', 'purrr')

model_geography = 'Rural'
# -------------------------------------------------- #

# Load and compute CV Errors from model tuning.
# -------------------------------------------------- #
search_string <- paste0(str_to_lower(model_geography)); search_string

param_str <- list.files(path = here::here('Analysis', 
                                          'Model_Training', 
                                          'Low_Access', 
                                          'RF', 
                                          model_geography), 
                        pattern = search_string)

num_tuning_param <- param_str %>% str_extract_all(pattern = '\\d+(?=\\.rds$)', simplify = TRUE)

training_params_cb <- param_str %>% 
  
  map(function(.x){
    
    training_params_x <- readRDS(here::here('Analysis', 
                                            'Model_Training', 
                                            'Low_Access', 
                                            'RF',
                                            model_geography, 
                                            .x) )
    
    training_params_x <- training_params_x %>% 
    group_by(fold_id) %>% 
      summarise(ce = sum(actual != cv_preds)/n() ) %>%
      ungroup() %>%
      summarise(ce = mean(ce))
    
    return(training_params_x)
  })

training_params_cb <- set_names(training_params_cb, nm = num_tuning_param)
training_params_cb <- bind_rows(training_params_cb, .id = 'param_set')
training_params_cb <- training_params_cb %>% 
  mutate(
    param_set = as.numeric(param_set)
    ) %>% 
  arrange(param_set)

if (model_geography == 'Urban'){ 
  opt_param_set <- training_params_cb %>% arrange(ce) %>% dplyr::slice(1:5) %>% select(param_set)    
} else if (model_geography == 'Rural'){ 
  opt_param_set <- training_params_cb[which.min(training_params_cb$ce), 'param_set']  
  }


# -------------------------------------------------- #
# Load the set of tuning parameters. 
# -------------------------------------------------- #
rf_tuning_params <- readRDS(here::here('Analysis', 
                                       'Model_Training', 
                                       'Low_Access', 
                                       'RF', 
                                       paste0('lhs_hyperparameters_training_', 
                                       str_to_lower(model_geography), '.rds') ) )

# Find the optimal hyperparameter set. 
# -------------------------------------------------- #
if (model_geography == 'Urban'){
  rf_opt_tuning_params <- rf_tuning_params[opt_param_set$param_set, ]
  
  row_index <- as.integer(rownames(rf_opt_tuning_params))
  
  rf_opt_tuning_params <- rf_opt_tuning_params[row_index==15, ]
} else if (model_geography == 'Rural'){ 
  rf_opt_tuning_params <- rf_tuning_params[opt_param_set$param_set, ]
  }

filename = paste0('rf_opt_hyperparameters_lhs_', str_to_lower(model_geography), '.rds' ); filename

saveRDS(rf_opt_tuning_params, 
        file = here::here('Analysis', 'Model_Training', 'Low_Access', 'RF', filename))
# -------------------------------------------------- #