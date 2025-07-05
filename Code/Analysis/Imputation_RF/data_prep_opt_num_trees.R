# Script that finds optimal ntrees out of all tuned models. 
# -------------------------------------------------- #
pacman::p_load('here', 'dplyr', 'stringr', 'purrr')

model_geography = 'Urban' # Change according to model geography. 
# -------------------------------------------------- #
# Load and compute CV Errors from model tuning.
# -------------------------------------------------- #
search_string <- paste0(str_to_lower(model_geography), '.*_trees'); search_string

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
    param_set = as.numeric(param_set), 
    ce = round(ce, digits = 5)
  ) %>% 
  arrange(param_set)

opt_param_set <- training_params_cb[which.min(training_params_cb$ce), 'param_set']  

# -------------------------------------------------- #
#Number of trees = Number of iterations of tree building. 
# -------------------------------------------------- #
ntrees <- seq(200, 450, 50); ntrees

ntrees <- ntrees[opt_param_set$param_set]
# -------------------------------------------------- #

ntrees <- data.frame(num.trees = ntrees)
# -------------------------------------------------- #
# Find the optimal hyperparameter set. 
# -------------------------------------------------- #
filename = paste0('rf_opt_num_trees_lhs_', str_to_lower(model_geography), '.rds' ); filename

saveRDS(ntrees, 
        file = here::here('Analysis', 'Model_Training', 'Low_Access', 'RF', filename))
# -------------------------------------------------- #