# Create K-Fold IDs for the Never-Treated observations. 
# Must convert GEOID to a factor to use with group_vfold_cv
print('Loaded: CV_Function')

CV_Function <- function(untreated_dta, cv_type, k){

  if (cv_type == 'horizontal rolling-origin-block-cv'){ # Horizontal CV method involving forecasting and back-casting. 
    
    untreated_dta_wo2005 <- untreated_dta %>% filter(year != '2005')
    
    obs_by_year <- untreated_dta_wo2005 %>% 
      group_by(year) %>%
      count() %>%
      ungroup() %>%
      mutate(total = sum(n), 
             share = n/total, 
             cumsum = cumsum(share))
    
    
    rec <- recipe(obs_by_year)
    
    prep <- rec %>% step_cut(cumsum, breaks = seq(min(obs_by_year$cumsum), max(obs_by_year$cumsum), length.out = k)) %>% prep()
    
    baked <- bake(prep, new_data = obs_by_year)
    
    baked$fold_id <- as.character( as.numeric(baked$cumsum) )
    
    time_ids <- baked %>% select(year, fold_id)
    
    year_2005 <- data.frame(year = '2005', fold_id = '0')
    
    time_ids <- bind_rows(year_2005, time_ids)
    
    untreated_dta_wfolds <- untreated_dta %>% left_join(time_ids, by = 'year')
    
    untreated_dta_wfolds <- untreated_dta_wfolds %>% relocate(fold_id)
    
  }
  
  if (cv_type == 'horizontal equal-sized-block-cv'){ # Horizontal CV method involving forecasting and back-casting. 
    
    untreated_dta_wo2005_2006 <- untreated_dta %>% filter(year != '2005' & year != '2006')
    
    time_ids <- data.frame(year = unique(untreated_dta_wo2005_2006$year), 
                           fold_id = as.character(c(rep(1, 2), rep(2, 2), rep(3, 2), 
                                                    rep(4, 2), rep(5, 2), rep(6, 2), rep(7, 2))))
    
    year_2005_2006 <- data.frame(year = c('2005', '2006'), fold_id = c('0', '0'))
    
    time_ids <- bind_rows(year_2005_2006, time_ids)
    
    untreated_dta_wfolds <- untreated_dta %>% left_join(time_ids, by = 'year')
    
    untreated_dta_wfolds <- untreated_dta_wfolds %>% relocate(fold_id)
    
  }
  
else if (cv_type == 'vertical') { # Samples at the GEOID/unit level. 
  # -------------------------------------------------------------------------------------------- #
  # Using the untreated observations, stratify the sample by GEOID and create five folds based on the grouping variable. 
  # -------------------------------------------------------------------------------------------- #
  n_folds = k # Parameter specified in the function when cv_type == 'vertical'
  set.seed(243444)
  geoid_folds <- group_vfold_cv(untreated_dta, 
                                group = GEOID, 
                                v = n_folds, 
                                balance = 'groups')

  holdout_ids <- list(NULL)
  for(i in 1:n_folds){
    print(i)
    holdout_dta <- data.frame()
    holdout_dta <- assessment(geoid_folds$splits[[i]]) %>%
      add_resample_id(geoid_folds$splits[[i]]) 
    
    holdout_dta <- holdout_dta %>%
      select(GEOID, id) %>%
      mutate(fold_id = as.numeric(str_remove(id, 'Resample'))) %>%
      select(-id)
    
    holdout_ids[[i]] <- holdout_dta
    
  }
  
  holdout_ids <- bind_rows(holdout_ids)
  holdout_ids <- holdout_ids %>% 
    group_by(GEOID) %>%
    filter(row_number() == 1L)
  
  untreated_dta_wfolds <- untreated_dta %>% 
    left_join(holdout_ids, by = 'GEOID') %>% 
    relocate(fold_id)
  
  # Check the share of observations in the assessment data frames. 
  for (i in 1:n_folds){
    anal <- analysis(geoid_folds$splits[[i]])
    asses <- assessment(geoid_folds$splits[[i]])
    print(anal %>% inner_join(asses, by = c('GEOID')))
    print(nrow(asses)/nrow(anal))
    print(nrow(anal)/nrow(untreated_dta_wfolds))
  }

}

return(untreated_dta_wfolds)

}
