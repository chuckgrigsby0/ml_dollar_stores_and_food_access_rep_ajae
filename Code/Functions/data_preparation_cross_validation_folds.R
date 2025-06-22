# Create K-Fold IDs for the Never-Treated observations. 
# Must convert GEOID to a factor to use with group_vfold_cv
print('Loaded: CV_Function')
CV_Function <- function(df_nevertreated, df_ytbt, k, cv_type, start_year = NULL){
geoid_levels_nt <- unique(df_nevertreated$GEOID)
df_nevertreated$GEOID_fact <- factor(df_nevertreated$GEOID, levels = geoid_levels_nt)
# -------------------------------------------------------------------------------------------- #
set.seed(865)
n_folds <- k # e.g., 5 or 10. 
folds_nt <- group_vfold_cv(df_nevertreated, group = GEOID_fact, balance = 'observations', v = n_folds)
holdout_ids <- list(NULL)
for(i in 1:n_folds){
  print(i)
  holdout_dta <- data.frame()
  holdout_dta <- assessment(folds_nt$splits[[i]]) %>%
    add_resample_id(folds_nt$splits[[i]]) 
  
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

df_nevertreated_wfolds <- df_nevertreated %>%
  left_join(holdout_ids, by = 'GEOID')

# Check the share of observations in the assessment data frames. 
for (i in 1:n_folds){
  anal <- analysis(folds_nt$splits[[i]])
  asses <- assessment(folds_nt$splits[[i]])
  print(anal %>% inner_join(asses, by = 'GEOID'))
  print(nrow(asses)/nrow(anal))
}
# -------------------------------------------------------------------------------------------- #


if (cv_type == 'block cv' & start_year == '2000'){
  custom_fold_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) %>% 
    map_df(function(.x) data.frame('fold_id' = rep(.x, 2)))
  
  time_ids <- data.frame('year' = sort(unique(df_ytbt$year)), 
                         'fold_id' = custom_fold_ids)
  
  df_ytbt_wfolds <- df_ytbt %>% left_join(time_ids, by = 'year')
  
} 


if (cv_type == 'block cv' & start_year == '2005'){
  custom_fold_ids <- c(1, 2, 3, 4, 5) %>% 
    map_df(function(.x) data.frame('fold_id' = rep(.x, 3)))
  
  time_ids <- data.frame('year' = sort(unique(df_ytbt$year)), 
                         'fold_id' = custom_fold_ids)
  
  df_ytbt_wfolds <- df_ytbt %>% left_join(time_ids, by = 'year')
  
} 

else if (cv_type == 'vertical') {
  # -------------------------------------------------------------------------------------------- #
  # Using the YTBT observations, stratify the sample by GEOID and create five folds based on the grouping variable. 
  # -------------------------------------------------------------------------------------------- #
  geoid_levels_ytbt <- unique(df_ytbt$GEOID)
  df_ytbt$GEOID_fact <- factor(df_ytbt$GEOID, levels = geoid_levels_ytbt)
  # -------------------------------------------------------------------------------------------- #
  set.seed(352)
  folds_ytbt <- group_vfold_cv(df_ytbt, group = 'GEOID_fact', v = n_folds, balance = 'observations')
  holdout_ids <- list(NULL)
  for(i in 1:n_folds){
    print(i)
    holdout_dta <- data.frame()
    holdout_dta <- assessment(folds_ytbt$splits[[i]]) %>%
      add_resample_id(folds_ytbt$splits[[i]]) 
    
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
  
  df_ytbt_wfolds <- df_ytbt %>% 
    left_join(holdout_ids, by = 'GEOID')
  
  # Check the share of observations in the assessment data frames. 
  for (i in 1:n_folds){
    anal <- analysis(folds_ytbt$splits[[i]])
    asses <- assessment(folds_ytbt$splits[[i]])
    print(anal %>% inner_join(asses, by = c('GEOID')))
    print(nrow(asses)/nrow(anal))
  }
  
  
}

# -------------------------------------------------------------------------------------------- #
# Combine the rows of never-treated w/ folds and the YTBT observations. 
# -------------------------------------------------------------------------------------------- #
panel_untreated_wfolds <- bind_rows(df_nevertreated_wfolds, df_ytbt_wfolds)
# -------------------------------------------------------------------------------------------- #
return(panel_untreated_wfolds)

}
