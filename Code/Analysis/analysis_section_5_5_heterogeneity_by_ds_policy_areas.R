# -------------------------------------------------------------------------------------------- #
.libPaths()
print(model_geography); print(model_dep_var)
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
# Specify bootstrap type. 
# -------------------------------------------------------------------------------------------- #  
bootstrap_by_tracts = '_tracts' # or NULL to bootstrap by block-group and stratify by relative time. 
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters above. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
# Using the bootstrap data as the primary data source, join treatment timing information to each observation. 
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year'), 
            multiple = 'all', relationship = 'one-to-one') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year'), 
            multiple = 'all', relationship = 'one-to-one') %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, market) %>%
  
  rename(preds = pred_class_cf) %>%
  
  filter(year >= '2006') # We obtain post-treatment predictions from 2006-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------- #
# Load the binned dollar store bans and restrictions data. 
# -------------------------------------------------------------------------------------------- #
ds_bans_binned <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 
                                     paste0('posttreatment_binned_ds_policy_vars_', 
                                            str_to_lower(model_geography), '.rds') ) )

posttre_effects <- model_preds %>% 
  
  filter(rel_year >= 0) %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  mutate(rel_year = factor(rel_year) )

# Define join variables. 
posttr_key_vars = c('GEOID', 'year', 'event_year', 'rel_year')
# -------------------------------------------------------------------------------------------- #
# Combine with join columns.  
# -------------------------------------------------------------------------------------------- #
posttre_effects_w_policy_vars_binned <- posttre_effects %>% 
  
  left_join(ds_bans_binned, by = posttr_key_vars, multiple = 'all', relationship = 'many-to-one')
# -------------------------------------------------------------------------------------------- #
# Create binary no-policy and restriction/restriction+defeated and defeated/defeated+restriction variables. 
posttre_effects_w_policy_vars_binned <- posttre_effects_w_policy_vars_binned %>%
  mutate(No_Policy_binary = if_else(policy_total == 0, 1, 0)) %>% 
  mutate(across(.cols = c(Restrictions_and_Defeated, Defeated_and_Restrictions), 
                .fns = \(x) if_else(x > 0, 1, 0), 
                .names = '{.col}_binary' ) )
# -------------------------------------------------------------------------------------------- #
policy_vars <- c('Defeated_and_Restrictions_binary', 'Restrictions_and_Defeated_binary')
policy_vars_strings <- c('Defeated', 'Restrictions')
# -------------------------------------------------------------------------------------------- #
sum_stats_by_policy <- function(dta, ds_policy_arg, ds_policy_str){
  
  
  sum_stats_by_policy <- dta %>% 
    
    filter(No_Policy_binary == 1 | .data[[ds_policy_arg]] == 1) %>%
    
    group_by(No_Policy_binary) %>%
    
    summarise(across(.cols = c(actual, preds, tau), 
                     .fns = list('avg' = mean), 
                     .names = '{.col}_{.fn}')) %>%
    
    mutate(pct_att = 100*(tau_avg/preds_avg)) %>%
    
    pivot_longer(cols = c(actual_avg:pct_att), 
                 names_to = 'stat', 
                 values_to = 'values') %>% 
    
    mutate(across(.cols = No_Policy_binary, 
                  .fns = \(x) if_else(x == 1, 'No Policy', ds_policy_str)))
  
  
  share_by_policy <-  dta %>% 
    
    filter(No_Policy_binary == 1 | .data[[ds_policy_arg]] == 1) %>%
    
    group_by(No_Policy_binary) %>%
    
    count(name = 'total_per_group') %>% 
    
    ungroup() %>% 
    
    mutate(total_obs = sum(total_per_group), 
           share_per_group = total_per_group/total_obs) %>%
    
    pivot_longer(cols = c(total_per_group:share_per_group), 
                 names_to = 'stat', 
                 values_to = 'values') %>% 
    mutate(across(.cols = No_Policy_binary, 
                  .fns = \(x) if_else(x == 1, 'No Policy', ds_policy_str))) 
  
  
  sum_stats_by_policy <- bind_rows(sum_stats_by_policy, 
                                   share_by_policy) %>% 
    arrange(No_Policy_binary) %>%
    filter(stat != 'total_per_group' & stat != 'total_obs')
  

  return(sum_stats_by_policy)
  
}


sum_stats_by_policy_df <- map2_dfr(policy_vars, policy_vars_strings, 
                                   function(.x, .y){ 
                                     
                                     sum_stats_by_policy(dta = posttre_effects_w_policy_vars_binned, 
                                                         ds_policy_arg = .x, 
                                                         ds_policy_str = .y)
                                     
                                     }) %>%
  group_by(No_Policy_binary, stat) %>%
  filter(row_number() == 1) %>% 
  mutate(No_Policy_binary = factor(No_Policy_binary, 
                                   levels = c('Defeated', 'Restrictions', 'No Policy'))) %>%
  arrange(No_Policy_binary)

colnames(sum_stats_by_policy_df)[1] <- 'Policy'


sum_stats_by_policy_df <- sum_stats_by_policy_df %>%
  pivot_wider(names_from = 'Policy', 
              values_from = 'values') 

sum_stats_by_policy_df$stat <- sum_stats_by_policy_df$stat %>% 
  str_replace_all(c('actual_avg' = 'Low Access (Actual)', 
                    'preds_avg' = 'Low Access (Pred.)', 
                    'tau_avg' = 'ATT',
                    'pct_att' = '% Change ATT',
                    'share_per_group' = 'Share of Obs.') )

sum_stats_by_policy_df <- sum_stats_by_policy_df %>% 
  mutate(across(.cols = where(is.numeric), 
                .fns = \(x) round(x, digits = 4)))


# Add in the bootstrapped SEs manually
# Note that you ran the code from analysis_plot_effects_and_ds_policy_vars.R
boot_sd <- boot_effects_on_ds_policies %>% filter(reg_type == 'policy_type') %>%
  select(variable, estimate, bootstrap_sd) %>% 
  pivot_longer(cols = c(estimate, bootstrap_sd), 
               names_to = 'stat', 
               values_to = 'values') %>%
  mutate(variable = case_when(variable == 'No Defeat or Restriction' ~ 'No Policy', 
                              TRUE ~ variable)) %>% 
  filter(stat == 'bootstrap_sd') %>% 
  pivot_wider(names_from = 'variable', 
              values_from = 'values')

boot_sd <- boot_sd %>%
  mutate(across(.cols = where(is.numeric), 
                .fns = \(x) round(x, digits = 4)))
boot_sd$stat <- 'ATT (SD)'

sum_stats_by_policy_df <- bind_rows(sum_stats_by_policy_df, boot_sd) 


library(knitr)
library(kableExtra)

# Generate the LaTeX table with detailed notes
latex_table <- kable(sum_stats_by_policy_df, "latex", booktabs = TRUE, 
                     caption = "Average Treatment Effects by Dollar Store Policy Areas") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  footnote(general = c("\"Low Access (Actual)\" and \"Low Access (Pred.)\" indicate the actual and predicted shares of low access block groups among those affected by dollar store entry for block groups in jurisdictions with and without dollar store defeat and restriction policies.",
                       "\"Share of Obs.\" denotes the proportion of the total dataset that each group's observations constitute."),
           general_title = "", threeparttable = TRUE, footnote_as_chunk = TRUE)

# Print the LaTeX code
print(latex_table)
