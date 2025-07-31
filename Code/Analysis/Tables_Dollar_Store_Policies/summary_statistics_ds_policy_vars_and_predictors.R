# Script produces necessary infomration for Tables A.10 and A.11 in supplementary text comparing the mean and standardized mean 
# of block groups with and without dollar store policies. 
# ----------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on CT bootstrap.
# ----------------------------------- #
model_dep_var = 'low_access' # Used in script below.
model_geography = 'Urban' # Used in script below.
print(model_dep_var); print(model_geography)
options(scipen = 999)
# ----------------------------------- #
# Load packages
# ----------------------------------- #
library(pacman)
p_load('here', 'dplyr', 'ggplot2', 'purrr', 'tidyr', 'stringr', 
       'recipes', 'rsample', 'fixest', 'sf', 'tictoc', 'glmnet', 
       'future', 'furrr', 'parallel', 'doParallel', 'effectsize')
# ----------------------------------- #
# Load data. 
# ----------------------------------- #
source(here::here('Code', 'Analysis', 'load_data_for_imputation_estimation.R'))
# ----------------------------------- #

# ----------------------------------- #
# Combine the dollar store entry data of untreated observations, the food access indicators, and all model covariates. 
# ----------------------------------- #
source(here::here('Code', 'Functions', 'Function_Combine_Treated_or_Untreated_Data.R'))
dta_untreated <- Combine_Data_Function(dta = panel_df_ds_entry_2_and_10mile$untreated, 
                                       panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                       retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile, 
                                       national = FALSE, 
                                       geography_str = model_geography)

# Columns not needed for imputation models. 
non_model_vars <- c('DS_Count_10mile_diff', # 'DS_Count_10mile',
                    'entry', 'entry_events', 'event_year', 'net_entry_cumsum', 'rel_year', 'treat', 
                    'Grocery_Count_10mile_diff', 'total_low_access',
                    'STATE', 'market', 'market_name_full', 'Geography')

# Modeling variables (Columns needed for imputation models). 
model_vars <- names(dta_untreated)[!(names(dta_untreated) %in% non_model_vars)]; model_vars
# ----------------------------------- #
# The NA observations are completely missing in covariates or are located in Puerto Rico, and therefore, will be discarded. 
# ----------------------------------- #
nas_ut <- dta_untreated[!complete.cases(dta_untreated), ]  
dta_untreated <- dta_untreated[complete.cases(dta_untreated), ]
# ----------------------------------- #
dta_untreated_non_model_vars <- dta_untreated %>% select(GEOID, year, all_of(non_model_vars))
dta_untreated <- dta_untreated %>% select(all_of(model_vars))
# ----------------------------------- #
# Create the data set of treated observations, as we did for the untreated. 
# ----------------------------------- #
dta_treated <- Combine_Data_Function(dta = panel_df_ds_entry_2_and_10mile$treated, 
                                     panel_df_access_inds_x_and_ymile = panel_df_access_inds_2_and_10mile, 
                                     retail_counts_2005_x_and_ymile = retail_counts_2005_2_and_10mile, 
                                     national = FALSE, 
                                     geography_str = model_geography)
# ----------------------------------- #
# The NA observations are completely missing in covariates, and therefore, will be discarded. 
# ----------------------------------- #
nas_tr <- dta_treated[!complete.cases(dta_treated), ]  
dta_treated <- dta_treated[complete.cases(dta_treated), ]  
# ----------------------------------- #
dta_treated_non_model_vars <- dta_treated %>% select(GEOID, year, all_of(non_model_vars), ends_with('bins'))
dta_treated <- dta_treated %>% select(all_of(model_vars))
# ----------------------------------- #
dta_tr_and_ut <- bind_rows(dta_untreated, dta_treated)
# ----------------------------------- #
rm(list=ls()[!(ls() %in% c('model_vars', 'non_model_vars', 'acs_covars', 'econ_geog_vars',
                           'dta_tr_and_ut', 'dta_untreated', 'dta_treated', 'dta_treated_w_folds',
                           'dta_untreated_non_model_vars', 'dta_treated_non_model_vars', 
                           'model_geography', 'model_dep_var', 'ncores'))])
gc()
# ----------------------------------- #

# ----------------------------------- #
# Load the dollar store bans and restrictions data. 
# ----------------------------------- #
ds_bans <- readr::read_csv(here::here('Data', 'block_groups_w_ds_policies.csv'), show_col_types = FALSE )
# Use to select relevant variables below. 
policy_vars = names(ds_bans) %>% str_subset(string=., pattern='^Def|^Mor|^Ord|^policy_*')
# ----------------------------------- #
# Load the block-group level data with geographic information. 
# ----------------------------------- #
load(here::here("Data", "bg_pop_centroids_2010_projected_w_urban_areas.RData"))
bg_pop_centroids_10_sfp_geo <- bg_pop_centroids_10_sfp_geo %>% select(GEOID, Geography) %>% st_drop_geometry()
# ----------------------------------- #
# Join geographic information to dollar store policy data. 
# ----------------------------------- #
ds_bans <- ds_bans %>% left_join(bg_pop_centroids_10_sfp_geo, by = 'GEOID')
# ----------------------------------- #
ds_bans_modeling <- ds_bans %>% 
  
  filter(grepl(model_geography, Geography)) %>%
  
  mutate(Restrictions = Moratorium + Ordinance) %>% 
  mutate(
    Defeated_and_Restrictions = case_when(Defeated >= 1 & Restrictions == 0 ~ Defeated + Restrictions, 
                                          Defeated >= 1 & Restrictions >= 1 ~ Defeated + Restrictions, 
                                          Defeated == 0 & Restrictions >= 1 ~ 0, 
                                          Defeated == 0 & Restrictions == 0 ~ 0),
    Restrictions_and_Defeated = case_when(Restrictions >= 1 & Defeated == 0 ~ Defeated + Restrictions, 
                                          Restrictions >= 1 & Defeated >= 1 ~ Defeated + Restrictions, 
                                          Restrictions == 0 & Defeated >= 1 ~ 0, 
                                          Restrictions == 0 & Defeated == 0 ~ 0),
    policy_total_binary = if_else(policy_total > 0, 1, 0) ) %>%
  
  relocate(c(Restrictions, contains('_and_'), policy_total_binary), .before = policy_total) %>%
  select(GEOID, zip_code_id, policy_total_binary, Restrictions_and_Defeated, Defeated_and_Restrictions) %>% 
  mutate(across(.cols = c(Restrictions_and_Defeated, Defeated_and_Restrictions), 
                .fns = \(x) if_else(x > 0, 1, 0), 
                .names = '{.col}_binary' ) )

# ----------------------------------- #
sel_ds_modeling_vars <- names(ds_bans_modeling) %>% str_subset(pattern = 'binary')
# ----------------------------------- #
sel_modeling_vars <- names(dta_treated) %>% 
  str_subset(pattern = 'GEOID|zip_code_id|^year$|^low_access|_perm$|_pers$|_x_', negate = TRUE)
# ----------------------------------- #
dta_model <- dta_treated %>% 
  left_join(ds_bans_modeling, by = 'GEOID', multiple = 'all', relationship = 'many-to-one') %>%
  select(zip_code_id, year, all_of(sel_ds_modeling_vars), all_of(sel_modeling_vars))
# ----------------------------------- #
dep_vars <- str_subset(names(dta_model), pattern = '_binary$')
indep_vars <- str_subset(names(dta_model), pattern = '_binary$|fold_id|zip_code_id|year', negate = TRUE)
# ----------------------------------- #

source(here::here('Code', 'Functions', 'Function_tidy_covar_names.R'))

sel_modeling_vars_names <- tidy_covar_names(covar_name_str = sel_modeling_vars)

sel_modeling_vars_table <- data.frame(variable = sel_modeling_vars, variable_clean = sel_modeling_vars_names)

# ----------------------------------- #
standardized_mean_var <- function(dta_arg, var_name, group_var){
  
  mean_grp_1 <- mean(dta_arg[var_name][dta_arg[group_var] == 1 ] )
  
  mean_grp_2 <- mean(dta_arg[var_name][dta_arg[group_var] == 0 ] )
  
  sd_grp_1 <- sd(dta_arg[var_name][dta_arg[group_var] == 1 ] )
  
  sd_grp_2 <- sd(dta_arg[var_name][dta_arg[group_var] == 0 ] )
  
  
  t_test_formula <- xpd(.[var_name] ~ .[group_var], data = dta_arg)
  
  t_test_res <- t.test(t_test_formula, data = dta_arg)
  
  
  var_grp_1 <- var(dta_arg[var_name][dta_arg[group_var] == 1 ] )
  
  var_grp_2 <- var(dta_arg[var_name][dta_arg[group_var] == 0 ] )
  
  if (var_name == 'urban_area'){ 
    pooled_sd <- sqrt( (mean_grp_1*(1-mean_grp_1) + mean_grp_2*(1-mean_grp_2))/2 )  
    } else { 
  pooled_sd <- sqrt( ((var_grp_1 + var_grp_2)/2) )
    }
  
  var_name_std <- paste0(var_name, '_std')
  
  dta_arg <- dta_arg %>% mutate(!!var_name_std := .data[[var_name]]/pooled_sd )
  
  std_mean_grp_1 <- mean(dta_arg[var_name_std][dta_arg[group_var] == 1 ] )

  std_mean_grp_2 <- mean(dta_arg[var_name_std][dta_arg[group_var] == 0 ] )

  diff_means_std <- std_mean_grp_1 - std_mean_grp_2

  reg_formula <- xpd(.[var_name_std] ~ .[group_var], data = dta_arg)
  
  reg_std <- feols(reg_formula, data = dta_arg, cluster ~ zip_code_id)
  
  reg_std <- broom::tidy(summary(reg_std))
  
  reg_std <- reg_std %>% filter(term == group_var)

  cd_res <- cohens_d(reg_formula, data = dta_arg, pooled_sd = FALSE)


  out_table <- data.frame('variable' = var_name, 'policy_var' = group_var,
                          'mean_grp_1' = mean_grp_1, 'mean_grp_2' = mean_grp_2,
                          'sd_grp_1' = sd_grp_1, 'sd_grp_2' = sd_grp_2,
                          't_stat' = t_test_res$statistic, 'p_value' = t_test_res$p.value,
                          'std_mean_grp_1' = std_mean_grp_1, 'std_mean_grp_2' = std_mean_grp_2,
                          'diff_means_std' = diff_means_std, 'cohens_d_stat' = cd_res$Cohens_d, 
                          reg_std
                          )
  
  return(out_table)
  
}
# ----------------------------------- #
print(paste('There are', nrow(filter(dta_model, policy_total_binary == 1)), 'block-group-by-year observations in places with policies.'))
print(paste('There are', nrow(filter(dta_model, policy_total_binary == 0)), 'block-group-by-year observations in places with policies.'))
# ----------------------------------- #
summary_stats <- map_dfr(dep_vars,
                         function(.x){ 
                           
                           map_dfr(sel_modeling_vars, 
                                   function(.y){ 
                                     
                                     standardized_mean_var(dta_arg = dta_model, var_name = .y, group_var = .x)        
                                     
                                     })
                           
                           })

rownames(summary_stats) <- NULL

model_dep_var_dir <- model_dep_var %>% 
  str_replace(pattern = '_', replacement = ' ') %>% 
  str_to_title() %>% 
  str_replace(pattern = ' ', replacement = '_')

model_geography_fname <- str_to_lower(model_geography)

saveRDS(summary_stats, 
        here::here('Analysis', 'Tables', model_dep_var_dir, model_geography, 
                   paste0(model_geography_fname, '_ds_policy_summary_stats_by_covar.rds') ) ) 


summary_stats <- readRDS(here::here('Analysis', 'Tables', model_dep_var_dir, model_geography, 
                                    paste0(model_geography_fname, '_ds_policy_summary_stats_by_covar.rds') ) ) 


summary_stats <- summary_stats %>% select(variable, policy_var, mean_grp_1, mean_grp_2, 
                                 std_mean_grp_1, std_mean_grp_2, 
                                 diff_means_std, std.error, statistic, p.value) %>% # Note that diff_means_std and estimate are identical.
  mutate(significance = case_when(
    p.value <= 0.01 ~ '***',
    p.value <= 0.05 ~ '**',
    p.value <= 0.1 ~ '*',
    TRUE ~ ''
  )) %>% 
  relocate(significance, .after = statistic) %>%
  left_join(sel_modeling_vars_table, by = 'variable', multiple = 'all', relationship = 'many-to-one') %>%
  relocate(variable_clean, .after = variable) %>%
  select(-c(p.value)) %>%
  filter(variable %in% acs_covars | grepl("(DS|Grocery)_Count_\\d+mile(_2005)?$", variable)) %>%
  mutate(abs_diff_means_std = abs(diff_means_std)) %>%
  group_by(policy_var) %>% 
  arrange(desc(significance), desc(abs_diff_means_std), .by_group = TRUE) %>% 
  select(-abs_diff_means_std) %>% 
  mutate(across(.cols = where(is.numeric), 
                .fns = \(x) round(x, digits = 3)))

summary_stats$policy_var <- summary_stats$policy_var %>% 
  str_replace_all(pattern = c('_' = ' ', 'binary' = '')) %>% 
  str_trim(., side = 'right') %>% 
  str_replace_all(pattern = c('Defeated and Restrictions' = 'Defeated/Defeated and Restrictions', 
                              'Restrictions and Defeated' = 'Restrictions/Restrictions and Defeated', 
                              'policy total' = 'Defeat or Restriction'))


colnames(summary_stats) <- colnames(summary_stats) %>% str_replace_all(pattern = c('policy_var' = 'Policy Type', '^mean_grp_1$' = 'Mean (w/ Policy)', '^mean_grp_2$' = 'Mean (w/o Policy)', 
                                            'std_mean_grp_1' = 'SM (w/ Policy)', 'std_mean_grp_2' = 'SM (w/o Policy)', 
                                            'diff_means_std' = 'SDM', 'statistic' = 't-stat.', 'significance' = 'Significance', 
                                            '^variable$' = 'Variable', 'variable_clean' = 'Variable Name'))


readr::write_csv(summary_stats, file = here::here('Analysis', 'Tables', model_dep_var_dir, model_geography, 
                                                  paste0(model_geography_fname, '_ds_policy_summary_stats_by_covar_table.csv') ) ) 
