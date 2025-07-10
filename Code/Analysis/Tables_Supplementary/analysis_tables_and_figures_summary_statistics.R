# Supplementary tables and figures, specifically includes Tables A.8 and A.9 showing summary statistics of 
# block group characteristics across treated and untreated areas. 
# ------------------------------------------------- #
# Load and prepare data. 
# ------------------------------------------------- #
library('pacman')
library('here') 
library('readr') 
model_dep_var = 'low_access'
model_geography = 'Rural' # Used in script below to subset by either Urban or Rural.
bootstrap_by_tracts <- '_tracts' 
options(scipen = 999)
# ------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# ------------------------------------------------- #
# Post-treatment bins (quartiles) 
# ------------------------------------------------- #
fname_posttr_binned_covars = paste0('posttreatment_binned_quartile_covars_', str_to_lower(model_geography), '.rds')

posttr_binned_covars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_covars))

# Remove tau calculated from the original/empirical data 

posttr_binned_covars <- posttr_binned_covars %>% select(-tau)

# ------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# ------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 

posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)

# ------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store bins. 
# ------------------------------------------------- #
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

posttr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_grocery))
# ------------------------------------------------- #

source(here::here('Code', 'Functions', 'Function_plots_for_low_access_by_time.R'))

# ------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# ------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# ------------------------------------------------- #
# Filter out urban_area and uc_area from the Rural models. 
# ------------------------------------------------- #
if (model_geography == 'Rural'){ 
  model_covars <- model_covars[!grepl('^urban_area$|^uc_area$', model_covars)]
}
# ------------------------------------------------- #
join_key_vars <- c('GEOID', 'year')
# ------------------------------------------------- #
# Untreated = yet-to-be treated and never-treated. 
# ------------------------------------------------- #
dta_ut <- dta_untreated_wfolds %>%
  left_join(select(dta_untreated_non_model_vars, 
                   GEOID, year, event_year, rel_year, treat, entry, entry_events, 
                   DS_Count_10mile, Grocery_Count_10mile, Grocery_Count_10mile_2005), 
            by = join_key_vars)
# ------------------------------------------------- #
# Treated. 
# ------------------------------------------------- #
dta_tr <- dta_treated %>%
  left_join(select(dta_treated_non_model_vars, 
                   GEOID, year, event_year, rel_year, treat, entry, entry_events, 
                   DS_Count_10mile, Grocery_Count_10mile, Grocery_Count_10mile_2005), 
            by = join_key_vars)


dta_all <- bind_rows(dta_ut, dta_tr)

dta_all <- dta_all %>% 
  mutate(treatment = case_when(event_year == 0 ~ 'Never Treated', 
                               event_year > 0 ~ 'Treated'))
# ------------------------------------------------- #
# Binned covariates, dollar store entries, and grocery stores (2005)
# ------------------------------------------------- #
dta_tr_bins <- select(dta_tr, 
                  GEOID, year, event_year, rel_year, treat, entry, entry_events)  %>%
  
  left_join(select(posttr_binned_dsvars, 
                   GEOID, year, 
                   DS_Count_10mile, gross_entry_cumsum_bins, entry_events_bins), 
            by = join_key_vars) %>%
  
  left_join(select(posttr_binned_grocery, GEOID, year, starts_with('Grocery_Count')), 
            by = join_key_vars) %>%
  
  left_join(select(posttr_binned_covars, GEOID, year, 
                   starts_with('poverty_rate'), starts_with('inc_per_capita'), matches('^pop_')), 
            by = join_key_vars)
# ------------------------------------------------- #
# Low access shares over time. 
# ------------------------------------------------- #
# All 
# ------------------------------------------------- #
dep_var_str <- names(dta_all)[grepl('^low_access', names(dta_all))]

sum_dep_var_all <- dta_all %>% 
  
  group_by(year) %>%
  
  summarise(across(.cols = all_of(dep_var_str), 
                   
                   .fns = c(Total = sum, Share = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(year = as.numeric(year)) 

# ------------------------------------------------- #
sum_dep_var_all_lng <- sum_dep_var_all %>% # Convert to numeric for plots. 
  
  pivot_longer(cols = c('Total_low_access':'Share_low_access_pers'), 
               names_pattern = '(.*)_(low_.*)',
               names_to = c('statistic', 'outcome'), 
               values_to = 'value') %>%
  
  arrange(statistic, outcome)

# Plot outcome on year. 
plot_dep_var_by_year(dta = sum_dep_var_all_lng, 
                     outcome_type = '^low_access$', 
                     statistic_type = 'Share', 
                     y_axis_title = 'Low-Access (Shares)', 
                     x_axis_title = 'Year', 
                     plot_title = NULL, plot_subtitle = NULL, 
                     decimal_place_y = 0.001)

# Save Figure. 
dep_var_title = str_replace_all(model_dep_var, '_', ' ') %>% str_to_title(.) %>% str_replace_all(., ' ', '_')
ggsave(filename = here::here('Analysis', 
                             'Figures', 
                             dep_var_title, 
                             str_to_title(model_geography), 
                             'descriptive_statistics',
                             paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'by_year', '.png')), 
       width = 12, height = 8, unit = 'in')
# ------------------------------------------------- #
# Tidy column names for table. 
# ------------------------------------------------- #
names(sum_dep_var_all) <- str_replace_all(names(sum_dep_var_all), '_', ' ') %>%
  
  str_to_title(.) %>% str_replace_all(., c('Total' = 'Total:', 'Share' = 'Share:', 
                                           'Low Access$' = 'Low Access (All)',
                                           'Perm' = '(Permanent) (All)', 'Pers' = '(Persistent) (All)'))

# Save table
write_csv(sum_dep_var_all, file = here::here('Analysis',
                                             'Tables', 
                                             dep_var_title, 
                                             str_to_title(model_geography),
                                             paste0(str_to_lower(model_geography), '_' , 'summary_dependent_variable_by_year', '.csv')))
# ------------------------------------------------- #
# Low-access shares over time. 
# ------------------------------------------------- #
# Treated vs untreated. 
# ------------------------------------------------- #
sum_dep_var_treat <- dta_all %>%
  
  group_by(year, treatment) %>%
  
  summarise(across(.cols = all_of(dep_var_str), 
                   
                   .fns = c(Total = sum, Share = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(year = as.numeric(year)) 

# ------------------------------------------------- #
sum_dep_var_treat_lng <- sum_dep_var_treat %>%
  
  pivot_longer(cols = c('Total_low_access':'Share_low_access_pers'), 
               names_pattern = '(.*)_(low_.*)',
               names_to = c('statistic', 'outcome'), 
               values_to = 'value') %>%
  
  arrange(treatment, statistic, outcome)

map(c('Never Treated', 'Treated'), function(.x){
  plot_dep_var_by_year_and_treatment(dta = sum_dep_var_treat_lng, 
                                     outcome_type = '^low_access$', 
                                     statistic_type = 'Share', 
                                     treatment_type = .x,
                                     y_axis_title = 'Low-Access (Shares)', 
                                     x_axis_title = 'Year', 
                                     plot_title = NULL, plot_subtitle = NULL, 
                                     decimal_place_y = 0.001)
  
  # Save Figure. 
  treatment_stat_str <- str_to_lower(.x) %>% str_replace_all(., ' ', '_')
  ggsave(here::here('Analysis', 
                    'Figures', 
                    dep_var_title, 
                    str_to_title(model_geography), 
                    'descriptive_statistics',
                    paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'by_year', '_', 
                           treatment_stat_str, '.png')), 
                    width = 12, height = 8, unit = 'in')
})
# ------------------------------------------------- #
# Table of outcome variables by treatment status. 
# ------------------------------------------------- #
sum_dep_var_treat_wd <- sum_dep_var_treat %>% 
  
  pivot_wider(names_from = treatment, 
              values_from = c('Total_low_access':'Share_low_access_pers'))

# Clean up col. names. 
names(sum_dep_var_treat_wd) <- names(sum_dep_var_treat_wd) %>% 
  str_replace_all(., '_', ' ') %>%
  str_to_title(.) %>% 
  str_replace_all(., c('Total' = 'Total:', 'Share' = 'Share:', 
                       'Perm' = '(Permanent)', 'Pers' = '(Persistent)', 
                       'Never Treated' = '(NT)', 'Treated' = '(T)'))

# Combine treated and untreated summary stats. with stats. using combined/all data. 
sum_dep_var_treat_wd <- bind_cols(sum_dep_var_treat_wd, select(sum_dep_var_all, -Year))

# Reorganize the columns. 
order_cols <- map2_chr(.x = rep(c(rep('Total: ', 3), rep('Share: ', 3)), 3), 
                       .y = c(rep(c('(All)', '(NT)', '(T)'), 2),  
                              rep(c('(Permanent) (All)', '(Permanent) (NT)', '(Permanent) (T)'), 2), 
                              rep(c('(Persistent) (All)', '(Persistent) (NT)', '(Persistent) (T)'), 2)), 
                       function(.x, .y){paste0(.x, 'Low Access ', .y)})

sum_dep_var_treat_wd <- sum_dep_var_treat_wd %>% select(Year, all_of(order_cols))

sum_dep_var_treat_wd <- sum_dep_var_treat_wd %>% 
  mutate(across(.cols = starts_with('Share:'), ~round(., digits = 3)))

# Save table
write_csv(sum_dep_var_treat_wd, file = here::here('Analysis', 
                                             'Tables', 
                                             dep_var_title, 
                                             str_to_title(model_geography),
                                             paste0(str_to_lower(model_geography), '_' , 
                                                    'summary_dependent_variable_by_year_and_treat_status', '.csv')))


# ------------------------------------------------- #
# Low-access shares over relative time to/from treatment 
# ------------------------------------------------- #
# Treated only.  
# ------------------------------------------------- #

sum_dep_var_rel_time <- dta_all %>%
  
  filter(year >= '2006' & event_year != 0) %>% 
  
  group_by(rel_year) %>%
  
  summarise(across(.cols = all_of(dep_var_str), 
                   
                   .fns = c(Total = sum, Share = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(rel_year = as.numeric(rel_year)) 

# ------------------------------------------------- #
sum_dep_var_rel_time_lng <- sum_dep_var_rel_time %>%
  
  pivot_longer(cols = c('Total_low_access':'Share_low_access_pers'), 
               names_pattern = '(.*)_(low_.*)',
               names_to = c('statistic', 'outcome'), 
               values_to = 'value') %>%
  
  arrange(statistic, outcome)

x_lims = range(sum_dep_var_rel_time_lng$rel_year)

# Plot outcome on relative time 
plot_dep_var_by_rel_time(dta = sum_dep_var_rel_time_lng, 
                         outcome_type = '^low_access$', 
                         statistic_type = 'Share', 
                         y_axis_title = 'Low-Access (Shares)', 
                         x_axis_title = 'Time from Treatment', 
                         plot_title = NULL, plot_subtitle = NULL, 
                         decimal_place_y = 0.001, 
                         xlim_1 = x_lims[1], xlim_2 = x_lims[2])

# Save Figure. 
ggsave(filename = here::here('Analysis', 
                             'Figures', 
                             dep_var_title, 
                             str_to_title(model_geography), 
                             'descriptive_statistics',
                             paste0(str_to_lower(model_geography), '_', model_dep_var, '_', 'by_relative_time', '.png')), 
       width = 12, height = 8, unit = 'in')
# ------------------------------------------------- #
# Tidy column names for table. 
# ------------------------------------------------- #
names(sum_dep_var_rel_time) <- str_replace_all(names(sum_dep_var_rel_time), '_', ' ') %>%
  
  str_to_title(.) %>% str_replace_all(., c('Rel Year' = 'Time from Treatment','Total' = 'Total:', 'Share' = 'Share:', 
                                           'Low Access$' = 'Low Access',
                                           'Perm' = '(Permanent)', 'Pers' = '(Persistent)'))

sum_dep_var_rel_time <- sum_dep_var_rel_time %>% 
  mutate(across(.cols = starts_with('Share:'), ~round(., digits = 3)))

# Save table
write_csv(sum_dep_var_rel_time, file = here::here('Analysis', 
                                             'Tables', 
                                             dep_var_title, 
                                             str_to_title(model_geography),
                                             paste0(str_to_lower(model_geography), '_' , 'summary_dependent_variable_by_relative_time', '.csv')))
# ------------------------------------------------- #
# Dollar store and grocery store counts/means by relative time. 
# ------------------------------------------------- #
sum_dsgroc_rel_time <- dta_all %>%
  
  filter(year >= '2006' & event_year != 0) %>% 
  
  group_by(rel_year) %>%
  
  summarise(across(.cols = c(Grocery_Count_10mile, DS_Count_10mile), 
                   
                   .fns = c(Total = sum, Mean = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(rel_year = as.numeric(rel_year)) 
# ------------------------------------------------- #
# Make data frame long. 
# ------------------------------------------------- #
sum_dsgroc_rel_time_lng <- sum_dsgroc_rel_time %>% 
  pivot_longer(cols = c('Total_Grocery_Count_10mile':'Mean_DS_Count_10mile'), 
               names_pattern = '(.*)_(.*)_(Count.*)', 
               names_to = c('statistic', 'outcome', 'count'), 
               values_to = 'value') %>%
  select(-count)

# ------------------------------------------------- #
# Plot store counts by relative time 
# ------------------------------------------------- #
map(c('DS', 'Grocery'), function(.x){
  plot_dep_var_by_rel_time(dta = sum_dsgroc_rel_time_lng, 
                           outcome_type = .x, 
                           statistic_type = 'Mean', 
                           y_axis_title = 'Average Store Count', 
                           x_axis_title = 'Time from Treatment', 
                           plot_title = NULL, plot_subtitle = NULL, 
                           decimal_place_y = 0.001, 
                           xlim_1 = x_lims[1], xlim_2 = x_lims[2])
  # Save Figure. 
  
  ggsave(filename = here::here('Analysis', 
                               'Figures', 
                               dep_var_title, 
                               str_to_title(model_geography), 
                               'descriptive_statistics',
                               paste0(str_to_lower(model_geography), '_', str_to_lower(.x), '_', 'by_relative_time', '.png')), 
         width = 12, height = 8, unit = 'in')
  
})
# ------------------------------------------------- #

# ------------------------------------------------- #
# Dollar store and grocery store counts/means by time/year.
# ------------------------------------------------- #
sum_dsgroc_year <- dta_all %>%
  
  group_by(year) %>%
  
  summarise(across(.cols = c(Grocery_Count_10mile, DS_Count_10mile), 
                   
                   .fns = c(Total = sum, Mean = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(year = as.numeric(year)) 

names(sum_dsgroc_year) <- names(sum_dsgroc_year) %>% 
  str_replace_all(., '_', ' ') %>%
  str_to_title(.) %>% 
  str_replace_all(., c('Total' = 'Total:', 'Mean' = 'Mean:', 
                       'Grocery Count 10mile' = 'Grocery Stores (All)', 'Ds Count 10mile' = 'Dollar Stores (All)', 
                       'Never Treated' = '(NT)', 'Treated' = '(T)'))
# ------------------------------------------------- #
# Dollar store and grocery store counts/means by time/year and treated vs untreated. 
# ------------------------------------------------- #
sum_dsgroc_year_treat <- dta_all %>%
  
  group_by(year, treatment) %>%
  
  summarise(across(.cols = c(Grocery_Count_10mile, DS_Count_10mile), 
                   
                   .fns = c(Total = sum, Mean = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(year = as.numeric(year)) 
# ------------------------------------------------- #

# ------------------------------------------------- #
# Table of dollar store and grocery store counts/averages by year and treatment status. 
# ------------------------------------------------- #
sum_dsgroc_year_treat_wd <- sum_dsgroc_year_treat %>% 
  
  pivot_wider(names_from = treatment, 
              values_from = c('Total_Grocery_Count_10mile':'Mean_DS_Count_10mile'))

# Clean up col. names. 
names(sum_dsgroc_year_treat_wd) <- names(sum_dsgroc_year_treat_wd) %>% 
  str_replace_all(., '_', ' ') %>%
  str_to_title(.) %>% 
  str_replace_all(., c('Total' = 'Total:', 'Mean' = 'Mean:', 
                       'Grocery Count 10mile' = 'Grocery Stores', 'Ds Count 10mile' = 'Dollar Stores', 
                       'Never Treated' = '(NT)', 'Treated' = '(T)'))

# Combine treated and untreated summary stats. with stats. using combined/all data. 
sum_dsgroc_year_treat_wd <- bind_cols(sum_dsgroc_year_treat_wd, select(sum_dsgroc_year, -Year))

# Reorganize the columns. 
order_cols <- map2_chr(.x = rep(c(rep('Total: ', 3), rep('Mean: ', 3)), 2), 
                       .y = c(rep(c('Grocery Stores (All)', 'Grocery Stores (NT)', 'Grocery Stores (T)'), 2),
                              rep(c('Dollar Stores (All)', 'Dollar Stores (NT)', 'Dollar Stores (T)'), 2)),
                       function(.x, .y){paste0(.x, .y)})

sum_dsgroc_year_treat_wd <- sum_dsgroc_year_treat_wd %>% select(Year, all_of(order_cols))

sum_dsgroc_year_treat_wd <- sum_dsgroc_year_treat_wd %>% select(-contains('Total:')) %>% 
  mutate(across(.cols = where(is.numeric), ~round(., digits = 3)))

# Save table
write_csv(sum_dsgroc_year_treat_wd, file = here::here('Analysis', 
                                                  'Tables', 
                                                  dep_var_title, 
                                                  str_to_title(model_geography),
                                                  paste0(str_to_lower(model_geography), '_' , 
                                                         'summary_ds_and_grocery_by_year_and_treat_status', '.csv')))

# ------------------------------------------------- #
# Summary statistics by Covariates/Predictors. 
# ------------------------------------------------- #
sum_covars_by_treat <- dta_all %>% 
  group_by(treatment) %>% 
  summarise(across(.cols = all_of(model_covars), 
                   .fns = c(Mean = mean, SD = sd), 
                   .names = '{col}_{.fn}')) %>%
  pivot_longer(cols = c('age_18_34_Mean':'fe_year_x_STATE_low_access_SD'), 
               names_to = c('variable', 'statistic'), 
               names_pattern = '(.*)_(.*)', 
               values_to = 'value') %>% 
  pivot_wider(names_from = c('treatment', 'statistic'))

# ------------------------------------------------- #
# Compute the 1st, middle, and 3rd quartiles. 
# ------------------------------------------------- #
sum_covars_by_treat_iqr <- dta_all %>% 
  group_by(treatment) %>% 
  reframe(across(.cols = all_of(model_covars), 
                 .fns = ~quantile(., probs = c(0.25, 0.5, 0.75)))) %>%
  mutate(quartile = rep(c('1st_Quartile', 'Median', 'Third_Quartile'), 2)) %>%
  relocate(quartile) %>%
  pivot_longer(cols = c('age_18_34':'fe_year_x_STATE_low_access'), 
               names_to = 'variable', 
               values_to = 'value') %>%
  pivot_wider(names_from = c('quartile', 'treatment'))

# Join to mean and standard deviation summary table. 
sum_covars_by_treat <- sum_covars_by_treat %>% left_join(sum_covars_by_treat_iqr, by = 'variable')
sum_covars_by_treat <- sum_covars_by_treat %>% select(variable, contains('Never Treated'), contains('Treated'))
# ------------------------------------------------- #
# Add predictor categories. 
# Note that I(model_covars_list) creates a column called values in which each row in the column contains the elements in the list. 
# Thereafter, we use tidyr::unnest() to flatten the lists with their associated variate category names. 
# ------------------------------------------------- #
var_categories <- data.frame(Predictor_Type = names(model_covars_list), 
                             variable = I(model_covars_list), 
                             stringsAsFactors = FALSE)
var_categories <- tidyr::unnest(var_categories, cols = variable)
var_categories$Predictor_Type <- var_categories$Predictor_Type %>% str_replace_all(., c('_' = ' ')) %>% str_to_title()
# Join predictor type categories to the summary tables. 
sum_covars_by_treat <- sum_covars_by_treat %>% 
  
  left_join(var_categories, by = 'variable')
# ------------------------------------------------- #
# Clean column containing variable names. 
sum_covars_by_treat <- sum_covars_by_treat %>%
  mutate(across(.cols = variable, 
                .fn = ~tidy_covar_names(.), 
                .names = '{col}_Tidy' )) %>%
  relocate(c(variable_Tidy, Predictor_Type), .after = variable)
# ------------------------------------------------- #
names(sum_covars_by_treat) <- names(sum_covars_by_treat) %>% 
  str_replace_all(., c('_' = ' ', 
                       'variable' = 'Variable',
                       '^Treated' = '(T)', 
                       '^Never Treated' = '(NT)',
                       '\\(NT\\) Mean' = 'Mean (NT)', 
                       '\\(T\\) Mean' = 'Mean (T)',
                       '\\(NT\\) SD' = 'SD', 
                       '\\(T\\) SD' = 'SD',
                       'Never Treated' = '', 
                       'Treated' = '')) %>%
  str_trim(., side = 'right') 
# ------------------------------------------------- #
# Save table
# ------------------------------------------------- #
write_csv(sum_covars_by_treat, file = here::here('Analysis', 
                                                      'Tables', 
                                                      dep_var_title, 
                                                      str_to_title(model_geography),
                                                      paste0(str_to_lower(model_geography), '_' , 
                                                             'summary_covariates_by_treat_status', '.csv')))
# ------------------------------------------------- #
# Number of gross entries. 
# ------------------------------------------------- #
summarise_entries <- function(dta, group_vars, entry_type_str, grocery){
  
  summary_dta <- dta %>%
    
    group_by(GEOID) %>% 
    
    distinct(across(all_of(group_vars) ) ) %>% 
    
    group_by(across(all_of(group_vars) ) ) %>%  
    
    summarise(across(.cols = GEOID, .fns = ~n(), .names = 'Count' ) ) %>%
    
    rename_with(.cols = contains('entry'), 
                .fn = ~paste(str_to_title(str_extract_all(., 'entry')[[1]]), 'Bins')) %>%
    
    filter(`Entry Bins` != 0) %>% 
    
    mutate(entry_type = entry_type_str) %>%
    
    ungroup() %>% 
    
    mutate(Total = sum(Count), 
           Share = Count/Total) %>%
    relocate(c(Share, Total), .after = Count)
  
  summary_dta$`Entry Bins` <- as.character(summary_dta$`Entry Bins`) %>% 
    str_replace_all(., pattern = c('\\,[[:digit:]]+\\]' = '', '\\(' = '\u003e '))
  
  summary_dta <- summary_dta %>% mutate(across(.cols = where(is.numeric), .fn = ~round(., digits = 3)))
  
  if (isTRUE(grocery)){
    
    summary_dta <- summary_dta %>%
      
      rename_with(.cols = contains('Grocery'), 
                  .fn = ~paste(str_extract_all(., 'Grocery')[[1]], 'Bins'))
    
    summary_dta <- summary_dta %>% 
      
      mutate(across(.cols = where(is.numeric), 
                    .fn = ~round(., digits = 3))) %>%
      
      pivot_longer(cols = c('Count', 'Share', 'Total'), 
                   values_to = 'value', 
                   names_to = 'statistic') %>%
      
      pivot_wider(names_from = `Entry Bins`) %>% 
      
      filter(statistic != 'Total') 
    
  }
  
  return(summary_dta)
}

sum_ds_bins_gross <- summarise_entries(dta = dta_tr_bins, 
                                       group_vars = 'gross_entry_cumsum_bins', 
                                       entry_type_str = 'Gross Entries', 
                                       grocery = FALSE)
sum_ds_bins_entry <- summarise_entries(dta = dta_tr_bins, 
                                       group_vars = 'entry_events_bins', 
                                       entry_type_str = 'Entries', 
                                       grocery = FALSE)

sum_ds_bins <- bind_rows(sum_ds_bins_entry, sum_ds_bins_gross)

# Save table
write_csv(sum_ds_bins, file = here::here('Analysis', 
                                         'Tables', 
                                         dep_var_title, 
                                         str_to_title(model_geography),
                                         paste0(str_to_lower(model_geography), '_' , 
                                                'summary_ds_entries_by_type', '.csv')))
# ------------------------------------------------- #
# Number of gross entries and entry events by grocery stores in 2005. 
# ------------------------------------------------- #
sum_dsgroc_bins_gross <- summarise_entries(dta = dta_tr_bins, 
                                           group_vars = c('gross_entry_cumsum_bins', 'Grocery_Count_10mile_2005_bins'), 
                                           entry_type_str = 'Gross Entries', 
                                           grocery = TRUE)

# Save table
write_csv(sum_dsgroc_bins_gross, file = here::here('Analysis', 
                                         'Tables', 
                                         dep_var_title, 
                                         str_to_title(model_geography),
                                         paste0(str_to_lower(model_geography), '_' , 
                                                'summary_ds_entries_by_grocery_gross', '.csv')))

sum_dsgroc_bins_entry <- summarise_entries(dta = dta_tr_bins, 
                                           group_vars = c('entry_events_bins', 'Grocery_Count_10mile_2005_bins'), 
                                           entry_type_str = 'Entries', 
                                           grocery = TRUE)

# Save table
write_csv(sum_dsgroc_bins_entry, file = here::here('Analysis', 
                                                   'Tables', 
                                                   dep_var_title, 
                                                   str_to_title(model_geography),
                                                   paste0(str_to_lower(model_geography), '_' , 
                                                          'summary_ds_entries_by_grocery', '.csv')))
# ------------------------------------------------- #
# Tables of dollar store entry bins and binned select socio-demographic covariates. 
# ------------------------------------------------- #
summarise_entries_by_covars <- function(dta, group_vars, entry_type_str, covariate){
  
  summary_dta <- dta %>%
    
    group_by(GEOID) %>%
    
    distinct(across(all_of(group_vars) ) ) %>%
    
    group_by(across(all_of(group_vars) ) ) %>%  
    
    summarise(across(.cols = GEOID, .fns = ~n(), .names = 'Count' ) ) %>%
    
    rename_with(.cols = contains('entry'), 
                .fn = ~paste(str_to_title(str_extract_all(., 'entry')[[1]]), 'Bins')) %>%
    
    filter(`Entry Bins` != 0) %>%
    
    mutate(entry_type = entry_type_str) %>%
    
    ungroup() %>% 
    
    mutate(Total = sum(Count), 
           Share = Count/Total) %>%
    relocate(c(Share, Total), .after = Count)
  
  summary_dta$`Entry Bins` <- as.character(summary_dta$`Entry Bins`) %>% 
    str_replace_all(., pattern = c('\\,[[:digit:]]+\\]' = '', '\\(' = '\u003e '))
  
  summary_dta <- summary_dta %>% mutate(across(.cols = where(is.numeric), .fn = ~round(., digits = 3)))
  
  times_rep <- summary_dta %>% 
    select(`Entry Bins`) %>% 
    distinct() %>% count() %>% as.numeric()
  
  unique_bins <- summary_dta %>% 
    select(ends_with('_bins')) %>% 
    distinct() %>% count() %>% as.numeric()
  
  summary_dta$Quartile <- rep(seq_len(unique_bins), times_rep)
  
  summary_dta <- summary_dta %>%
    mutate(across(.cols = ends_with('_bins'), 
                  .fn = ~as.character(.) ) )
  
  
  summary_dta[[covariate]][summary_dta$Quartile == unique_bins] <- 
    as.character(summary_dta[[covariate]][summary_dta$Quartile == unique_bins]) %>%
    str_replace_all(., pattern = c('\\(' = '\u003e ', 'e\\+' = '')) %>%
    str_replace_all(., pattern = c('\\,.*' = ''))
  
  
  summary_dta <- summary_dta %>% 
    
    mutate(across(.cols = where(is.numeric),
                  .fn = ~round(., digits = 3))) %>%
    
    pivot_longer(cols = c('Count', 'Share', 'Total'),
                 values_to = 'value',
                 names_to = 'statistic') %>%
    
    pivot_wider(names_from = `Entry Bins`) %>%
    
    filter(statistic != 'Total')
  
  names(summary_dta)[grepl('_bins', names(summary_dta))] <- str_remove_all(covariate, '_bins') %>% tidy_covar_names()
  
  return(summary_dta)
}
# ------------------------------------------------- #
# Create and save tables. 
# ------------------------------------------------- #
covar_vec <- c('inc_per_capita_bins', 'poverty_rate_bins', 
               'pop_black_bins', 'pop_hispanic_bins', 'pop_white_bins', 'pop_asian_bins')
entry_type_vec_1 <- c('gross_entry_cumsum_bins', 'entry_events_bins')
entry_type_vec_2 <- c('Gross Entries', 'Entries')

covar_vec %>% 
  
  map(function(.x){ 
    
    map2(entry_type_vec_1, entry_type_vec_2, function(.y, .z){
      
      sum_ds_x_covars_bins <- summarise_entries_by_covars(dta = dta_tr_bins, 
                                                          group_vars = c(.y, .x), 
                                                          covariate = .x,
                                                          entry_type_str = .z)
      
      entry_name_str <- str_extract_all(.y, 'gross_entry|entry_events')[[1]]
      covar_name_str <- str_remove_all(.x, '_bins')
      
      write_csv(sum_ds_x_covars_bins, 
                file = here::here('Analysis', 
                                  'Tables', 
                                  dep_var_title, 
                                  str_to_title(model_geography),
                                  paste0(str_to_lower(model_geography), '_' , 
                                         'summary_', entry_name_str, '_', covar_name_str, '.csv')))
      
      
    })
  })