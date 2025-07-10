# Script creates Table C.4 of supplementary text showing regression estimates of relative treatment timing on 
# characteristics most associated with growing treatment effects since the initial treatment period. 
# -------------------------------------------------------------------------------------------- #
# Specify Urban/Rural, dependent variable, and results based on census-tract bootstrap. 
# -------------------------------------------------------------------------------------------- #
model_geography <- 'Urban' # Used in script below.
model_dep_var <- 'low_access' # Used in script below.
bootstrap_by_tracts <- '_tracts' # Used in script below.
options(scipen=999)
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
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_'); dir_dep_var # e.g., Low_Access
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')); dep_var_title 
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', replacement = 'actual') ) %>%
  
  left_join(select(dta_untreated_wfolds, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau) %>%
  
  rename(preds = pred_class_cf) %>%
  
  left_join(select(dta_treated, GEOID, year, all_of(model_covars)), by = c('GEOID', 'year')) %>%
  
  filter(year >= '2006') # We obtain counterfactual predictions from 2006 to 2020.
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment dollar store bins and factors. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_dsvars = paste0('posttreatment_binned_and_factor_dsvars_', str_to_lower(model_geography), '.rds')

posttr_binned_dsvars <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_dsvars))

# Remove tau calculated from the original/empirical data 
posttr_binned_dsvars <- posttr_binned_dsvars %>% select(-tau)
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Post-treatment observations, year 2005 Grocery Store bins. 
# -------------------------------------------------------------------------------------------- #
fname_posttr_binned_grocery = paste0('posttreatment_binned_grocery_', str_to_lower(model_geography), '.rds')

posttr_binned_grocery <- readRDS(here::here('Data', 'Data_2_and_10_Miles', fname_posttr_binned_grocery))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Load the binned dollar store bans and restrictions data. 
# -------------------------------------------------------------------------------------------- #
ds_bans_binned <- readRDS(here::here('Data', 'Data_2_and_10_Miles', 
                                     paste0('posttreatment_binned_ds_policy_vars_', 
                                            str_to_lower(model_geography), '.rds') ) )
# -------------------------------------------------------------------------------------------- #

# Select the socio-demographic variables of interest. 
# -------------------------------------------------------------------------------------------- #
varnames_tidy <- tibble::enframe(model_covar_names, name = 'category', value = 'varname_tidy') %>% 
  unnest(cols = c('category', 'varname_tidy'))

varnames <- tibble::enframe(model_covars, name = 'category', value = 'varname') %>% 
  unnest(cols = c('category', 'varname'))


varnames_tidy <- bind_cols(varnames_tidy, select(varnames, varname) )

socio_demog_vars <- varnames_tidy %>% 
  filter(category == 'socioeconomics') %>%
  filter(!grepl('^age_|population$', varname)) 

# -------------------------------------------------------------------------------------------- #
posttr_model_preds <- model_preds %>% 
  
  filter(rel_year >= 0 & event_year != 0) %>% 
  
  left_join(select(ds_bans_binned, GEOID, year, policy_total), by = c('GEOID', 'year') ) %>% 
  
  mutate(DS_Policy = if_else(policy_total > 0, 1, 0) ) %>% 
  
  left_join(select(posttr_binned_dsvars, GEOID, year, matches('entry_.*_bins$')), by = c('GEOID', 'year') ) %>%
  
  left_join(select(posttr_binned_grocery, GEOID, year, Grocery_Count_10mile_2005_bins), by = c('GEOID', 'year') )

# Create urban area dummy to use below. 
urban_area_dummy <- str_subset(names(posttr_model_preds), pattern = '^urban_area$')
# -------------------------------------------------------------------------------------------- #
# Set up data for regressing relative time on covariates + Grocery count-x-dollar store entry events. 
# -------------------------------------------------------------------------------------------- #
reg_data <- posttr_model_preds %>%
  select(rel_year, 
         all_of(socio_demog_vars$varname), 
         entry_events_bins, 
         Grocery_Count_10mile_2005_bins,
         DS_Policy, 
         any_of(urban_area_dummy) )


reg_data <- reg_data %>% 
  
  mutate(entry_events_bins_recode = as.character(entry_events_bins), 
         Grocery_Count_10mile_2005_bins_recode = as.character(Grocery_Count_10mile_2005_bins) ) %>%
  
  mutate(entry_events_bins_recode = case_when(entry_events_bins_recode == '1' ~ '1', 
                                              entry_events_bins_recode == '2' ~ '2',
                                              entry_events_bins_recode == '3' ~ '3', 
                                              TRUE ~ '\u003e 3'), 
         Grocery_Count_10mile_2005_bins_recode = case_when(Grocery_Count_10mile_2005_bins_recode == '0' ~ '0',
                                                           Grocery_Count_10mile_2005_bins_recode == '1' ~ '1', 
                                                           Grocery_Count_10mile_2005_bins_recode == '2' ~ '2',
                                                          TRUE ~ '\u003e 2') ) %>% 
  mutate(entry_events_bins_recode = factor(entry_events_bins_recode, 
                                           levels = c('1', '2', '3', '\u003e 3')), 
         Grocery_Count_10mile_2005_bins_recode = factor(Grocery_Count_10mile_2005_bins_recode, 
                                                        levels = c('0', '1', '2', '\u003e 2') ) )


reg_form <- xpd(rel_year ~ -1 + urban_area + DS_Policy + ..ctrl + Grocery_Count_10mile_2005_bins_recode:entry_events_bins_recode, 
                ..ctrl = socio_demog_vars$varname, data = reg_data)


reg_model <- feols(reg_form, data = reg_data)

reg_table <- broom::tidy(reg_model)

# Clean up the rows containing variable names. 
reg_table <- reg_table %>% mutate(variable = term) %>% relocate(variable, .before = term)

reg_table <- reg_table %>% 
  separate_wider_delim(cols = term, delim = ':', too_few = 'align_start', names = c('grocery_stores', 'entry_events') )

reg_table$grocery_stores <- reg_table$grocery_stores %>% str_remove_all('Grocery_Count_10mile_2005_bins_recode') 

reg_table$entry_events <- reg_table$entry_events %>% str_remove_all('entry_events_bins_recode')

reg_table$grocery_stores[is.na(reg_table$entry_events)] <- NA    

reg_table$grocery_stores[!is.na(reg_table$grocery_stores)] <- paste(reg_table$grocery_stores[!is.na(reg_table$grocery_stores)], 
                                                                    'Grocery Stores (2005)')

reg_table$entry_events[!is.na(reg_table$entry_events)] <- paste(reg_table$entry_events[!is.na(reg_table$entry_events)], 
                                                                    'Entry')

grocery_entry_var <- paste0(reg_table$grocery_stores[!is.na(reg_table$grocery_stores)], ', ', 
                            reg_table$entry_events[!is.na(reg_table$entry_events)])

reg_table$variable[!is.na(reg_table$entry_events)] <- grocery_entry_var

# Join the tidy variable names to the regression table. 
reg_table <- reg_table %>% 
  left_join(select(socio_demog_vars, varname, varname_tidy), by = c('variable' = 'varname') ) %>% 
  relocate(varname_tidy)

reg_table$variable <- reg_table$variable %>% 
  str_replace_all(pattern = c('urban_area' = 'Urban Area', 
                              'DS_Policy' = 'Dollar Store Defeat/Restriction Policy') )

reg_table$variable[reg_table$variable %in% socio_demog_vars$varname] = reg_table$varname_tidy[!is.na(reg_table$varname_tidy)]


reg_table <- reg_table %>% select(variable, estimate, std.error, statistic, p.value)

reg_table$bootstrap_id <- 0
# -------------------------------------------------------------------------------------------- #
# Load bootstrapped estimates to compute SEs. 
# -------------------------------------------------------------------------------------------- #
dir_geography <- paste(model_geography, 'Bootstrap', sep = '_') # e.g., Rural_Bootstrap, Urban_Bootstrap

dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_') # e.g., Low_Access

dir_bootstrap <- paste0('bootstrap_rel_year_heterogeneity', bootstrap_by_tracts) # NULL or '_tracts'

bootstrap_ids <- seq(1, 499, 1)


rel_year_heterogeneity <- map(bootstrap_ids, function(.x){
  
  
filename <- paste0('bootstrap_',
                   'rel_year_heterogeneity_',
                   str_to_lower(model_geography), '_', # e.g., rural or urban
                   model_dep_var, # e.g., low_access
                   bootstrap_by_tracts,
                   '_', .x,
                   '.rds')

boot_dta <- readRDS(here::here('Analysis',
                   dir_geography,
                   dir_dep_var, 
                   dir_bootstrap, 
                   filename))

})
# -------------------------------------------------------------------------------------------- #
rel_year_heterogeneity <- c(list(reg_table), rel_year_heterogeneity)
rel_year_heterogeneity <- bind_rows(rel_year_heterogeneity)
# -------------------------------------------------------------------------------------------- #

rel_year_heterogeneity_summary <- rel_year_heterogeneity %>%
  group_by(variable) %>%
  summarise(across(.cols = estimate, 
                   .fns = list('avg' = mean, 
                               'sd' = sd), 
                   .names = '{.col}_{.fn}'))

rel_year_heterogeneity_emp <- reg_table %>%
  left_join(rel_year_heterogeneity_summary, by = 'variable') %>%
  relocate(c(estimate_sd, estimate_avg), .after = estimate) %>% 
  select(variable, matches('^estimate'))


rel_year_heterogeneity_emp <- rel_year_heterogeneity_emp %>% 
  mutate(p_value = 2*(1-pnorm(abs(estimate/estimate_sd) ) ), 
         significance = case_when(
           p_value < 0.01 ~ "***",
           p_value < 0.05 ~ "**",
           p_value < 0.10 ~ "*",
           TRUE ~ ""
         ))

table_rel_year_heterogeneity_emp <- rel_year_heterogeneity_emp %>%
  select(variable, estimate, estimate_sd, significance) %>%
  mutate(across(.cols = where(is.numeric), 
                .fns = \(x) round(x, digits = 4) ) )

names(table_rel_year_heterogeneity_emp) <- c('Variable', 'Estimate', 'SD', 'Significance')
# -------------------------------------------------------------------------------------------- #
library(knitr)
library(kableExtra)
# -------------------------------------------------------------------------------------------- #
# Generate the LaTeX table with detailed notes
latex_table <- kable(table_rel_year_heterogeneity_emp, "latex", booktabs = TRUE, 
                     caption = "Regression Estimates of Relative Treatment Timing on Socio-demographic Characteristics, Baseline Grocery Store Counts, Dollar Store Entry Events, and Dollar Store Policy Dummies") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  footnote(general = c('Ordinary Least Squares is used to regress relative treatment timing on socio-demographic characteristics, grocery store count-dollar store entry event interactions, and a dollar store policy dummy variable, 
                       where one indicates that the block group belonged to a jurisdiction with at least one dollar store defeat or restriction policy.'),
           general_title = "", threeparttable = TRUE, footnote_as_chunk = TRUE)

# Print the LaTeX code
print(latex_table)
# -------------------------------------------------------------------------------------------- #