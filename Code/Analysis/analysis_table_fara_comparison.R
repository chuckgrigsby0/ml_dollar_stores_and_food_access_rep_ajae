# Load and prepare data. 
library('pacman')
library('here') # To route directories. 
library('readr') # To save tables as .csv files. 
model_dep_var = 'low_access'
dep_var_title <- model_dep_var %>% str_replace_all(., '_', ' ') %>% str_to_title(.) %>% str_replace_all(., ' ', '_')
bootstrap_by_tracts <- '_tracts' # NULL for bootstrap at block-group level; '_tracts' for bootstrap at census-tract level. 
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #  
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation_national.R'))
# -------------------------------------------------------------------------------------------- #

source(here::here('Code', 'Functions', 'Function_plots_for_low_access_by_time.R'))

# -------------------------------------------------------------------------------------------- #
# Vectors of character strings containing raw variable names and tidy variable names. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_model_covars_lists.R'))
model_covars <- unlist(model_covars_list, use.names = FALSE) 
# -------------------------------------------------------------------------------------------- #
join_key_vars <- c('GEOID', 'year')
# -------------------------------------------------------------------------------------------- #  
# Untreated = yet-to-be treated and never-treated. 
dta_ut <- dta_untreated_wfolds %>%
  left_join(select(dta_untreated_non_model_vars, 
                   GEOID, year, event_year, rel_year, treat, entry, entry_events, 
                   DS_Count_10mile, Grocery_Count_10mile, Grocery_Count_10mile_2005), 
            by = join_key_vars)

# Treated. 
dta_tr <- dta_treated %>%
  left_join(select(dta_treated_non_model_vars, 
                   GEOID, year, event_year, rel_year, treat, entry, entry_events, 
                   DS_Count_10mile, Grocery_Count_10mile, Grocery_Count_10mile_2005), 
            by = join_key_vars)


dta_all <- bind_rows(dta_ut, dta_tr)

dta_all <- dta_all %>% 
  mutate(treatment = case_when(event_year == 0 ~ 'Never Treated', 
                               event_year > 0 ~ 'Treated'))

# -------------------------------------------------------------------------------------------- # 

# -------------------------------------------------------------------------------------------- #  
# Low access shares over time. 
# -------------------------------------------------------------------------------------------- #  
# All 
# -------------------------------------------------------------------------------------------- #  
dep_var_str <- names(dta_all)[grepl('^low_access', names(dta_all))]
# -------------------------------------------------------------------------------------------- #  
sum_dep_var_all <- dta_all %>% 
  
  group_by(year) %>%
  
  summarise(across(.cols = all_of(dep_var_str), 
                   
                   .fns = c(Total = sum, Share = mean), 
                   
                   .names = '{.fn}_{.col}') ) %>%
  
  mutate(year = as.numeric(year)) 
# -------------------------------------------------------------------------------------------- #
# From https://www.ers.usda.gov/webdocs/publications/82101/eib-165.pdf?v=1840.3 
# and 
# https://ageconsearch.umn.edu/record/323869/

tract_population = 72531 

fara_dta <- data.frame(year = c(2010, 2015, 2019), 
                       la_1_10 = c(28541, 27527, 27548))

fara_dta <- fara_dta %>% mutate(sh_la_1_10 = la_1_10/tract_population, 
                                diff_la_1_10 = la_1_10 - lag(la_1_10))

comp_dta <- sum_dep_var_all %>% 
  filter(year %in% c(2010, 2015, 2019)) %>% 
  select(year, Total_low_access, Share_low_access) %>%
  mutate(diff_la_2_10 = Total_low_access - lag(Total_low_access)) %>%
  rename_with(.cols = contains('low_access'), 
              .fn = ~str_replace_all(., pattern = c('low_access' = 'la_2_10', 
                                                    'Share' = 'sh', 
                                                    'Total_' = '')))
  

fara_dta <- fara_dta %>%
  left_join(comp_dta, by = 'year')

# -------------------------------------------------------------------------------------------- #
names(fara_dta) <- str_replace_all(names(fara_dta), 
                                   c('^la_' = 'Low Access: ',
                                     'sh_la_' = 'Low Access (Shares): ',
                                     'diff_la_' = 'Low Access (Change): ', 
                                     '1_10' = '1 and 10 Miles (Census Tracts (FARA))',
                                     '2_10' = '2 and 10 Miles (Block Groups)',
                                     'year' = 'Year'))
# -------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------- #
# Save table
# -------------------------------------------------------------------------------------------- #
write_csv(fara_dta, file = here::here('Analysis',
                                      'Tables', 
                                      dep_var_title, 
                                      paste0('national', '_' , 'fara_low_access_comparison', '.csv')))
# -------------------------------------------------------------------------------------------- #