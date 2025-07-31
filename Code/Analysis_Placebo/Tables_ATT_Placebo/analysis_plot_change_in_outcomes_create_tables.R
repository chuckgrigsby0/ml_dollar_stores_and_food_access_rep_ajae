# Script creates latex table comparing ATTs of main result with placebo result. 
# Load packages. 
# ------------------------------------------------- #
pacman::p_load('here', 'dplyr', 'tidyr', 'stringr', 'purrr', 'readr')
options(scipen=999)
# ------------------------------------------------- #
# Read the original treatment effects 
# ------------------------------------------------- #

geog_types <- c('Urban', 'Rural')
dir_dep_var <- 'Low_Access'

# ------------------------------------------------- #
emp_change_in_outcomes_orig <- geog_types %>% 
  
  map(function(.x){
    
    read_csv(file = here::here('Analysis',
                               'Tables',
                               dir_dep_var,
                               str_to_title(.x),
                               paste0(str_to_lower(.x), '_' ,
                                      'average_change_in_outcomes_relative_time', '.csv') ) )
  })

emp_change_in_outcomes_orig <- emp_change_in_outcomes_orig %>% set_names(nm = geog_types)
emp_change_in_outcomes_orig <- bind_rows(emp_change_in_outcomes_orig, .id = 'Sample')
# ------------------------------------------------- #
emp_change_in_outcomes_plac <- geog_types %>% 
  
  map(function(.x){ 
    
    readRDS(file = here::here('Analysis_Placebo',
                               'Tables',
                               dir_dep_var,
                               str_to_title(.x),
                               paste0(str_to_lower(.x), '_' ,
                                      'average_change_in_outcomes_relative_time_plac', '.csv') ) )
    
    })

emp_change_in_outcomes_plac <- emp_change_in_outcomes_plac %>% set_names(nm = geog_types)
emp_change_in_outcomes_plac <- bind_rows(emp_change_in_outcomes_plac, .id = 'Sample')
# ------------------------------------------------- #
rm_var_orig <- setdiff(names(emp_change_in_outcomes_orig), names(emp_change_in_outcomes_plac))

emp_change_in_outcomes_orig <- emp_change_in_outcomes_orig %>% select(-all_of(rm_var_orig))

emp_change_in_outcomes_orig <- emp_change_in_outcomes_orig %>% 
  mutate(effect_type = paste0(effect_type, '-', 'Actual'), 
         rel_year = factor(rel_year))

emp_change_in_outcomes_plac <- emp_change_in_outcomes_plac %>% 
  mutate(effect_type = paste0(effect_type, '-', 'Placebo'), 
         year = as.numeric(year))

treat_effects <- bind_rows(emp_change_in_outcomes_plac, emp_change_in_outcomes_orig) %>%
  select(Sample, rel_year, percentage_type, effect_type, 
         tau_avg, tau_avg_sd, matches('^tau_.*99$'), 
         pct_att, pct_att_sd, matches('^pct_.*99$')) %>%
  filter(str_detect(percentage_type, pattern = 'Year', negate = TRUE)) %>%
  arrange(percentage_type)


# Obtain the percentage_type values beginning with All. 
# These refer to the overall average ATTs as opposed to the relative time ATTs
treat_effects_all <- treat_effects %>% 
  
  filter(grepl('^All', percentage_type)) %>% 
  
  select(-rel_year)


treat_effects_lng <- treat_effects_all %>%
  
  pivot_longer(cols = 'tau_avg':last_col(), 
               names_to = 'Parameter',
               values_to = 'Estimate') %>%
  
  relocate(c('Parameter', 'Estimate'), .after = effect_type) %>%
  
  mutate(Parameter = str_replace_all(Parameter, 
                                     c('tau_avg$' = 'ATT', 'tau_avg_sd' = 'SD', 
                                       'tau_avg_lci_99' = 'LCI', 'tau_avg_hci_99' = 'UCI', 
                                       'pct_att$' = 'ATT (%)', 'pct_att_sd' = 'SD (%)', 
                                       'pct_att_lci_99' = 'LCI (%)', 'pct_att_hci_99' = 'UCI (%)') ) ) 


treat_effects_lng <- treat_effects_lng %>% 
  mutate(estimate_type = case_when(str_detect(Parameter, '(%)') ~ 'ATT (%)', 
                                   TRUE ~ 'ATT') )

att_type_vec <- treat_effects_lng %>% 
  mutate(Sample = factor(Sample, levels = c('Urban', 'Rural'))) %>% 
  distinct(percentage_type, effect_type, Sample) %>% 
  arrange(Sample, effect_type) %>%
  mutate(Sample = as.character(Sample))

att_type_vec_actual <- att_type_vec %>% filter(grepl('Actual', effect_type))
att_type_vec_placebo <- att_type_vec %>% filter(grepl('Placebo', effect_type))
# ---------------------------- #
format_placebo_efx_table <- function(percent_type, efx_type, sample_type, estimate_type_str, att_str){ 
  
  att_filter = paste0('ATT', att_str); print(att_filter)
  sd_filter = paste0('SD', att_str); print(sd_filter)
  
  treat_effects_lng_sel <- treat_effects_lng %>% 
      filter(percentage_type == percent_type &
               effect_type == efx_type &
               Sample == sample_type &
               estimate_type == estimate_type_str)
  
  if (estimate_type_str == 'ATT (%)'){
    treat_effects_lng_sel <- treat_effects_lng_sel %>% 
      mutate(Estimate = Estimate * 100)
  }

   
  att <- treat_effects_lng_sel %>%

    select(-estimate_type) %>%

    pivot_wider(names_from = Parameter,
                values_from = Estimate) %>%

    rename_with(.cols = matches('ATT'),
                \(x) str_replace_all(x, pattern = 'ATT( \\(%\\))?', replacement = 'Estimate') ) %>%

    rename_with(.cols = matches('LCI|UCI'),
                \(x) str_remove_all(x, pattern = '( \\(%\\))?') ) %>%

  mutate(Key = att_filter)

  att <- att %>%
    mutate(
      Estimate_Format = case_when(
        (LCI > 0 & UCI > 0) | (LCI < 0 & UCI < 0) ~ paste0(format(round(Estimate, 4), nsmall = 4), "***"),
        TRUE ~ format(round(Estimate, 4), nsmall = 4)
      )
    ) %>%
    select(Sample, percentage_type, effect_type, Key, Estimate_Format) %>%
    rename('Estimate' = 'Estimate_Format')

  sd <- treat_effects_lng_sel %>%

    filter(Parameter == sd_filter) %>%

    mutate(Estimate = paste0('(', format(round(Estimate, 4), nsmall = 4), ')' ) ) %>% 
    
    mutate(Key = sd_filter) %>%
    
    select(Sample, percentage_type, effect_type, Key, Estimate)

  estimates <- bind_rows(att, sd)

  return(estimates)
  
  
}


# ---------------------------- #
att_type_vec_actual <- expand_grid(att_type_vec_actual, 
                                   Estimate_type = unique(treat_effects_lng$estimate_type)) %>% 
  mutate(att_str = str_extract(Estimate_type, ' \\(%\\)')) %>%
  replace_na(list(att_str = ''))
# ---------------------------- #

# ---------------------------- #
att_type_vec_placebo <- expand_grid(att_type_vec_placebo, 
                                   Estimate_type = unique(treat_effects_lng$estimate_type)) %>% 
  mutate(att_str = str_extract(Estimate_type, ' \\(%\\)')) %>%
  replace_na(list(att_str = ''))
# ---------------------------- #

estimates_actual <- pmap_dfr(list(percent_type = att_type_vec_actual$percentage_type, 
                         efx_type = att_type_vec_actual$effect_type, 
                         sample_type = att_type_vec_actual$Sample, 
                         estimate_type_str = att_type_vec_actual$Estimate_type, 
                         att_str = att_type_vec_actual$att_str), 
                         format_placebo_efx_table)


estimates_placebo <- pmap_dfr(list(percent_type = att_type_vec_placebo$percentage_type, 
                              efx_type = att_type_vec_placebo$effect_type, 
                              sample_type = att_type_vec_placebo$Sample, 
                              estimate_type_str = att_type_vec_placebo$Estimate_type, 
                              att_str = att_type_vec_placebo$att_str), 
                         format_placebo_efx_table)

estimates_placebo_full_sample <- estimates_placebo %>% 
  filter(percentage_type == 'All') %>% 
  rename('Estimate_Placebo_Full' = 'Estimate')

estiamtes_placebo_wo_first_cohort <- estimates_placebo %>% 
  filter(percentage_type != 'All') %>%
  rename('Estimate_Placebo_wo_2006' = 'Estimate')


placebo_efx_table <- estimates_actual %>% 
  left_join(select(estimates_placebo_full_sample, Sample, Key, Estimate_Placebo_Full), 
            by = c('Sample', 'Key') ) %>%
  left_join(select(estiamtes_placebo_wo_first_cohort, Sample, Key, Estimate_Placebo_wo_2006), 
            by = c('Sample', 'Key') )
  

placebo_efx_table <- placebo_efx_table %>% select(Sample, Key, matches('^Estimate'))


placebo_efx_table$Sample[c(2:4, 6:8)] <- ''
placebo_efx_table$Key[seq(2, length(placebo_efx_table$Key), 2)] <- ''

library('kableExtra')

# Create the LaTeX table
placebo_efx_table %>% 
  kable(format = "latex", 
        align = 'lcccc', 
        booktabs = TRUE,
        escape = TRUE, # Note: This is the default.
        digits = 4,
        position = '!h',
        linesep = '',
        centering = TRUE, # Note: This is the default. 
        col.names = c('Sample', 'Parameter', 'Estimate', 'Estimate (Placebo)', 'Estimate (Placebo)'),
        caption = paste0("Placebo Average Treatment Effects of Dollar Store Entry on Low Food Access Status, Block Groups Receiving a Dollar Store from 2006-2020")) %>% 
  kable_styling() %>%
  add_header_above(c(rep('', 2), 'Full Sample' = 2, 'Reduced Sample' = 1)) %>%
  footnote(general = c("Notes: The bootstrapped standard errors below the ATT and ATT% estimates.", 
                       "The placebo ATTs are estimated by moving the treatment date ahead one year of the actual treatment date.",  
                       "The Full Sample placebo ATT includes all treated cohorts.", 
                       "The Reduced Sample placebo ATT includes cohorts treated from 2007 to 2020."), 
           general_title = '', 
           threeparttable = TRUE, 
           escape = TRUE)
