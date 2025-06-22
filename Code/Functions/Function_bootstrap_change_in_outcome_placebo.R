print('Sourced: bootstrap_change_in_outcomes <- function(bootstrap_ids, iter)')

bootstrap_change_in_outcomes <- function(dta, iter){ 
# dta <- model_preds; iter = bootstrap_id
  # -------------------------------------------------------------------------------------------- #
  # Net increase in low-access (i.e., Net increase of grocery exit)
  # Relative time at period 0 and greater implies years from 2006-2020. 
  # -------------------------------------------------------------------------------------------- #
  posttre_effects <- dta %>% 
    filter(rel_year >= 0) %>% 
    filter(grepl(model_geography, Geography)) %>%
    mutate(rel_year = factor(rel_year))
  
  join_sel_vars <- c('GEOID', 'year', 'event_year', 'rel_year')
  # -------------------------------------------------------------------------------------------- #
  # Net increase. 
  # tau includes c(1, -1, 0), where 
  # 1 indicates increase in low access (counterfactual predicts not low access but actually low-access), 
  # -1 indicates decrease in low access (counterfactual predicts low access but actually not low-access), 
  # 0 indicates no change. 
  # -------------------------------------------------------------------------------------------- #
  
  # Percentage change in the ATT by relative time. 
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_relyear <- posttre_effects %>% 
    
    group_by(rel_year) %>%
    
    summarise( across(.cols = c(tau, preds, actual), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att = tau_avg/preds_avg, 
           id = iter, 
           percentage_type = 'Relative Time')
  # -------------------------------------------------------------------------------------------- #
  # Percentage change in the ATT by year. 
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_year <- posttre_effects %>% 
    
    group_by(year) %>%
    
    summarise( across(.cols = c(tau, preds, actual), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att = tau_avg/preds_avg, 
           id = iter, 
           percentage_type = 'Year')
  
  # -------------------------------------------------------------------------------------------- #
  # Overall percentage change in the ATT.
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_att <- posttre_effects %>% 
    
    summarise( across(.cols = c(tau, preds, actual), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att = tau_avg/preds_avg, 
           id = iter, 
           percentage_type = 'All')
  
  # -------------------------------------------------------------------------------------------- #
  
  outcome_pct_change_relyear_wo_cohort <- posttre_effects %>% 
    
    filter(event_year != 2006) %>%
    
    group_by(rel_year) %>%
    
    summarise( across(.cols = c(tau, preds, actual), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att = tau_avg/preds_avg, 
           id = iter, 
           percentage_type = 'Relative-Time-without-first-cohort')
  # -------------------------------------------------------------------------------------------- #
  # Percentage change in the ATT by year. 
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_year_wo_cohort <- posttre_effects %>% 
    
    filter(event_year != 2006) %>%
    
    group_by(year) %>%
    
    summarise( across(.cols = c(tau, preds, actual), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att = tau_avg/preds_avg, 
           id = iter, 
           percentage_type = 'Year-without-first-cohort')
  
  # -------------------------------------------------------------------------------------------- #
  # Overall percentage change in the ATT.
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_att_wo_cohort <- posttre_effects %>% 
    
    filter(event_year != 2006) %>%
    
    summarise( across(.cols = c(tau, preds, actual), 
                      .fn = list(avg = mean), 
                      .names = '{.col}_{.fn}') ) %>%
    
    mutate(pct_att = tau_avg/preds_avg, 
           id = iter, 
           percentage_type = 'All-without-first-cohort')
  
 
  # -------------------------------------------------------------------------------------------- #
  outcome_change <- bind_rows(outcome_pct_change_relyear, 
                              outcome_pct_change_year, 
                              outcome_pct_change_att, 
                              outcome_pct_change_relyear_wo_cohort, 
                              outcome_pct_change_year_wo_cohort, 
                              outcome_pct_change_att_wo_cohort) 
  # -------------------------------------------------------------------------------------------- #
  return(outcome_change)
  
}


