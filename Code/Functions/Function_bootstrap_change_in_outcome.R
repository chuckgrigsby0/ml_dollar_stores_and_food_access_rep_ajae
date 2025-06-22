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
  
  posttre_effects <- posttre_effects %>% 
    
    left_join(select(posttr_binned_covars, all_of(join_sel_vars), 
                     poverty_rate_bins, pop_black_bins, 
                     vacant_housing_bins, public_assistance_bins, 
                     no_vehicle_bins), by = join_sel_vars, 
              multiple = 'all', relationship = 'many-to-one' )
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
  # Heterogeneity across select covariates: Overall percentage change in the ATT and by year.
  # -------------------------------------------------------------------------------------------- #
  outcome_pct_change_heterogeneity <- function(df, covariate, start_pctl, end_pctl){ 
    
    start_pctl <- start_pctl
    end_pctl <- end_pctl
    var_levels <- levels(df[[covariate]])
    start_level <- ceiling(length(var_levels) * start_pctl)
    end_level <- ceiling(length(var_levels) * end_pctl)
    var_levels_sel <- var_levels[start_level:end_level]
    
    subset_df <- df %>%

      filter(.data[[covariate]] %in% var_levels_sel)
    
    print(nrow(subset_df))
    
    outcome_pct_change_covar_year <- subset_df %>% 
      
      group_by(year) %>%
      
      summarise( across(.cols = c(tau, preds, actual), 
                        .fn = list(avg = mean), 
                        .names = '{.col}_{.fn}') ) %>%
      
      mutate(pct_att = tau_avg/preds_avg, 
             id = iter, 
             percentage_type = 'Year', 
             variable = covariate)
    
    
    outcome_pct_change_covar_att <- subset_df %>% 
      
      summarise( across(.cols = c(tau, preds, actual), 
                        .fn = list(avg = mean), 
                        .names = '{.col}_{.fn}') ) %>%
      
      mutate(pct_att = tau_avg/preds_avg, 
             id = iter, 
             percentage_type = 'All', 
             variable = covariate)
    
    combined_results <- bind_rows(outcome_pct_change_covar_year, 
                                  outcome_pct_change_covar_att)
    
    return(combined_results)

    
    }
  
  hetero_vars_sel <- c('poverty_rate_bins', 'pop_black_bins', 
                       'vacant_housing_bins', 'public_assistance_bins', 
                       'no_vehicle_bins')
  
  outcome_pct_change_het <- map_dfr(hetero_vars_sel, 
                                    function(.x){ 
                                      
                                      outcome_pct_change_heterogeneity(df = posttre_effects, 
                                                                       covariate = .x, 
                                                                       start_pctl = 0.7, end_pctl = 1)
                                      })
  # -------------------------------------------------------------------------------------------- #
  outcome_change <- bind_rows(outcome_pct_change_relyear, 
                              outcome_pct_change_year, 
                              outcome_pct_change_att, 
                              outcome_pct_change_het) 
  # -------------------------------------------------------------------------------------------- #
  return(outcome_change)
  
}


