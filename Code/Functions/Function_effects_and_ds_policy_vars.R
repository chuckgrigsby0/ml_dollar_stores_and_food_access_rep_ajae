print('Sourced: effects_and_ds_policies <- function(df, bootstrap_idx)')

effects_and_ds_policies <- function(df, bootstrap_idx){ 
  
  # Subset the post-treatment data. 
  posttre_effects <- df %>% 
    
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
# Regress treatment effects on policy type. 
policy_vars <- c('Defeated_and_Restrictions_binary', 'Restrictions_and_Defeated_binary')

reg_policy_type_function <- function(df, ind_var){ 
  
  df_subset <- df %>% 
    
    filter(No_Policy_binary == 1 | .data[[ind_var]] == 1)
  
  reg_policy_type <- feols(xpd(tau ~ No_Policy_binary + .[ind_var] - 1), data = df_subset)
  
  reg_policy_type_table <- reg_policy_type$coeftable
  
  reg_policy_type_table <- tibble::rownames_to_column(reg_policy_type_table, 'variable') 
  
  reg_policy_type_table$variable <- reg_policy_type_table$variable %>% 
    str_replace_all(pattern = c('_' = ' ', 'binary' = '')) %>% 
    str_trim(., side = 'right') %>% 
    str_replace_all(pattern = c('Defeated and Restrictions' = 'Defeated/Defeated and Restrictions', 
                                'Restrictions and Defeated' = 'Restrictions/Restrictions and Defeated', 
                                'No Policy' = 'No Defeat or Restriction'))
  
  reg_policy_type_table <- reg_policy_type_table %>%
    mutate(
      bootstrap_id = bootstrap_idx, 
      outcome = model_dep_var, 
      Geography = model_geography, 
      reg_type = 'policy_type'
    )
  
  return(reg_policy_type_table)
  
}
# -------------------------------------------------------------------------------------------- #

reg_policy_type_table <- map_dfr(policy_vars, function(.x) { 
  
  reg_policy_type_function(df = posttre_effects_w_policy_vars_binned, ind_var = .x)
  
  })
# This will keep only one of the No Defeat or Restriction estimates, 
# since they are the same across both regressions.
reg_policy_type_table <- reg_policy_type_table %>% group_by(variable) %>% filter(row_number() == 1)
# -------------------------------------------------------------------------------------------- #
# Regress treatment effects on binary policy variable. 
reg_policy_binary <- feols(xpd(tau ~ -1 + policy_total_binary_bins), 
                          data = posttre_effects_w_policy_vars_binned)

reg_policy_binary_table <- (reg_policy_binary$coeftable)
reg_policy_binary_table <- tibble::rownames_to_column(reg_policy_binary_table, 'variable') 
reg_policy_binary_table <- reg_policy_binary_table %>% 
  mutate(variable = case_when(
    variable == "policy_total_binary_bins0" ~ "No Defeat or Restriction",
    variable == "policy_total_binary_bins1" ~ "At Least One Defeat and/or Restriction",
    TRUE ~ as.character(variable)  # Keep other values unchanged
  ), 
  bootstrap_id = bootstrap_idx, 
  outcome = model_dep_var, 
  Geography = model_geography, 
  reg_type = 'policy_total_binary')
# -------------------------------------------------------------------------------------------- #
# Regress treatment effects on discrete-valued policy variables. 
reg_policy_total <- feols(xpd(tau ~ -1 + policy_total_bins), 
                          data = posttre_effects_w_policy_vars_binned)

reg_policy_total_table <- (reg_policy_total$coeftable)
reg_policy_total_table <- tibble::rownames_to_column(reg_policy_total_table, 'variable') 
reg_policy_total_table <- reg_policy_total_table %>% 
  mutate(variable = case_when(
    variable == "policy_total_bins0"   ~ "No Defeat or Restriction",
    variable == "policy_total_bins1"   ~ "One Defeat or Restriction",
    variable == "policy_total_bins> 1" ~ "More Than One Defeat and/or Restriction",
    TRUE ~ as.character(variable)  # Keep other values unchanged
  ), 
  bootstrap_id = bootstrap_idx, 
  outcome = model_dep_var, 
  Geography = model_geography, 
  reg_type = 'policy_total')
# -------------------------------------------------------------------------------------------- #
# Regress treatment effects on binary policy year variables  
policy_year_bin_vars <- str_subset(names(posttre_effects_w_policy_vars_binned), 
                                   pattern = 'policy_year_.*_bins$')

factor_levels_policy_years <- map(select(posttre_effects_w_policy_vars_binned, all_of(policy_year_bin_vars)), 
                                  function(.x) levels(.x))

one_level_vars <- which(lengths(factor_levels_policy_years) <= 1)

policy_year_bin_vars <- policy_year_bin_vars[!policy_year_bin_vars %in% names(one_level_vars)]

reg_policy_year <- feols(xpd(tau ~ 1 + .[policy_year_bin_vars]), 
                         data = posttre_effects_w_policy_vars_binned )


reg_policy_year_table <- (reg_policy_year$coeftable)

reg_policy_year_table <- tibble::rownames_to_column(reg_policy_year_table, 'variable') 

reg_policy_year_table$variable <- str_extract(reg_policy_year_table$variable, pattern = "(?<=policy_year_)\\d+")

reg_policy_year_table <- reg_policy_year_table %>% 
  mutate(across(.cols = variable, .fns = \(x) if_else(is.na(x), 'No Defeat or Restriction', x) ),
  bootstrap_id = bootstrap_idx, 
  outcome = model_dep_var, 
  Geography = model_geography, 
  reg_type = 'policy_year')
# -------------------------------------------------------------------------------------------- #
# Combine estimates. 
# -------------------------------------------------------------------------------------------- #
reg_policy_all_list <- bind_rows(reg_policy_binary_table, reg_policy_type_table, 
                                 reg_policy_total_table, reg_policy_year_table)

reg_policy_all_list <- reg_policy_all_list %>% rename(sd = `Std. Error`, 
                                                      estimate = Estimate)
# -------------------------------------------------------------------------------------------- #
return(reg_policy_all_list)

}