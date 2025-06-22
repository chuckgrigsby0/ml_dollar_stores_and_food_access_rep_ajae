pacman::p_load('dplyr', 'tidyr', 'purrr', 'stringr','kableExtra')
# -------------------------------------------------- #
print('Sourced: create_table_est_mult_thresholds')
create_table_est_mult_thresholds <- function(est_type){
  
  target_dir <- paste0('Analysis/Tables/Low_Access/Rural'); target_dir
  rural_dta <- readRDS(file = here::here(target_dir, paste0('rural_', est_type, '_w_multiple_thresholds.rds') ) )
  # -------------------------------------------------- #
  target_dir <- paste0('Analysis/Tables/Low_Access/Urban'); target_dir
  urban_dta <- readRDS(file = here::here(target_dir, paste0('urban_', est_type, '_w_multiple_thresholds.rds') ) )
  # -------------------------------------------------- #
  
  add_geography <- function(dta, geog_str){ 
    
    dta_w_geog <- dta %>%
      mutate(
        Sample = geog_str
      ) %>%
      relocate(Sample)
    
    # dta_w_geog$Sample[-1] <- ''
    
    return(dta_w_geog)
  }
  
  rural_dta <- rural_dta %>% add_geography(dta = ., geog_str = 'Rural')
  urban_dta <- urban_dta %>% add_geography(dta = ., geog_str = 'Urban')
  
  
  dta_comb <- bind_rows(urban_dta, rural_dta)
  
  dta_comb <- dta_comb %>% 
    mutate(
      across(.cols = everything(), 
             .fn = \(x) if_else(is.na(x), '', x))
    )
  
  
  return(dta_comb)
}
