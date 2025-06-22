# -------------------------------------------------------------------------------------------- #
# Function that combines the data sources for either untreated or treated observations. 
print('Sourced: Combine_Data_Function')
# -------------------------------------------------------------------------------------------- #
Combine_Data_Function <- function(dta, panel_df_access_inds_x_and_ymile, retail_counts_2005_x_and_ymile, geography_str){ # Treated or untreated data. 

  dta <- list(dta, # Untreated/Treated dollar store entry data with relative treatment timing. 
              panel_df_access_inds_x_and_ymile, # Low-access indicators
              land_use, 
              covar_df_sel_jn) %>% # Covariates from census/acs. 
    reduce(left_join, by = c('GEOID', 'year')) %>%
    ungroup() %>%
    left_join(retail_counts_2005_x_and_ymile, by = 'GEOID') %>% 
    left_join(park_access, by = 'GEOID') %>%
    left_join(schools, by = 'GEOID') %>%
    left_join(dist_to_urb, by = 'GEOID') %>% 
    left_join(roads, by = 'GEOID') %>%
    left_join(geog_data, by = 'GEOID') %>%
    # --------------------- #
    left_join(fes_state_by_time, by = c('year', 'STATE')) %>%
    # --------------------- #
    mutate(urban_area = if_else(Geography == 'Urbanized', 1, 0), 
           uc_area = if_else(Geography == 'Urban Cluster', 1, 0)) %>%
    mutate(across(.cols = matches('^fe_'), 
                  .fns = ~if_else(is.na(.), 0, .)))  
  
  dta <- dta %>% filter(grepl(geography_str, Geography))
  
  if (model_geography == 'Urban'){ 
    dta <- dta %>% select(-(uc_area))
  } else if (model_geography == 'Rural'){ 
    dta <- dta %>% select(-c(urban_area, uc_area))
    }
  
  return(dta)
  
}
# -------------------------------------------------------------------------------------------- #