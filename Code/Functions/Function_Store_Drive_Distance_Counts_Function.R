print('Sourced: Store_Drive_Distance_Counts_Function <- function(retail_loc_dta, 
                                                 filter_year, filter_retailer, 
                                                 isochrones_data, 
                                                 isochrone_boundary_str)')

Store_Drive_Distance_Counts_Function <- function(retail_loc_dta, 
                                                 filter_year, filter_retailer, 
                                                 isochrones_data, 
                                                 isochrone_boundary_str, 
                                                 p){
  
  
  p()
  
  retail_loc_dta_sub <- retail_loc_dta %>% filter(year == filter_year & Store.Type == filter_retailer)
    
    bg_intersects_w_stores <- st_intersects(x = isochrones_data, 
                                            y = retail_loc_dta_sub)
    
    store_count_df <- data.frame(Count = lengths(bg_intersects_w_stores) )
    
    store_count_df <- store_count_df %>% mutate(GEOID = st_drop_geometry(isochrones_data)$GEOID, 
                                                year = filter_year)
    
    
    store_count_df <- store_count_df %>% 
      
      rename_with(.cols = Count, 
                  .fn = \(x) str_to_lower(paste0(x, 
                                                 '_', 
                                                 str_replace_all(filter_retailer, '( |/|-)', '_'),
                                                 '_', 
                                                 isochrone_boundary_str) ) ) %>%
      relocate(c(GEOID, year))
    
    
    return(store_count_df)
    
}

    