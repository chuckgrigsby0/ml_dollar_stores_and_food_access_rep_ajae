cat('Loaded:', 'Store_Drive_Time_and_Distance_Counts_Function', '\n',
    'To use, loop over list IDs.')
#--------------------------------------------------------------------------------#
Store_Drive_Time_and_Distance_Counts_Function <- function(id, isochrones_data, isochrone_boundary_str){
  
  #----------------------------------------------------------------------------------------------------#
  #Initiate loop by cycling through store-channel data. 
  #----------------------------------------------------------------------------------------------------#
  store_channel <- store_channel_sfp_list[[id]]
  store_names_str <- names(store_channel_sfp_list)[id]
  #----------------------------------------------------------------------------------------------------#
  #The output file has four elements in the outer list. The inner lists have 21 elements. There
  #are 21 inner list elements, one for each year. 
  #----------------------------------------------------------------------------------------------------#
  #The following function finds the stores within e.g., 1-mile, 3-miles, 5-miles, or 10-miles of census-tract/block-group
  #population-weighted centroids. Then takes the output and converts it to a data frame and counts the number 
  #of stores found within the given distance for each census-tract/block-group observation using the lengths() function. 
  #We generically call the variable Count/Density, which we will later rename for each store and buffer zone. 
  #----------------------------------------------------------------------------------------------------#
  Stores_within_Drive_Time_Distance_Function <- function(isochrones_data_arg, store_dta){
    # Find the intersection of block group drive time or distances for each zone and individual stores. 
    bg_intersects_w_stores <- st_intersects(x = isochrones_data_arg, # block group drive-time 1-, 3-, 5-, and 10-miles. 
                                            y = store_dta) # store-year combination. 
    
    count_df = data.frame(Count = lengths(bg_intersects_w_stores)) # lengths() finds the number of stores in each element of the list. 
    count_df$GEOID <- st_drop_geometry(isochrones_data_arg)$GEOID
    count_df <- count_df %>% relocate(GEOID)
    
    return(count_df)
    
  }
  #----------------------------------------------------------------------------------------------------#
  #Apply the function to the buffers and store channel lists. 
  #----------------------------------------------------------------------------------------------------#
  store_counts_raw <- store_channel %>%
        
        map(function(.x){# For each year
          
          Stores_within_Drive_Time_Distance_Function(isochrones_data_arg = isochrones_data, store_dta = .x)
          
        })
  #----------------------------------------------------------------------------------------------------#
  #----------------------------------------------------------------------------------------------------#
  #Create a function that allows you to rename each density list using a prefix and suffix that take the values
  #of the store channel as prefix and 1M, 3M, 5M, or 10M, or 10Min, 5Min, etc. as suffix. 
  #----------------------------------------------------------------------------------------------------#
  #Set up strings to use in the function
  isochrone_boundary_str <- isochrone_boundary_str; isochrone_boundary_str #suffix_arg
  store_names_str #prefix_arg
  #----------------------------------------------------------------------------------------------------#
  Rename_Density_Function <- function(count_dta, prefix_arg, suffix_arg){
    
    count_df <- count_dta %>% 
      
      rename_with(.cols = Count, 
                  .fn = ~paste0(prefix_arg, '_', ., '_', suffix_arg))
    
    return(count_df)
    
  }
  #----------------------------------------------------------------------------------------------------#
  #Notice the use of map2, in order to loop over buffer zone strings and store names, in order to loop over string names
  #and use in the functions. 
  #----------------------------------------------------------------------------------------------------#
  store_counts_raw <- map(store_counts_raw, function(.x){# For each buffer zone. 
                                  
                                  Rename_Density_Function(count_dta = .x, 
                                                          prefix_arg = store_names_str, #Store type prefix. 
                                                          suffix_arg = isochrone_boundary_str) #Buffer zone suffix.   
                                    
                                  })
  #----------------------------------------------------------------------------------------------------#
  #Create a function to combine each store types densities into a single data frame for each year. 
  #----------------------------------------------------------------------------------------------------#
  #Set up strings to use in the function
  year_str <- as.character(seq(2000, (2000+length(store_counts_raw)-1), by = 1)); year_str
  #----------------------------------------------------------------------------------------------------#
  #Add year labels to the lists. 
  #The buffer zones have disappeared because they are combined in the data. 
  #----------------------------------------------------------------------------------------------------#
  store_counts_raw <- set_names(store_counts_raw, nm = year_str)
  #----------------------------------------------------------------------------------------------------#
  return(store_counts_raw)
}
#--------------------------------------------------------------------------------#

