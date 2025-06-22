cat('Loaded:', 'Store_Densities_Distances_Tract_Counts_Function.', '\n',
    'To use, loop over list IDs.')
#--------------------------------------------------------------------------------#
Store_Densities_Distances_Tract_Counts_Function <- function(id){

#----------------------------------------------------------------------------------------------------#
#Initiate loop by cycling through store-channel data. 
#----------------------------------------------------------------------------------------------------#
store_channel <- store_channel_sfp_list[[id]]
store_names_str <- names(store_channel_sfp_list)[id]
#----------------------------------------------------------------------------------------------------#
#The output file has four elements in the outer list. The inner lists have 21 elements. There
#are 21 inner list elements, one for each year. 
#----------------------------------------------------------------------------------------------------#
#The following function finds the stores within e.g., 1-mile, 3-miles, 5-miles, or 10-miles of block-group
#population-weighted centroids. Then takes the output and converts it to a data frame and counts the number 
#of stores found within the given distance for each block-group observation using the lengths() function. 
#We generically call the variable Density, which we will later rename for each store and buffer zone. 
#----------------------------------------------------------------------------------------------------#
Stores_within_BG_Buffers_Function <- function(bg_buffers_dta, store_dta){
  # Find the intersection of block group buffers for each buffer zone and individual stores. 
  # Find stores within buffers. 
  bg_intersects_w_stores <- st_intersects(x = bg_buffers_dta, # block group buffer 1-, 3-, 5-, and 10-miles. 
                                          y = store_dta) # store-year combination. 
  
  density_df = data.frame(Density = lengths(bg_intersects_w_stores)) # lengths() finds the number of stores in each element of the list. 
  
  return(density_df)
  
}
#----------------------------------------------------------------------------------------------------#
#Apply the function to the buffers and store channel lists. 
#----------------------------------------------------------------------------------------------------#
bg_store_counts_raw <- bg_buffers_10 %>%
  
  map(function(.x){# For each buffer zone.
    
    store_channel %>%
      
      map(function(.y){# For each year
            
            Stores_within_BG_Buffers_Function(bg_buffers_dta = .x, 
                                              store_dta = .y)
            
          })
            
        })
#----------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------#
#Create a function that allows you to rename each density list using a prefix and suffix that take the values
#of the store channel as prefix and 1M, 3M, 5M, or 10M, as suffix. 
#----------------------------------------------------------------------------------------------------#
#Set up strings to use in the function
density_str <- names(bg_buffers_10); density_str #suffix_arg
store_names_str #prefix_arg
#----------------------------------------------------------------------------------------------------#
Rename_Density_Function <- function(density_dta, prefix_arg, suffix_arg){
  
  density_df <- density_dta %>% 
    
    rename_with(.cols = Density, 
                .fn = ~paste0(prefix_arg, '_', ., '_', suffix_arg))
  
  return(density_df)
  
}
#----------------------------------------------------------------------------------------------------#
#Notice the use of map2, in order to loop over buffer zone strings and store names, in order to loop over string names
#and use in the functions. 
#----------------------------------------------------------------------------------------------------#
bg_store_counts_raw <- map2(bg_store_counts_raw, density_str, 
                            function(.x, .y){# For each buffer zone. 
                              
                              .x %>%
                                
                                map(function(.z){# For each year. 
                                  
                                      Rename_Density_Function(density_dta = .z, 
                                                              prefix_arg = store_names_str, #Store type prefix. 
                                                              suffix_arg = .y) #Buffer zone suffix.   
                                      
                                    })
                                       
                                  })
#----------------------------------------------------------------------------------------------------#
#Create a function to combine each store types densities into a single data frame for each year. 
#----------------------------------------------------------------------------------------------------#
#Set up strings to use in the function
year_str <- as.character(seq(2000, (2000+length(bg_store_counts_raw$`1M`)-1), by = 1)); year_str
store_names_str # Already created the store name string above. Printed here. 
#----------------------------------------------------------------------------------------------------#
Combine_Density_Function <- function(density_dta, year){
  #bind_cols() is safe to use here because the rows in block-group data are the same regardless of buffer zone or store channel/type. 
  density_df <- bind_cols(density_dta$`1M`[[year]], density_dta$`3M`[[year]], 
                          density_dta$`5M`[[year]], density_dta$`10M`[[year]])
  
  return(density_df)
  
}
#----------------------------------------------------------------------------------------------------#
#Apply the density function. 
#----------------------------------------------------------------------------------------------------#
bg_store_counts_raw_cmb <- year_str %>% 
      
      map(function(.y){# For each year. 
        
        Combine_Density_Function(density_dta = bg_store_counts_raw, 
                                 year = .y)
        
      })
#----------------------------------------------------------------------------------------------------#
#Add year labels to the lists. 
#The buffer zones have disappeared because they are combined in the data. 
#----------------------------------------------------------------------------------------------------#
bg_store_counts_raw_cmb <- set_names(bg_store_counts_raw_cmb, nm = year_str)
#----------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------#
#Finally, combine the block group data containing population shares with the store density data to compute
#population-weighted densities at the census tract level based on the year 2010 census tract geographies. 
#----------------------------------------------------------------------------------------------------#
store_densities <- bg_store_counts_raw_cmb %>% 
  
  map(function(.x){ # For each year. 
    
    #cbind/merge the block-group centroid data that contains the population-share data and GEOID information. 
    
    bind_cols(.x, # year i
              st_drop_geometry(bg_pop_centroids_10_sfp_shares)) %>% # BG population centroid data. 
      
      mutate(across(.cols = contains('Density'), 
                    .fns = ~.x*POPULATION_SHARES)) %>% # BG population shares per tract. 
      
      group_by(GEOID) %>% 
      
      summarise(across(.cols = contains('Density'), 
                       .fns = sum))
    
    
  }) 
#----------------------------------------------------------------------------------------------------#
#Stores within tracts. 
#----------------------------------------------------------------------------------------------------#
Stores_Within_Tract_Function <- function(store_dta, store_label){
  
  #Find stores inside polygons/census tracts. 
  
  intersects_dta <- st_intersects(x = tracts_2010_sfp, y = store_dta)
  
  #Compute store counts within each census tract. 
  stores_within_tract <- data.frame(within_tract = lengths(intersects_dta))
  
  #Rename the within_tract variable by adding a store-channel prefix. 
  stores_within_tract <- stores_within_tract %>% 
    
    rename_with(.cols = within_tract, 
                .fn = ~paste0(store_label, '_', .))
  
  #Combine year 2010 census tract data, selecting only the GEOID and the within tract count. 
  
  stores_within_tract <- bind_cols(st_drop_geometry(tracts_2010_sfp), 
                                   stores_within_tract) %>%
    select(GEOID, contains('within_tract')) #Select these columns only. 
  
  return(stores_within_tract)
  
}
#----------------------------------------------------------------------------------------------------#
store_counts_within_tract <- map(store_channel, 
                                   
                                  function(.x){# For each year. 
                                    
                                        Stores_Within_Tract_Function(store_dta = .x, 
                                                                     store_label = store_names_str)
                                        
                                      })
#----------------------------------------------------------------------------------------------------#
#Nearest Neighbor
#--------------------------------------------------------------------------------#
#The following function simultaneously finds the nearest neighbor store, its distances, 
#and converts the distance to meters and then miles before multiplying by the population shares 
#represented by each block group. The population-weighted average distance for each census tract
#is computed by summing up at the census-tract level. 
#--------------------------------------------------------------------------------#
Nearest_Neighbors_Function <- function(bg_pop_centorids_yr, stores_yr, store_label){
  
  output <- tryCatch({#Do the following tasks if the function works. 
    
  #Find the nearest store to each block group centroid. 
  #Setting returnDist = TRUE allows you to obtain a second list element, the distances, in meters. 
  nn <- st_nn(bg_pop_centorids_yr, stores_yr, returnDist = TRUE)
  
  #Use the block-group centroid data, but drop the geometry column. 
  bg_pop_centroids_dist <- st_drop_geometry(bg_pop_centorids_yr) %>% 
    
    #Use the nn$dist object found above for each block-group indicating the distance
    #from the block-group centroid to the nearest store. 
    
    #Convert units to meters. 
    mutate(dist_nn_m = units::set_units(as.numeric(nn$dist), 'm')) %>%
    
    #Convert units to miles. 
    mutate(dist_nn_mi = units::set_units(dist_nn_m, 'mi')) %>%
    
    #Drop units
    mutate(dist_nn = units::drop_units(dist_nn_mi)) %>%
    
    #Multiply by the block-group population share variables. 
    mutate(dist_nn_wt = dist_nn*POPULATION_SHARES) %>%
    
    group_by(GEOID) %>% 
    
    #To obtain the population-weighted census-tract average distance to nearest store. 
    summarise(across(.cols=c(dist_nn_wt), .fns=sum))
  
  #Rename the data frame with a store_level_prefix. 
  #Notice the use of str_to_lower in renaming the distance for each channel. 
  
  bg_pop_centroids_dist <- bg_pop_centroids_dist %>%
    rename_with(.cols = dist_nn_wt, 
                .fn = ~paste0(str_to_lower(store_label), '_', .)) 
  
  return(bg_pop_centroids_dist)}, #End of task if the function works. 
  #--------------------------------------------------------------------------------#
  error = function(cond){#Emit the following error message if the task does not work. 
    message(paste("There are zero store observations, so nearest neighbors cannot be found for", store_label))
    message(paste(cond))
    #Choose a return value in case of error
    return(NULL)}, 
  #--------------------------------------------------------------------------------#  
  warning = function(cond){
    message(paste(cond))
    #Choose a return value in case of error
    return(NULL)
  })
  #--------------------------------------------------------------------------------#
  return(output)
  #--------------------------------------------------------------------------------#
  } #End function. 

#--------------------------------------------------------------------------------#
#Apply the function to the store_channel_sfp_list data and use the store_names_str object for the store_label prefixes. 
#--------------------------------------------------------------------------------#
store_nn <- map(store_channel, 
                 function(.x){# For each year.  
                   
                       Nearest_Neighbors_Function(bg_pop_centorids_yr = bg_pop_centroids_10_sfp_shares, #Held fixed for each year.
                                                  stores_yr = .x, 
                                                  store_label = store_names_str)
                     })
#--------------------------------------------------------------------------------#
#Combine the lists into a list of three elements. 
#--------------------------------------------------------------------------------#
store_data <- list(store_densities = store_densities, 
                   store_distances_dta = store_nn, 
                   store_counts_within_tract = store_counts_within_tract)
#--------------------------------------------------------------------------------#
return(store_data)
}
#--------------------------------------------------------------------------------#

