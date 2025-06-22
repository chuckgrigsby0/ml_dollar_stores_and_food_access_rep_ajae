print('Sourced: Drug_and_Grocery_Mutate_Function')
# -------------------------------------------------------------------------------------------- #
Drug_and_Grocery_Mutate_Function <- function(retail_store_data, drug_conv_count, drug_pharm_count, 
                                             supermarket_count, natural_grocery_count, 
                                             supercenter_count, la_supermarket_count, 
                                             warehouse_grocery_count, dist_band){
  
  retail_store_data <-  retail_store_data %>% 
    
    mutate('Drug_Count_{dist_band}' := .data[[drug_conv_count]] + .data[[drug_pharm_count]], 
           
           'Grocery_Count_{dist_band}' := .data[[supermarket_count]] + .data[[natural_grocery_count]] + 
             .data[[supercenter_count]] + .data[[la_supermarket_count]] + .data[[warehouse_grocery_count]]) %>%
    
    relocate(starts_with('Drug_Count'), 
             .before = paste0('Drug_Conv_Count_', dist_band)) %>% 
    relocate(starts_with('Grocery_Count'), 
             .before = paste0('Supermarket_Count_', dist_band))
  
  return(retail_store_data)
  
}
# -------------------------------------------------------------------------------------------- #
# test <- map(retail_dta, function(.x){
  #Drug_and_Grocery_Mutate_Function(retail_store_data = .x, 
   #                                drug_conv_count = 'Drug_Conv_Count_10min', 
    #                               drug_pharm_count = 'Drug_Pharm_Count_10min', 
     #                              supermarket_count = 'Supermarket_Count_10min', 
      #                             natural_grocery_count = 'Natural_Grocery_Count_10min', 
       #                            supercenter_count = 'Supercenter_Count_10min', 
        #                           la_supermarket_count = 'La_Supermarket_Count_10min', 
         #                          warehouse_grocery_count = 'Warehouse_Grocery_Count_10min', 
          #                         dist_band = '10min')
#})
# -------------------------------------------------------------------------------------------- #