# Function used to determine when we begin tracking dollar store entries. 
print('Sourced: First_Treatment_Year')

First_Treatment_Year <- function(start_year, ds_panel_dta, dist_band){
  
  if (start_year == '2005' & dist_band == '5mile'){
    
    ds_panel_dta_new <- ds_panel_dta %>%
      group_by(GEOID) %>%
      mutate(DS_Count_5mile_diff = case_when(as.numeric(year) <= 2005 ~ 0, 
                                             TRUE~DS_Count_5mile_diff))
  }
  
  
  if (start_year == '2005' & dist_band == '10mile'){
    
    ds_panel_dta_new <- ds_panel_dta %>%
      group_by(GEOID) %>%
      mutate(DS_Count_10mile_diff = case_when(as.numeric(year) <= 2005 ~ 0, 
                                             TRUE~DS_Count_10mile_diff))
  }
  
  if (start_year == '2005' & dist_band == '10min'){
    
    ds_panel_dta_new <- ds_panel_dta %>%
      group_by(GEOID) %>%
      mutate(DS_Count_10min_diff = case_when(as.numeric(year) <= 2005 ~ 0, 
                                             TRUE~DS_Count_10min_diff))
  }
  
  
  else if (start_year == '2000'){
    
    ds_panel_dta_new <- ds_panel_dta
    
  }
  
  return(ds_panel_dta_new)
  

}
