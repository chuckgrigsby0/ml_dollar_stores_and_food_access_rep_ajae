# Function that creates entry event information for a given variable. 
print('Sourced: Make_Entry_Events')
# -------------------------------------------------------------------------------------------- #
Make_Entry_Events <- function(entry_dta, treat_var){
  
  entry_dta <- entry_dta %>%
    
    select(GEOID, year, all_of(treat_var)) %>%
    
    group_by(GEOID) %>%
    
    mutate(entry = if_else(.data[[treat_var]] > 0, 1, 0), 
           entry_events = cumsum(entry), 
           event_year = case_when(entry == 1 & entry_events == 1 ~ as.numeric(year), 
                                  TRUE ~ NA_real_) ) %>%
    
    group_by(GEOID) %>%
    
    fill(event_year, .direction = 'updown') %>% # For each unit, fill the cells up and down with the year of first entry. 
    
    mutate(rel_year = (as.numeric(year) - event_year), # Compute indices of the relative year of entry. 
           
           rel_year = if_else(is.na(rel_year), Inf, rel_year), # For untreated units, assign Inf values to the cells. 
           
           treat = case_when(is.infinite(rel_year) ~ FALSE, # When the rel_year is Inf, assign treat = FALSE. 
                             rel_year < 0 ~ FALSE, # When yet-to-be treated assign treat = FALSE. 
                             rel_year >= 0 ~ TRUE), # When rel_year = 0 or positive, then TRUE. 
           
           event_year = if_else(is.na(event_year), 0, event_year) ) # When event_year is NA, then 0, else event_year. 
  
}
# -------------------------------------------------------------------------------------------- #