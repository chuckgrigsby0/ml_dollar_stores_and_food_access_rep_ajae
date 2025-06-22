# -------------------------------------------------------------------------------------------- #
tidy_covar_names <- function(covar_name_str){
  
  covar_name_str <- str_to_title(str_replace_all(covar_name_str, pattern = '_', replacement = ' ')) 
  # Old name = New name. 
  covar_name_str <- str_replace_all(covar_name_str, c('18 34' = '18-34', '35 64' = '35-64', '65 Over' = '>= 65',
                                                      'Age' = 'Population Share Age',
                                                      'Commute Public' = 'Share Commuting with Public Transportation', 
                                                      'Commute Private' = 'Share Commuting with Private Vehicle', 
                                                      'Educ$' = "Share with >= Bachelor's Degree", 
                                                      'Educ Lhs' = "Share with < High School Diploma",
                                                      'Inc Per' = 'Income Per',
                                                      'No Vehicle' = 'Share without Private Vehicle', 
                                                      'Pop ' = 'Population Share ', 
                                                      'Public Assistance' = 'Population Share on Public Assistance', 
                                                      'Unemployed' = 'Share Unemployed', 
                                                      'Vacant Housing' = 'Share of Vacant Housing', 
                                                      ' Count 10mile' = '', 
                                                      'Ds' = 'Dollar Stores', 
                                                      'Convenience' = 'Convenience Stores',
                                                      'Drug' = 'Drug Stores',
                                                      'Gen' = 'General', 
                                                      'Merch' = 'Merchandisers', 
                                                      'Wholesale Club' = 'Wholesale Club Stores',
                                                      'Superette' = 'Superettes',
                                                      'Grocery' = 'Grocery Stores',
                                                      '2005' = '(2005)', 
                                                      'Distance Urban' = 'Distance to Urban Area (Miles)', 
                                                      ' Tot Road Length Mi' = '', 
                                                      'Ushwy' = 'U.S. Highway', 
                                                      '^State$' = 'State Highway', 
                                                      'Interstate' = 'Interstate Highway', 
                                                      'County' = 'County Highway', 
                                                      'Other' = 'Other Highway', 
                                                      'Highway' = 'Highway (Miles)', 
                                                      'Fe State' = 'State Fixed Effects',
                                                      'Fe Year' = 'Year',
                                                      ' Low Access' = '', 
                                                      ' Perm' = '', ' Pers' = '', 
                                                      'Frac [[0-9]]+' = '', 
                                                      '7nn' = '', 
                                                      '^Year$' = 'Year Fixed Effects', 
                                                      'r{1} X S{1}' = 'r-x-S', 
                                                      'State$' = 'State Fixed Effects', 
                                                      'School Count Pub 10mile' = 'Public Schools', 
                                                      'School Count Priv 10mile' = 'Private Schools', 
                                                      'Urban Area' = 'Urbanized Area', 
                                                      'Uc Area' = 'Urban Cluster Area', 
                                                      'Net Entry Cumsum' = 'Dollar Store Entry (Net)',
                                                      'Gross Entry Cumsum' = 'Dollar Store Entry (Gross)',
                                                      'Entry Events' = 'Dollar Store Entry', 
                                                      'Planted Cultivated' = 'Planted-Cultivated', 
                                                      'Total Population' = 'Population', 
                                                      'Park Access' = 'Park Distance'))
  
  covar_name_str <- str_trim(covar_name_str, side = 'both')
  
  return(covar_name_str)
  
}
