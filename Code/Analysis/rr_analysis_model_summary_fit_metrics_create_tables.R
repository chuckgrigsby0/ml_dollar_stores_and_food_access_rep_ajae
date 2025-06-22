# load packages # 
# -------------------------------------------------------------------------------------------- #
pacman::p_load('here', 'dplyr', 'purrr', 'stringr', 'tidyr', 'kableExtra')
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
read_fit_metrics <- function(model_geog){ 
  
  overall <- readRDS(file = here::here('Analysis', 
                                       'Tables', 
                                       'Low_Access', 
                                       model_geog, 
                                       paste0('overall_fit_metrics_', 
                                              str_to_lower(model_geog), 
                                              '.rds')) )
  
  overall <- overall %>% mutate(Geography = model_geog) 
  names(overall) <- paste0(model_geog, '_', names(overall))
  
  relative_time <- readRDS(file = here::here('Analysis', 
                                       'Tables', 
                                       'Low_Access', 
                                       model_geog, 
                                       paste0('relative_time_fit_metrics_', 
                                              str_to_lower(model_geog), 
                                              '.rds')) )
  
  relative_time <- relative_time %>% mutate(Geography = model_geog)
  
  return(list('overall' = overall, 
              'relatime_time' = relative_time))
  
  }

rural_fit_metrics <- read_fit_metrics(model_geog = 'Rural')
urban_fit_metrics <- read_fit_metrics(model_geog = 'Urban')


overall_fit_metrics <- bind_cols(rural_fit_metrics$overall, urban_fit_metrics$overall) %>%
  select(-matches('_Geography|Urban_Metric|CI$'))

equations <- c('$\\frac{TP + TN}{TP + TN + FP + FN}$', 
               '$\\frac{TP}{TP + FN}$', 
               '$\\frac{TN}{TN + FP}$', 
               '$\\frac{TP}{TP + FP}$', 
               '$\\frac{Sensitivity + Specificity}{2}$',
               '$2 \\cdot \\frac{Precision \\cdot Recall}{Precision + Recall}$', 
               '$\\sqrt{Sensitivity \\cdot Specificity}$', 
               '$\\frac{TP \\cdot TN - FP \\cdot FN}{\\sqrt{(TP + FP)(TP + FN)(TN + FP)(TN + FN)}}$', 
               'Area Under the Receiver Operating Characteristic curve', 
               'Area Under the Precision-Recall curve')

overall_fit_metrics$equations <- equations

# Create the LaTeX table
overall_fit_latex_table <- kable(overall_fit_metrics, 
                     format = "latex", 
                     align = 'lcccc', 
                     booktabs = TRUE,
                     escape = FALSE, # Note: This is the default.
                     digits = 4,
                     position = '!h',
                     linesep = '', # Remove \addlinespace every 5 rows. 
                     centering = TRUE, # Note: This is the default. 
                     col.names = c("Metric", "Estimate", "SD", 'Estimate', 'SD', 'Equation'),
                     caption = paste0("Pre-Treatment Cross-Validation Prediction Performance Metrics")) %>% 
  kable_styling() %>%
  add_header_above(c('', 'Rural' = 2, 'Urban' = 2, '') ) %>%
  footnote(general = c('Notes: We report the bootstrapped standard errors. 
                       TP = True Positives, TN = True Negatives, FP = False Positives, FN = False Negatives'), 
           general_title = '', 
           threeparttable = TRUE, 
           escape = FALSE); overall_fit_latex_table
