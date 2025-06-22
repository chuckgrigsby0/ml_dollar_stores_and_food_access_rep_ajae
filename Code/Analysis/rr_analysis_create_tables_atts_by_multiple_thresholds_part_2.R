# Did not use in final revisions. See rr_analysis_create_tables_cv_error_and_atts_by_multiple_thresholds.R, 
# which combines CV Errors and ATTs from both Urban and Rural areas. 
# -------------------------------------------------- #
pacman::p_load('dplyr', 'tidyr', 'purrr', 'stringr')
# -------------------------------------------------- #
target_dir <- paste0('Analysis/Tables/Low_Access/Rural'); target_dir
rural_atts <- readRDS(file = here::here(target_dir, 'rural_att_w_multiple_thresholds.rds') )
# -------------------------------------------------- #
target_dir <- paste0('Analysis/Tables/Low_Access/Urban'); target_dir
urban_atts <- readRDS(file = here::here(target_dir, 'urban_att_w_multiple_thresholds.rds') )
# -------------------------------------------------- #

add_geography <- function(dta, geog_str){ 
  
  dta_w_geog <- dta %>%
    mutate(
      Sample = geog_str
    ) %>%
    relocate(Sample)
  
  dta_w_geog$Sample[-1] <- ''
  
  return(dta_w_geog)
}

rural_atts <- rural_atts %>% add_geography(dta = ., geog_str = 'Rural')
urban_atts <- urban_atts %>% add_geography(dta = ., geog_str = 'Urban')


atts_comb <- bind_rows(urban_atts, rural_atts)

atts_comb <- atts_comb %>% 
  mutate(
    across(.cols = everything(), 
           .fn = \(x) if_else(is.na(x), '', x))
  )
# Create the LaTeX table
library(kableExtra)
atts_comb_table <- kable(atts_comb, 
                                 format = "latex", 
                                 align = 'lcccccc', 
                                 booktabs = TRUE,
                                 escape = TRUE, # Note: This is the default.
                                 digits = 4,
                                 position = '!h',
                                 linesep = '', # Remove \addlinespace every 5 rows. 
                                 centering = TRUE, # Note: This is the default. 
                                 col.names = colnames(atts_comb),
                                 caption = paste0("Average Treatment Effects of Dollar Store Entry on Low Food Access Status Using Multiple Thresholds, 
                                                  Block Groups Receiving a Dollar Store from 2006-2020")) %>% 
  kable_styling() %>%
  footnote(general = c("Notes: We report the bootstrapped standard errors.",
                       "The J-Index (Youden's J-Statistic) is equal to Sensitivity + Specificity - 1, while the 
                       F1 is calculated as $\\frac{2 \\times \\text{Precision} \\times \\text{Recall}}{\\text{Precision} + \\text{Recall}}$.", 
                       "Optimal probability thresholds for maximizing the J-Index and F1 are determined
                       using the Sensitivity and Specificity values calculated during ROC curve construction.", 
                       "For F1/2, we set the threshold to one-half of the optimal F1 score attained by our model.",
                       "The Average is equal to the mean of the optimal J Index and F1."), 
           general_title = '', 
           threeparttable = TRUE, 
           escape = FALSE, 
           footnote_as_chunk = TRUE); atts_comb_table

