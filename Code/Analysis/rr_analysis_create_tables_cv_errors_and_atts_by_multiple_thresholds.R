# Creates Table 5 in R1 responses.  
# -------------------------------------------------- #
pacman::p_load('dplyr', 'tidyr', 'purrr', 'stringr','kableExtra')
# -------------------------------------------------- #
source(here::here('Code', 'Functions', 'Function_create_table_estimates_multiple_thresholds.R'))

cv_errors_comb <- create_table_est_mult_thresholds(est_type = 'cv_errors')
att_comb <- create_table_est_mult_thresholds(est_type = 'att')

# Remove the predicted proportions. 
cv_errors_comb <- cv_errors_comb %>% filter(!grepl('^Proportion', Measure))

# Keep only the ATTs and ATT (%)
att_comb <- att_comb %>% filter(grepl('ATT', Measure) )

# Combine CV Errors and ATTs
estimates_comb <- cv_errors_comb %>% bind_rows(att_comb)

measure_levels <- unique(estimates_comb$Measure)

estimates_comb <- estimates_comb %>% 
  mutate(Measure = factor(Measure, levels = measure_levels), 
         Sample = factor(Sample, levels = c('Urban', 'Rural')))

estimates_comb <- estimates_comb %>% arrange(Sample, Measure)

estimates_comb <- estimates_comb %>% 
  group_by(Sample, Measure) %>%
  mutate(id = row_number()) %>% 
  mutate(Measure = case_when(id == 1 ~ Measure, 
                             TRUE ~ '') ) %>%
  select(-id) %>% 
  ungroup() %>%
  group_by(Sample) %>% 
  mutate(id = row_number()) %>%
  mutate(Sample = case_when(id == 1 ~ Sample, 
                            TRUE ~ '') ) %>%
  select(-id) %>% 
  ungroup() 

# Create the LaTeX table
table_estimates_comb <- kable(estimates_comb, 
                         format = "latex", 
                         align = 'lcccccc', 
                         booktabs = TRUE,
                         escape = TRUE, # Note: This is the default.
                         digits = 4,
                         position = '!h',
                         linesep = '', # Remove \addlinespace every 5 rows. 
                         centering = TRUE, # Note: This is the default. 
                         col.names = colnames(estimates_comb),
                         caption = paste0("Average Cross-Validation Errors for Pre-Treatment Block Groups Using Multiple Thresholds, 
                                                  Block Groups Receiving a Dollar Store from 2006-2020")) %>% 
  kable_styling() %>%
  footnote(general = c("Notes: We report the bootstrapped standard errors in parentheses.",
                       "The J-Index (Youden's J-Statistic) is equal to Sensitivity + Specificity - 1, while the 
                       F1 is calculated as $\\frac{2 \\times \\text{Precision} \\times \\text{Recall}}{\\text{Precision} + \\text{Recall}}$.", 
                       "Optimal probability thresholds for maximizing the J-Index and F1 are determined
                       using the Sensitivity and Specificity values calculated during ROC curve construction.", 
                       "For F1/2, we set the threshold to one-half of the optimal F1 score attained by our model.",
                       "The Average is equal to the mean of the optimal J Index and F1."), 
           general_title = '', 
           threeparttable = TRUE, 
           escape = FALSE, 
           footnote_as_chunk = TRUE); table_estimates_comb
