# -------------------------------------------------------------------------------------------- #
library(tidymodels)
library(kableExtra)
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
model_geography = 'Rural'; 
model_dep_var = 'low_access'; 
bootstrap_by_tracts <- '_tracts'; 
bootstrap_ids = '01_499'
# -------------------------------------------------------------------------------------------- #
# Load data based on above parameters. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' '))
# -------------------------------------------------------------------------------------------- #
model_output <- readRDS(here::here('Analysis', 'Model_Training', dir_dep_var, filename))
# -------------------------------------------------------------------------------------------- #
untreated_preds <- model_output$cv_errors_opt %>% 
  
  left_join(dta_untreated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, all_of(model_dep_var), cv_preds, pred_probs) %>%
  
  rename_with(.cols = cv_preds, .fn = ~str_replace_all(., pattern='cv_', replacement = '')) %>%
  
  rename_with(.cols = starts_with('low_access'), 
              .fn = ~str_replace_all(., pattern = '^low_access$|^low_access_perm$|low_access_pers$', 
                                     replacement = 'actual') ) %>%
  
  filter(year >= '2007') # We obtain CV predictions from 2007-2020
# -------------------------------------------------------------------------------------------- #
treated_preds <- model_output$data_cf_preds %>%
  
  left_join(dta_treated_non_model_vars, by = c('GEOID', 'year')) %>%
  
  select(GEOID, year, event_year, rel_year, Geography, actual, pred_class_cf, tau, pred_probs_cf) %>%
  
  rename(preds = pred_class_cf) %>%
  
  rename(pred_probs = pred_probs_cf) %>%
  
  filter(year >= '2006') # We obtain post-treatment predictions from 2006-2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
dta_for_metrics <- model_preds %>%
  filter(rel_year < 0) %>% 
  mutate(across(.cols = c(actual, preds), 
                .fns = \(x) factor(x, levels = c('1', '0') ) ) ) %>% 
  select(rel_year, actual, pred_probs, preds)

source(here::here('Code', 'Functions', 'Function_compute_fit_metrics.R'))

summary_fit_metrics <- compute_metrics(dta = dta_for_metrics, bootstrap_idx = 0)

emp_fit_metrics <- summary_fit_metrics$metrics

emp_conf_mat <- summary_fit_metrics$confusion_matrix
# -------------------------------------------------------------------------------------------- #
# Load bootstrap data. 
# -------------------------------------------------------------------------------------------- #
boot_data <- seq(1, 499, 1) %>%
  
  map(function(.iter){ 
    
    dir_geography <- paste(model_geography, 'Bootstrap', sep = '_')
    
    dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
    
    filename <- paste('bootstrap_fit_metrics', str_to_lower(model_geography), 
                      paste0(model_dep_var, bootstrap_by_tracts, '_', .iter, '.rds'), sep = '_')
    
    bootstrap_data <- readRDS(here::here('Analysis',
                                         dir_geography,
                                         dir_dep_var, 
                                         'bootstrap_fit_metrics_tracts', 
                                         filename))
    
  })


boot_data_metrics <- boot_data %>% map_dfr(function(.x) .x$`metrics`)

boot_data_metrics <- bind_rows(emp_fit_metrics, 
                               boot_data_metrics)


boot_sd_mean <- boot_data_metrics %>%
  group_by(.metric, rel_year) %>%
  summarise(across(.cols = .estimate, 
                   .fns = c('avg' = mean, 
                            'sd' = sd), 
                   .names = '{col}_{.fn}'))

summary_fit_metrics <- summary_fit_metrics$metrics %>%
  left_join(
    boot_sd_mean, 
    by = c('.metric', 'rel_year')
  )

summary_fit_metrics <- summary_fit_metrics %>%
  arrange(.metric, rel_year) %>%
  select(-c(.estimator, boot_iter)) %>%
  relocate(.estimate_sd, .before = .estimate_avg)

# Compute confidence intervals based on bootstrapped SDs. 

summary_fit_metrics <- summary_fit_metrics %>% 
  mutate(uci_99 = .estimate + qnorm(1 - 0.01/2)*.estimate_sd, 
         lci_99 = .estimate - qnorm(1 - 0.01/2)*.estimate_sd)

metrics_of_interest <- c('accuracy', # TP + TN/(All Observations)
                         'sens',  # TP/(TP + FN)
                         'spec', # TN/(TN + FP)
                         'precision', # TP/(TP+FP)
                         'bal_accuracy', # sens+spec/2
                         'f_meas', # (1+Beta^2)*precision*recall/(beta^2*precision)+recall)
                         'g_mean', # sqrt(sens, specificity)
                         'mcc', # (TP*TN-FP*FN)/sqrt((TP+FN)*(TP+FP)*(TN+FN)*(TN+FP))
                         'roc_auc', # ROC --> Sensitivity (TP/Total Negatives) against 1-Specificity (TN/(Total Negatives))
                         'pr_auc') # PR --> Precision (TP/(TP+FP) (TP/(Total Predicted Positives))) against Sensitivity/Recall
                         

metric_names <- c('^accuracy$' = 'Accuracy', # TP + TN/(All Observations)
                  'sens' = 'Sensitivity',  # TP/(TP + FN)
                  'spec' = 'Specificity', # TN/(TN + FP)
                  'precision' = 'Precision', # TP/(TP+FP)
                  '^bal_accuracy$' = 'Balanced Accuracy', # sens+spec/2
                  'f_meas' = 'F1 Score', # (1+Beta^2)*precision*recall/(beta^2*precision)+recall)
                  'g_mean' = 'G-Mean', # sqrt(sens, specificity)
                  'mcc' = 'Matthews Correlation Coefficient',
                  'roc_auc' = 'ROC-AUC', # ROC --> Sensitivity (TP/Total Negatives) against 1-Specificity (TN/(Total Negatives))
                  'pr_auc' = 'PR-AUC')

# We obtain the performance metrics across all pre-treatment periods. 
# By filtering with !is.na(rel_year), one can obtain performance metrics by relative time to treatment. 
overall_fit_metrics <- summary_fit_metrics %>%
  select(-c(.estimate_avg)) %>%
  filter(is.na(rel_year)) %>%
  mutate(across(.cols = where(is.numeric), 
                .fns = \(x) round(x, digits = 3))) %>%
  select(-rel_year) %>%
  filter(.metric %in% metrics_of_interest) %>% 
  arrange(match(.metric, metrics_of_interest)) %>%
  mutate(.metric = str_replace_all(.metric, metric_names))

tidy_cols <- c('.metric' = 'Metric', 
               '^\\.estimate$' = 'Estimate', 
               '^\\.estimate_sd$' = 'SD', 
               'uci_99' = 'Upper CI', 
               'lci_99' = 'Lower CI')

names(overall_fit_metrics) <- names(overall_fit_metrics) %>% str_replace_all(pattern = tidy_cols)

# Create the LaTeX table
latex_table <- kable(overall_fit_metrics, 
                     format = "latex", 
                     align = 'lcccc', 
                     booktabs = TRUE,
                     escape = TRUE, # Note: This is the default.
                     digits = 3,
                     position = '!h',
                     linesep = '', # Remove \addlinespace every 5 rows. 
                     centering = TRUE, # Note: This is the default. 
                     col.names = c("Metric", "Estimate", "SD", "UCI (99%)", "LCI (99%)"),
                     caption = paste0("Performance Metrics for ", 
                                      model_geography, 
                                      " Area Pre-Treatment Cross-Validation Predictions")); latex_table 