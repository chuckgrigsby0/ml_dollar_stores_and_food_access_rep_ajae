print('Sourced: compute_metrics <- function(dta, bootstrap_idx)')

compute_metrics <- function(dta, bootstrap_idx){ 
  
  confusion_mat <- conf_mat(data = dta, 
                            truth = actual, 
                            estimate = preds, 
                            dnn = c('Predicted', 'Observed'))
  
  # f-measure = (1+Beta^2)*precision*recall/(beta^2*precision)+recall)
  # Computes multiple measures, including accuracy, balanced accuracy, and f-1 score
  metric_summary <- summary(confusion_mat)
  
  sens_and_spec <- metric_summary %>% 
    filter(.metric == 'sens' | .metric == 'spec') %>% 
    select(.estimate) %>% 
    pluck('.estimate')
  
  # Compute the geometric mean for the entrie pre-treatment period. 
  g_mean <- data.frame(.metric = 'g_mean', .estimator = 'binary', .estimate = sqrt(prod(sens_and_spec)) )
  
  metric_summary <- bind_rows(metric_summary, g_mean)
  
  confusion_mat_relyear <- dta %>%
    group_by(rel_year) %>%
    conf_mat(truth = actual, 
             estimate = preds, 
             dnn = c('Predicted', 'Observed'))
  
  confusion_mat_relyear <- confusion_mat_relyear %>% 
    mutate(summary = map(conf_mat, function(.x) summary(.x) ) )
  
  metric_summary_rel_year <- confusion_mat_relyear %>% 
    select(rel_year, summary) %>% 
    unnest(summary)
  
  # Compute geometric mean for accuracy for each relative time period. 
  g_mean_relyear <- metric_summary_rel_year %>%
    filter(.metric == 'sens' | .metric == 'spec') %>%
    pivot_wider(names_from = .metric, 
                values_from = .estimate) %>%
    mutate(g_mean = sqrt(sens*spec) ) %>%
    select(-c(sens, spec)) %>% 
    pivot_longer(cols = 'g_mean', 
                 names_to = '.metric', 
                 values_to = '.estimate') %>%
    relocate(.estimator, .before = .metric)
  
  metric_summary_rel_year <- bind_rows(metric_summary_rel_year, 
                                       g_mean_relyear) %>%
    arrange(rel_year)
  
  # Compute Precision-Recall (ROC) AUC for the entire pre-entry period and for each relative-time-to-treatment group. 
  pr_auc_rel_year <- dta %>% 
    group_by(rel_year) %>% 
    pr_auc(truth = actual, 
           pred_probs)
  
  pr_auc <- dta %>% 
    pr_auc(truth = actual, 
           pred_probs)
  
  roc_auc <- dta %>%
    roc_auc(truth = actual, 
            pred_probs)
  
  # Compute ROC-AUC for each relative-time-to-treatment group. 
  roc_auc_rel_year <- dta %>% 
    group_by(rel_year) %>% 
    roc_auc(truth = actual, 
            pred_probs)
  
  all_metrics <- bind_rows(metric_summary, 
                           metric_summary_rel_year, 
                           pr_auc, 
                           pr_auc_rel_year, 
                           roc_auc, 
                           roc_auc_rel_year)
  
  all_metrics <- all_metrics %>% mutate(boot_iter = bootstrap_idx)
  
  fit_metrics <- list('confusion_matrix' = confusion_mat, 
                      'metrics' = all_metrics)
  
  
  return(fit_metrics)
  
}