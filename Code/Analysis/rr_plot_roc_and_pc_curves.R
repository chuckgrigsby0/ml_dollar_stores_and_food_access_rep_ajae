# -------------------------------------------------------------------------------------------- #
library(tidymodels)
# -------------------------------------------------------------------------------------------- #
options(scipen = 999)
# -------------------------------------------------------------------------------------------- #
model_geography = 'Rural'; 
model_dep_var = 'low_access'; 
bootstrap_by_tracts <- '_tracts'; 
bootstrap_ids = '01_499'
# -------------------------------------------------------------------------------------------- #
# .libPaths()
# -------------------------------------------------------------------------------------------- #
# Load data based on parameters. 
# -------------------------------------------------------------------------------------------- #
source(here::here('Code', 'Analysis', 'data_preparation_imputation_estimation.R'))
# -------------------------------------------------------------------------------------------- #
# Load the optimal estimated model following tuning/training. 
# -------------------------------------------------------------------------------------------- #
filename <- paste0('xgboost_10m_', str_to_lower(model_geography), '_', model_dep_var, '_final', '.rds'); filename
dir_dep_var <- str_replace_all(str_to_title(str_replace_all(model_dep_var, '_', ' ')), ' ', '_')
dep_var_title <- str_to_title(str_replace_all(model_dep_var, '_', ' ')) # For plot titles (below). 
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
  
  filter(year >= '2006') # For consistency with the out-of-sample predictions during CV, 
# we plot counterfactual predictions from 2007 to 2020. 
# -------------------------------------------------------------------------------------------- #
model_preds <- bind_rows(untreated_preds, treated_preds) %>%
  
  filter(is.finite(rel_year)) # Filter out observations with rel_year == Inf because these are never-treated observations. 
# -------------------------------------------------------------------------------------------- #
dta_for_metrics <- model_preds %>%
  filter(rel_year < 0) %>% 
  mutate(across(.cols = c(actual, preds), 
                .fns = \(x) factor(x, levels = c('1', '0') ) ) ) %>% 
  select(rel_year, actual, pred_probs, preds)

# f1_opt <- dta_for_metrics %>%
#   pr_curve(truth = actual, 
#            pred_probs) %>% 
#   mutate(f1 = (2*precision*recall)/(precision+recall) )
# 
# f1_opt[which.max(f1_opt$f1), ]  

# Compute Precision-Recall AUC curve for the entire pre-entry period.. 

pr_auc <- dta_for_metrics %>% 
  pr_curve(truth = actual, 
           pred_probs)

# Compute the AUC PR. 
pr_auc_summary <- dta_for_metrics %>% 
  pr_auc(truth = actual, 
         pred_probs)

# Compute ROC-AUC for pre-entry period. 
roc_auc <- dta_for_metrics %>%
  roc_curve(truth = actual, 
            pred_probs)

# Compute the AUC PR. 
roc_auc_summary <- dta_for_metrics %>% 
  roc_auc(truth = actual, 
         pred_probs)


library(viridisLite)
sequential_levels_mako <- mako(30, begin = 0, end = 1, direction = 1)
# 1 - specificty = 1 - TN/(TN+FP) = FP/(TN+FP) = FP/All Negatives
# recall = sensitivity = = TP/(TP+FP) = TP/All predicted positives
# precision = TP/(TP+FP)

# Plot and save the ROC curve. 
roc_auc %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(colour = sequential_levels_mako[10], 
            linetype = 'solid', linewidth = 1.2, 
            lineend = 'round') +
  geom_abline(color = sequential_levels_mako[1], alpha = 0.50, 
              linetype = 'dashed', linewidth = 1.2) +
  coord_equal() +
  scale_x_continuous(breaks = seq(0, 1, 0.20))+
  scale_y_continuous(breaks = seq(0, 1, 0.20))+
  labs(x = '1 - Specificity', y = 'Sensitivity') +
  theme(axis.text.x = element_text(color = 'black', size = 14, angle=90, vjust=0.75), 
        axis.text.y = element_text(color = 'black', size = 14), 
        axis.title.y = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.ticks = element_line(color='black'), 
        axis.ticks.length = unit(.25, 'cm'), 
        panel.grid.major.x=element_line(colour='grey', linewidth = 0.1),
        panel.grid.minor = element_line(colour='grey', linewidth = 0.1),
        panel.grid.major.y = element_line(colour='grey', linewidth = 0.1),
        panel.grid.minor.y = element_line(colour='grey', linewidth = 0.1),
        #       # axis.line = element_line(colour='black'), 
        #       # legend.position='bottom', 
        #       # legend.title = element_blank(), 
        #       # legend.text = element_text(size=12), 
        #       # legend.background = element_blank(), 
        #       # legend.box.background = element_rect(colour = 'black'), 
        panel.border = element_rect(color='black', fill=NA),
        #       strip.background = element_rect(colour = 'black', fill='grey'),
        #       strip.text = element_text(size = 15),
        panel.background = element_rect(fill = 'white'))


figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_roc_curve', '.png'); figname

ggsave(here::here('Analysis', 'Figures', 'Low_Access', model_geography, 'roc_and_pr_curves', figname), 
       width = 6, height = 5, dpi = 300, unit = 'in')


# Plot and save the PR-curve
pr_auc %>% 
  ggplot(aes(x = recall, y = precision)) +
  geom_path(colour = sequential_levels_mako[10], 
            linetype = 'solid', linewidth = 1.2, 
            lineend = 'round') +
  coord_equal() +
  labs(x = 'Recall', y = 'Precision') +
  scale_x_continuous(breaks = seq(0, 1, 0.20))+
  scale_y_continuous(breaks = seq(0, 1, 0.20))+
  theme(axis.text.x = element_text(color = 'black', size = 14, angle=90, vjust=0.75), 
        axis.text.y = element_text(color = 'black', size = 14), 
        axis.title.y = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.ticks = element_line(color='black'), 
        axis.ticks.length = unit(.25, 'cm'), 
        panel.grid.major.x=element_line(colour='grey', linewidth = 0.1),
        panel.grid.minor = element_line(colour='grey', linewidth = 0.1),
        panel.grid.major.y = element_line(colour='grey', linewidth = 0.1),
        panel.grid.minor.y = element_line(colour='grey', linewidth = 0.1),
        #       # axis.line = element_line(colour='black'), 
        #       # legend.position='bottom', 
        #       # legend.title = element_blank(), 
        #       # legend.text = element_text(size=12), 
        #       # legend.background = element_blank(), 
        #       # legend.box.background = element_rect(colour = 'black'), 
        panel.border = element_rect(color='black', fill=NA),
        #       strip.background = element_rect(colour = 'black', fill='grey'),
        #       strip.text = element_text(size = 15),
        panel.background = element_rect(fill = 'white'))


figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_pr_curve', '.png'); figname

ggsave(here::here('Analysis', 'Figures', 'Low_Access', model_geography, 
                  'roc_and_pr_curves', figname), 
       width = 6, height = 5, dpi = 300, unit = 'in')
