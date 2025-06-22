library(viridisLite)
library(ggh4x) # For facet_wrap2, facet_grid2
sequential_levels_mako <- mako(30, begin = 0, end = 1, direction = 1)
#--------------------------------------------------------------------------------------------#
library(RColorBrewer)
display.brewer.pal(n = 9, name = 'Greys')
greys <- brewer.pal(n = 9, name = 'Greys')
#--------------------------------------------------------------------------------------------#
# Create a new data frame to include in the plot_effects_on_covars_norm function and the plot_errors_on_covars_binned function.
# This allows us to specify a vertical line in each facet to indicate a null effect of the predictor 
# on the causal estimate. 
null_effect_df <- data.frame(null_effect = 0)
print(paste('Loaded: null_effect_df$null_effect =', null_effect_df$null_effect))
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_relyear')
#--------------------------------------------------------------------------------------------#
plot_errors_on_relyear <- function(dta, ci_label_str, ci_level,
                                   y_value, standard_error, 
                                   y_axis_title, x_axis_title, 
                                   plot_title, plot_subtitle,
                                   decimal_place_y, 
                                   y1_lim, y2_lim){
  #y_value <- sym(y_value)
  #standard_error <- sym(standard_error)
  
  dta <- dta %>% mutate(ci_label = paste0(ci_label_str))
  
  dta <- dta %>% mutate(Outcome = paste('Average', Outcome))
  
  dta %>%
    
    ggplot(aes(x=rel_year, y = {{y_value}}))+
    
    geom_point(aes(color = Outcome), 
               shape = 16, 
               size = 4, 
               alpha = 1) +
    
    geom_linerange(aes(ymax = {{y_value}} + ci_level*{{standard_error}}, 
                       ymin = {{y_value}} - ci_level*{{standard_error}}, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_linetype_manual(values = 'solid')+
    
    scale_color_manual(values = greys[8], labels = 'Avg. CV Error')+
    
    guides(color = guide_legend(order = 1), 
           line = guide_legend(order = 2))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10),
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-13, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title = plot_title, 
         subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 15),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_actual_on_predicted')
#--------------------------------------------------------------------------------------------#
plot_actual_on_predicted <- function(dta, y_value, standard_error,
                                     ci_level,
                                     y_axis_title, x_axis_title, 
                                     plot_title, plot_subtitle,
                                     x_intercept_val, decimal_place_y, y1_lim, y2_lim){
  
  dta %>% 
    
    ggplot(aes(x=rel_year, y = {{y_value}}, group = Outcome))+
    
    
    geom_point(aes(color = Outcome, shape = Outcome, alpha = Outcome), 
               size = 4, 
               position = position_dodge(width = 0.6)) +
    
    geom_linerange(aes(ymax = {{y_value}} + ci_level*{{standard_error}}, # Computes 99% CIs
                       ymin = {{y_value}} - ci_level*{{standard_error}}, 
                       color = Outcome, 
                       alpha = Outcome), 
                   linewidth = 0.75, 
                   position = position_dodge(width = 0.6))+
    
    
    geom_vline(xintercept = x_intercept_val, 
               color = greys[6], 
               alpha = 0.7, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_color_manual(values = c(greys[8], greys[6]))+
    
    scale_shape_manual(values = c(17, 16))+
    
    scale_alpha_manual(values = c(1, 0.9))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-13, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title = plot_title, 
         subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=  element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour = 'black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill = NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 14),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_relyear')
#--------------------------------------------------------------------------------------------#
plot_effects_on_relyear <- function(dta, 
                                    ci_label_str, 
                                    ci_level,
                                    legend_lab_str,
                                    y_value, standard_error, 
                                    y_axis_title, x_axis_title, 
                                    plot_title, plot_subtitle,
                                    legend_lab,
                                    #breaks_y, 
                                    decimal_place_y, 
                                    y1_lim, y2_lim){
  #y_value <- sym(y_value)
  #standard_error <- sym(standard_error)
  
  dta <- dta %>% mutate(ci_label = ci_label_str, 
                        legend_lab = legend_lab_str)
  
  dta %>%
    
    ggplot(aes(x=rel_year, y = {{y_value}}))+
    
    geom_point(aes(color = legend_lab), 
               shape = 16,
               size = 4, 
               alpha = 1) +
    
    geom_linerange(aes(ymax = {{y_value}} + ci_level*{{standard_error}}, 
                       ymin = {{y_value}} - ci_level*{{standard_error}}, 
                       linetype = ci_label),
                   color = greys[8],
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_linetype_manual(values = 'solid') +
    
    scale_color_manual(values = greys[8], labels = 'ATT')+ # Need to include twice to not replicate legend point.
    
    guides(color = guide_legend(order = 1), 
           line = guide_legend(order = 2))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-13, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title = plot_title,
         subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 15),
          panel.background = element_blank(),
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_relyear_by_entry_bin')
#--------------------------------------------------------------------------------------------#
plot_effects_on_relyear_by_entry_bin <- function(dta, y_value, standard_error, 
                                                 y_axis_title, x_axis_title, 
                                                 plot_title, plot_subtitle,
                                                 decimal_place_y, 
                                                 y1_lim, y2_lim){
  #y_value <- sym(y_value)
  #standard_error <- sym(standard_error)
  
  dta %>% 
    
    ggplot(aes(x=rel_year, y = {{y_value}}, group = entry_events))+
    
    geom_point(aes(color = entry_events, shape = entry_events), 
               size = 4, 
               alpha = 0.95, 
               position = position_dodge(width = 0.3)) +
    
    geom_line(aes(linetype = entry_events), linewidth = 1, alpha = 0.95)+
    
    geom_linerange(aes(ymax = {{y_value}} + 2.576*{{standard_error}}, 
                       ymin = {{y_value}} - 2.576*{{standard_error}}, 
                       color = entry_events), 
                   alpha = 0.95, 
                   linewidth = 1.05,
                   position = position_dodge(width = 0.3))+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_color_manual(values = c(sequential_levels_mako[1], sequential_levels_mako[3], sequential_levels_mako[10]))+
    
    scale_shape_manual(values = c(15, 16, 17))+
    
    scale_linetype_manual(values = c('solid', 'dotted', 'longdash'))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-13, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title = plot_title, 
         subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 14, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 14), 
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 15, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 15),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_pred_and_actual_by_year')
#--------------------------------------------------------------------------------------------#
plot_pred_and_actual_by_year <- function(dta, 
                                         outcome_type,
                                         standard_error,
                                         y_axis_title, x_axis_title, 
                                         plot_title, plot_subtitle,
                                         decimal_place_y, 
                                         y1_lim, y2_lim, 
                                         x_intercept_val = NULL){
  
  
  dta %>% 
    
    filter(grepl(outcome_type, Outcome)) %>%
    
    ggplot(aes(x= year, y = estimate, group = Outcome))+
    
    geom_point(aes(color = Outcome, shape = Outcome), 
               size = 4, 
               alpha = 0.95, 
               position = position_dodge(width = 0.2)) +
    
    geom_line(aes(linetype = Outcome), linewidth = 1, alpha = 0.95)+
    
    geom_linerange(aes(ymax = estimate + 2.576*{{standard_error}}, 
                       ymin = estimate - 2.576*{{standard_error}}, 
                       color = Outcome), 
                   alpha = 0.95, 
                   linewidth = 1.05,
                   position = position_dodge(width = 0.2))+
    
    
    scale_color_manual(values = c(sequential_levels_mako[1], sequential_levels_mako[10]))+
    
    scale_shape_manual(values = c(16, 17))+
    
    scale_linetype_manual(values = c('solid', 'longdash'))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n= 10),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = scales::breaks_pretty(n = 16))+
    
    geom_vline(xintercept = x_intercept_val, color = 'black', linetype = 'solid', linewidth = 1)+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title= plot_title, subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 14, angle=0, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 14), 
          axis.title.y = element_text(size = 15, face = 'bold'),
          axis.title.x = element_text(size = 15, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 15),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#

print('Sourced: plot_errors_on_covars_binned')

# Function used to print every nth label on the discrete x-axis labels. 
every_nth <- function(n) {
  return(function(x) {
    nrow <- length(x)
    x[floor(seq(1, nrow, length.out = n))]
  })
}

#--------------------------------------------------------------------------------------------#
plot_errors_on_covars_binned <- function(dta, 
                                         filter_covariate_type, 
                                         ci_label_str, 
                                         ci_level, 
                                         y_axis_title, 
                                         title_str, subtitle_str, 
                                         decimal_place_y){
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>% filter(!!!filter_covariate_type) %>% 
    
    mutate(across(.cols = term, .fn = ~factor(., levels = unique(.)) ) )
  
  # Add an additional label for the plot legends.
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta %>% 
    
    ggplot(aes(x=term, y = estimate))+
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name = y_axis_title, 
                       breaks = scales::breaks_pretty(n = 6), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = 'Predictor Bins')+
    
    scale_color_manual(values = greys[8])+
    
    scale_linetype_manual(values = 'solid')+
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title= 'Pre-Treatment Cross-Validation Errors and Predictors', 
         
      #    subtitle = paste0('Geography: ', model_geography, "\n", 
        #                    'Outcome: ', dep_var_subtitle)) + 
    
    facet_wrap2(~tidy_name, ncol = 1, scales = 'free')+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle = 45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color = 'black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.text = element_text(size = 12),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_int_covars')
#--------------------------------------------------------------------------------------------#
plot_errors_on_int_covars <- function(dta, 
                                      filter_covariate_type, 
                                      ci_label_str, 
                                      ci_level, 
                                      y_axis_title, 
                                      title_str, 
                                      subtitle_str, 
                                      decimal_place_y){
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>% filter(!!!filter_covariate_type) %>% 
    
    mutate(across(.cols = term, .fn = ~factor(., levels = unique(.)) ) )
  
  # Add an additional label for the plot legends.
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta %>% 
    
    ggplot(aes(x=term, y = estimate))+
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, 
               aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name = y_axis_title, 
                       breaks = scales::breaks_pretty(n = 6), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = 'Predictor Bins') +
    
    scale_color_manual(values = greys[8]) +
    
    scale_linetype_manual(values = 'solid') +
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    # labs(title= 'Pre-Treatment Cross-Validation Errors and Predictors', 
         
      #    subtitle = paste0('Geography: ', model_geography, "\n", 
        #                    'Outcome: ', dep_var_subtitle)) + 
    
    facet_wrap2(~tidy_name, ncol = 1, scales = 'free')+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color = 'black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.text = element_text(size = 12),
          panel.background = element_blank())
}
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_covars_norm')
#--------------------------------------------------------------------------------------------#
plot_errors_on_covars_norm <- function(dta, 
                                       filter_covariate_type, 
                                       ci_label_str, 
                                       ci_level, 
                                       title_str, 
                                       subtitle_str, 
                                       decimal_place_y){
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>% filter(!!!filter_covariate_type)
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta %>%
    
    ggplot(aes(x = tidy_name, y = estimate)) +
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd, 
                      linetype = ci_label), 
                  color = greys[8],  
                  alpha = 1, 
                  linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    
    coord_flip() +
    
    # facet_wrap(covariate_type~., nrow = 2, scales = 'free_y')+ #This means that the variable categories are on y-axis and and store types are on x-axis. 
    
    scale_y_continuous(name = 'Effect Size', 
                       breaks = scales::breaks_pretty(n = 15), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_color_manual(values = greys[8]) +
    
    scale_linetype_manual(values = 'solid') +
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    scale_x_discrete(name = 'Predictors') +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    # labs(title = 'Treatment Effect Heterogeneity by Predictor', 
    
    #      subtitle = paste0('Geography: ', model_geography, "\n", 
    #                        'Outcome: ', dep_var_subtitle)) + 
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12, angle=0), 
          axis.title = element_text(size = 12),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.ticks.y = element_blank(), 
          axis.ticks.length.x = unit(.25, 'cm'), 
          axis.ticks.x = element_line(color='black'), 
          panel.grid.major.x = element_line(colour='grey', linewidth = 0.5), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.5), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size = 12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color = 'black', fill=NA), 
          strip.background.y = element_rect(colour = NA, fill=NA),
          strip.background.x = element_rect(colour = 'black', fill = 'white'),
          strip.text.y = element_text(size = 12, face='bold', angle = 90),
          strip.text.x = element_text(size = 12, face='bold', angle = 0),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#

print('Sourced: plot_effects_on_covars_binned')

#--------------------------------------------------------------------------------------------#
plot_effects_on_covars_binned <- function(dta, 
                                          filter_covariate_type, 
                                          ci_label_str, 
                                          ci_level, 
                                          y_axis_title, 
                                          title_str, 
                                          subtitle_str, 
                                          decimal_place_y){
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>% filter(!!!filter_covariate_type) %>% 
    
    mutate(across(.cols = term, .fn = ~factor(., levels = unique(.)) ) )
  
  
  # Add an additional label for the plot legends.
  
  dta <- dta %>% mutate(ci_label = paste0(ci_label_str))
  
  dta %>% 
    
    ggplot(aes(x= term, y = estimate))+
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name = y_axis_title, 
                       breaks = scales::breaks_pretty(n = 6), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = 'Predictor Bins')+
    
    scale_color_manual(values = greys[8])+
    
    scale_linetype_manual(values = 'solid')+
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title= 'Pre-Treatment Cross-Validation Errors and Predictors', 
    
    #    subtitle = paste0('Geography: ', model_geography, "\n", 
    #                    'Outcome: ', dep_var_subtitle)) + 
    
    facet_wrap2(~tidy_name, ncol = 1, scales = 'free')+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle = 45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color = 'black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.text = element_text(size = 12),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
  
}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_int_covars')
#--------------------------------------------------------------------------------------------#
plot_effects_on_int_covars <- function(dta, 
                                       filter_covariate_type, 
                                       ci_label_str, 
                                       ci_level, 
                                       y_axis_title, 
                                       title_str, 
                                       subtitle_str, 
                                       decimal_place_y){
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>% filter(!!!filter_covariate_type) %>% 
    
    mutate(across(.cols = term, .fn = ~factor(., levels = unique(.)) ) )
  
  # Add an additional label for the plot legends.
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta %>% 
    
    ggplot(aes(x=term, y = estimate))+
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name = y_axis_title, 
                       breaks = scales::breaks_pretty(n = 6), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = 'Predictor Bins') +
    
    scale_color_manual(values = greys[8]) +
    
    scale_linetype_manual(values = 'solid') +
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    # labs(title= 'Pre-Treatment Cross-Validation Errors and Predictors', 
    
    #    subtitle = paste0('Geography: ', model_geography, "\n", 
    #                    'Outcome: ', dep_var_subtitle)) + 
    
    facet_wrap2(~tidy_name, ncol = 1, scales = 'free')+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color = 'black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.text = element_text(size = 12),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
  
}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_covars_norm')
#--------------------------------------------------------------------------------------------#
plot_effects_on_covars_norm <- function(dta, 
                                        filter_covariate_type, 
                                        ci_label_str, 
                                        ci_level,
                                        title_str, 
                                        subtitle_str, 
                                        decimal_place_y){

  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
    dta <- dta %>% filter(!!!filter_covariate_type)
    
    # Add CI and point estimate labels for the plot legend. 
    dta <- dta %>% mutate(ci_label = ci_label_str)
    
    dta %>%
    
    ggplot(aes(x = tidy_name, y = estimate)) +
      
      geom_point(aes(color = label), 
                 shape = 16, 
                 size = 4, 
                 alpha = 1)+
      
      geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                         ymin = estimate - ci_level*bootstrap_sd, 
                         linetype = ci_label), 
                     color = greys[8],  
                     alpha = 1, 
                     linewidth = 0.75)+
      
      geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
                 color = greys[6], 
                 alpha = 0.70, 
                 linetype = 'dashed', 
                 linewidth = 0.5)+
      
      
      coord_flip() +
      
      # facet_wrap(covariate_type~., nrow = 2, scales = 'free_y')+ #This means that the variable categories are on y-axis and and store types are on x-axis. 
      
      scale_y_continuous(name = 'Effect Size', 
                         breaks = scales::breaks_pretty(n = 15), 
                         labels = scales::label_number(accuracy = decimal_place_y))+
      
      scale_color_manual(values = greys[8]) +
      
      scale_linetype_manual(values = 'solid') +
      
      guides(color = guide_legend(order = 1), # Changes order of legend items. 
             linetype = guide_legend(order = 2)) +
      
      scale_x_discrete(name = 'Predictors') +
      
      labs(title = title_str, 
           subtitle = subtitle_str) +
      # labs(title = 'Treatment Effect Heterogeneity by Predictor', 
      
      #      subtitle = paste0('Geography: ', model_geography, "\n", 
      #                        'Outcome: ', dep_var_subtitle)) + 
      
      theme(axis.text.x = element_text(color = 'black', size = 12, angle=45, hjust = 1, vjust = 1), 
            axis.text.y = element_text(color = 'black', size = 12, angle=0), 
            axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold'),
            axis.title.y = element_text(face = 'bold'),
            axis.ticks.y = element_blank(), 
            axis.ticks.length.x = unit(.25, 'cm'), 
            axis.ticks.x = element_line(color='black'), 
            panel.grid.major.x = element_line(colour='grey', linewidth = 0.5), 
            panel.grid.minor = element_blank(), 
            panel.grid.major.y = element_line(colour='grey', linewidth = 0.5), 
            panel.grid.minor.y = element_blank(), 
            axis.line = element_blank(), 
            legend.position='bottom', 
            legend.title = element_blank(), 
            legend.text = element_text(size = 12), 
            legend.background = element_blank(), 
            legend.box.background = element_rect(colour = 'black'), 
            panel.border = element_rect(color = 'black', fill=NA), 
            strip.background.y = element_rect(colour = NA, fill=NA),
            strip.background.x = element_rect(colour = 'black', fill = 'white'),
            strip.text.y = element_text(size = 12, face='bold', angle = 90),
            strip.text.x = element_text(size = 12, face='bold', angle = 0),
            panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_dsvars')
#--------------------------------------------------------------------------------------------#
plot_effects_on_dsvars <- function(dta, 
                                   filter_covariate_type,
                                   ci_label_str, 
                                   ci_level,
                                   y_axis_title, x_axis_title, 
                                   title_str, subtitle_str,
                                   decimal_place_y){
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>% filter(!!!filter_covariate_type) 
  
  if (grepl('Gross', unique(dta$tidy_name))){ # If Gross entry, filter out the term in which gross entry was zero, which only occurs due to 
    dta <- dta %>% filter(term != '0') # Missing values for observations in post-treatment periods, causing their initial gross entry to be zero temporarily
    } # until the following entry. 
    
    dta <- dta %>% 
      
      mutate(across(.cols = term, .fn = ~factor(., levels = unique(.)) ) )
    
    dta <- dta %>% 
      
      mutate(ci_label = ci_label_str)
  
  dta %>%
    
    ggplot(aes(x=term, y = estimate))+
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    # Need to include in scale_color_manual and scale_shape_manual to not replicate legend point.
    # c(expression(paste(tau, ' ~ Counts')), 
    # expression(paste(tau, ' ~ Entries'))) )+
    
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_color_manual(values = greys[8]) +
    
    scale_linetype_manual(values = 'solid') +
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title = paste('Treatment Effect Heterogeneity by Dollar Store Counts and Entry Events'),
    #       subtitle = paste0('Geography: ', model_geography, "\n", 
    #                      'Outcome: ', dep_var_subtitle))+
    
    # ggh4x::facet_grid2(tidy_name ~ ., scales = 'free_x', independent = 'x')+ #This means that the variable categories are on y-axis and and store types are on x-axis. 
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'),
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 12),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
}
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_vs_region_or_division')
#--------------------------------------------------------------------------------------------#
plot_effects_vs_region_division <- function(dta, ci_label_str, ci_level, reg_div_str, decimal_place_y, title_str, subtitle_str){
  
  dta <- dta %>%
    
    mutate(ci_label = ci_label_str, 
           coefficient_label = paste0('Coefficient Estimate'))
  
  dta %>%
    
    ggplot(aes(x = tidy_name, y = estimate)) +
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    coord_flip() +
    
    # facet_wrap(label~., nrow = fwrap_nrow, ncol = fwrap_ncol, scales = 'fixed')+ #This means that the variable categories are on y-axis and and store types are on x-axis. 
    
    scale_y_continuous(name = 'Effect Size',  
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = reg_div_str) +
    
    scale_color_manual(values = greys[8]) +
    
    scale_linetype_manual(values = 'solid') + 
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title = paste('Treatment Effect Heterogeneity by', reg_div_str, sep = ' '), 
    #      paste0('Geography: ', model_geography, "\n", 
    #           'Outcome: ', dep_var_subtitle)) + 
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle = 45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12, angle = 0, hjust = 0.9), 
          axis.title = element_text(size = 12),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.ticks.length.x = unit(.25, 'cm'), 
          axis.ticks.x = element_line(color='black'), 
          axis.ticks.y = element_blank(),
          panel.grid.major.x = element_line(colour='grey', linewidth = 0.5), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.5), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size = 12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color = 'black', fill=NA), 
          strip.background.y = element_rect(colour = NA, fill=NA),
          strip.background.x = element_rect(colour = 'black', fill = 'white'),
          strip.text.y = element_text(size = 12, face='bold', angle = 90),
          strip.text.x = element_text(size = 12, face='bold', angle = 0),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
print('Sourced: plot_ds_counts_entries_on_time')
#--------------------------------------------------------------------------------------------#
plot_ds_counts_entries_on_time <- function(dta, 
                                           filter_covariate_type,
                                           ci_label_str,
                                           ci_level,
                                           y_axis_title, x_axis_title, 
                                           title_str, 
                                           subtitle_str,
                                           decimal_place_y){
  
  
  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta <- dta %>%
    
    filter(!!!filter_covariate_type) %>%
  
    mutate(ci_label = ci_label_str )
  
  dta %>%
    
    ggplot(aes(x = term, y = estimate, group = label))+
    
    geom_point(aes(color = label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    # geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
    #            color = greys[6], 
    #            alpha = 0.70, 
    #            linetype = 'dashed', 
    #            linewidth = 0.5)+
    
    # ggh4x::facet_grid2(label ~ tidy_name, scales = 'free_x', independent = 'x')+ # This means that the grid is arranged where each row represents an outcome variables. 
    # facet_wrap(~tidy_name, scales = 'free')+ # This means that the grid is arranged where each row represents an outcome variables. 
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 5),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, 
                       breaks = scales::breaks_pretty(n = 15))+
    
    scale_color_manual(values = greys[8])+
    
    scale_linetype_manual(values = 'solid') +
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title = plot_title, 
    #      
    #    subtitle = paste0('Geography: ', model_geography, "\n", 
    #                      'Outcome: ', dep_var_subtitle)) + 
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.background.y = element_rect(colour = 'white'),
          strip.text = element_text(size = 12, face = 'bold'),
          panel.background = element_blank())
  
}

#--------------------------------------------------------------------------------------------#
print('Sourced: plot_ds_entry_on_covar_interacts')
#--------------------------------------------------------------------------------------------#
plot_ds_entry_on_covar_interacts <- function(dta, 
                                             ci_label_str,
                                             ci_level,
                                             filter_covariate_type,
                                             y_axis_title, 
                                             x_axis_title,
                                             x_axis_subtitle,
                                             legend_title_str,
                                             title_str, 
                                             subtitle_str,
                                             decimal_place_y){
  
  
  # Add an additional label for the plot legends.
  
  dta <- dta %>% mutate(ci_label = ci_label_str)

  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  dta %>% 
    
    filter(!!!filter_covariate_type) %>%
    
    ggplot(aes(x = entry, y = estimate, group = quartile))+
    
    geom_col(aes(fill = quartile), 
             color = greys[9],
             linewidth = 0.5,
             position = position_dodge(width = 0.90), 
             width = 0.75) +
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd,
                      linetype = ci_label),
                  color = greys[9], 
                  alpha = 1, 
                  linewidth = 0.75, 
                  width = 0.4,
                  position = position_dodge(width = 0.90))+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 8),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_color_manual(values = c(greys[c(8, 7, 6)]), 
                       labels = paste(c('First', 'Middle', 'Third'), 'Quartile'), 
                       name = legend_title_str)+
    
    scale_fill_manual(values = c(greys[c(8, 7, 6)]), 
                       labels = paste(c('First', 'Middle', 'Third'), 'Quartile'), 
                       name = legend_title_str)+
    
    scale_linetype_manual(values = 'solid',
                          name = '') +
    
    
    guides(color = guide_legend(order = 1, title.position = 'top'), 
           fill = guide_legend(order = 1, title.position = 'top'), 
           linetype = guide_legend(order = 2, title.position = 'top'))+
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_text(size=12, face = 'bold', hjust = 0.65), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.background.y = element_rect(colour = 'white'),
          strip.text = element_text(size = 12, face = 'bold'),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_ds_entry_on_grocery_interacts')
#--------------------------------------------------------------------------------------------#
plot_ds_entry_on_grocery_interacts <- function(dta, 
                                               ci_label_str, 
                                               ci_level,
                                               filter_covariate_type,
                                               one_or_more_grocers,
                                               y_axis_title, 
                                               x_axis_title,
                                               x_axis_subtitle,
                                               legend_title_str,
                                               title_str, 
                                               subtitle_str,
                                               decimal_place_y, 
                                               lower_ylim, 
                                               upper_ylim, 
                                               nbreaks){ # If one_or_more_grocers = FALSE, then these must be specified. 
  

  filter_covariate_type <- dplyr::enquos(filter_covariate_type)
  
  # Remove cases in which year 2005 grocery stores were 0, implying low-food access in 2005. 
  # Estimates were negative, implying dollar stores improved food access following entry.   
 if (isTRUE(one_or_more_grocers)){
  grocery_of_interest <- as.character(1:4)  # For grocers 1 to 4
  } else {
  grocery_of_interest <- as.character(0:4)  # For grocers 0 to 4
  }
  
  dta <- dta %>% filter(grocery_stores %in% grocery_of_interest) 
  
  # Add an additional label for the plot legends.
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta <- dta %>%
    
    filter(!!!filter_covariate_type) %>%
    
    mutate(across(.cols = c(entry, grocery_stores), 
                  .fn = ~factor(., levels = unique(.))) ) # Set to factors to order x-axis and legend labels in ascending order. 
  
  # Set legend labels for plots. 
  legend_labels <- unique(dta$grocery_stores)
  
 p <- dta %>%
    
    ggplot(aes(x = entry, y = estimate, group = grocery_stores))+
    
   geom_col(aes(fill = grocery_stores), 
            color = greys[9],
            linewidth = 0.50,
            position = position_dodge(width = 0.90), 
            width = 0.75) +
    
   geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                     ymin = estimate - ci_level*bootstrap_sd,
                     linetype = ci_label),
                 color = greys[9], 
                 alpha = 1, 
                 linewidth = 0.75, 
                 width = 0.4,
                 position = position_dodge(width = 0.90))+
    
   geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
              color = greys[6], 
              alpha = 0.70, 
              linetype = 'dashed', 
              linewidth = 0.5)+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = nbreaks),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_fill_manual(values = greys[seq(from = 8, to = 3)], 
                      labels = legend_labels, 
                      name = legend_title_str)+
    
    scale_linetype_manual(values = 'solid',
                          name = '') +

   guides(color = guide_legend(order = 1, title.position = 'top', title.hjust = 0.5),  
          fill = guide_legend(order = 1, title.position = 'top', title.hjust = 0.5), 
          linetype = guide_legend(order = 2, title.position = 'top', title.hjust = 0.5)) +
   
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
   theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
         axis.text.y = element_text(color = 'black', size = 12), 
         axis.title.y = element_text(size = 12, face = 'bold'),
         axis.title.x = element_text(size = 12, face = 'bold'),
         axis.ticks = element_line(color='black'), 
         axis.ticks.length = unit(.25, 'cm'), 
         panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
         panel.grid.minor = element_blank(), 
         panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
         panel.grid.minor.y = element_blank(), 
         axis.line = element_blank(), 
         legend.position='bottom', 
         legend.title = element_text(size=12, face = 'bold', hjust = 0.5), 
         legend.text = element_text(size=12), 
         legend.background = element_blank(), 
         legend.box.background = element_rect(colour = 'black'), 
         panel.border = element_rect(color='black', fill=NA), 
         strip.background = element_rect(colour = 'black', fill='white'),
         strip.background.y = element_rect(colour = 'white'),
         strip.text = element_text(size = 12, face = 'bold'),
         panel.background = element_blank(), 
         plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
 
 if (isTRUE(one_or_more_grocers)) { 
   p <- p  # If this is meant to represent the plot without modifications
 } else {  # No condition after 'else'
   p <- p + coord_cartesian(ylim = c(lower_ylim, upper_ylim))
 }
 
 p
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_change_in_outcome_on_relyear')
#--------------------------------------------------------------------------------------------#
plot_change_in_outcome_on_relyear <- function(dta, 
                                              ci_label_str, 
                                              ci_level, 
                                              percentage_type_str,
                                              x_value, y_value, standard_error, 
                                              y_axis_title, x_axis_title, 
                                              plot_title, plot_subtitle,
                                              decimal_place_y){
  
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta <- dta %>% filter(percentage_type == percentage_type_str)
  
  dta %>%
    
    ggplot(aes(x={{x_value}}, y = {{y_value}}))+
    
    geom_point(aes(color = effect_type), 
               shape = 16,
               size = 4, 
               alpha = 1) +
    
    geom_linerange(aes(ymax = {{y_value}} + ci_level*{{standard_error}}, 
                       ymin = {{y_value}} - ci_level*{{standard_error}}, 
                       linetype = ci_label),
                   color = greys[8],
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, 
               aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.7, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_linetype_manual(values = 'solid') +
    
    scale_color_manual(values = greys[8], labels = 'ATT (%)')+ # Need to include twice to not replicate legend point.
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::percent_format(accuracy = decimal_place_y))+
                       #labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = plot_title,
         subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold', vjust = -0.75), # Increase distance between x-axis title and time periods. 
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size = 15),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_ds_policy_vars')
#--------------------------------------------------------------------------------------------#
plot_effects_on_ds_policy_vars <- function(dta, 
                                           filter_reg_type, 
                                           geom_col_width, 
                                           ci_label_str, 
                                           ci_level,
                                           decimal_place_y, 
                                           analysis_dir){

# filter_reg_type <- dplyr::enquos(filter_reg_type)

dta_for_plot <- dta %>%
  
  filter(reg_type == {{filter_reg_type}})

# Set legend labels for plots. 
legend_labels <- dta_for_plot$variable

legend_labels <- c(str_subset(legend_labels, pattern = 'Defeated and Restrictions|More Than One|At Least One'),
                   str_subset(legend_labels, pattern = 'Defeated and Restrictions|More Than One|At Least One|^No Defeat', 
                              negate = TRUE),
                   str_subset(legend_labels, pattern = 'No Defeat')); legend_labels



dta_for_plot <- dta_for_plot %>% mutate(variable = factor(variable, levels = legend_labels), 
                              ci_label = paste0(ci_label_str) ) # '99% CI'

dta_for_plot %>%
  
  ggplot(aes(x = variable, y = estimate, group = variable))+
  
  geom_col(aes(color = variable, fill = variable), 
           position = position_dodge(width = 0.90), 
           alpha = 1,
           width = geom_col_width) +
  
  geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                    ymin = estimate - ci_level*bootstrap_sd,
                    linetype = ci_label),
                color = greys[9], 
                alpha = 1, 
                linewidth = 0.75, 
                width = 0.4,
                position = position_dodge(width = 0.90))+
  
  geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
             color = greys[6], 
             alpha = 0.70, 
             linetype = 'dashed', 
             linewidth = 0.5)+
  
  scale_y_continuous(name= "Average Treatment Effects",
                     breaks = scales::breaks_pretty(n = 25),#,
                     labels = scales::label_comma(accuracy = decimal_place_y))+ # decimal_place_y
  
  scale_x_discrete(name = 'Dollar Store Policy')+
  
  scale_color_manual(values = greys[c(8, 6, 4)], # 12, 15, 18, 21, 24, 27, 30 # sequential_levels_mako[c(12, 10, 8, 6)]
                     labels = legend_labels,
                     name = NULL)+
  
  scale_fill_manual(values = greys[c(8, 6, 4)],# sequential_levels_mako[c(12, 10, 8, 6)],
                    labels = legend_labels,
                    name = NULL)+
  
  scale_linetype_manual(values = 'solid',
                        name = '') +
  
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2))+
  
  labs(title = NULL,
       subtitle = NULL) +
  
  theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, hjust = 1, vjust = 0.5), 
        axis.text.y = element_text(color = 'black', size = 12, angle = 90, hjust = 0.5), 
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.ticks.x = element_line(color='black'), 
        axis.ticks.length.x = unit(.25, 'cm'), 
        axis.ticks.length.y = unit(0, 'pt'),
        panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
        panel.grid.minor.y = element_blank(), 
        axis.line = element_blank(), 
        legend.position='bottom', 
        legend.title = element_blank(), 
        legend.title.position = 'top',
        legend.title.align = 0.5, # To make legend title centered. 
        legend.text = element_text(size=12), 
        legend.background = element_blank(), 
        legend.box.background = element_rect(colour = 'black'), 
        panel.border = element_rect(color='black', fill=NA), 
        strip.background = element_rect(colour = 'black', fill='white'),
        strip.background.y = element_rect(colour = 'white'),
        strip.text = element_text(size = 15, face = 'bold'),
        panel.background = element_blank(), 
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm")) + 
  
  coord_flip()


figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_effects_on_ds_policy_vars_', filter_reg_type, bootstrap_by_tracts, '.pdf')

ggsave(here::here(analysis_dir, 'Figures', dir_dep_var, model_geography, paste0('effects_on_ds_policy_vars', bootstrap_by_tracts), # Saves to directories and subdirectories.
                  figname),
       width = 9, height = 7, unit = 'in', dpi = 600)

}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_effects_on_ds_entry_grocery_and_superettes')
#--------------------------------------------------------------------------------------------#
plot_effects_on_ds_entry_grocery_and_superettes <- function(dta,
                                                            filter_ds_entry_type,
                                                            ci_label_str, 
                                                            ci_level,
                                                            end_num_stores,
                                                            one_store_pairs,
                                                            y_axis_title,
                                                            x_axis_title,
                                                            legend_title_str,
                                                            title_str,
                                                            subtitle_str,
                                                            decimal_place_y,
                                                            nbreaks){ # If one_or_more_grocers = FALSE, then these must be specified.
  
  
  
  
  reg_coefs_subsample <- dta %>% 
    
    filter(ds_entry == {{filter_ds_entry_type}})  %>%
    
    filter(grocery_stores %in% seq(0, 2, 1) ) %>% # Only 0 or 1 grocery stores pre-entry.
    
    filter(superettes %in% seq(0, 2, 1) ) %>% # Only 0 or 1 superettes pre-entry.
    
    filter(ds_entry_bins %in% seq(1, 4, 1) ) # 1 to 4 dollar store entries. 
  
  
  reg_coefs_subsample$total_stores <- factor(paste0( 
    reg_coefs_subsample$grocery_stores, ' Grocery', 
    ', ', 
    reg_coefs_subsample$superettes, ' Superette'
  ) )
  
  reg_coefs_subsample <- reg_coefs_subsample %>% 
    relocate(total_stores, .after = superettes)
  
    
  
  
  if (isTRUE(one_store_pairs)){
  reg_coefs_subsample <- reg_coefs_subsample %>% 
    filter(total_stores == "0 Grocery, 0 Superette" | 
             total_stores == "0 Grocery, 1 Superette" | 
             total_stores == "1 Grocery, 0 Superette" | 
             total_stores == "1 Grocery, 1 Superette") 
  
  reg_coefs_subsample$total_stores <- factor(reg_coefs_subsample$total_stores, 
                                              levels = c("0 Grocery, 0 Superette", 
                                                         "0 Grocery, 1 Superette", 
                                                         "1 Grocery, 0 Superette", 
                                                         "1 Grocery, 1 Superette") ) # Set to factors to order x-axis and legend labels in ascending order. 
  
  
  } else if (isFALSE(one_store_pairs)){
  reg_coefs_subsample <- reg_coefs_subsample %>% 
    filter(total_stores == "0 Grocery, 2 Superette" | 
             total_stores == "2 Grocery, 0 Superette" | 
             total_stores == "2 Grocery, 1 Superette" | 
             total_stores == "1 Grocery, 2 Superette" | 
             total_stores == "2 Grocery, 2 Superette") 
  
  reg_coefs_subsample$total_stores <- factor(reg_coefs_subsample$total_stores, 
                                              levels = c("0 Grocery, 2 Superette", 
                                                         "2 Grocery, 0 Superette",
                                                         "1 Grocery, 2 Superette", 
                                                         "2 Grocery, 1 Superette", 
                                                         "2 Grocery, 2 Superette") ) # Set to factors to order x-axis and legend labels in ascending order. 
  }
 
  # Set legend labels for plots. 
  legend_labels <- levels(reg_coefs_subsample$total_stores) 
  
  reg_coefs_subsample <- reg_coefs_subsample %>% mutate(ci_label = ci_label_str)
  
  reg_coefs_subsample %>%
    
    ggplot(aes(x = ds_entry_bins, y = estimate, group = total_stores))+
    
    geom_col(aes(fill = total_stores), 
             color = greys[9],
             linewidth = 0.50,
             position = position_dodge(width = 0.90), 
             width = 0.75) +
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd,
                      linetype = ci_label),
                  color = greys[9], 
                  alpha = 1, 
                  linewidth = 0.75, 
                  width = 0.4,
                  position = position_dodge(width = 0.90))+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = nbreaks),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_fill_manual(values = greys[seq(from = 8, to = 1)], 
                      labels = legend_labels, 
                      name = legend_title_str)+
    
    scale_linetype_manual(values = 'solid',
                          name = '') +
    
    guides(fill = guide_legend(order = 1, nrow = 2, title.position = 'top', title.hjust = 0.5), 
           linetype = guide_legend(order = 2, nrow = 1, title.position = 'top', label.position = 'top', title.hjust = 0.5)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_text(size=12, face = 'bold', hjust = 0.5), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.background.y = element_rect(colour = 'white'),
          strip.text = element_text(size = 12, face = 'bold'),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_ds_policy_vars')
#--------------------------------------------------------------------------------------------#
plot_errors_on_ds_policy_vars <- function(dta, 
                                          ci_label_str, 
                                          ci_level, 
                                          geom_col_width, 
                                          decimal_place_y, 
                                          analysis_dir){
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  
  dta %>%
    
    ggplot(aes(x = variable, y = estimate, group = variable))+
    
    geom_col(aes(color = variable, fill = variable), 
             position = position_dodge(width = 0.90), 
             alpha = 1,
             width = geom_col_width) +
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd,
                      linetype = ci_label),
                  color = greys[9], 
                  alpha = 1, 
                  linewidth = 0.75, 
                  width = 0.4,
                  position = position_dodge(width = 0.90))+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name= "Average Cross-Validation Errors",
                       breaks = scales::breaks_pretty(n = 25),#,
                       labels = scales::label_comma(accuracy = decimal_place_y))+ # decimal_place_y
    
    scale_x_discrete(name = 'Dollar Store Policy')+
    
    scale_color_manual(values = greys[c(8, 6, 4)], # 12, 15, 18, 21, 24, 27, 30 # sequential_levels_mako[c(12, 10, 8, 6)]
                       labels = legend_labels,
                       name = NULL)+
    
    scale_fill_manual(values = greys[c(8, 6, 4)],# sequential_levels_mako[c(12, 10, 8, 6)],
                      labels = legend_labels,
                      name = NULL)+
    
    scale_linetype_manual(values = 'solid',
                          name = '') +
    
    guides(color = guide_legend(order = 1), 
           fill = guide_legend(order = 1),
           linetype = guide_legend(order = 2))+
    
    labs(title = NULL,
         subtitle = NULL) +
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, hjust = 1, vjust = 0.5), 
          axis.text.y = element_text(color = 'black', size = 12, angle = 90, hjust = 0.5), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks.x = element_line(color='black'), 
          axis.ticks.length.x = unit(.25, 'cm'), 
          axis.ticks.length.y = unit(0, 'pt'),
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.title.position = 'top',
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.background.y = element_rect(colour = 'white'),
          strip.text = element_text(size = 15, face = 'bold'),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm")) + 
    
    coord_flip()
  
  figname <- paste0(str_to_lower(model_geography), '_', model_dep_var, '_errors_on_ds_policy_vars_policy_type', bootstrap_by_tracts, '.pdf')

  ggsave(here::here(analysis_dir, 'Figures', dir_dep_var, model_geography, paste0('errors_on_ds_policy_vars', bootstrap_by_tracts), # Saves to directories and subdirectories.
                    figname),
         width = 9, height = 7, unit = 'in', dpi = 600)

}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_vs_region')
#--------------------------------------------------------------------------------------------#
plot_errors_vs_region <- function(dta, 
                                  ci_label_str, 
                                  ci_level, 
                                  reg_div_str, 
                                  decimal_place_y, 
                                  title_str, 
                                  subtitle_str){
  
  dta <- dta %>% mutate(ci_label = ci_label_str)

  dta %>%
    
    ggplot(aes(x = tidy_name, y = estimate)) +
    
    geom_point(aes(color = coefficient_label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd, 
                      linetype = ci_label), 
                  color = greys[8],  
                  alpha = 1, 
                  linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    coord_flip() +
    
    # facet_wrap(label~., nrow = fwrap_nrow, ncol = fwrap_ncol, scales = 'fixed')+ #This means that the variable categories are on y-axis and and store types are on x-axis. 
    
    scale_y_continuous(name = 'Average Cross-Validation Error',  
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = reg_div_str) +
    
    scale_color_manual(values = greys[8]) +
    
    scale_linetype_manual(values = 'solid') + 
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title = paste('Treatment Effect Heterogeneity by', reg_div_str, sep = ' '), 
    #      paste0('Geography: ', model_geography, "\n", 
    #           'Outcome: ', dep_var_subtitle)) + 
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle = 45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12, angle = 0, hjust = 0.9), 
          axis.title = element_text(size = 12),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.ticks.length.x = unit(.25, 'cm'), 
          axis.ticks.x = element_line(color='black'), 
          axis.ticks.y = element_blank(),
          panel.grid.major.x = element_line(colour='grey', linewidth = 0.5), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.5), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size = 12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color = 'black', fill=NA), 
          strip.background.y = element_rect(colour = NA, fill=NA),
          strip.background.x = element_rect(colour = 'black', fill = 'white'),
          strip.text.y = element_text(size = 12, face='bold', angle = 90),
          strip.text.x = element_text(size = 12, face='bold', angle = 0),
          panel.background = element_blank())
  
  
  
}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_grocery_stores_2005')
#--------------------------------------------------------------------------------------------#
plot_errors_on_grocery_stores_2005 <- function(dta, 
                                               ci_label_str, 
                                               ci_level,
                                               y_axis_title, x_axis_title, 
                                               title_str, subtitle_str,
                                               decimal_place_y){
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  dta %>%
    
    ggplot(aes(x=count, y = estimate))+
    
    geom_point(aes(color = coefficient_label), 
               shape = 16, 
               size = 4, 
               alpha = 1)+
    
    geom_linerange(aes(ymax = estimate + ci_level*bootstrap_sd, 
                       ymin = estimate - ci_level*bootstrap_sd, 
                       linetype = ci_label), 
                   color = greys[8],  
                   alpha = 1, 
                   linewidth = 0.75)+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    scale_y_continuous(name = y_axis_title, 
                       breaks = scales::breaks_pretty(n = 15), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_color_manual(values = greys[8])+
    
    scale_linetype_manual(values = 'solid')+
    
    guides(color = guide_legend(order = 1), # Changes order of legend items. 
           linetype = guide_legend(order = 2)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    # labs(title = paste('Treatment Effect Heterogeneity by Dollar Store Counts and Entry Events'),
    #       subtitle = paste0('Geography: ', model_geography, "\n", 
    #                      'Outcome: ', dep_var_subtitle)
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle = 45, hjust = 1, vjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color = 'black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.text = element_text(size = 12),
          panel.background = element_blank())

}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_ds_entry_x_grocery_stores')
#--------------------------------------------------------------------------------------------#
plot_errors_on_ds_entry_x_grocery_stores <- function(dta, 
                                                     ci_label_str, 
                                                     ci_level,
                                                     y_axis_title, 
                                                     x_axis_title,
                                                     x_axis_subtitle,
                                                     legend_labels_store_str,
                                                     legend_title_str,
                                                     title_str, 
                                                     subtitle_str,
                                                     decimal_place_y,
                                                     nbreaks){ # If one_or_more_grocers = FALSE, then these must be specified. 
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  p <- dta %>%
    
    ggplot(aes(x = entry, y = estimate, group = grocery_count))+
    
    geom_col(aes(fill = grocery_count), 
             color = greys[9],
             linewidth = 0.5,
             position = position_dodge(width = 0.90), 
             width = 0.75) +
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd,
                      linetype = ci_label),
                  color = greys[9], 
                  alpha = 1, 
                  linewidth = 0.75, 
                  width = 0.4,
                  position = position_dodge(width = 0.90))+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    # ggh4x::facet_grid2(label ~ tidy_name, scales = 'free_x', independent = 'x')+ # This means that the grid is arranged where each row represents an outcome variables. 
    # facet_wrap(~tidy_name, scales = 'free')+ # This means that the grid is arranged where each row represents an outcome variables. 
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = nbreaks),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_fill_manual(values = greys[seq(from = 8, to = 3)], 
                      labels = legend_labels_store_str, 
                      name = legend_title_str)+
    
    scale_linetype_manual(values = 'solid',
                          name = '') +
    
    guides(color = guide_legend(order = 1, title.position = 'top', title.hjust = 0.5),  
           fill = guide_legend(order = 1, title.position = 'top', title.hjust = 0.5), 
           linetype = guide_legend(order = 2, title.position = 'top', title.hjust = 0.5)) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_text(size=12, face = 'bold', hjust = 0.5), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.background.y = element_rect(colour = 'white'),
          strip.text = element_text(size = 12, face = 'bold'),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
  
  p
}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_ds_entry_x_baseline_stores')
#--------------------------------------------------------------------------------------------#
plot_errors_on_ds_entry_x_baseline_stores <- function(dta, 
                                                     ci_label_str,
                                                     ci_level,
                                                     y_axis_title, 
                                                     x_axis_title,
                                                     x_axis_subtitle,
                                                     legend_labels_store_str,
                                                     legend_title_str,
                                                     title_str, 
                                                     subtitle_str,
                                                     decimal_place_y,
                                                     nbreaks){ # If one_or_more_grocers = FALSE, then these must be specified. 
  
  dta <- dta %>% mutate(ci_label = ci_label_str)
  
  p <- dta %>%
    
    ggplot(aes(x = entry, y = estimate, group = store_count))+
    
    geom_col(aes(fill = store_count), 
             color = greys[9],
             linewidth = 0.5,
             position = position_dodge(width = 0.90), 
             width = 0.75) +
    
    geom_errorbar(aes(ymax = estimate + ci_level*bootstrap_sd, 
                      ymin = estimate - ci_level*bootstrap_sd,
                      linetype = ci_label),
                  color = greys[9], 
                  alpha = 1, 
                  linewidth = 0.75, 
                  width = 0.4,
                  position = position_dodge(width = 0.90))+
    
    geom_hline(data = null_effect_df, aes(yintercept = null_effect), 
               color = greys[6], 
               alpha = 0.70, 
               linetype = 'dashed', 
               linewidth = 0.5)+
    
    # ggh4x::facet_grid2(label ~ tidy_name, scales = 'free_x', independent = 'x')+ # This means that the grid is arranged where each row represents an outcome variables. 
    # facet_wrap(~tidy_name, scales = 'free')+ # This means that the grid is arranged where each row represents an outcome variables. 
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = nbreaks),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    # scale_color_manual(values = greys[seq(from = 8, to = 3)], # 12, 15, 18, 21, 24, 27, 30 
    #                    labels = legend_labels_store_str,
    #                    name = legend_title_str)+
    
    scale_fill_manual(values = greys[seq(from = 8, to = 3)], 
                      labels = legend_labels_store_str, 
                      name = legend_title_str)+
    
    scale_linetype_manual(values = 'solid',
                          name = '') +
    # scale_shape_manual(values = c(16, 17, 15), 
    #                    labels = paste(c('First', 'Middle', 'Third'), 'Quartile'), 
    #                    name = legend_title_str)+
    
    guides(color = guide_legend(order = 1, title.position = 'top', title.hjust = 0.5),  
           fill = guide_legend(order = 1, title.position = 'top', title.hjust = 0.5), 
           linetype = guide_legend(order = 2, title.position = 'top', title.hjust = 0.5)) +
    
    # guides(color = guide_legend(order = 1, title.position = 'top'),  
    #        fill = guide_legend(order = 1, title.position = 'left'), 
    #        linetype = guide_legend(order = 1, title.position = 'left') ) +
    
    labs(title = title_str, 
         subtitle = subtitle_str) +
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_blank(), 
          legend.position='bottom', 
          legend.title = element_text(size=12, face = 'bold', hjust = 0.5), 
          legend.text = element_text(size=12), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='white'),
          strip.background.y = element_rect(colour = 'white'),
          strip.text = element_text(size = 12, face = 'bold'),
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = .5, unit = "cm"))
  
  
  p
}

#--------------------------------------------------------------------------------------------#

