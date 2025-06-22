print('Sourced: plot_actual_on_predicted')
print('Sourced: plot_actual_on_predicted_facet')

library(viridisLite)
sequential_levels_mako <- mako(30, begin = 0, end = 1, direction = 1)
# Create a new data frame to include in the plot_effects_on_covars_norm function and the plot_errors_on_covars_binned function.
# This allows us to specify a vertical line in each facet to indicate a null effect of the predictor 
# on the causal estimate. 
null_effect_df <- data.frame(null_effect = 0)
print(paste('Loaded: null_effect_df$null_effect =', null_effect_df$null_effect))
#--------------------------------------------------------------------------------------------#
plot_actual_on_predicted <- function(dta, y_value, standard_error,
                                     alpha_level, 
                                     y_axis_title, x_axis_title, 
                                     plot_title, plot_subtitle,
                                     x_intercept_val, decimal_place_y){
  
  dta %>% 
    
    ggplot(aes(x=rel_year, y = {{y_value}}, group = Outcome))+
    
    
    geom_point(aes(fill = Outcome, shape = Outcome), 
               stroke = 0.5, color = 'black', size = 4, alpha = 0.95, position = position_dodge(width = 0.3)) +
    
    geom_linerange(aes(ymax = {{y_value}} + qnorm(1 - alpha_level/2)*{{standard_error}}, 
                       ymin = {{y_value}} - qnorm(1 - alpha_level/2)*{{standard_error}}, 
                       color = Outcome), 
                   alpha = 0.95, linewidth = 1.05, 
                   position = position_dodge(width = 0.3))+
    
    
    geom_vline(xintercept = x_intercept_val, color = 'black', linetype = 'dashed', linewidth = 1)+
    
    scale_fill_manual(values = sequential_levels_mako[c(1, 10)])+
    
    scale_color_manual(values = sequential_levels_mako[c(1, 10)])+
    
    scale_shape_manual(values = c(24, 21))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n = 10), 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-13, 14, by = 1))+
    
   #  coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title = plot_title, 
         subtitle = plot_subtitle)+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 14, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 14), 
          axis.title.y = element_text(size = 15, face = 'bold'),
          axis.title.x = element_text(size = 15, face = 'bold'),
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
          panel.background = element_blank(), 
          plot.margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = 'cm'))
  
}