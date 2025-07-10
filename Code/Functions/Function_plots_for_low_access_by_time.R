#--------------------------------------------------------------------------------------------#
library(viridisLite)
sequential_levels_mako <- mako(30, begin = 0, end = 1, direction = 1)
library(ggh4x) # For facet_wrap2, facet_grid2
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_treated_vs_nevertreated_by_year')
#--------------------------------------------------------------------------------------------#
plot_treated_vs_nevertreated_by_year <- function(dta, 
                                                 stat_type,
                                                 x_value, 
                                                 y_value, 
                                                 group_var,
                                                 y_axis_title, x_axis_title, 
                                                 plot_title, plot_subtitle,
                                                 decimal_place_y, 
                                                 y1_lim, y2_lim, 
                                                 x_intercept_val = NULL){
  

  dta %>% 
    
    filter(Statistic == stat_type) %>%
    
    ggplot(aes(x={{x_value}}, y = {{y_value}}, group = {{group_var}}))+
    
    geom_point(aes(color = {{group_var}}, shape = {{group_var}}), 
               size = 3, 
               alpha = 0.8) +
    
    geom_line(aes(linetype = {{group_var}}), linewidth = 1, alpha = 0.8)+
    
    
    scale_color_manual(values = c(sequential_levels_mako[1], sequential_levels_mako[6]))+
    
    scale_shape_manual(values = c(16, 17))+
    
    scale_linetype_manual(values = c('solid', 'longdash'))+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::pretty_breaks(n= 10),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = scales::pretty_breaks(n = 16))+
    
    geom_vline(xintercept = x_intercept_val, color = 'black', linetype = 'solid', linewidth = 1)+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    labs(title= plot_title, subtitle = plot_subtitle)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks.x = element_blank(), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=11), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size=12),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
print('Sourced: plot_dep_var_by_year')
#--------------------------------------------------------------------------------------------#
plot_dep_var_by_year <- function(dta, 
                                 outcome_type,
                                 statistic_type, 
                                 y_axis_title, x_axis_title, 
                                 plot_title, plot_subtitle,
                                 decimal_place_y){
  
  
  dta %>% 
    
    filter(grepl(outcome_type, outcome) & statistic == statistic_type) %>%
    
    ggplot(aes(x= year, y = value, group = outcome))+
    
    geom_point(aes(color = outcome, shape = outcome), 
               show.legend = FALSE, 
               size = 3, 
               alpha = 0.8) +
    
    geom_line(linetype = 'solid', linewidth = 1.05, alpha = 0.8)+
    
    scale_color_manual(values = sequential_levels_mako[1])+
    
    scale_shape_manual(values = 16)+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n= 10),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = scales::breaks_pretty(n = 16))+
    
    labs(title= plot_title, subtitle = plot_subtitle)+
     
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=11), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size=12),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
print('Sourced: plot_dep_var_by_year_and_treatment')
#--------------------------------------------------------------------------------------------#
plot_dep_var_by_year_and_treatment <- function(dta, 
                                 outcome_type,
                                 statistic_type, 
                                 treatment_type,
                                 y_axis_title, x_axis_title, 
                                 plot_title, plot_subtitle,
                                 decimal_place_y){
  
  
  dta %>% 
    
    filter(grepl(outcome_type, outcome) & statistic == statistic_type & treatment == treatment_type) %>%
    
    ggplot(aes(x= year, y = value, group = treatment))+
    
    geom_point(color = sequential_levels_mako[1], 
               shape = 16,
               size = 3, 
               alpha = 0.8) +
    
    geom_line(linetype = 'solid', linewidth = 1.05, alpha = 0.8)+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n= 10),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = scales::breaks_pretty(n = 16))+
    
    labs(title= plot_title, subtitle = plot_subtitle)+

    
    theme(axis.text.x = element_text(color = 'black', size = 12), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=11), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size=12),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------#
print('Sourced: plot_dep_var_by_rel_time')
#--------------------------------------------------------------------------------------------#
plot_dep_var_by_rel_time <- function(dta, 
                                 outcome_type,
                                 statistic_type, 
                                 y_axis_title, x_axis_title, 
                                 plot_title, plot_subtitle,
                                 decimal_place_y, xlim_1, xlim_2){
  
  dta %>% 
    
    filter(grepl(outcome_type, outcome) & statistic == statistic_type) %>%
    
    ggplot(aes(x= rel_year, y = value, group = outcome))+
    
    geom_point(aes(color = outcome, shape = outcome), 
               show.legend = FALSE, 
               size = 3, 
               alpha = 0.8) +
    
    coord_cartesian(xlim = c(xlim_1, xlim_2)) +
    
    geom_line(linetype = 'solid', linewidth = 1.05, alpha = 0.8)+
    
    geom_vline(xintercept = 0, linetype = 'dashed', linewidth = 1, color = 'black', alpha = 0.8)+
    
    scale_color_manual(values = sequential_levels_mako[1])+
    
    scale_shape_manual(values = 16)+
    
    scale_y_continuous(name= y_axis_title, 
                       breaks = scales::breaks_pretty(n= 10),
                       labels = scales::label_comma(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = scales::breaks_pretty(n = 28), 
                       expand = c(0.01, 0.01))+ # Determines cutoff of x axis on left- and right-hand sides. 
    
    labs(title= plot_title, subtitle = plot_subtitle)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=0, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks = element_line(color='black'), 
          axis.ticks.length = unit(.25, 'cm'), 
          panel.grid.major.x=element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
          panel.grid.minor.y = element_blank(), 
          axis.line = element_line(colour='black'), 
          legend.position='bottom', 
          legend.title = element_blank(), 
          legend.text = element_text(size=11), 
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = 'black'), 
          panel.border = element_rect(color='black', fill=NA), 
          strip.background = element_rect(colour = 'black', fill='grey'),
          strip.text = element_text(size=12),
          panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#

