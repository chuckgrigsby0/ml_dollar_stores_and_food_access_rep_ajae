library(viridisLite)
sequential_levels_mako <- mako(30, begin = 0, end = 1, direction = 1)
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_errors_on_relyear')
#--------------------------------------------------------------------------------------------#
plot_errors_on_relyear <- function(dta, y_value, standard_error, 
                                   y_axis_title, x_axis_title, 
                                   #breaks_y, 
                                   decimal_place_y, 
                                   y1_lim, y2_lim){
  #y_value <- sym(y_value)
  #standard_error <- sym(standard_error)
  
  dta %>% 
    
    ggplot(aes(x=rel_year, y = {{y_value}}, group=Outcome))+
    
    geom_point(aes(color = Outcome, shape = Outcome), 
               size = 3, alpha = 0.8) +
    
    geom_linerange(aes(ymax = {{y_value}} + 1.96*{{standard_error}}, 
                       ymin = {{y_value}} - 1.96*{{standard_error}}, 
                       color = Outcome), 
                   alpha = 0.8, linewidth = 1.05)+
    
    scale_color_manual(values = sequential_levels_mako[2])+
    
    scale_shape_manual(values = 16)+
    
    scale_y_continuous(name= y_axis_title, 
                       #breaks = breaks_y, 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-14, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    #labs(title='Number of Stores by Region (2000-2020)')+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks.x = element_blank(), 
          #axis.ticks.length.x = unit(.25, 'cm'), 
          #axis.ticks.x = element_line(color='black'), 
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
print('Sourced: plot_actual_on_predicted')
#--------------------------------------------------------------------------------------------#
plot_actual_on_predicted <- function(dta, y_value, standard_error,
                                     y_axis_title, x_axis_title, 
                                     #breaks_y, 
                                     x_intercept_val, decimal_place_y, y1_lim, y2_lim){

dta %>% 
  
  ggplot(aes(x=rel_year, y = {{y_value}}, group = Outcome))+
  
  geom_point(aes(color = Outcome, shape = Outcome), 
             size = 3, alpha = 0.8, position = position_dodge(width = 0.5)) +
    
    
    
    geom_linerange(aes(ymax = {{y_value}} + 1.96*{{standard_error}}, 
                       ymin = {{y_value}} - 1.96*{{standard_error}}, 
                       color = Outcome), 
                   alpha = 0.8, linewidth = 1.05, position = position_dodge(width = 0.5))+
    
  
  geom_vline(xintercept = x_intercept_val, color = 'black', linetype = 'solid', linewidth = 1)+
  
  scale_color_manual(values = sequential_levels_mako[c(2, 5)])+
  
  scale_shape_manual(values = c(17, 16))+
  
  scale_y_continuous(name= y_axis_title, 
                     #breaks = breaks_y, 
                     labels = scales::label_number(accuracy = decimal_place_y))+
  
  scale_x_continuous(name = x_axis_title, breaks = seq(-14, 14, by = 1))+
    
  coord_cartesian(ylim=c(y1_lim, y2_lim))+
  
  #labs(title='Number of Stores by Region (2000-2020)')+
  
  # facet_wrap(~Geography, scales='fixed', nrow =2)+
  
  theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
        axis.text.y = element_text(color = 'black', size = 12), 
        axis.title.y = element_text(size=12, face = 'bold'),
        axis.title.x = element_text(size=12, face = 'bold'),
        axis.ticks.x = element_blank(), 
        #axis.ticks.length.x = unit(.25, 'cm'), 
        #axis.ticks.x = element_line(color='black'), 
        panel.grid.major.x=  element_line(colour='grey', linewidth = 0.1), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(colour='grey', linewidth = 0.1), 
        panel.grid.minor.y = element_blank(), 
        axis.line = element_line(colour = 'black'), 
        legend.position='bottom', 
        legend.title = element_blank(), 
        legend.text = element_text(size=11), 
        legend.background = element_blank(), 
        legend.box.background = element_rect(colour = 'black'), 
        panel.border = element_rect(color='black', fill = NA), 
        strip.background = element_rect(colour = 'black', fill='grey'),
        strip.text = element_text(size = 12),
        panel.background = element_blank())
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: plot_error_vs_covars')
#--------------------------------------------------------------------------------------------#
plot_error_vs_covars <- function(dta, 
                                 y_value, 
                                 standard_error,
                                 y_axis_title, 
                                 x_axis_title, 
                                 #breaks_y, 
                                 decimal_place_y, 
                                 y1_lim, 
                                 y2_lim){
  
  dta %>% 
    
    ggplot(aes(x=term, y = {{y_value}}, group = Label))+
    
    geom_point(aes(color = Label), size = 3, shape = 16, alpha = 0.8) +
    
    geom_linerange(aes(ymax = {{y_value}} + 1.96*{{standard_error}}, 
                       ymin = {{y_value}} - 1.96*{{standard_error}}), 
                   color = Outcome, alpha = 0.8, linewidth = 1.05)+
    
    scale_y_continuous(name= y_axis_title, 
                       #breaks = breaks_y, 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_discrete(name = x_axis_title)+
    
    scale_color_manual(values = sequential_levels_mako[2])+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    #labs(title='Number of Stores by Region (2000-2020)')+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 9, angle=90, vjust=0.1, hjust = 1), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, vjust = -0.5, face = 'bold'),
          axis.ticks.length.x = unit(.25, 'cm'), 
          axis.ticks.x = element_line(color = 'black'), 
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

print('Sourced: plot_effects_on_relyear')
#--------------------------------------------------------------------------------------------#
plot_effects_on_relyear <- function(dta, y_value, standard_error, 
                                   y_axis_title, x_axis_title, 
                                   #breaks_y, 
                                   decimal_place_y, 
                                   y1_lim, y2_lim){
  #y_value <- sym(y_value)
  #standard_error <- sym(standard_error)
  
  dta %>% 
    
    ggplot(aes(x=rel_year, y = {{y_value}}, group=Outcome))+
    
    geom_point(aes(color = Outcome, shape = Outcome), 
               size = 3, alpha = 0.8) +
    
    geom_linerange(aes(ymax = {{y_value}} + 1.96*{{standard_error}}, 
                       ymin = {{y_value}} - 1.96*{{standard_error}}, 
                       color = Outcome), 
                   alpha = 0.8, linewidth = 1.05)+
    
    scale_color_manual(values = sequential_levels_mako[2], labels = expression(tau))+ # I needed to include twice to not replicate legend point.
    
    scale_shape_manual(values = 16, labels = expression(tau))+
    

    
    scale_y_continuous(name= y_axis_title, 
                       #breaks = breaks_y, 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-14, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    #labs(title='Number of Stores by Region (2000-2020)')+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks.x = element_blank(), 
          #axis.ticks.length.x = unit(.25, 'cm'), 
          #axis.ticks.x = element_line(color='black'), 
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
print('Sourced: plot_effects_on_relyear_by_entry_bin')
#--------------------------------------------------------------------------------------------#
plot_effects_on_relyear_by_entry_bin <- function(dta, y_value, #standard_error, 
                                                 y_axis_title, x_axis_title, 
                                                 #breaks_y, 
                                                 decimal_place_y, 
                                                 y1_lim, y2_lim){
  #y_value <- sym(y_value)
  #standard_error <- sym(standard_error)
  unique_bins <- unique(dta$entry_events)
  
  dta %>% 
    
    ggplot(aes(x=rel_year, y = {{y_value}}, group = entry_events))+
    
    geom_point(aes(color = entry_events, shape = entry_events), 
               size = 3, alpha = 0.8) +
    
    geom_line(aes(), 
              linewidth = 1, alpha = 0.8)+
    
    #geom_linerange(aes(ymax = {{y_value}} + 1.96*{{standard_error}}, 
     #                  ymin = {{y_value}} - 1.96*{{standard_error}}, 
      #                 color = Outcome), 
       #            alpha = 0.8, linewidth = 1.05)+
    
    scale_color_manual(values = c(sequential_levels_mako[2], sequential_levels_mako[4], sequential_levels_mako[6]), 
                       labels = expression(tau))+
    
    scale_shape_manual(values = c(15, 16, 17), 
                       labels = expression(tau))+
    
    scale_y_continuous(name= y_axis_title, 
                       #breaks = breaks_y, 
                       labels = scales::label_number(accuracy = decimal_place_y))+
    
    scale_x_continuous(name = x_axis_title, breaks = seq(-14, 14, by = 1))+
    
    coord_cartesian(ylim=c(y1_lim, y2_lim))+
    
    #labs(title='Number of Stores by Region (2000-2020)')+
    
    # facet_wrap(~Geography, scales='fixed', nrow =2)+
    
    theme(axis.text.x = element_text(color = 'black', size = 12, angle=90, vjust=0.75), 
          axis.text.y = element_text(color = 'black', size = 12), 
          axis.title.y = element_text(size=12, face = 'bold'),
          axis.title.x = element_text(size=12, face = 'bold'),
          axis.ticks.x = element_blank(), 
          #axis.ticks.length.x = unit(.25, 'cm'), 
          #axis.ticks.x = element_line(color='black'), 
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
