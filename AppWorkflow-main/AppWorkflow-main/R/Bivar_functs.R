barplot_logic_logic<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(y=.data[[var1]],
                 fill=.data[[var2]]))+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.y = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_logic_factnonord_stack<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(y=.data[[var1]],
                 fill=.data[[var2]]),
             position = "stack")+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.y = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_logic_factnonord_dodge<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(y=.data[[var1]],
                 fill=.data[[var2]]),
             position = "dodge")+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.y = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_logic_factord_dodge<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(x=.data[[var1]],
                 fill=.data[[var2]]),
             position = "dodge")+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.x = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_logic_factord_stack<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(y=.data[[var1]],
                 fill=.data[[var2]]),
             position = "stack")+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.y = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

density_logic_num<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_density(aes(x=.data[[var2]],
                     fill=.data[[var1]],
                     color=.data[[var1]]),
                 alpha=0.5,
                 position = "identity")+
    scale_fill_manual(values = palettes[var1][[1]])+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text.x = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

boxplot_logic_num<-function(df,var1,var2,palettes){
    ggplot2::ggplot(df)+
    geom_boxplot(aes(y=.data[[var2]],
                     x=.data[[var1]]),
                 alpha=0.5,
                 fill=palettes[var1][[1]],
                 color=palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text = element_text(family = "Martel Sans",size=15),
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

histogram_logic_num_identity<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_histogram(aes(x=.data[[var2]],
                     fill=.data[[var1]],
                     color=.data[[var1]]),
                 alpha=0.5,
                 position = "identity")+
    scale_fill_manual(values = palettes[var1][[1]])+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text = element_text(family = "Martel Sans",size=15),
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

histogram_logic_num_stack<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_histogram(aes(x=.data[[var2]],
                       fill=.data[[var1]],
                       color=.data[[var1]]),
                   alpha=0.5,
                   position = "stack")+
    scale_fill_manual(values = palettes[var1][[1]])+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text = element_text(family = "Martel Sans",size=15),
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_factornonord_factornonord<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(y=.data[[var1]],
                 fill=.data[[var2]]))+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.y = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_factornonord_factorord<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(x=.data[[var2]],
                 fill=.data[[var1]]),
             position = "stack")+
    scale_fill_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text.x = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_factorord_factornonord<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(y=.data[[var1]],
                 fill=.data[[var2]]),
             position = "stack")+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      axis.text.y = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15)
    )
}

density_factornonord_num<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_density(aes(x=.data[[var2]],
                     color=.data[[var1]]),
                 alpha=0.5,
                 position = "identity")+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text.x = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      legend.text = element_text(family = "Martel Sans",size=15),
    )
}

histogram_factornonord_num_identity<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_histogram(aes(x=.data[[var2]],
                       fill=.data[[var1]],
                       color=.data[[var1]]),
                   alpha=0.5,
                   position = "identity")+
    scale_fill_manual(values = palettes[var1][[1]])+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      axis.text = element_text(family = "Martel Sans",size=15),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15),
    )
}

histogram_factornonord_num_stack<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_histogram(aes(x=.data[[var2]],
                       fill=.data[[var1]],
                       color=.data[[var1]]),
                   alpha=0.5,
                   position = "stack")+
    scale_fill_manual(values = palettes[var1][[1]])+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      axis.text = element_text(family = "Martel Sans",size=15),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15),
    )
}

boxplot_factornonord_num<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_boxplot(aes(x=.data[[var2]],
                     y=.data[[var1]]),
                 alpha=0.5,
                 fill=palettes[var1][[1]],
                 color=palettes[var1][[1]])+
    my_theme+
    theme(
      panel.grid.major.x = element_line(color="grey",linewidth = 0.1),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      panel.grid.minor.x = element_line(color="grey",linewidth = 0.1),
      axis.text = element_text(family = "Martel Sans",size=15)
    )
}

barplot_factord_factord_dodge<-barplot_logic_factord_dodge
  
barplot_factord_factord_stack<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_bar(aes(x=.data[[var1]],
                 fill=.data[[var2]]),
             position = "stack")+
    scale_fill_manual(values = palettes[var2][[1]])+
    my_theme+
    theme(
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      axis.text = element_text(family = "Martel Sans",size=15)
    )
}

density_factorord_num<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_density_ridges(aes(x=.data[[var2]],
                            y=.data[[var1]],
                            fill=.data[[var1]]),
                        color="white",
                        alpha=1,
                        position = "identity")+
    coord_flip()+
    scale_fill_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text = element_text(family = "Martel Sans",size=15),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15),
    )
}

boxplot_factorord_num<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_boxplot(aes(y=.data[[var2]],
                     x=.data[[var1]],
                     fill=.data[[var1]],
                     color=.data[[var1]]),
                 alpha=0.5)+
    scale_fill_manual(values = palettes[var1][[1]])+
    scale_color_manual(values = palettes[var1][[1]])+
    my_theme+
    theme(
      axis.text = element_text(family = "Martel Sans",size=15),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      panel.grid.major.y = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor.y = element_line(color="grey",linewidth = 0.1),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15),
    )
}

scatter_num_num<-function(df,var1,var2,palettes){
  ggplot2::ggplot(df)+
    geom_point(aes(x=.data[[var1]],
                   y=.data[[var2]]))+
    my_theme+
    theme(
      axis.text = element_text(family = "Martel Sans",size=15),
      axis.title.x = element_text(family = "Martel Sans",size=15),
      axis.title.y = element_text(family = "Martel Sans",size=15,angle=90),
      panel.grid.major = element_line(color="grey",linewidth = 0.1),
      panel.grid.minor = element_line(color="grey",linewidth = 0.1),
      legend.title = element_text(family = "Martel Sans",size=15),
      legend.text = element_text(family = "Martel Sans",size=15),
    )
}
