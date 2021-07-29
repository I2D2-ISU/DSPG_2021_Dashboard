
# ACS Time Series Function

acs_time_ser <- function(df, var){
  ggplot(df, aes(x=year, y=!!(as.name(var)) , color=NAME)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = percent(!!(as.name(var)), accuracy = 0.1)),
                     box.padding   = 0, 
                     point.padding = 0,
                     segment.color = 'grey50') +
    labs(caption = "Source: ACS 5-Year Table Estimates") +
    theme_fivethirtyeight() +
    theme(panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          legend.position = "top",
          legend.title=element_blank(),
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="solid")) +
    scale_x_continuous(breaks=seq(min(df$year),max(df$year)+.9,by=1)) +
    scale_y_continuous(labels=scales::percent_format())
}
