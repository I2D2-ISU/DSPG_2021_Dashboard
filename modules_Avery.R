
# ACS Time Series Function

acs_time_ser <- function(df, var){
  
  df <- df %>% select(year, !!(as.name(var))) %>%
  
  ggplot(df, aes(x=year, y=!!(as.name(var)) , color=NAME)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = percent(!!(as.name(var)), accuracy = 0.1)),
                     box.padding   = 0, 
                     point.padding = 0,
                     segment.color = 'grey50', show.legend = FALSE) +
    theme_fivethirtyeight() +
    theme(panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          legend.position = "top",
          legend.title=element_blank(),
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="solid")) +
    scale_x_continuous(breaks=seq(min(df$year),max(df$year)+.9,by=1),
                       limits = c(min(df$year),max(df$year))) +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0, 1.1))
}


# Rate Time Series Function

rate_time_ser <- function(df, var){
  
  df <- df %>% select(year, !!(as.name(var))) %>%
  
  ggplot(df, aes(x=year, y=!!(as.name(var)) , color=NAME)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = round(!!(as.name(var)), 1)),
                     box.padding   = 0, 
                     point.padding = 0,
                     segment.color = 'grey50', show.legend = FALSE) +
    theme_fivethirtyeight() +
    theme(panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          legend.position = "top",
          legend.title=element_blank(),
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="solid")) +
    scale_x_continuous(breaks=seq(min(df$year),max(df$year)+.9,by=1),
                       limits = c(min(df$year),max(df$year)))
}


# ACS Category Function

acs_cat_plot <- function(df, type, est) {
  ggplot(df, aes(x=reorder({{type}}, {{est}}), y={{est}}, fill=NAME)) + 
    geom_col(position = "dodge") +
    theme_fivethirtyeight() + 
    scale_y_continuous(labels = scales::percent,limits = c(0,1.1)) +
    theme(text = element_text(family = "Arial"), 
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"), legend.position="top", legend.title=element_blank()) +
    geom_label(aes(label = percent({{est}}, accuracy = .1)), position = position_dodge(0.9), size=5, color="white", show.legend = FALSE)
}
