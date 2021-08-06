
# Unemployment Time Series Function

unemp_timeser <- function(df){
  
  df <- na.omit(df %>% select(year, value, name))
  
  df %>% 
    ggplot(aes(x=year, y = value/100 , color=name)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = percent(value/100, accuracy = 0.1)),
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
    scale_y_continuous(labels=scales::percent_format(), limits=c(0, 0.1))
}



# unemployment Box Plot Function

unemp_box_plot <- function(df) {
  ggplot(df, aes(x = year, value/100, fill=name)) + 
    geom_col(position = "dodge") +
    theme_fivethirtyeight() + 
    scale_y_continuous(labels = scales::percent,limits = c(0,0.1)) +
    theme(text = element_text(family = "Arial"), 
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"), legend.position="top", legend.title=element_blank()) +
    geom_label(aes(label = percent(value/100, accuracy = .1)), position = position_dodge(0.9), size=5, color="white", show.legend = FALSE)
}

# Childcare Cost Box Plot Function

childcare_rate_boxplot <- function(df) {
  ggplot(df, aes(x = year, cost, fill=name)) + 
    geom_col(position = "dodge") +
    theme_fivethirtyeight() + 
    scale_y_continuous(labels = scales::percent,limits = c(0,0.1)) +
    theme(text = element_text(family = "Arial"), 
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"), legend.position="top", legend.title=element_blank()) +
    geom_label(aes(label = percent(value, accuracy = .1)), position = position_dodge(0.9), size=5, color="white", show.legend = FALSE)
}

# Childcare Cost Box Plot Function

childabuse_bar_plot <- function(df) {
  ggplot(df, aes(x = year, value, fill=name)) + 
    geom_col(position = "dodge") +
    theme_fivethirtyeight() + 
    scale_y_continuous(labels = scales::percent,limits = c(0,0.1)) +
    theme(text = element_text(family = "Arial"), 
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"), legend.position="top", legend.title=element_blank()) +
    geom_label(aes(label = percent(value, accuracy = .1)), position = position_dodge(0.9), size=5, color="white", show.legend = FALSE)
}



# Childabuse Time Series Function

childabuse_timeser <- function(df){ #var = year
  
  df <- na.omit(df %>% select(name, value, name))
  
  df %>% 
    ggplot(aes(x=year, y = value , color=name)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = percent(value, accuracy = 0.1)),
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
    scale_y_continuous(labels=scales::percent_format(), limits=c(0, 0.1))
}


# Childcare rate Time Series Function

childcare_rate_timeser <- function(df, var){
  
  df <- na.omit(df %>% select(age, value, county))
  
  df %>% 
    ggplot(aes(x=year, y = value , color=county)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = percent(value, accuracy = 0.1)),
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
    scale_x_continuous(breaks=seq(min(df$age),max(df$age)+.9,by=1),
                       limits = c(min(df$age),max(df$age))) +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0, 500))
}

# Childcare_space Time Series Function

childcare_space_timeser <- function(df, var){
  
  df <- na.omit(df %>% select(year, !!(as.name(var)), NAME))
  
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
