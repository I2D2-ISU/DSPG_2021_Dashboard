
# CREATE MODULES FOR REPETATIVE TASKS/ELEMENTS OF DASHBOARD


# FIGURES -----------------------------------------------------------------


# . Line Plot -------------------------------------------------------------

plot_line_year <- function(df, PERCENT = FALSE, COUNTY_COL = "orange", STATEWIDE_COL = "grey20") {
  
  
  # specify data format for hover text in plotly
  if(PERCENT) {
    plotly_hover_text <- "<b>%s</b><br>Year: <b>%s</b><br>Birth less than high school: <b>%.1f%%</b>"
    m = 100
  } else {
    plotly_hover_text <- "<b>%s</b><br>Year: <b>%s</b><br>Birth less than high school: <b>%.2f</b>"
    m = 1
  }
  
  
  df %>%
    ggplot(aes(x = year, 
               y = value, 
               col = county, 
               group = county,
               text = 
                 sprintf(plotly_hover_text, 
                         county, year, value*m))) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = seq(1900, 2025, 1)) +
    # switch axis to percentage
    {if(PERCENT)scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) } +
    scale_color_manual(name = NULL, values = c(COUNTY_COL, STATEWIDE_COL)) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 90, size=8, vjust =0.5)) 
}


# . Map -------------------------------------------------------------------









# Indicator ValueBox ------------------------------------------------------

indicator_box_ui <- function(ID, INDICATOR, VALUE, FORMAT = "numeric",
                             FONT_SIZE_VALUE = 150,
                             FONT_SIZE_INDICATOR = 200,
                             COLOR = "purple", ICON = NULL, WIDTH = 4) {
  ns <- NS(ID)
  
  VALUE = as.numeric(VALUE)
  
  if (str_to_lower(FORMAT) %in% c("percent", "%")) {
    VALUE = scales::percent(VALUE, accuracy = 0.1)
  } else if(str_to_lower(FORMAT) %in% c("count", "large", "big.mark")) {
    VALUE = format(VALUE, big.mark = ",")
  } else if(str_to_lower(FORMAT) %in% c("money", "currency", "dollar", "$")) {
    VALUE = scales::dollar(VALUE)
  } else {
    VALUE = VALUE
  }
  
  div(valueBox(value = tags$p(VALUE, 
                              style = paste0("font-size: ", FONT_SIZE_VALUE, "%;")), 
               subtitle = tags$p(INDICATOR, 
                                 style = paste0("font-size: ", FONT_SIZE_INDICATOR, "%;")), 
               icon = icon(ICON), 
               color = COLOR, 
               width = WIDTH, 
               href = NULL))
}



# Toggle Button -----------------------------------------------------------

toggle_button <- function(ID, CHOICE, LABEL = "") {
  
  radioGroupButtons(
    inputId = ID,
    label = LABEL,
    choices = CHOICE,
    individual = TRUE,
    checkIcon = list(
      yes = tags$i(class = "fa fa-circle", 
                   style = "color: steelblue"),
      no = tags$i(class = "fa fa-circle-o", 
                  style = "color: steelblue"))
  )
}

