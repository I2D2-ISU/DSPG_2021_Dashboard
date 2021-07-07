
# CREATE MODULES FOR REPETATIVE TASKS/ELEMENTS OF DASHBOARD


# Map ---------------------------------------------------------------------


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


