
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
    theme(axis.text.x = element_text(angle = 0, size=8, vjust =0.5))
}


# . Column Plot -----------------------------------------------------------

plot_bar_mean <- function(df, YEARS, PERCENT = FALSE, COUNTY_COL = "orange", STATEWIDE_COL = "grey20") {
  
  # get year range used for plotting
  years <- if(length(YEARS) > 1) {
    paste(YEARS, collapse = " to ")
  } else {
    paste(YEARS)
  }
  
  # specify data format for hover text in plotly
  if(PERCENT) {
    plotly_hover_text <- paste0("<b>%s</b><br>Year(s): <b>",years,"</b><br>%s: <b>%.1f%%</b>")
    m = 100
  } else {
    plotly_hover_text <- paste0("<b>%s</b><br>Year(s): <b>",years,"</b><br>%s: <b>%.2f</b>")
    m = 1
  }
  
  df %>%
    ggplot(aes(x = group_3, 
               y = value, 
               fill = county, 
               group = county,
               text = 
                 sprintf(plotly_hover_text, 
                         county, group_3, value*m))) +
    geom_bar(stat = "summary", fun = mean, position = "dodge") +
    facet_grid(~group_2) +
    scale_x_discrete(name = NULL, 
                     labels = function(x) str_wrap(x, width = 12)) +
    # switch axis to percentage
    {if(PERCENT)scale_y_continuous(labels = scales::percent_format(accuracy = 1)) } +
    scale_fill_manual(name = NULL, values = c(COUNTY_COL, STATEWIDE_COL)) +
    coord_flip() +
    labs(#title = "Plot Title",
         subtitle = paste("Year", years)) + 
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 90, size=8, vjust =0.5),
          strip.text = element_text(size = 12))
}


# . Map -------------------------------------------------------------------

plot_map_mean <- function(df, COUNTY, REVERSE = TRUE) {
  
  # different color scheme options
  # https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/
  pal <- colorBin(heat.colors(10, rev = REVERSE), domain = c(0,ceiling(max(df$value)/5)*5), bins = 5, pretty = FALSE)
  # pal <- colorBin(terrain.colors(10, rev = REVERSE), domain = c(0,ceiling(max(data_136$value)/2.5)*2.5), bins = 5, pretty = FALSE)
  # pal <- colorBin(topo.colors(10, rev = REVERSE), domain = c(0,ceiling(max(data_136$value)/2.5)*2.5), bins = 5, pretty = FALSE)
  # pal <- colorBin(cm.colors(10, rev = REVERSE), domain = c(0,ceiling(max(data_136$value)/2.5)*2.5), bins = 5, pretty = FALSE)
  # pal <- colorBin("YlOrRd", domain = c(0,ceiling(max(data_136$value)/2.5)*2.5), bins = 5, pretty = FALSE)
  # pal <- colorBin(c("#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"), 
  #                 domain = c(0,ceiling(max(data_136$value)/2.5)*2.5), bins = 5, pretty = FALSE)
  # pal <- colorNumeric("viridis", NULL)
  
  iowa_map["fips"] %>%
    inner_join(df, "fips") %>%
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 6, maxZoom = 8,
                                     dragging = TRUE)) %>% 
    addTiles() %>%
    addPolygons(stroke = TRUE, 
                weight = 1,
                color = "grey",
                smoothFactor = 0.3, 
                fillOpacity = 0.75,
                fillColor = ~pal(value), 
                label = ~popup_label,
                labelOptions = labelOptions(
                  direction = "auto", 
                  textsize = "15px",
                  style = list(
                    "border-color" = "rgba(0,0,0,0.5)",
                    padding = "4px 8px"
                  ))) %>% 
    addLegend(title = " ", pal = pal, values = ~value, 
              labFormat = labelFormat(suffix = "%"),
              opacity = 0.75) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(COUNTY)), "[:punct:]") 
                                            ),
                 stroke = TRUE,
                 weight = 2.5,
                 color = "black")
  
}







# Indicator ValueBox ------------------------------------------------------

indicator_box_ui <- function(ID, INDICATOR, VALUE, FORMAT = "numeric",
                             FONT_SIZE_VALUE = 150,
                             FONT_SIZE_INDICATOR = 150,
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



# Flip Box ----------------------------------------------------------------

myFlipBox <-
  function (id, front, back, bcolor = "white", fcolor = "white",
            trigger = c("click", "hover"),  width = 4) {
    if (is.null(id) || missing(id)) 
      stop("card id cannot be null or missing!")
    trigger <- match.arg(trigger)
    shiny::column(width = width, 
                  shiny::tags$div(style = "height:350px; position: relative; padding:20px;",
                                  class = "flipbox", 
                                  id = id, 
                                  `data-rotate` = trigger, 
                                  shiny::tags$div(class = "card-front active", 
                                                  style = paste0("background-color: ", fcolor, "; padding:10px;"), 
                                                  front), 
                                  shiny::tags$div(class = "card-back",
                                                  style = "background-color: white;", 
                                                  back)))
  }


myFlipbox <- function (id, front, back, trigger = c("click", "hover"), 
                       width = 6) 
{
  if (is.null(id) || missing(id)) 
    stop("card id cannot be null or missing!")
  trigger <- match.arg(trigger)
  shiny::column(width = width, 
                shiny::tags$div(style = "position: relative; padding-bottom: 10px;", 
                                class = "flipbox", 
                                id = id, 
                                `data-rotate` = trigger, 
                                shiny::tags$div(class = "card-front active", 
                                                style = "background-color: orange;", 
                                                front), 
                                shiny::tags$div(class = "card-back", 
                                                style = "background-color: white;", 
                                                back)))
}

