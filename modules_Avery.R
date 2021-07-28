
# Mapping Function

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
                                     minZoom = 7, maxZoom = 7,
                                     dragging = FALSE)) %>% 
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