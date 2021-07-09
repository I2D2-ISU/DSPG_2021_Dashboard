library(ggthemes)

# Data --------------------------------------------------------------------

# read the data
data <- readxl::read_xlsx("Data/ECI_Needs_Assessment_Data_final_GIO.xlsx") %>%
  arrange(FIPS)

data_state_wide <- data %>% filter(FIPS == 19000)

varnames <- readxl::read_xlsx("Data/ECI_Needs_Assessment_Data_final_GIO.xlsx", 
                              sheet = 2, range = readxl::cell_cols("A:B"))

iowa_map <- sf::st_as_sf(map(database = "county", 
                             regions = "iowa", 
                             fill = TRUE, 
                             plot = FALSE)) %>%
  # add county fips code
  left_join(county.fips, by = c("ID" = "polyname")) %>%
  separate(ID, into = c("state", "county"), sep = ",", remove = FALSE)



# Read row ACS data

read_ACS <- function(file_path) {
  read_csv(file_path) %>%
    select(fips = GEOID, county = NAME, variable, estimate, year) %>%
    spread(variable, estimate) %>%
    mutate(county = ifelse(fips == "19", "Statewide", county))
} 

data_ACS <- 
  read_ACS("data/ACS/newMomEducation5yr_acs.csv") %>%
  mutate(var004 = B13014_004/B13014_002,
         var005 = B13014_005/B13014_002,
         var006 = B13014_006/B13014_002,
         var007 = B13014_007/B13014_002,
         var008 = B13014_008/B13014_002,
         var010 = B13014_010/B13014_002,
         var011 = B13014_011/B13014_002,
         var012 = B13014_012/B13014_002,
         var013 = B13014_013/B13014_002,
         var014 = B13014_014/B13014_002)





# data_ACS %>%
#   select(fips:year, starts_with("var")) %>%
#   gather(var, value, starts_with("var")) %>%
#   mutate(married = ifelse(var %in% c(paste0("var00", 4:8)),
#                           "Married", "Unmarried")) %>%
#   filter(county %in% c("Story", "Statewide"),
#          var %in% c("var004", "var010")) %>%
#   mutate(county = factor(county, levels = c("Story", "Statewide"))) %>%
#   filter(var == "var004") %>%
#   ggplot(aes(x = year,
#              y = value,
#              col = county,
#              group = county)) +
#   geom_point(size = 3) +
#   geom_line(size = 1) +
#   scale_x_continuous(breaks = seq(1900, 2025, 1)) +
#   # switch axis to percentage
#   scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
#   scale_color_manual(name = NULL, values = c("orange", "grey20")) +
#   theme_fivethirtyeight() +
#   theme(axis.text.x = element_text(angle = 90, size=8, vjust =0.5))
  
  
  # plot_line_year(PERCENT = T) +
  # labs(
  #   title="Proportion of Women Who Has A Birth In The Past 12 Months",
  #   subtitle="Proportion of women with less than high schoo education",
  #   caption="Source: ACS 5-Year Survey Table B13014"
  # ) -> fig

# ggplotly(fig, tooltip = "text") %>%
#   layout(hovermode = "x unified")
# 
# 
# PLOT_TITLE <- "Proportion of Women Who Has A Birth In The Past 12 Months"
# PLOT_SUBTITLE <- "Proportion of women with less than high schoo education"
# PLOT_CAPTION <- "Source: ACS 5-Year Survey Table B13014"
# 
# 
# plot_line_year(df, PERCENT = T) + 
#   labs(
#     title = PLOT_TITLE,
#     subtitle = PLOT_SUBTITLE,
#     caption = PLOT_CAPTION
#   )
# 
# 
# ggplotly(plot_line_year(df, PERCENT = T),
#          tooltip = "text")  %>%
#   layout(hovermode = "x unified",
#          title = paste0('<b>', PLOT_TITLE, '</b><br>','<sup>', PLOT_SUBTITLE, '</sup>'),
#          annotations = list(x = 1, y = -0.1, text = "Source: data I found somewhere.", 
#                             showarrow = F, xref='paper', yref='paper', 
#                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
#                             font=list(size=15, color="red")))
