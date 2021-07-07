library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(DT)
library(leaflet)
library(maps)
library(plotly)
library(scales)


source("modules.R")







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



# Header ------------------------------------------------------------------
header <-  dashboardHeader(title = "DSPG Dashboard")



# Sidebar -----------------------------------------------------------------
sidebar <-  
  dashboardSidebar(
    # Menu content
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data by Topic", tabName = "topic", icon = icon("database"),
               # whether statewide data should be included
               checkboxInput(
                 inputId = "STATEWIDE",
                 label = "Include Statewide Data", 
                 value = FALSE
               ),
               # county to display results for
               pickerInput(
                 inputId = "COUNTY",
                 label = "Select County",
                 choices = data$county,
                 multiple = FALSE,
                 selected = "Story",
                 choicesOpt = list(
                   content = data$county)
               ),
               # years to display data for
               sliderTextInput(
                 inputId = "YEAR",
                 label = "Choose Years", 
                 choices = 2010:2019,
                 selected = c(2010, 2019)
               ),
               p("Explore Data of Interest:", style="margin-left: 15px"),
               # data categories to show
               menuSubItem("Children & Families", tabName = "demographics", icon = icon("baby")),
               menuSubItem("Employment & Income", tabName = "employment", icon = icon("hand-holding-usd")),
               menuSubItem("Education", tabName = "education", icon = icon("book-open")),
               menuSubItem("Physical & Mental Health", tabName = "health", icon = icon("heartbeat")),
               menuSubItem("Community", tabName = "community", icon = icon("users")),
               menuSubItem("Services", tabName = "services", icon = icon("user-cog")),
               p()
      ),
      # UNDER DEVELOPMENT
      menuItem("Multicounty Analysis", tabName = "one", icon = icon("bar-chart"))
    )
  )



# Body --------------------------------------------------------------------
body <-
  dashboardBody(
    # set fixed height of valueBox
    tags$head(tags$style(HTML(".small-box {height: 180px}"))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              h1("ECI Board Approved Indicators"),
              indicator_box_ui("INDICATORS", INDICATOR = "Educational attainment of mothers", 
                               VALUE = .12, FORMAT = "%", COLOR = "blue"),
              indicator_box_ui("INDICATORS", INDICATOR = "Children under age 6 living in poverty", 
                               VALUE = .12, FORMAT = "%", COLOR = "blue"),
              indicator_box_ui("INDICATORS", INDICATOR = "Children under age 6 with all parents in the workforce", 
                               VALUE = .12, FORMAT = "%", COLOR = "blue"),
              indicator_box_ui("INDICATORS", INDICATOR = "Low birth weight", 
                               VALUE = .12, FORMAT = "%"),
              indicator_box_ui("INDICATORS", INDICATOR = "Immunized children", 
                               VALUE = .12, FORMAT = "%"),
              indicator_box_ui("INDICATORS", INDICATOR = "Dental services", 
                               VALUE = .12, FORMAT = "%"),
              indicator_box_ui("INDICATORS", INDICATOR = "K students proficient by K literacy assessment", 
                               VALUE = .12, FORMAT = "%", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Students entering K with no obvious dental problems", 
                               VALUE = .12, FORMAT = "%", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Serious crime/100,000 population", 
                               VALUE = 1250, FORMAT = "numeric", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Juvenile arrests/100,000 population", 
                               VALUE = 1250, FORMAT = "numeric", COLOR = "navy"),
              indicator_box_ui("INDICATORS", INDICATOR = "Unemployment rate", 
                               VALUE = .12, FORMAT = "%", COLOR = "navy"),
              indicator_box_ui("INDICATORS", INDICATOR = "Incidence of child abuse/1,000 children", 
                               VALUE = .12, FORMAT = "numeric", COLOR = "navy"),
              h2("Subgroup Name"),
              indicator_box_ui("INDICATORS", INDICATOR = "Child deaths due to unintentional injuries", 
                               VALUE = .12, FORMAT = "%", COLOR = "black"),
              indicator_box_ui("INDICATORS", INDICATOR = "Domestic violence rate", 
                               VALUE = .12, FORMAT = "%", COLOR = "black"),
              indicator_box_ui("INDICATORS", INDICATOR = "Teen births", 
                               VALUE = .12, FORMAT = "%", COLOR = "black"),
              indicator_box_ui("INDICATORS", INDICATOR = "Accredited family support programs in the state", 
                               VALUE = 1250, FORMAT = "count", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Quality early learning environments, QRS rating", 
                               VALUE = 1250, FORMAT = "count", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Number of programs in a quality initiative", 
                               VALUE = 1250, FORMAT = "count", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Availability of child care, cost" , 
                               VALUE = 1250.145, FORMAT = "$", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Number of childcare providers", 
                               VALUE = 1250, FORMAT = "count", COLOR = "olive"),
              indicator_box_ui("INDICATORS", INDICATOR = "Number of childcare spaces", 
                               VALUE = 1250, FORMAT = "count", COLOR = "olive"),
              
              fluidRow(),
              fluidRow(
                box(title = "InfoBox", footer = "This is footer", width = 12,
                    status = 'primary', solidHeader = TRUE, 
                    background = "black", collapsible = TRUE, collapsed = TRUE,
                    h2("Indicator Boxes with figures and tables"), 
                    br(),
                    valueBox(tags$p("3,155,070", style = "font-size: 150%;"), 
                             subtitle = tags$p("POPULATION", 
                                               style = "font-size: 150%;"), 
                             icon = icon("users fa-2x"), 
                             color = "maroon",
                             width = 4),
                    valueBox(tags$p("1,418,600", style = "font-size: 150%;"), 
                             subtitle = tags$p("TOTAL HOUSING UNITS", 
                                               style = "font-size: 150%;"), 
                             icon = icon("home fa-2x"), 
                             color = "green",
                             width = 4),
                    valueBox(tags$p("$61,691", style = "font-size: 150%;"), 
                             subtitle = tags$p("MEDIAN HOUSEHOLD INCOME", 
                                               style = "font-size: 150%;"), 
                             icon = icon("hand-holding-usd fa-2x"), 
                             color = "teal", 
                             width = 4),
                    div(id='click_si_UNEMPLOYMENT',
                        valueBox(tags$p(percent(data_state_wide$var140/100, accuracy = 0.1), 
                                        style = "font-size: 150%;"), 
                                 subtitle = tags$p("UNEMPLOYMENT", 
                                                   style = "font-size: 150%;"), 
                                 icon = icon("house-damage fa-2x"), 
                                 color = "lime", 
                                 # width = 4, 
                                 href = NULL)),
                    div(id='click_si_CRIME',
                        valueBox(tags$p(format(data_state_wide$var100, big.mark = ","),
                                        style = "font-size: 150%;"), 
                                 subtitle = tags$p("# OF GROUP A CRIMES", 
                                                   style = "font-size: 150%;"), 
                                 icon = icon("folder fa-2x"), 
                                 color = "aqua", 
                                 # width = 4, 
                                 href = NULL)),
                    div(id='click_si_POVERTY6',
                        valueBox(tags$p(percent(data_state_wide$var019, accuracy = 0.1), 
                                        style = "font-size: 150%;"), 
                                 subtitle = tags$p("BELOW POVERTY (under 6)", 
                                                   style = "font-size: 150%;"), 
                                 icon = icon("bicycle fa-2x"), 
                                 color = "orange", 
                                 # width = 4, 
                                 href = NULL)),
                    div(id='click_si_ABUSE6',
                        valueBox(tags$p(percent(data_state_wide$var157, accuracy = 0.1), 
                                        style = "font-size: 150%;"), 
                                 subtitle = tags$p("ABUSE CASES (under 6)", 
                                                   style = "font-size: 150%;"), 
                                 icon = icon("universal-access fa-2x"), 
                                 color = "yellow", 
                                 width = 4, 
                                 href = NULL)),
                    div(id='click_si_RENTER6',
                        valueBox(tags$p(percent(data_state_wide$var042, accuracy = 0.1),
                                        style = "font-size: 150%;"), 
                                 subtitle = tags$p("RENTER HOUSING (under 6)", 
                                                   style = "font-size: 150%;"), 
                                 icon = icon("house-user fa-2x"), 
                                 color = "red", 
                                 width = 4, 
                                 href = NULL)),
                    div(id='click_si_FEMALE',
                        valueBox(tags$p(format(data_state_wide$var062, big.mark = ","),
                                        style = "font-size: 150%;"), 
                                 subtitle = tags$p("FEMALE 18-44", 
                                                   style = "font-size: 150%;"), 
                                 icon = icon("female fa-2x"), 
                                 color = "fuchsia", 
                                 width = 4, 
                                 href = NULL))
                    )),
              
              bsModal("modalExample", 
                      "Name of the Table 1", 
                      "click_si_UNEMPLOYMENT", 
                      size = "large",
                      dataTableOutput("table")),
              
              bsModal("modalExample2",
                      "Name of the Table 2",
                      "click_si_CRIME",
                      size = "large",
                      dataTableOutput("table2")),
              
              bsModal("modalExample3",
                      "Name of the Table 3",
                      "click_si_POVERTY6",
                      size = "large",
                      dataTableOutput("table3")),

              bsModal("modalExample4",
                      "Data Table",
                      "click_si_ABUSE6",
                      size = "large",
                      plotOutput(outputId = "line")),
              
              bsModal("modalExample5",
                      "Data Table - label to show",
                      "click_si_RENTER6",
                      size = "large",
                      plotOutput(outputId = "line2")),
              
              bsModal("modalExample6",
                      "Figure - label to show",
                      "click_si_FEMALE",
                      size = "large",
                      leafletOutput(outputId = "dataMap"))
              
      ),
      
      tabItem(tabName = "health",
              tabsetPanel(type = "tabs",
                          tabPanel(h3("Child Health"),
                                   pickerInput(
                                     inputId = "CHILD.HEALTH.VAR",
                                     label = "Select Variable", 
                                     choices = list(
                                       Abuse = 
                                         c("Total number of domestic abuse victims", 
                                           "Total number of female domestic abuse victims", 
                                           "Percent of domestic abuse victims female", 
                                           "Total number of juvenile domestic abuse victims",
                                           "Percent of domestic abuse victims juveniles"),
                                       Other = 
                                         c("Suicide Rates", 
                                           "Dental Services", 
                                           "Binge Alcohol Prevalence")),
                                     options = list(
                                       `live-search` = TRUE)
                                   )
                          ),
                          tabPanel(h3("Parent Health"),
                                   pickerInput(
                                     inputId = "PARENT.HEALTH.VAR",
                                     label = "Select Variable", 
                                     choices = list(
                                       Abuse = 
                                         c("Number of child abuse cases under age 6",
                                           "Percent of child abuse cases under age 6"),
                                       Imunization = 
                                         c("Percent of 2 year olds immunized", 
                                           "Infant Mortality")),
                                     options = list(
                                       `live-search` = TRUE)
                                   )
                          )
              )
      ),
      
      
      tabItem(tabName = "demographics",
              h3("This figure is taken from CENSUS website"),
              tags$div(
                HTML("<p>You can find it <a href='https://data.census.gov/cedsci/profile?g=0400000US19'>here</a></p>")
              ),
              fluidRow(
                box(width = 6,
                    tags$iframe(seamless="seamless",
                                src="https://data.census.gov/cedsci/vizwidget?g=0400000US19&infoSection=Health%20Insurance&type=chart&chartType=line", 
                                width="100%", height="475px", style="border:0")
                ),
                box(width = 6,
                    tags$iframe(seamless="seamless",
                                src="https://data.census.gov/cedsci/vizwidget?g=0400000US19&infoSection=Homeownership&type=chart&chartType=line", 
                                width="100%", height="475px", style="border:0"
                    )
                ),
                box(width = 8, 
                    tags$iframe(seamless="seamless",
                                src="https://data.census.gov/cedsci/vizwidget?g=0400000US19&infoSection=Housing%20Value&type=map",
                                width="100%", height="475px", style="border:0")
                ),
                box(width = 4,
                    tags$iframe(seamless="seamless",
                                src="https://data.census.gov/cedsci/vizwidget?g=0400000US19&infoSection=Educational%20Attainment&type=chart&chartType=bar", 
                                width="100%", height="475px", style="border:0")
                )
              )
      ),
      
      
      tabItem(tabName = "employment"),
      
      
      tabItem(tabName = "services"),
      
      
      tabItem(tabName = "education"),
      
      
      tabItem(tabName = "community",
              h1("Testing"),
              verbatimTextOutput("TEST")
      )
    )
  )


# UI - User Interface -----------------------------------------------------
ui <- dashboardPage(header, sidebar, body, title = "I2D2 Dashboard", skin = "blue")



# Server ------------------------------------------------------------------
server <- function(input, output) { 
  
  output$table <- renderDataTable({
    head(data) %>% select(1:5)
  })
  
  output$table2 <- renderDataTable({
    head(data) %>% select(110:105)
  })
  
  output$TEST <- renderText(input$COUNTY)
  
  output$line <- renderPlot({
    data %>%
      filter(county %in% input$COUNTY) %>%
      select(county, var146:var157) %>%
      gather(Var, value, -county) %>%
      left_join(varnames) %>%
      separate(Description, into = c("Var", "Year"), sep = ",", convert = TRUE) %>%
      separate(Var, into = c("key","of", "var"), extra = "merge") %>%
      spread(key, value) %>%
      filter(between(Year, input$YEAR[1], input$YEAR[2])) %>%
      ggplot(aes(Year, Percent, col = county)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = NULL, y =  NULL, col = "County") +
      scale_x_continuous(labels = function(x) sprintf("%.0f", x),
                         breaks = seq(input$YEAR[1], input$YEAR[2], 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  output$dataMap <- renderLeaflet({
    pal <- colorNumeric("viridis", NULL)
    iowa_map %>% 
      left_join(select(data, fips = FIPS, label = county, value = var019),
                "fips") %>%
      leaflet() %>% 
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.75,
                  fillColor = ~pal(value), 
                  label = ~paste0(label, ": ", scales::percent(round(value, 3)))) %>% 
      addLegend(title = " ", pal = pal, values = ~value*100, 
                labFormat = labelFormat(suffix = "%"),
                opacity = 0.75) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
}



# Shiny -------------------------------------------------------------------
shinyApp(ui, server)

