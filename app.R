library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(DT)
library(leaflet)
library(maps)
library(plotly)
library(scales)
library(lubridate)
library(ggrepel)
library(mapview)


source("modules.R")
source("data.R")
source("data_Avery.R")
source("modules_Avery.R")
source("data_Sonyta.R")
source("modules_Sonyta.R")


# Header ------------------------------------------------------------------
header <-  dashboardHeader(title = div(img(src="https://earlychildhood.iowa.gov/sites/default/files/styles/400px_square_cropped/public/2020-11/I2D2_Logo_Stack_Gradient_RGB.jpg?itok=E0rc2hDR", height='60', width='60'), " DATA DRIVE"))



# Sidebar -----------------------------------------------------------------
sidebar <-  
  dashboardSidebar(
    # Menu content
    sidebarMenu(
      # freezes the side bar
      #style = "position:fixed;",
      menuItem("Iowa Insights", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data by Topic", tabName = "topic", icon = icon("database"),
               # data categories to show
               p(),
               p("Explore Data of Interest:", style="margin-left: 15px"),
               menuSubItem("Children & Families", tabName = "demographics", icon = icon("baby")),
               menuSubItem("Employment & Income", tabName = "employment", icon = icon("hand-holding-usd")),
               menuSubItem("Education", tabName = "education", icon = icon("book-open")),
               menuSubItem("Physical & Mental Health", tabName = "health", icon = icon("heartbeat")),
               menuSubItem("Community", tabName = "community", icon = icon("users")),
               # menuSubItem("Services", tabName = "services", icon = icon("user-cog")),
               
               # county to display results for
               pickerInput(
                 inputId = "COUNTY",
                 label = "Select County",
                 choices = iowa_counties,
                 multiple = FALSE,
                 selected = "Story",
                 choicesOpt = list(
                   content = iowa_counties)
               ),
               # whether statewide data should be included
               checkboxInput(
                 inputId = "STATEWIDE",
                 label = "Include Statewide Data", 
                 value = FALSE
               ),
               # years to display data for
               sliderTextInput(
                 inputId = "YEAR",
                 label = "Choose Years", 
                 choices = 2009:2019,
                 selected = c(2009, 2019)
               ),
               p()
      )
    )
  )


# Body --------------------------------------------------------------------
body <-
  dashboardBody(
    # set fixed height of valueBox
    tags$head(tags$style(HTML(".small-box {height: 150px}"))),
    
    
    # . Dashboard body ----------------------------------------------------------
    tabItems(
      tabItem(tabName = "dashboard",
              h1("ECI Board Approved Indicators"),
              h2("Showing Most Recent Statewide Values For Indicators"),
              
              fluidRow(
                h2("HEALTHY CHILDREN", style="margin-left: 20px;  font-weight: bold;"),
                indicator_box_ui("INDICATORS", INDICATOR = "Very low birth weight", 
                                 VALUE = ind_vlow_bw, FORMAT = "%", COLOR = "orange"),
                indicator_box_ui("INDICATORS", INDICATOR = "Immunized children", 
                                 VALUE = ind_immun, FORMAT = "%", COLOR = "orange"),
                indicator_box_ui("INDICATORS", INDICATOR = "Dental services", 
                                 VALUE = .499, FORMAT = "%", COLOR = "orange")
              ),
              fluidRow(
                h2("CHILDREN READY TO SUCCEED IN SCHOOL", style="margin-left: 20px;  font-weight: bold;"),
                indicator_box_ui("INDICATORS", INDICATOR = "K students proficient by K literacy assessment", 
                                 VALUE = .66, FORMAT = "%", COLOR = "olive"),
                indicator_box_ui("INDICATORS", INDICATOR = "Educational attainment of women age 15-50 (less than a high school)", 
                                 VALUE = ind_plesshigh, FORMAT = "%", COLOR = "olive")
              ),
              fluidRow(
                h2("SAFE AND SUPPORTIVE COMMUNITIES", style="margin-left: 20px;  font-weight: bold;"),
                indicator_box_ui("INDICATORS", INDICATOR = "Child deaths due to unintentional injuries", 
                                 VALUE = .096, FORMAT = "%", COLOR = "blue"),
                indicator_box_ui("INDICATORS", INDICATOR = "Serious crime rate (per 100,000 population)", 
                                 VALUE = ind_ser_crime, FORMAT = "large", COLOR = "blue"),
                indicator_box_ui("INDICATORS", INDICATOR = "Juvenile arrests rate (per 100,000 population)", 
                                 VALUE = ind_juv_crime, FORMAT = "large", COLOR = "blue"),
                indicator_box_ui("INDICATORS", INDICATOR = "Unemployment rate", 
                                 VALUE = .027, FORMAT = "%", COLOR = "blue"),
                indicator_box_ui("INDICATORS", INDICATOR = "Children under age 6 living in poverty", 
                                 VALUE = ind_pup6, FORMAT = "%", COLOR = "blue")
              ),
              fluidRow(
                h2("SECURE AND NURTURING FAMILIES", style="margin-left: 20px;  font-weight: bold;"),
                indicator_box_ui("INDICATORS", INDICATOR = "Incidence of child abuse (per 1,000 children)", 
                                 VALUE = 21.9, FORMAT = "numeric", COLOR = "teal"),
                indicator_box_ui("INDICATORS", INDICATOR = "Domestic violence rate", 
                                 VALUE = 186.7, FORMAT = "numeric", COLOR = "teal"),
                indicator_box_ui("INDICATORS", INDICATOR = "Teen births (per 1,000 women ages 15-19)", 
                                 VALUE = ind_teen_moms, FORMAT = "large", COLOR = "teal")
              ),
              fluidRow(
                h2("SECURE AND NURTURING EARLY LEARNING ENVIRONMENTS", style="margin-left: 20px;  font-weight: bold;"),
                indicator_box_ui("INDICATORS", INDICATOR = "Quality early learning environments", 
                                 VALUE = 505, FORMAT = "count", COLOR = "red"),
                indicator_box_ui("INDICATORS", INDICATOR = "Number of childcare providers", 
                                 VALUE = 3000, FORMAT = "count", COLOR = "red"),
                indicator_box_ui("INDICATORS", INDICATOR = "Number of childcare spaces", 
                                 VALUE = 130000, FORMAT = "count", COLOR = "red"),
                indicator_box_ui("INDICATORS", INDICATOR = "Children under age 6 with all parents in the workforce", 
                                 VALUE = ind_pil, FORMAT = "%", COLOR = "red")
              ),
              fluidRow(
                h2("OTHER", style="margin-left: 20px;  font-weight: bold;"),
                indicator_box_ui("INDICATORS", INDICATOR = "Accredited family support programs in the state", 
                                 VALUE = 1250, FORMAT = "count", COLOR = "olive"),
                indicator_box_ui("INDICATORS", INDICATOR = "Number of programs in a quality initiative", 
                                 VALUE = 1250, FORMAT = "count", COLOR = "olive"),
                indicator_box_ui("INDICATORS", INDICATOR = "Students entering K with no obvious dental problems", 
                                 VALUE = .12, FORMAT = "%", COLOR = "olive")
              ),
              fluidRow()
      ),
      

      # . Child and Families body -----------------------------------------------
      
      tabItem(tabName = "demographics",
              tabsetPanel( type="tabs",
                           tabPanel(h4("Child Care Rate"), 
                                    fluidRow(
                                      box(width=12,
                                          pickerInput( inputId = "child_age",
                                                       label = "Select Age",
                                                       choices = childcare_rates%>% select(age)%>%distinct()%>%pull(),
                                                       multiple = FALSE,
                                                       selected = "Infant (0-12 Months)"),
                                          title=strong("Childcare Average Cost Per Week"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, strong("Childcare Rate By Registered Child Development Homes Provider"),leafletOutput("childcare_rate_map_1")),
                                          column(width=6, strong("Childcare Rate By DHS Licensed Centers/Preschools Provider"), leafletOutput("childcare_rate_map_2"))
                                      )
                                      
                                    ),
                                    fluidRow(
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          width = 12,
                                          #plotOutput("childcare_rate_boxplot"),
                                          downloadButton("childcare_rate_download_csv", "Download CSV"),
                                          downloadButton("childcare_rate_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("childcare_rate_table")
                                      )
                                    )),

                           tabPanel(h4("Child Care Availability"),
                                    fluidRow(
                                      box(width = 12,
                                          pickerInput( inputId = "select_year",
                                                       label = "Select Year",
                                                       choices = childcare_spaces%>% select(year)%>%distinct()%>%pull(),
                                                       multiple = FALSE,
                                                       selected = "Infant (0-12 Months)"),
                                          title=strong("Childcare Program Spaces"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          leafletOutput("childcare_space_map")
                                          )
                                    ),
                                    fluidRow(
                                      box(width = 12,
                                          title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("childcare_space_download_csv", "Download CSV"),
                                          downloadButton("childcare_space_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("childcare_space_table")
                                      )
                                    )
                                   
                           ),
                          
                           tabPanel(h4("Child abuse"),
                                    fluidRow(
                                      box(width = 12,
                                          title=strong("Incidence of Child Abuse Under 6 Per 1,000 Children By Year"),
                                          pickerInput(inputId = "abuse_year",
                                                        label = "Select Year",
                                                        choices = child_abuse_county_state%>% select(year)%>%distinct()%>%pull(),
                                                        multiple = FALSE,
                                                        selected = "2019"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          leafletOutput("childabuse_map")
                                          )
                                    ),
                                    fluidRow(
                                      box(width = 12,
                                          title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("childabuse_download_csv", "Download CSV"),
                                          downloadButton("childabuse_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("childabuse_table")
                                      )
                                    )

                           )
              )
      ),
      
      
      
      
      # . Employment body -------------------------------------------------------
      
      tabItem(tabName = "employment",
              tabsetPanel( type="tabs",
                           tabPanel(h4("Child Poverty"), 
                                    fluidRow(
                                      box(width=12,
                                          pickerInput(inputId = "emp_race",
                                                      label = "Select Race/Ethnicity Category",
                                                      choices = c("All", 
                                                                  "White", #"White Alone, Not Hispanic",
                                                                  "Black", #"Black or African American Alone",
                                                                  "Hispanic", #"Hispanic or Latino", 
                                                                  "Asian", #"Asian Alone",
                                                                  "Native American", #"American Indian and Alaska Native Alone",
                                                                  "Pacific Islander", #"Native Hawaiian and Other Pacific Islander Alone",
                                                                  "Other", #"Some Other Race Alone",
                                                                  "Multiple", #"Two or More Races", 
                                                                  "All None White" #"Minority"
                                                                  ),
                                                       multiple = FALSE,
                                                       selected = "All"),
                                          title=strong("Percent of Children Under 6 in Poverty"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Averaged over selected years",
                                                 leafletOutput("emp_map_1"),
                                                 downloadButton("emp_map_1_png", "Download PNG")),
                                          column(width=6, "Over time", 
                                                 plotOutput("emp_timeser_1"),
                                                 downloadButton("emp_timeser_1_png", "Download PNG"))
                                      )
                                      
                                    ),
                                    fluidRow(
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          width = 12,
                                          downloadButton("emp_1_download_csv", "Download CSV"),
                                          downloadButton("emp_1_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("emp_table_1")
                                      )
                                    )),
                           tabPanel(h4("Parental Workforce Participation"),
                                    fluidRow(
                                      box(width=12,
                                          title=strong("Percent of Children By Parental Workforce Participation"),
                                          toggle_button("emp_lf_toggle",
                                                        c("All Parents in Workforce", "No Parents in Workforce")),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Over time", plotOutput("emp_timeser_3")),
                                          column(width=6, "Averaged over selected years", 
                                                 leafletOutput("emp_map_2"),
                                                 downloadButton("emp_map_2_png", "Download PNG")))
                                    ),
                                    fluidRow(
                                      box(
                                        title=strong("Side-by-side: Percent Parental Particiaption in Labor Force, Averaged Over Selected Years"),
                                        closable = FALSE,
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        plotOutput("emp_boxplot_1")),
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("emp_2_download_csv", "Download CSV"),
                                          downloadButton("emp_2_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("emp_table_2")
                                      )
                                    )
                           ),
                           
                           tabPanel(h4("General Population Poverty"),
                                    fluidRow(
                                      box(width=12,
                                          title=strong("Percent of Population in Poverty Over Time"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          plotOutput("emp_timeser_2")))
                           ),
                           
                           tabPanel(h4("Unemployment Rate"),
                                    fluidRow(
                                      box(width=12,
                                          pickerInput( inputId = "Unemp_year",
                                                       label = "Select year",
                                                       choices = unemployment_rate_by_year%>% select(year)%>%distinct()%>%pull(),
                                                       multiple = FALSE,
                                                       selected = "2021"),
                                          title=strong("Unemployment Rate by Year"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, strong("Unemployment Rate by Year"),leafletOutput("unemp_map")),
                                          column(width=6, strong("Unemployment Rate Time Series by County"), plotOutput("unemp_timeser"))
                                      )
                                      
                                    ),
                                    
                                    fluidRow(
                                      box(
                                        title=strong("Unemployment Rate Data"),
                                        closable = FALSE,
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        plotOutput("unemp_boxplot")),
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("unemp_download_csv", "Download CSV"),
                                          downloadButton("unemp_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("unemp_table")
                                      )
                                    ),
                                    
                           )
              )
      ),
      
      
      
      # . Education body --------------------------------------------------------
      
      tabItem(tabName = "education",
              tabsetPanel(
                type = "tabs",
                
                tabPanel(h4("Educational Attainment"),
                         fluidRow(
                           br(),
                           h3("Proportion of Women Who Has A Birth In The Past 12 Months",
                              style ="margin-left: 20px;  font-weight: bold;"),
                           br(),
                           box(width = 7, height = 500, 
                               title = "Less than High School Education",
                               toggle_button("EDU_plot_line_01_toggle",
                                             c("Married", "Unmarried", "Both")),
                               plotOutput("EDU_plot_line_01")
                           ),
                           
                           box(width = 5, height = 500,
                               title = "Level of Education Attained",
                               plotOutput("EDU_plot_bar_01", height = "480px")
                           )
                         ),
                         fluidRow(
                           box(width = 12, height = 500, 
                               title = "Education Attainment by County",
                               pickerInput(
                                 inputId = "EDU_plot_map_01_toggle",
                                 label = "Select Education Level",
                                 choices = levels(data_ACS$group_3)),  
                               leafletOutput("EDU_plot_map_01")
                           )
                         ),
                         fluidRow(
                           box(width = 12, height = 500, collapsible = TRUE,
                               collapsed = TRUE,
                               title = "Data",
                               DT::dataTableOutput("EDU_plot_01_table"),
                               downloadButton("EDU_download_csv", "Download CSV"),
                               downloadButton("EDU_download_xlsx", "Download Excel")
                           )
                         ),
                         fluidRow(
                           # downloadButton("EDU_download_csv", "Download CSV"),
                           # downloadButton("EDU_download_xlsx", "Download Exel")
                         )
                ),
                tabPanel(h4('Early Literacy Skills'),
                         h3("Beginning Reading Skills", style="margin-left:20px; font-weight:bold;"),
                         fluidRow(
                           box(width = 12, height = 500,
                               title = "Percent of kindergarten students proficient by kindergarten literacy assessment",
                               plotlyOutput("EDU_plot_bar_03"))
                         ),
                         fluidRow(
                           box(width = 12, height = 500, collapsible = TRUE,
                               collapsed = FALSE,
                               title = "Data",
                               DT::dataTableOutput("EDU_plot_03_table"),
                               downloadButton("EDU_03_download_csv", "Download CSV"),
                               downloadButton("EDU_03_download_xlsx", "Download Excel"))
                           )
                )
              )
      ),
      
      
      # . Health body------------------------------------------------------------
      
      tabItem(tabName = "health",
              tabsetPanel(type="tabs",
                          tabPanel(h4("Immunized Children"),
                                   fluidRow(
                                     box(width = 12,
                                         title=strong("Rate of Immunized Children in 2020"),
                                         closable = FALSE,
                                         solidHeader = TRUE,
                                         collapsible = FALSE,
                                         leafletOutput("immun_map"))
                                     ),
                                   fluidRow( 
                                     box(width = 12,
                                         title=strong("Data"),
                                         closable = FALSE,
                                         solidHeader = TRUE,
                                         collapsible = FALSE,
                                         downloadButton("immun_download_csv", "Download CSV"),
                                         downloadButton("immun_download_xlsx", "Download Excel"),
                                         DT::dataTableOutput("immun_table"))
                                     )
                                   ))),

      
      # . Community body --------------------------------------------------------
      
      tabItem(tabName = "community",
              tabsetPanel(type="tabs",
                          tabPanel(h4("Crime"),
                                   fluidRow(
                                     box(
                                       width=12,
                                       title=strong("Juvenile Arrests (per 100,000 population)"),
                                       closable = FALSE,
                                       solidHeader = TRUE,
                                       collapsible = FALSE,
                                       column(width=6, "Averaged over selected years",leafletOutput("juvcrime_map")),
                                       column(width=6, "Over time", plotOutput("juvcrime_timeser"))
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title=strong("Serious Crime (per 100,000 population) 2019"),
                                       closable = FALSE,
                                       solidHeader = TRUE,
                                       collapsible = FALSE,
                                       leafletOutput("sercrime_map")
                                     ),
                                     box(
                                       title=strong("Data"),
                                       closable = FALSE,
                                       solidHeader = TRUE,
                                       collapsible = FALSE,
                                       downloadButton("crime_download_csv", "Download CSV"),
                                       downloadButton("crime_download_xlsx", "Download Excel"),
                                       DT::dataTableOutput("crime_table")
                                     ))
                          ))),
      
      
      # . Services body ---------------------------------------------------------
      
      tabItem(tabName = "services"),
      tabItem(tabName = "one")
      )
  )



# UI - User Interface -----------------------------------------------------
ui <- dashboardPage(header, sidebar, body, 
                    footer = dashboardFooter(
                      left = HTML("<p>I2D2 & DSPG Project"),
                      right = HTML("<p>i2d2@iastate.edu")
                    ),
                    title = "I2D2 Dashboard", skin = "blue")



# Server ------------------------------------------------------------------
server <- function(input, output, session) { 
  
  
  # Prepare data for Education Attainment line plot and table
  EDU_data_01_county <- reactive({
    # make a list of counties to plot
    my_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    # filter data
    data_ACS %>%
      filter(group_3 == "Less than High School Graduate",
             between(year, input$YEAR[1], input$YEAR[2]),
             county %in% my_county) %>%
      mutate(county = factor(county, levels = my_county))
  })
  
  # Make line plot for Education Attainment tab
  output$EDU_plot_line_01 <- renderPlot({
    EDU_data_01_county() %>%
      filter(group_2 == input$EDU_plot_line_01_toggle) %>%
      plot_line_year(df = ., PERCENT = TRUE) +
      labs(
        # title="Proportion of Women Who Has A Birth In The Past 12 Months",
        # subtitle="less than high school education",
        caption="Source: ACS 5-Year Survey Table B13014")
  })
  
  
  # Prepare data for Education Attainment map and bar chart
  EDU_data_01_averaged <- reactive({
    data_ACS %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      group_by(fips, county, group_2, group_3) %>%
      summarise(value = mean(value))
  })
  
  
  # Make map for Education Attainment tab
  output$EDU_plot_map_01 <- renderLeaflet({
    EDU_data_01_averaged() %>%
      filter(group_3 == input$EDU_plot_map_01_toggle,      # chose education grade
             group_2 == input$EDU_plot_line_01_toggle) %>% #choose marital status
      mutate(value = value *100) %>%
      rowwise() %>%
      mutate(
        popup_label = htmltools::HTML(sprintf('<b>%s</b>
    <br><span style="padding-left: 10px;">%s: <b>%.1f%%</b>
    <br><span style="padding-left: 10px;">Marital Status: <b>%s</b>',
                                              county, group_3, value, group_2))) %>%
      ungroup() %>%
      plot_map_mean(COUNTY = input$COUNTY)
  })
  
  # Make bar plot for Education Attainment tab
  output$EDU_plot_bar_01 <- renderPlot({
    # make a list of counties to plot
    my_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    
    my_years <- c(input$YEAR[1], input$YEAR[2])
    
    EDU_data_01_averaged() %>%
      filter(county %in% my_county,
             group_2 != "Both") %>%
      mutate(county = factor(county, levels = my_county)) %>%
      plot_bar_mean(PERCENT = TRUE, YEARS = my_years) 
  })
  
  # Make table to go with the Education Attainment line plot
  output$EDU_plot_01_table <- DT::renderDataTable({
    EDU_data_01_county() %>%
      spread(group_2, value) %>%
      select(County = county, Year = year, Married, Unmarried, Both) %>%
      datatable() %>%
      formatPercentage(3:5, 2)
  })
  
  
  # Download data as csv
  output$EDU_download_csv <- downloadHandler(
    filename = function() {
      paste0("Education", ".csv")
    },
    content = function(file) {
      write.csv(EDU_data_01_county(), file, row.names = FALSE)
    }
  )
  
  # Download data as xlsx
  output$EDU_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("Education", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(EDU_data_01_county(), file)
    }
  )
  
  # Prepare data for Early Literacy Skills plot and table
  EDU_data_03_county <- reactive({
    # make a list of counties to plot
    my_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    
    my_years <- c(input$YEAR[1], input$YEAR[2])
    
    data_k_assessment %>%
      filter(county %in% my_county) %>%
      mutate(county = factor(county, levels = my_county),
             year = factor(year))
  })
  
  
  # Make bar plot for Early Literacy Skills tab
  output$EDU_plot_bar_03 <- renderPlotly({
    EDU_data_03_county() %>% 
      ggplot(aes(year, percent_met_benchmark, fill = county)) + 
      geom_col(position = "dodge") +
      scale_fill_manual(name = NULL, values = c("orange", "grey20")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 0, size=8, vjust =0.5),
            strip.text = element_text(size = 12)) 
  })
  
  # Make table to go with the Early Literacy Skills plot
  output$EDU_plot_03_table <- DT::renderDataTable({
    data_k_assessment %>%
      select(County = county, Year = year, 
             `ECI Area Name` = eci_area_name,
             `Percent Met Benchmark` = percent_met_benchmark) %>%
      datatable() %>%
      formatPercentage(4, 1)
  })
  
  # Download data as csv
  output$EDU_03_download_csv <- downloadHandler(
    filename = function() {
      paste0("Education", ".csv")
    },
    content = function(file) {
      write.csv(data_k_assessment %>%
                  select(County = county, Year = year, 
                         `ECI Area Name` = eci_area_name,
                         `Percent Met Benchmark` = percent_met_benchmark), 
                file, row.names = FALSE)
    }
  )
  
  # Download data as xlsx
  output$EDU_03_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("Education", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data_k_assessment %>%
                            select(County = county, Year = year, 
                                   `ECI Area Name` = eci_area_name,
                                   `Percent Met Benchmark` = percent_met_benchmark), 
                          file)
    }
  )
  

  output$line2 <- renderPlot({
    data %>%
      filter(county %in% c("Story", "Boone")) %>%
      select(county, var146:var157) %>%
      gather(Var, value, -county) %>%
      left_join(varnames) %>%
      separate(Description, into = c("Var", "Year"), sep = ",", convert = TRUE) %>%
      separate(Var, into = c("key","of", "var"), extra = "merge") %>%
      spread(key, value) %>%
      ggplot(aes(Year, Percent, col = county)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = NULL, y =  NULL, col = "County") +
      scale_x_continuous(labels = function(x) sprintf("%.0f", x),
                         breaks = seq(1990, 2021, 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  
  output$line3 <- renderPlot({
    data %>%
      filter(county %in% c("Story", "Dalas")) %>%
      select(county, var146:var157) %>%
      gather(Var, value, -county) %>%
      left_join(varnames) %>%
      separate(Description, into = c("Var", "Year"), sep = ",", convert = TRUE) %>%
      separate(Var, into = c("key","of", "var"), extra = "merge") %>%
      spread(key, value) %>%
      ggplot(aes(Year, Percent, col = county)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = NULL, y =  NULL, col = "County") +
      scale_x_continuous(labels = function(x) sprintf("%.0f", x),
                         breaks = seq(1990, 2021, 1)) +
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
  
  acs_pov_map <- reactive ({
    plotting_var <- 
      if(input$emp_race == "All") {
        "B17020_pup6"
      } else if(input$emp_race == "All None White") {
        "B17020_pup6m"
      } else if(input$emp_race == "Black") {
        "B17020_pup6b"
      } else if(input$emp_race == "Native American") {
        "B17020_pup6n"
      } else if(input$emp_race == "Asian Alone") {
        "B17020_pup6a"
      } else if(input$emp_race == "Pacific Islander") {
        "B17020_pup6pci"
      } else if(input$emp_race == "Other") {
        "B17020_pup6s"
      } else if(input$emp_race == "Multiple") {
        "B17020_pup6t"
      }  else if(input$emp_race == "Hispanic") {
        "B17020_pup6l"
      } else {"B17020_pup6wa"}
    
    acs_inds %>% 
      select(GEOID, NAME, year, B17020_pup6,
             B17020_pup6m, 
             B17020_pup6wa,
             B17020_pup6w, 
             B17020_pup6b,
             B17020_pup6n,
             B17020_pup6a,
             B17020_pup6pci,
             B17020_pup6s,
             B17020_pup6t,
             B17020_pup6l) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(NAME!= "Statewide", year>=2013) %>%
      select(GEOID, NAME, value = plotting_var) %>%
      group_by(GEOID, NAME) %>%
      summarise(value=mean(value, na.rm=TRUE)) %>%
      left_join(iowa_map, by = c("GEOID" = "fips")) %>%
      sf::st_as_sf(.) 
  })
  
  output$emp_map_1 <- renderLeaflet({
    
    mypal <- colorNumeric("YlOrRd", acs_pov_map()$value*100)
    mytext <- paste(
      "County: ", acs_pov_map()$NAME,"<br/>", 
      "Percent: ", percent(acs_pov_map()$value, accuracy=0.1), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    acs_pov_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(value*100)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~value*100,
                title = "Estimate",
                opacity = .8,
                labFormat = labelFormat(suffix = "%")) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$COUNTY)), "[:punct:]")))
  })
  
  plotInput_emp_map_1 <- reactive ({
    mypal <- colorNumeric("YlOrRd", acs_pov_map()$value*100)
    mytext <- paste(
      "County: ", acs_pov_map()$NAME,"<br/>", 
      "Percent: ", percent(acs_pov_map()$value, accuracy=0.1), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    acs_pov_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(value*100)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~value*100,
                title = "Estimate",
                opacity = .8,
                labFormat = labelFormat(suffix = "%")) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$COUNTY)), "[:punct:]")))
  })
  
  output$emp_map_1_png <- downloadHandler(
    filename = paste("ChildPoverty_", input$COUNTY, "_Map.png", sep = ""),
    content = function(file) {
      # device <- function(..., width, height) {
      #   grDevices::png(..., width = width, height = height,
      #                  res = 300, units = "in")
      # }
      mapshot(plotInput_emp_map_1(), file = file)
    })
  
 
  acs_pov_react <- reactive ({
    plotting_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    plotting_var <- 
      if(input$emp_race == "All") {
        "B17020_pup6"
      } else if(input$emp_race == "All None White") {
        "B17020_pup6m"
      } else if(input$emp_race == "Black") {
        "B17020_pup6b"
      } else if(input$emp_race == "Native American") {
        "B17020_pup6n"
      } else if(input$emp_race == "Asian Alone") {
        "B17020_pup6a"
      } else if(input$emp_race == "Pacific Islander") {
        "B17020_pup6pci"
      } else if(input$emp_race == "Other") {
        "B17020_pup6s"
      } else if(input$emp_race == "Multiple") {
        "B17020_pup6t"
      }  else if(input$emp_race == "Hispanic") {
        "B17020_pup6l"
      } else {"B17020_pup6wa"}
    
    acs_inds %>% 
      select(NAME, year, value = !!as.name(plotting_var)) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(year>=min(year), NAME%in%plotting_county)
  })
  
  output$emp_timeser_1 <- renderPlot({
    acs_time_ser(acs_pov_react(), "value")
  })
  
  plotInput_emp_timeser_1_png <- reactive({
    acs_time_ser(acs_pov_react(), "value")
    })
     
  output$emp_timeser_1_png <- downloadHandler(
    filename = paste("ChildPoverty_", input$COUNTY, "_TimeSeries.png", sep = ""),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput_emp_timeser_1_png(), device = "png")
    })
  
  acs_genpov_react <- reactive ({
    plotting_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    
    acs_inds %>% 
      select(NAME, year, B17020_pup) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(year>=2013, NAME%in%plotting_county)
  })
  
  output$emp_timeser_2 <- renderPlot({
    acs_time_ser(acs_genpov_react(), "B17020_pup")
  })
  
  
  acs_parental_workforce_react <- reactive ({
    plotting_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    
    acs_inds %>% 
      select(NAME, year, B23008_pil, B23008_pnl) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(NAME%in%plotting_county)
  })
  
  output$emp_timeser_3 <- renderPlot({
    plotting_var <- if_else(input$emp_lf_toggle == "All Parents in Workforce", "B23008_pil", "B23008_pnl")
    
    acs_time_ser(acs_parental_workforce_react(), plotting_var)
  })
  
  
  acs_lf <- reactive ({
    plotting_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    acs_inds %>% 
      select(GEOID, NAME, year, B23008_pil, B23008_pnl) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(NAME%in%plotting_county) %>%
      gather(lf, value, B23008_pil:B23008_pnl, factor_key=TRUE) %>%
      group_by(GEOID, NAME, lf) %>%
      summarise(value=mean(value, na.rm=TRUE)) %>%
      mutate(lf=if_else(lf=="B23008_pil", "All Parents in LF", "No Parents in LF"))
  })
  
  output$emp_boxplot_1 <- renderPlot({
    acs_cat_plot(acs_lf(), lf, value)
  })
  
  
  output$emp_table_1 <- DT::renderDataTable({
    acs_inds %>%
      select(county = NAME, year, all = B17020_pup6, white = B17020_pup6wa, black = B17020_pup6b, 
             hispanic = B17020_pup6l, asian = B17020_pup6a, native = B17020_pup6n, 
             pci = B17020_pup6pci, other = B17020_pup6s, multiple = B17020_pup6t, 
             nonewite = B17020_pup6m) %>%
      datatable() %>%
      formatPercentage(3:12, 2)
  })
  
  # Download data as csv
  output$emp_1_download_csv <- downloadHandler(
    filename = function() {
      paste0("Child_Poverty", ".csv")
    },
    content = function(file) {
      write.csv(acs_inds %>%
                  select(county = NAME, year, all = B17020_pup6, white = B17020_pup6wa, black = B17020_pup6b, 
                         hispanic = B17020_pup6l, asian = B17020_pup6a, native = B17020_pup6n, 
                         pci = B17020_pup6pci, other = B17020_pup6s, multiple = B17020_pup6t, 
                         nonewite = B17020_pup6m), file, row.names = FALSE)
    }
  )
  
  # Download data as xlsx
  output$emp_1_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("Child_Poverty", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(acs_inds %>%
                            select(county = NAME, year, all = B17020_pup6, white = B17020_pup6wa, black = B17020_pup6b, 
                                   hispanic = B17020_pup6l, asian = B17020_pup6a, native = B17020_pup6n, 
                                   pci = B17020_pup6pci, other = B17020_pup6s, multiple = B17020_pup6t, 
                                   nonewite = B17020_pup6m), file)
    }
  )
  
  output$emp_table_2 <- DT::renderDataTable({
    acs_inds %>%
      select(name = NAME, year, all_in_LF = B23008_pil, no_in_LF = B23008_pnl) %>%
      datatable() %>%
      formatPercentage(3:4,2)
  })
  
  
  # Download data as csv
  output$emp_1_download_csv <- downloadHandler(
    filename = function() {
      paste0("Parents_in_LF", ".csv")
    },
    content = function(file) {
      write.csv(acs_inds %>%
                  select(name = NAME, year, all_in_LF = B23008_pil, no_in_LF = B23008_pnl), file, row.names = FALSE)
    }
  )
  
  # Download data as xlsx
  output$emp_1_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("Parents_in_LF", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(acs_inds %>%
                            select(name = NAME, year, all_in_LF = B23008_pil, no_in_LF = B23008_pnl), file)
    }
  )
  
  acs_lf_map <- reactive ({
    plotting_var <- if_else(input$emp_lf_toggle == "All Parents in Workforce", "B23008_pil", "B23008_pnl")
    
    acs_inds %>% 
      select(GEOID, NAME, year, B23008_pil, B23008_pnl) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(NAME!= "Statewide") %>%
      select(GEOID, NAME, value = plotting_var) %>%
      group_by(GEOID, NAME) %>%
      summarise(value=mean(value, na.rm=TRUE)) %>%
      left_join(iowa_map, by = c("GEOID" = "fips")) %>%
      sf::st_as_sf(.) 
  })
  
  output$emp_map_2 <- renderLeaflet({
    
    mypal <- colorNumeric("YlOrRd", acs_lf_map()$value*100)
    mytext <- paste(
      "County: ", acs_lf_map()$NAME,"<br/>", 
      "Percent: ", percent(acs_lf_map()$value, accuracy=0.1), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    acs_lf_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(value*100)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~value*100,
                title = "Estimate",
                opacity = .8,
                labFormat = labelFormat(suffix = "%")) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$COUNTY)), "[:punct:]")))
  })
  
  
  immun_rate_map <- reactive ({
    immun_clean %>%
      filter(NAME != "Statewide") %>%
      select(NAME, value = Percent) %>%
      mutate(NAME=str_to_lower(NAME))%>%
      left_join(iowa_map, by = c("NAME"="county")) %>%
      sf::st_as_sf(.)
    
  })
  
  output$immun_map <- renderLeaflet({
    
    mypal <- colorNumeric("YlOrRd", immun_rate_map()$value*100)
    mytext <- paste(
      "County: ", str_to_title(immun_rate_map()$NAME),"<br/>", 
      "Percent: ", percent(immun_rate_map()$value, accuracy=0.1), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    immun_rate_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(value*100)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~value*100,
                title = "Estimate",
                opacity = .8,
                labFormat = labelFormat(suffix = "%")) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$COUNTY)), "[:punct:]")))
  })
  
  
  output$immun_table <- DT::renderDataTable({
    immun_clean %>%
      select(name = NAME, year, percent = Percent) %>%
      datatable() %>%
      formatPercentage(3,2)
  })
  
  
  # Download data as csv
  output$immun_download_csv <- downloadHandler(
    filename = function() {
      paste0("child_immunization", ".csv")
    },
    content = function(file) {
      write.csv(immun_clean %>%
                  select(name = NAME, year, percent = Percent), file, row.names = FALSE)
    })
  
  # Download data as xlsx
  output$immun_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("child_immunization", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(immun_clean %>%
                            select(name = NAME, year, percent = Percent), file)
    })
  
  sercrime_rate_map <- reactive ({
    ser_crime %>%
      filter(NAME != "Statewide") %>%
      select(NAME, fips, value = per100kRate) %>%
      left_join(iowa_map[-3], by = c("fips")) %>%
      sf::st_as_sf(.)
    
  })
  
  output$sercrime_map <- renderLeaflet({
    
    mypal <- colorNumeric("YlOrRd", sercrime_rate_map()$value)
    mytext <- paste(
      "County: ", sercrime_rate_map()$NAME,"<br/>", 
      "Esimate Per 100k: ", round(sercrime_rate_map()$value, 1), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    sercrime_rate_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(value)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~value,
                title = "Estimate",
                opacity = .8) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$COUNTY)), "[:punct:]")))
  })
  
  juvcrime_rate_map <- reactive ({
    juv_crime %>%
      filter(NAME != "Statewide") %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      group_by(NAME, fips)%>%
      summarise(value=mean(rate, na.rm=TRUE)) %>%
      left_join(iowa_map[-3], by = c("fips")) %>%
      sf::st_as_sf(.)
  })
  
  output$juvcrime_map <- renderLeaflet({
    
    mypal <- colorNumeric("YlOrRd", juvcrime_rate_map()$value)
    mytext <- paste(
      "County: ", juvcrime_rate_map()$NAME,"<br/>", 
      "Esimate Per 100k: ", round(juvcrime_rate_map()$value, 1), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    juvcrime_rate_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(value)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~value,
                title = "Estimate",
                opacity = .8) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$COUNTY)), "[:punct:]")))
  })
  
  juvcrime_react <- reactive ({
    plotting_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    
    juv_crime %>% 
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      filter(NAME%in%plotting_county)
  })
  
  
  output$juvcrime_timeser <- renderPlot({
    rate_time_ser(juvcrime_react(), "rate")
  })
  
  output$crime_table <- DT::renderDataTable({
    juv_crime %>%
      left_join(ser_crime, by=c("NAME", "year")) %>%
      select(name = NAME, year, serious_crime = per100kRate, juvenile_crime = rate) %>%
      mutate(serious_crime=round(serious_crime,2),
             juvenile_crime=round(juvenile_crime, 2)) %>%
      datatable()
  })
  
  
  # Download data as csv
  output$crime_download_csv <- downloadHandler(
    filename = function() {
      paste0("crime", ".csv")
    },
    content = function(file) {
      write.csv(juv_crime %>%
                  left_join(ser_crime, by=c("NAME", "year")) %>%
                  select(name = NAME, year, serious_crime = per100kRate, juvenile_crime = rate), file, row.names = FALSE)
    })
  
  # Download data as xlsx
  output$crime_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("crime", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(juv_crime %>%
                            left_join(ser_crime, by=c("NAME", "year")) %>%
                            select(name = NAME, year, serious_crime = per100kRate, juvenile_crime = rate), file)
    })
  
  #unemployment rate map reactive 
  unemp_map <- reactive ({
    unemployment_rate_by_year %>%
      filter(year == input$Unemp_year) %>%
      left_join(iowa_map[-3], by= "fips") %>%
      sf::st_as_sf(.)
  })
  #unemployment map
  output$unemp_map <- renderLeaflet({
    mypal <- colorNumeric("viridis", unemp_map()$unemprate)
    mytext <- paste(
      "County: ", unemp_map()$name,"<br/>",
      "Per Year: ", round(unemp_map()$unemprate, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    unemp_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(name, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(unemprate)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright",
                pal = mypal,
                values = ~unemprate,
                title = "Estimate",
                opacity = .8)
  })
  
  
  unemp_rate_statewide <- reactive({
    unemployment_rate_by_year %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      group_by(year) %>%
      summarise(value = mean(unemprate, na.rm=TRUE))%>%
      mutate(name = "Statewide")
  })
  
  output$unemp_timeser <- renderPlot({
    unemp_timeser(unemp_rate_statewide())  #allow to select different county
  })
   
  output$unemp_boxplot <- renderPlot({
    unemp_box_plot(unemp_rate_statewide())
  })
  
  
  output$unemp_table <- DT::renderDataTable({
    unemployment_rate_by_year %>%
      select(name, year, unemprate) %>%
      datatable()
  })

  
  # Download data as csv
  output$unemp_download_csv <- downloadHandler(
    filename = function() {
      paste0("unemployment_rate", ".csv")
    },
    content = function(file) {
      write.csv(unemployment_rate_by_year %>%
                  select(name, year, percent = unemprate), file, row.names = FALSE)
    })
  
  # Download data as xlsx
  output$unemp_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("unemployment_rate", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(unemployment_rate_by_year %>%
                            select(name, year, percent = unemprate), file)
    })
  
  
  #childcare_rate_map_by provider 1 
  childcare_rate_map_1 <- reactive ({
    childcare_rates_provider1 %>%
      filter(age == input$child_age) %>%
      left_join(iowa_map[-3], by= "fips") %>%
      sf::st_as_sf(.)
  })
  #childcare rate by provider 1 map
  output$childcare_rate_map_1 <- renderLeaflet({
    mypal <- colorNumeric("YlOrRd", childcare_rate_map_1()$cost)
    mytext <- paste(
      "County: ", childcare_rate_map_1()$county,"<br/>",
      "Per Week: ", round(childcare_rate_map_1()$cost, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    childcare_rate_map_1() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(county, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(cost)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright",
                pal = mypal,
                values = ~cost,
                title = "Cost Per Week",
                opacity = .8) 
  })
  
  #childcare_rate_map_by provider 2
  childcare_rate_map_2 <- reactive ({
    childcare_rates_provider2 %>%
      filter(age == input$child_age) %>%
      left_join(iowa_map[-3], by= "fips") %>%
      sf::st_as_sf(.)
  })
  #childcare rate map by provider 2
  output$childcare_rate_map_2 <- renderLeaflet({
    mypal <- colorNumeric("viridis", childcare_rate_map_2()$cost)
    mytext <- paste(
      "County: ", childcare_rate_map_2()$county,"<br/>",
      "Per Week: ", round(childcare_rate_map_2()$cost, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    childcare_rate_map_2() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(county, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(cost)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright",
                pal = mypal,
                values = ~cost,
                title = "Cost Per Week",
                opacity = .8)
  })
  
  
  
  #Childcare rate per week
  rate <- reactive({
    childcare_rates %>%
      filter(age, input$age) %>%
      group_by(year) %>%
      summarise(value = mean(cost, na.rm=TRUE))
  })

    
  #Table Provider type 
  output$childcare_rate_table <- DT::renderDataTable({
    childcare_rates %>%
      select(county, year, provider_type, age, cost) %>%
      datatable()
  })
  
  # Download data as csv
  output$childcare_rate_download_csv <- downloadHandler(
    filename = function() {
      paste0("childcare_rate", ".csv")
    },
    content = function(file) {
      write.csv(child_care_rates %>%
                  select(county, year,provider_type, age, cost), file, row.names = FALSE)
    })

  # Download data as xlsx
  output$childcare_rate_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("childcare_rate", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(childcare_rates %>%
                            select(county, year,provider_type, age, cost), file)
    })
  
  
  #Childcare Availability Childcare Program spaces map reactive 
  childcare_space_map <- reactive ({
    childcare_spaces %>%
      filter(year == input$select_year)  %>%
      left_join(iowa_map[, -3], by= "fips") %>%
      sf::st_as_sf(.)
  })
  #Childcare Availability map
  output$childcare_space_map <- renderLeaflet({
    mypal <- colorNumeric("viridis", childcare_space_map()$spaces)
    mytext <- paste(
      "County: ", childcare_space_map()$county,"<br/>",
      "Program: ", round(childcare_space_map()$programs, 1),"<br/>",
      "Space: ", round(childcare_space_map()$spaces, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    childcare_space_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(county, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(spaces)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright",
                pal = mypal,
                values = ~spaces,
                title = "Childcare Available",
                opacity = .8)
  })

  output$childcare_space_table <- DT::renderDataTable({
    childcare_spaces %>%
      select(county, year, programs, spaces) %>%
      datatable() 
  })
  

  
  # Download data as csv
  output$childcare_space_download_csv <- downloadHandler(
    filename = function() {
      paste0("childcare_program_space", ".csv")
    },
    content = function(file) {
      write.csv(childcare_spaces %>%
                  select(county, year, programs, spaces), file, row.names = FALSE)
    })
  
  # Download data as xlsx
  output$childcare_space_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("childcare_program_space", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(childcare_spaces %>%
                            select(county, year, programs, spaces), file)
    })
  
  #child abuse map reactive  WORK
  childabuse_map <- reactive ({
    child_abuse_county_state %>%
      filter(year == input$abuse_year)  %>%
      mutate(county=str_to_lower(name))%>%
      left_join(iowa_map, by= "county") %>%
      sf::st_as_sf(.)
  })
  #unemployment map
  output$childabuse_map <- renderLeaflet({
    mypal <- colorNumeric("YlOrRd", childabuse_map()$child_abuse_under_6)
    mytext <- paste(
      "County: ", str_to_title(childabuse_map()$name),"<br/>",
      "Under 6: ", round(childabuse_map()$child_abuse_under_6, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    childabuse_map() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 7, maxZoom = 7,
                                       dragging = FALSE)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(name, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = .4, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(child_abuse_under_6)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright",
                pal = mypal,
                values = ~child_abuse_under_6,
                title = "Childabuse",
                opacity = .8)
  })
  
  
  output$childabuse_table <- DT::renderDataTable({
    child_abuse_county_state %>%
      select(county =name, year, age_under_3 = child_abuse_under_3, age_3_and_4 = child_abuse_3_and_4, 
             age_5 = child_abuse_5, age_under_6 =child_abuse_under_6) %>%
      filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
      datatable() 
  })

  # Download data as csv
  output$childabuse_download_csv <- downloadHandler(
    filename = function() {
      paste0("child abuse", ".csv")
    },
    content = function(file) {
      write.csv(child_abuse_county_state %>%
                  select(county = name, year, age_under_3 = child_abuse_under_3, age_3_and_4 = child_abuse_3_and_4, 
                         age_5 = child_abuse_5, age_under_6 =child_abuse_under_6), file, row.names = FALSE)
    })

  # Download data as xlsx
  output$childabuse_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("child abuse", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(child_abuse_county_state %>%
                            select(county = name, year, age_under_3 = child_abuse_under_3, age_3_and_4 = child_abuse_3_and_4, 
                                   age_5 = child_abuse_5, age_under_6 =child_abuse_under_6), file)
    })

  
}



# Shiny -------------------------------------------------------------------
shinyApp(ui, server)



