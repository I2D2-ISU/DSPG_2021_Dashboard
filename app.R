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
library(lubridate)
library(shinydashboardPlus)


source("modules.R")
source("data.R")
source("data_Avery.R")





# Header ------------------------------------------------------------------
header <-  dashboardHeader(title = "DSPG Dashboard")



# Sidebar -----------------------------------------------------------------
sidebar <-  
  dashboardSidebar(
    # Menu content
    sidebarMenu(
      # freezes the side bar
      #style = "position:fixed;",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data by Topic", tabName = "topic", icon = icon("database"),
               # data categories to show
               p(),
               p("Explore Data of Interest:", style="margin-left: 15px"),
               menuSubItem("Children & Families", tabName = "demographics", icon = icon("baby")),
               menuSubItem("Employment & Income", tabName = "employment", icon = icon("hand-holding-usd")),
               menuSubItem("Education", tabName = "education", icon = icon("book-open")),
               menuSubItem("Physical & Mental Health", tabName = "health", icon = icon("heartbeat")),
               menuSubItem("Community", tabName = "community", icon = icon("users")),
               menuSubItem("Services", tabName = "services", icon = icon("user-cog")),
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
                 choices = 2010:2019,
                 selected = c(2010, 2019)
               ),
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
    tags$head(tags$style(HTML(".small-box {height: 150px}"))),
    
    
    # . Dashboard body ----------------------------------------------------------
    tabItems(
      tabItem(tabName = "dashboard",
              h1("ECI Board Approved Indicators"),
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
              indicator_box_ui("INDICATORS", INDICATOR = "Educational attainment of mothers", 
                               VALUE = .12, FORMAT = "%", COLOR = "blue"),
              indicator_box_ui("INDICATORS", INDICATOR = "Children under age 6 living in poverty", 
                               VALUE = ind_pup6, FORMAT = "%", COLOR = "blue"),
              indicator_box_ui("INDICATORS", INDICATOR = "Children under age 6 with all parents in the workforce", 
                               VALUE = ind_pil, FORMAT = "%", COLOR = "blue"),
              indicator_box_ui("INDICATORS", INDICATOR = "Low birth weight", 
                               VALUE = .12, FORMAT = "%"),
              indicator_box_ui("INDICATORS", INDICATOR = "Immunized children", 
                               VALUE = .12, FORMAT = "%"),
              indicator_box_ui("INDICATORS", INDICATOR = "Dental services", 
                               VALUE = .12, FORMAT = "%"),
              
              fluidRow(),
              fluidRow(
                box(title = "InfoBox", 
                    width = 12,
                    status = 'primary', 
                    solidHeader = TRUE, 
                    background = "black", 
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    
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
              
              # bsModal("modalExample", 
              #         "Name of the Table 1", 
              #         "click_si_UNEMPLOYMENT", 
              #         size = "large",
              #         dataTableOutput("table")),
              # 
              # bsModal("modalExample2",
              #         "Name of the Table 2",
              #         "click_si_CRIME",
              #         size = "large",
              #         dataTableOutput("table2")),
              # 
              # bsModal("modalExample3",
              #         "Name of the Table 3",
              #         "click_si_POVERTY6",
              #         size = "large",
              #         dataTableOutput("table3")),
              # 
              # bsModal("modalExample4",
              #         "Data Table",
              #         "click_si_ABUSE6",
              #         size = "large",
              #         plotOutput(outputId = "line")),
              # 
              # bsModal("modalExample5",
              #         "Data Table - label to show",
              #         "click_si_RENTER6",
              #         size = "large",
              #         plotOutput(outputId = "line2")),
              # 
              # bsModal("modalExample6",
              #         "Figure - label to show",
              #         "click_si_FEMALE",
              #         size = "large",
              #         leafletOutput(outputId = "dataMap"))
              # 
      ),
      
      
      # . Child and Families body -----------------------------------------------
      
      tabItem(tabName = "demographics"),
      
      
      # . Employment body -------------------------------------------------------
      
      tabItem(tabName = "employment",
              fluidRow(
                box(title="% in Pov",
                    closable = FALSE,
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    sliderTextInput(
                      inputId = "yrx",
                      label = "Choose Years", 
                      choices = 2013:2019,
                      selected = c(2013, 2019)),
                    selectInput(
                      inputId = "countyx",
                      label = strong("Select County"),
                      choices = unique(str_to_title(iowa_map$county)),
                      selected = NULL),
                    leafletOutput("emp_map_1")
                    )
              )),
      
      
      # . Education body --------------------------------------------------------
      
      tabItem(tabName = "education",
              tabsetPanel(
                type = "tabs",
                
                tabPanel(h4("Educational Attainment"),
                         fluidRow(
                           br(),
                           box(width = 7, height = 500,
                               toggle_button("EDU_plot_line_01_toggle",
                                             c("Married", "Unmarried", "Both")),
                               plotOutput("EDU_plot_line_01")
                           ),
                           
                           box(width = 5, height = 500,
                               plotOutput("EDU_plot_bar_01", height = "480px")
                           )
                         ),
                         fluidRow(
                           box(width = 7, height = 500,
                               pickerInput(
                                 inputId = "EDU_plot_map_01_toggle",
                                 label = "Select Education Level",
                                 choices = levels(data_ACS$group_3)),  
                               leafletOutput("EDU_plot_map_01")
                           ),
                           box(width = 5, height = 500,
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
                
                tabPanel(h4("Education by Sex"),
                         h3("Proportion of Women Who Has A Birth In The Past 12 Months"),
                         fluidRow(
                           br(),
                           box(width = 7, height = 500,
                               toggle_button("EDU_plot_line_02_toggle",
                                             c("Married", "Unmarried", "Both")),
                               plotlyOutput("EDU_plot_line_02")
                           ),
                           
                           box(width = 5, height = 500,
                               plotlyOutput("EDU_plot_bar_02", height = "480px")
                           )
                         ),
                         fluidRow(
                           box(width = 7, height = 500,
                               pickerInput(
                                 inputId = "EDU_plot_map_02_toggle",
                                 label = "Select Education Level",
                                 choices = levels(data_ACS$group_3)),  
                               leafletOutput("EDU_plot_map_02")
                           ),
                           box(width = 5, height = 500,
                               DT::dataTableOutput("EDU_plot_02_table")
                           )
                         )
                ),
                
                tabPanel(h4("Educational Other"),
                         fluidRow(
                           plotOutput("line2"),
                           column(7,
                                  h3("Percentage own children Under 6"),
                                  box(width = 12,
                                      textOutput("TEST_INPUT_Statewide"),
                                      plotOutput("dataPlot33")
                                  )
                           ),
                           
                           column(5),
                           plotOutput("line3")
                         )
                ),
                
                tabPanel(h4('Add Tab'))
              )
      ),
      
      
      # . Health body------------------------------------------------------------
      
      tabItem(tabName = "health"),
      
      
      # . Community body --------------------------------------------------------
      
      tabItem(tabName = "community"),
      
      
      # . Services body ---------------------------------------------------------
      
      tabItem(tabName = "services",
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
      )
    )
  )



# UI - User Interface -----------------------------------------------------
ui <- dashboardPage(header, sidebar, body, title = "I2D2 Dashboard", skin = "blue")



# Server ------------------------------------------------------------------
 server <- function(input, output, session) { 


  showModal(
    modalDialog(
      title = h2("Welcome to I2D2 Dashboard",
                 img(src = 'https://media-exp1.licdn.com/dms/image/C4D0BAQGExeY0J4R5Pw/company-logo_200_200/0/1626097601158?e=1634774400&v=beta&t=Ucaroc-NsOEeqXWdMQdFvwRIHILza3OS3g7-wU1xLJ8', height = 50, align="right")),
      easyClose = TRUE,
      footer = modalButton("Explore the Dashboard"),
      img(src = 'https://media-exp1.licdn.com/dms/image/C4D0BAQGExeY0J4R5Pw/company-logo_200_200/0/1626097601158?e=1634774400&v=beta&t=Ucaroc-NsOEeqXWdMQdFvwRIHILza3OS3g7-wU1xLJ8', height = 100, align="right"),
      tags$p(
        "It's just that this dashboard is more fun when a conference is going on",
        tags$strong(tags$em("Russian Doll")),
        "than after the fact. So we're going to pretend that",
        tags$a(href="https://i2d2.iastate.edu/", tags$strong("I2D2")), "is going on right now"),
      tags$p(
        "The dates in tweet previews will be correct, but everywhere else in the app:"
      ),
      tags$ul(
        tags$li(
          "First line"
          ),
        tags$li(
          strftime(now() - days(), "%b %e"), "is really"),
        tags$li("and so on...")
      ),
      tags$p("Come back tomorrow and relive it all over again!"),
      # tags$p(HTML("&mdash;&commat; Garrick"),
      #        HTML('(<a href="https://i2d2.iastate.edu/" target="_blank">I2D2</a>)'))
    )
  )



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
        title="Proportion of Women Who Has A Birth In The Past 12 Months",
        subtitle="less than high school education",
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

  # EDUCATION by SEX

  # Make line plot for Education Attainment tab
  output$EDU_plot_line_02 <- renderPlotly({
    EDU_data_01_county() %>%
      filter(group_2 == input$EDU_plot_line_02_toggle) %>%
      plot_line_year(df = ., PERCENT = TRUE) %>%
      ggplotly(., tooltip = "text") %>%
      layout(title = "less than high school education")
  })

  # Make table to go with the Education Attainment line plot
  output$EDU_plot_02_table <- DT::renderDataTable({
    EDU_data_01_county() %>%
      spread(group_2, value) %>%
      select(County = county, Year = year, Married, Unmarried, Both) %>%
      datatable() %>%
      formatPercentage(3:5, 2)
  })

  # Make map for Education Attainment tab
  output$EDU_plot_map_02 <- renderLeaflet({
    EDU_data_01_averaged() %>%
      filter(group_3 == input$EDU_plot_map_02_toggle,      # chose education grade
             group_2 == input$EDU_plot_line_02_toggle) %>% #choose marital status
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
  output$EDU_plot_bar_02 <- renderPlotly({
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
      plot_bar_mean(PERCENT = TRUE, YEARS = my_years) %>%
      ggplotly(., tooltip = "text")
  })

  # # ---- % Poverty Under 6 ---------
  # # Prepare data for Education Attainment line plot and table
  # EMP_data_01_county <- reactive({
  #   # make a list of counties to plot
  #   my_county <-
  #     if(input$STATEWIDE) {
  #       c(input$COUNTY, "Statewide")
  #     } else {
  #       input$COUNTY
  #     }
  #   # filter data
  #   data_ACS %>%
  #     filter(group_3 == "Less than High School Graduate",
  #            between(year, input$YEAR[1], input$YEAR[2]),
  #            county %in% my_county) %>%
  #     mutate(county = factor(county, levels = my_county))
  # })
  # 
  # # Make line plot for Education Attainment tab
  # output$EMP_plot_line_01 <- renderPlot({
  #   EMP_data_01_county() %>%
  #     filter(group_2 == input$EMP_plot_line_01_toggle) %>%
  #     plot_line_year(df = ., PERCENT = TRUE) +
  #     labs(
  #       title="Proportion of Women Who Has A Birth In The Past 12 Months",
  #       subtitle="less than high school education",
  #       caption="Source: ACS 5-Year Survey Table B13014")
  # })




  # TESTING  SOME FOR INFOBOXEs
  output$TEST_INPUT_Statewide <- renderText({
    input$COUNTY
  })


  output$table <- renderDataTable({
    head(data) %>% select(1:5)
  })


  output$TEST <- renderText(input$COUNTY)

  # output$line <- renderPlot({
  #   data %>%
  #     filter(county %in% input$COUNTY) %>%
  #     select(county, var146:var157) %>%
  #     gather(Var, value, -county) %>%
  #     left_join(varnames) %>%
  #     separate(Description, into = c("Var", "Year"), sep = ",", convert = TRUE) %>%
  #     separate(Var, into = c("key","of", "var"), extra = "merge") %>%
  #     spread(key, value) %>%
  #     filter(between(Year, input$YEAR[1], input$YEAR[2])) %>%
  #     ggplot(aes(Year, Percent, col = county)) +
  #     geom_line(size = 1) +
  #     geom_point(size = 3) +
  #     labs(x = NULL, y =  NULL, col = "County") +
  #     scale_x_continuous(labels = function(x) sprintf("%.0f", x),
  #                        breaks = seq(input$YEAR[1], input$YEAR[2], 1)) +
  #     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #     theme_minimal() +
  #     theme(panel.grid.minor.x = element_blank(),
  #           panel.grid.major.x = element_blank())
  # })


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

  # Emp_pov_averaged <- reactive({
  #   data_ACS %>%
  #     filter(between(year, input$YEAR[1], input$YEAR[2])) %>%
  #     group_by(fips, county, group_2, group_3) %>%
  #     summarise(value = mean(value)) })
  
  mapfun <- reactive ({
    acs_inds %>% 
    select(GEOID, NAME, year, B17020_pup6) %>%
    filter(between(year, input$yrx[1], input$yrx[2])) %>%
    filter(NAME!= "Statewide", year>=2013) %>%
      group_by(GEOID, NAME) %>%
      summarise(B17020_pup6m=mean(B17020_pup6, na.rm=TRUE)) %>%
      left_join(iowa_map, by = c("GEOID" = "fips")) %>%
      sf::st_as_sf(.) 
    })
  
  output$emp_map_1 <- renderLeaflet({

    mypal <- colorNumeric("YlOrRd", mapfun()$B17020_pup6m*100)
    mytext <- paste(
      "County: ", mapfun()$NAME,"<br/>", 
      "Percent: ", percent(mapfun()$B17020_pup6m), 
      sep="") %>%
      lapply(htmltools::HTML)

    mapfun() %>%
      sf::st_transform(crs = "+init=epsg:4326") %>%
      leaflet(width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE,  # Set True for border color
                  weight = 1,
                  smoothFactor = 0.3,
                  fillOpacity = 0.7,
                  opacity = 0, # setting opacity to 1 prevents transparent borders, you can play around with this.
                  color = "white", #polygon border color
                  label = mytext,
                  fillColor = ~ mypal(B17020_pup6m*100)) %>% #instead of using color for fill, use fillcolor
      addLegend("bottomright", 
                pal = mypal, 
                values = ~B17020_pup6m*100,
                title = "Estimate",
                opacity = .8,
                labFormat = labelFormat(suffix = "%")) %>%
      addPolylines(data = iowa_map %>% filter(county == str_remove(str_to_lower(paste(input$countyx)), "[:punct:]")))
  })
  
  
}



# Shiny -------------------------------------------------------------------
shinyApp(ui, server)

