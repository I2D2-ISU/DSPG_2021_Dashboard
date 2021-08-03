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


source("modules.R")
source("data.R")
source("data_Avery.R")
source("modules_Avery.R")
source("data_Sonyta.R")
source("modules_Sonyta.R")




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
      
      tabItem(tabName = "demographics",
              tabsetPanel( type="tabs",
                           tabPanel(h4("Child Care Cost"), 
                                    fluidRow(
                                      box(width=12,
                                          pickerInput( inputId = "childcare_rate",
                                                       label = "Select age",
                                                       choices = childcare_rates%>% select(age)%>%distinct()%>%pull(),
                                                       multiple = FALSE,
                                                       selected = "Infant (0-12 Months)"),
                                          title=strong("Childcare cost by age of child"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Childcare Rate by Provider",leafletOutput("childcare_rate_map_1")),
                                          column(width=6, "Childcare Rate by DHS Liencent", plotOutput("childcare_rate_timeser_1"))
                                      )
                                      
                                    ),
                                    fluidRow(
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          width = 12,
                                          downloadButton("childcare_rate_1_download_csv", "Download CSV"),
                                          downloadButton("childcare_rate_1_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("childcare_rate_table_1")
                                      )
                                    )),
                           tabPanel(h4("Child Care Avaibility"),
                                    fluidRow(
                                      box(width=12,
                                          title=strong("Number of ailability of child care by county"),
                                          toggle_button("childcare_lf_toggle",
                                                        c("Program", "Space")),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Childcare Program Space", plotOutput("childcare_space_timeser_3")),
                                          column(width=6, "Childcare avaibility", leafletOutput("childcare_space_map_2")))
                                    ),
                                    fluidRow(
                                      box(
                                        title=strong("Number of availability of child care by county"),
                                        closable = FALSE,
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        plotOutput("chldcare_space_boxplot_1")),
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("childcare_space_2_download_csv", "Download CSV"),
                                          downloadButton("childcare_space_2_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("childcare_space_table_2")
                                      )
                                    )
                           ),
                           tabPanel(h4("Child abuse"),
                                    fluidRow(
                                      box(width=12,
                                          title=strong("incidence of child abuse per 1,000 children"),
                                          toggle_button("childabuse_lf_toggle",
                                                        c("under 3 years old", "4 and 5 years old", "5 years old", "under 6 years old")),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Childabuse Program Space", plotOutput("childabuse_timeser_3")),
                                          column(width=6, "Childabuse avaibility", leafletOutput("childabuse_map_2")))
                                    ),
                                    fluidRow(
                                      box(
                                        title=strong("incidence of child abuse per 1,000 children"),
                                        closable = FALSE,
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        plotOutput("chldabuse_boxplot_1")),
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("childabuse_2_download_csv", "Download CSV"),
                                          downloadButton("childabuse_2_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("childabuse_space_table_2")
                                      )
                                    )
                           ),
                           tabPanel(h4("Child Care Provider"),
                                    fluidRow(
                                      box(title=strong("Child Care Provider"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          plotOutput("childabuse_timeser_2")))
                           ))),
      
    
      
      
      # . Employment body -------------------------------------------------------
      
      tabItem(tabName = "employment",
              tabsetPanel( type="tabs",
                           tabPanel(h4("Child Poverty"), 
                                    fluidRow(
                                      box(width=12,
                                        pickerInput( inputId = "emp_race",
                                                       label = "Select Race/Ethnicity Category",
                                                       choices = c("All", "Minority", "White Alone, Not Hispanic",
                                                                   "Black or African American Alone",
                                                                   "American Indian and Alaska Native Alone",
                                                                   "Asian Alone",
                                                                   "Native Hawaiian and Other Pacific Islander Alone",
                                                                   "Some Other Race Alone",
                                                                   "Two or More Races",
                                                                   "Hispanic or Latino"),
                                                       multiple = FALSE,
                                                       selected = "All"),
                                          title=strong("Percent of Children Under 6 in Poverty"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Averaged over selected years",leafletOutput("emp_map_1")),
                                          column(width=6, "Over time", plotOutput("emp_timeser_1"))
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
                                        column(width=6, "Averaged over selected years", leafletOutput("emp_map_2")))
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
                                      box(title=strong("Percent of Population in Poverty Over Time"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          plotOutput("emp_timeser_2")))
                           ),
                           
                           tabPanel(h4("Unemployment Rate"),
                                    fluidRow(
                                      box(width=12,
                                          title=strong("Unemployment Rate by Year"),
                                          toggle_button("unemp_lf_toggle",
                                                        c("By County", "Statewide")),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          column(width=6, "Unemployment Rate", plotOutput("unemp_timeser_3")),
                                          column(width=6, "Unemployment Rate", leafletOutput("unemp_map_2")))
                                    ),
                                    fluidRow(
                                      box(
                                        title=strong("Unemployment Rate by Year"),
                                        closable = FALSE,
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        plotOutput("unemp_boxplot_1")),
                                      box(title=strong("Data"),
                                          closable = FALSE,
                                          solidHeader = TRUE,
                                          collapsible = FALSE,
                                          downloadButton("unemp_2_download_csv", "Download CSV"),
                                          downloadButton("unemp_2_download_xlsx", "Download Excel"),
                                          DT::dataTableOutput("unemp_table_2")
                                      )
                                    )
                           )
                           
                           
                           
                           )),
                          
      
      
      
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
                           box(width = 7, height = 500, 
                               title = "Education Attainment by County",
                               pickerInput(
                                 inputId = "EDU_plot_map_01_toggle",
                                 label = "Select Education Level",
                                 choices = levels(data_ACS$group_3)),  
                               leafletOutput("EDU_plot_map_01")
                           ),
                           box(width = 5, height = 500,
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
                
                tabPanel(h4("Educational Attainmnet (Plotly Style)"),
                         h3("Proportion of Women Who Has A Birth In The Past 12 Months"),
                         fluidRow(
                           br(),
                           box(width = 7, height = 500,
                               title = "Less than High School Education",
                               toggle_button("EDU_plot_line_02_toggle",
                                             c("Married", "Unmarried", "Both")),
                               plotlyOutput("EDU_plot_line_02")
                           ),
                           
                           box(width = 5, height = 500,
                               title = "Level of Education Attained",
                               plotlyOutput("EDU_plot_bar_02", height = "480px")
                           )
                         ),
                         fluidRow(
                           box(width = 7, height = 500,
                               title = "Education Attainment by County",
                               pickerInput(
                                 inputId = "EDU_plot_map_02_toggle",
                                 label = "Select Education Level",
                                 choices = levels(data_ACS$group_3)),  
                               leafletOutput("EDU_plot_map_02")
                           ),
                           box(width = 5, height = 500,
                               title = "Data",
                               DT::dataTableOutput("EDU_plot_02_table")
                           )
                         )
                ),
                
                # tabPanel(h4("Educational Other"),
                #          fluidRow(
                #            plotOutput("line2"),
                #            column(7,
                #                   h3("Percentage own children Under 6"),
                #                   box(width = 12,
                #                       textOutput("TEST_INPUT_Statewide"),
                #                       plotOutput("dataPlot33")
                #                   )
                #            ),
                #            
                #            column(5),
                #            plotOutput("line3")
                #          )
                # ),
                
                tabPanel(h4('Early Literacy Skills'),
                         h3("Beginning Reading Skills", style="margin-left:20px; font-weight:bold;"),
                         fluidRow(
                           box(width = 6, height = 500,
                               title = "Percent of kindergarten students proficient by kindergarten literacy assessment",
                               plotlyOutput("EDU_plot_line_03")),
                           
                           box(width = 6, height = 500,
                               title = "Percent of kindergarten students proficient by kindergarten literacy assessment",
                               plotlyOutput("EDU_plot_bar_03"))
                         ),
                         fluidRow()
                )
              )
      ),
      
      
      # . Health body------------------------------------------------------------
      
      tabItem(tabName = "health",
              tabsetPanel(type="tabs",
                          tabPanel(h4("Immunized Children"),
                                   fluidRow(
                                     box(title=strong("Rate of Immunized Children in 2020"),
                                         closable = FALSE,
                                         solidHeader = TRUE,
                                         collapsible = FALSE,
                                         leafletOutput("immun_map")),
                                     box(title=strong("Data"),
                                         closable = FALSE,
                                         solidHeader = TRUE,
                                         collapsible = FALSE,
                                         downloadButton("immun_download_csv", "Download CSV"),
                                         downloadButton("immun_download_xlsx", "Download Excel"),
                                         DT::dataTableOutput("immun_table")
                                       
                                     )
                                   )))),
      
      
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
        "Some general welcoming message and short background about this",
        tags$strong(tags$em("dashboard")),
        "can be helpful for first time commers. We can credit contributors and authors like",
        tags$a(href="https://i2d2.iastate.edu/", tags$strong("I2D2")), ", or provide list of sources used 
        to build this dashboard."),
      tags$ul(
        tags$li(
          "First source"
        ),
        tags$li(
          "Scendo sorese (updated on:",
          strftime(now() - days(), "%b %e"), ", 2020)"),
        tags$li("or some other info")
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
  
  # EDUCATION by SEX
  
  # Make line plot for Education Attainment tab
  output$EDU_plot_line_02 <- renderPlotly({
    EDU_data_01_county() %>%
      filter(group_2 == input$EDU_plot_line_02_toggle) %>%
      plot_line_year(df = ., PERCENT = TRUE) %>%
      ggplotly(., tooltip = "text") #%>%
    # layout(title = "less than high school education")
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
  
  acs_pov_map <- reactive ({
    plotting_var <- 
      if(input$emp_race == "All") {
        "B17020_pup6"
      } else if(input$emp_race == "Minority") {
        "B17020_pup6m"
      } else if(input$emp_race == "Black or African American Alone") {
        "B17020_pup6b"
      } else if(input$emp_race == "American Indian and Alaska Native Alone") {
        "B17020_pup6n"
      } else if(input$emp_race == "Asian Alone") {
        "B17020_pup6a"
      } else if(input$emp_race == "Native Hawaiian and Other Pacific Islander Alone") {
        "B17020_pup6pci"
      } else if(input$emp_race == "Some Other Race Alone") {
        "B17020_pup6s"
      } else if(input$emp_race == "Two or More Races") {
        "B17020_pup6t"
      }  else if(input$emp_race == "Hispanic or Latino") {
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
  
  acs_pov_react <- reactive ({
    plotting_county <-
      if(input$STATEWIDE) {
        c(input$COUNTY, "Statewide")
      } else {
        input$COUNTY
      }
    acs_inds %>% 
      select(NAME, year, B17020_pup6,
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
      filter(year>=min(year), NAME%in%plotting_county)
  })
  
  output$emp_timeser_1 <- renderPlot({
    plotting_var <- 
      if(input$emp_race == "All") {
        "B17020_pup6"
      } else if(input$emp_race == "Minority") {
        "B17020_pup6m"
      } else if(input$emp_race == "Black or African American Alone") {
        "B17020_pup6b"
      } else if(input$emp_race == "American Indian and Alaska Native Alone") {
        "B17020_pup6n"
      } else if(input$emp_race == "Asian Alone") {
        "B17020_pup6a"
      } else if(input$emp_race == "Native Hawaiian and Other Pacific Islander Alone") {
        "B17020_pup6pci"
      } else if(input$emp_race == "Some Other Race Alone") {
        "B17020_pup6s"
      } else if(input$emp_race == "Two or More Races") {
        "B17020_pup6t"
      }  else if(input$emp_race == "Hispanic or Latino") {
        "B17020_pup6l"
      } else {"B17020_pup6wa"}
    
    acs_time_ser(acs_pov_react(), plotting_var)
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
      select(name = NAME, year, all_under6 = B17020_pup6, minority_under6 = B17020_pup6m, black_under6 = B17020_pup6b, 
             native_under6 = B17020_pup6n, asian_under6 = B17020_pup6a, pci_under6 = B17020_pup6pci,
             other_under6 = B17020_pup6s, two_under6 = B17020_pup6t, hispanic_under6 = B17020_pup6l,
             whitealone_under6 = B17020_pup6wa) %>%
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
                  select(name = NAME, year, all_under6 = B17020_pup6, minority_under6 = B17020_pup6m, black_under6 = B17020_pup6b, 
                         native_under6 = B17020_pup6n, asian_under6 = B17020_pup6a, pci_under6 = B17020_pup6pci,
                         other_under6 = B17020_pup6s, two_under6 = B17020_pup6t, hispanic_under6 = B17020_pup6l,
                         whitealone_under6 = B17020_pup6wa), file, row.names = FALSE)
    }
  )
  
  # Download data as xlsx
  output$emp_1_download_xlsx <- downloadHandler(
    filename = function() {
      paste0("Child_Poverty", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(acs_inds %>%
                            select(name = NAME, year, all_under6 = B17020_pup6, minority_under6 = B17020_pup6m, black_under6 = B17020_pup6b, 
                                   native_under6 = B17020_pup6n, asian_under6 = B17020_pup6a, pci_under6 = B17020_pup6pci,
                                   other_under6 = B17020_pup6s, two_under6 = B17020_pup6t, hispanic_under6 = B17020_pup6l,
                                   whitealone_under6 = B17020_pup6wa), file)
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
         select(NAME, value = per100kRate) %>%
         mutate(NAME=str_to_lower(NAME))%>%
         left_join(iowa_map, by = c("NAME"="county")) %>%
         sf::st_as_sf(.)
       
     })
     
     output$sercrime_map <- renderLeaflet({
       
       mypal <- colorNumeric("YlOrRd", sercrime_rate_map()$value)
       mytext <- paste(
         "County: ", str_to_title(sercrime_rate_map()$NAME),"<br/>", 
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
         group_by(NAME)%>%
         summarise(value=mean(rate, na.rm=TRUE)) %>%
         mutate(NAME=str_to_lower(NAME))%>%
         left_join(iowa_map, by = c("NAME"="county")) %>%
         sf::st_as_sf(.)
     })
     
     output$juvcrime_map <- renderLeaflet({
       
       mypal <- colorNumeric("YlOrRd", juvcrime_rate_map()$value)
       mytext <- paste(
         "County: ", str_to_title(juvcrime_rate_map()$NAME),"<br/>", 
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
  
  
}



# Shiny -------------------------------------------------------------------
shinyApp(ui, server)



