##################################################### Loading of all library and sources necessary for the application #################################################

#permet d'enelver la limite de chargement de java (pour gros fichier excel)
options(java.parameters = "- Xmx1024m")
options(java.parameters = "- UseGCOverheadLimit")

library("rJava")
library("xlsx")
library("zoo")
library(shiny)
library("Hmisc")
library(ggthemes)
library(plotly)
library("scales")
library(leaflet)
library(shinycssloaders)
library(shinyjs)
library(tcltk2)
library(officer)
library(mschart)
library(assertthat)
library(maptools)
library(htmltools)
library(shinydashboard)
library(highcharter)
library(imager)

library(viridis)
library(shinyWidgets)
library(shinydashboardPlus)
library(stats)
library(dplyr)
library(magrittr)

#permet de recharger la page apres la projection
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
gc()
source("../Functions/Input/General_Inputs_Functions.R")
source("../Functions/Input/Residential_Inputs_Functions.R")
source("../Functions/Input/Industrial_Inputs_Functions.R")
source("../Functions/Input/Agriculture_Inputs_Functions.R")
source("../Functions/Input/Services_Inputs_Functions.R")
source("../Functions/Input/Transport_Inputs_Functions.R")

source("../Functions/Demand Projection/IND_Projection.R")
source("../Functions/Demand Projection/NEU_Projection.R")
source("../Functions/Demand Projection/AGR_Projection.R")
source("../Functions/Demand Projection/TRA_Projection_v2.R")
source("../Functions/Demand Projection/RES_Projection.R")
source("../Functions/Demand Projection/TER_Projection.R")

source("../Functions/Output/General_Output.R")
source("../Functions/Output/Benchmark_Import.R")

########################################################################################################################################################################
################################################ Define UI for application that display the layout and object of the interface #########################################
ui <- dashboardPagePlus(skin = "blue",
                        
                        
                        header  = dashboardHeaderPlus(title = "AMADEUS",                  
                                                      disable = F, 
                                                      fixed = TRUE,
                                                      
                                                      enable_rightsidebar = TRUE,
                                                      rightSidebarIcon = "gears"
                        ),
                        sidebar = dashboardSidebar(collapsed = T,
                                                   sidebarMenu(
                                                   menuItem(text = "Inputs", tabName = "Inputs", icon = icon("th"),
                                                
                                                              menuSubItem("Visualisation", tabName = "visualisation"),
                                                              menuSubItem("Calibration", tabName = "calibration"),
                                                              menuSubItem("Checking", tabName = "checking")
                                                            ),
                                                   menuItem("Projection model", tabName = "projection_model", icon = icon("fas fa-anchor")),
                                                   menuItem("Outputs", tabName = "Outputs", icon = icon("bar-chart-o"),
                                                            
                                                            menuSubItem("Overview", tabName = "overview"),
                                                            menuSubItem("CO2 emissions", tabName = "co2_emissions"),
                                                            menuSubItem("Load profile", tabName = "load_profile"),
                                                            menuSubItem("Decentralized/Centralized", tabName = "decentralized_centralized")
                                                            )
                                                   )

                                                   
                                                   
                                                   
                                                   # tags$head(tags$style(".skin-black .main-sidebar {background-color: #ffffff;}
                                                   #                      .skin-black .main-body {background-color: #ffffff;}
                                                   #                      .country_summary {color: #dddddd;}
                                                   #                      .small-box {height: 130px}
                                                   #                      .sidebar {height: 1000px; overflow-x: hidden; overflow-y: hidden; overflow:scroll;}
                                                   #                      ::-webkit-scrollbar {
                                                   #                      width: 0px;
                                                   #                      background: transparent; /* make scrollbar transparent */
                                                   #                      }
                                                   #                      .sidebar-collapse .left-side, .sidebar-collapse .main-sidebar{ 
                                                   #                      transform: translate(0,0);
                                                   #                      width: 500px;
                                                   #                      }
                                                   #                      .shiny-output-error { visibility: hidden; }
                                                   #                      .shiny-output-error:before { visibility: hidden; } 
                                                   #                      .box.box-solid.box-primary{ background: #222d32}
                                                   #                      .box.box-solid.box-primary>.box-header {color:#fffff;
                                                   #                      background:#222d32}
                                                   #                      "
                       
                     
 ),
 
 body = dashboardBody(useShinyjs(),
                      tags$style(".progress-bar-disabled {background: #FFFFFF;}"),
          tabItems(
            tabItem(tabName = "visualisation",
                    fluidPage(
                      fluidRow(
                        br(),
                        br(),
                        br(),
                        
                        box(title = "Display chart",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            # div(style = 'overflow-x: scroll'),
                            plotlyOutput("Trend_Line")
                            
                        ),
                        
                        box(title = "Energy intensity",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            # div(style = 'overflow-x: scroll'),
                            plotlyOutput("intensity_chart")
                            
                        )
                      ),
                      fluidRow(
                        br(),
                        box(title = "Datatable",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            # div(style = 'overflow-x: scroll'),
                            div(style = 'overflow-x: scroll',tableOutput("dataset")
                                )
                            )
                      )
                    )
            ),
            
            tabItem(tabName = "calibration",
                    fluidPage(
                      fluidRow(
                        br(),
                        br(),
                        br(),
                        
                        boxPlus(title = "Industry substitution rate",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            closable = FALSE,
                            enable_sidebar = TRUE,
                            enable_dropdown = TRUE,
                            sidebar_content = tagList(
                              
                              uiOutput("Country_Substitution"), 
                              uiOutput("Sub_Sector_Substitution")
                            ),
                            
                            dropdown_menu = dropdownItemList(
                              uiOutput("Date_Range_Calibration_Sub"),
                              uiOutput("Launch_Substitution_Calibration")
                              ),
                            # div(style = 'overflow-x: scroll'),
                            withSpinner(plotlyOutput("Simulated_Demand")),
                            br(),
                            h4("Substitution rate table"),
                            div(style = 'overflow-x: scroll',withSpinner(tableOutput("Substitution_Rate")))
                            ),
                        
                        boxPlus(title = "Transport",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                closable = FALSE,
                                enable_sidebar = TRUE,
                                enable_dropdown = TRUE,
                                sidebar_content = tagList(
                                  uiOutput("Country_veh_per_cap_chart")
                                ),
                                
                                dropdown_menu = dropdownItemList(
                                  uiOutput("Calibrate_Road_Transport")
                                ),
                                # div(style = 'overflow-x: scroll'),
                                withSpinner(plotlyOutput("pkm_calibration_Transport_Chart")),
                                br(),
                                withSpinner(plotlyOutput("veh_per_cap_calibration_Transport_Chart"))
                        )
                      )
                    )
            ),
            
            tabItem(tabName = "checking",
                    fluidPage(
                      fluidRow(
                        br(),
                        br(),
                        br(),
                        box(title = "Odysee shares vs Eurostat",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            # div(style = 'overflow-x: scroll'),
                            div(style = 'overflow-x: scroll',tableOutput("Table_Odysee_shares_checking")),
                        width = 12)
                      )
                    )
            ),
            tabItem(tabName = "projection_model", 
                    br(),
                    br(),
                    br(),
                    fluidRow(
                      column(
                        width = 9,
                        leafletOutput("mymap", height = 900)
                        ),
                      column(
                        width = 3,
                        boxPad(
                          color = "black",
                          fluidRow(
                            column(width = 6, 
                              descriptionBlock(header = HTML("End-uses countries:</br></br>"),
                                               text = HTML(list_country_modeling("End-uses"))
                                               )
                                  ),
                            column(width = 6, 
                              descriptionBlock(header = HTML("Sector countries:</br></br>"),
                                               text = HTML(list_country_modeling("Sector"))
                                               )
                                  )
                            ),
                          br(),
                          
                          materialSwitch(
                            inputId = "project_end_uses_choice",
                            label = "Project end-use countries", 
                            value = TRUE,
                            status = "primary",
                            right = TRUE
                          ),
                          
                          materialSwitch(
                            inputId = "project_sector_choice",
                            label = "Project sector countries", 
                            value = TRUE,
                            status = "primary",
                            right = TRUE
                          ),
                          
                          materialSwitch(
                            inputId = "project_load_profile_choice",
                            label = "Project load profiles", 
                            value = FALSE,
                            status = "primary",
                            right = TRUE
                          ),
                          br(),
                          actionButton("Project_Demand","Launch projection", width = "100%")
                          # height = 900
                      )
                    )
                  )
            ),
            tabItem(tabName = "overview",
                    br(),
                    br(),
                    br(),
                    boxPlus(title = "Final energy demand",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        closable = FALSE,
                      fluidRow(
                        column(
                          width = 4,
                          highchartOutput("Final_energy_chart")
                        ),
                        column(
                          width = 4,
                          highchartOutput("Final_sector_chart")
                        )
                      ),
                    width = 12),
                    
                    boxPlus(title = "Residential energy demand",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            closable = FALSE,
                            fluidRow(
                              column(
                                width = 4,
                                highchartOutput("RES_energy_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("RES_usage_chart")
                              )
                            ),
                            width = 12),

                    boxPlus(title = "Services energy demand",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            closable = FALSE,
                            fluidRow(
                              column(
                                width = 4,
                                highchartOutput("TER_energy_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("TER_usage_chart")
                              )
                            ),
                            width = 12),
                    
                    boxPlus(title = "Industrial energy demand",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            closable = FALSE,
                            fluidRow(
                              column(
                                width = 4,
                                highchartOutput("IND_energy_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("IND_sub_sector_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("IND_usage_chart")
                              )
                            ),
                            width = 12),
                    
                    boxPlus(title = "Transport energy demand",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            closable = FALSE,
                            fluidRow(
                              column(
                                width = 4,
                                highchartOutput("TRA_energy_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("TRA_sub_sector_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("TRA_usage_chart")
                              )
                            ),
                            width = 12),
                    
                    boxPlus(title = "Others energy demand",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            closable = FALSE,
                            fluidRow(
                              column(
                                width = 4,
                                highchartOutput("AGR_energy_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("NEU_energy_chart")
                              ),
                              column(
                                width = 4,
                                highchartOutput("ESU_energy_chart")
                              )
                            ),
                            width = 12)
          ),
          tabItem(tabName = "load_profile",
                  br(),
                  br(),
                  br(),
                  boxPlus(title = "Load profile visualisation",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          closable = FALSE,
                          enable_sidebar = TRUE,
                          sidebar_width = 15,
                          width = 12,
                          sidebar_content = tagList(
                            
                            uiOutput("Info_Load_Profile"),
                            uiOutput("Country_Load_Profile"),
                            uiOutput("Year_Load_Profile"),
                            uiOutput("Usage_Load_Profile"),
                            uiOutput("Temporal_Load_Profile"),
                            uiOutput("Year_Time_Load_Profile")
                            
                          ),
                          br(),
                          fluidRow(column(withSpinner(highchartOutput("Load_Profile_Chart", height = 850)), width = 12))
                          )        
          
          )
          )
 ),
 
 rightsidebar = rightSidebar(
   background = "dark",
   title = "Right Sidebar",
   rightSidebarTabContent(
     id = 1,
     icon = "desktop",
     active = TRUE,
     
     actionButton("go", "Import input data", width = "100%",  class = "btn-primary"),
     br(),
     br(),
     actionButton("Export_Data_Output", "Export output data", width = "100%",  class = "btn-primary"),
     br(),
     selectInput(
       "Type_data",
       "Type of Data:",
       c(
         "Historical Energy Data" = "Hist_Energy",
         "Historical MacroEconomics Data" = "Hist_Macro",
         "Historical Assumption Data" = "Hist_Assumption",
         "Projected MacroEconomics Data" = "Proj_Macro",
         "Projection Target for Assumption Data" = "Proj_Target",
         "Projection Assumptions Data" = "Proj_Assumption"
       )
     ),
     uiOutput("Item"),
     uiOutput("Scenario"),
     uiOutput("Energy"),
     uiOutput("Sector"),
     uiOutput("Sub_Sector"),
     uiOutput("Usage"),
     uiOutput("Country"),
     dateRangeInput(
       "Date",
       "Date Selection",
       start = "2000-01-01",
       end = "2050-12-31",
       min =
         "2000-01-01",
       max = "2050-12-31",
       format = "yyyy",
       startview = "decade",
       separator = "to"
     ),
     uiOutput("Chart_Display")
     )
   ),
 # ),
 
 collapse_sidebar = F
)
# # Run the application 
# shinyApp(ui = ui, server = server)