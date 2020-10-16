##################################################### Loading of all library and sources necessary for the application #################################################

#permet d'enelver la limite de chargement de java (pour gros fichier excel)
options(java.parameters = "- Xmx1024m")

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
source("../Functions/Demand Projection/Load_profile.R")

source("../Functions/Output/General_Output.R")
source("../Functions/Output/Benchmark_Import.R")


############################################################## Define server logic required to execute smart and interactive task ######################################

server <- function(input, output, session) {
  
  # Creation liste dossier INputs Data (Permet de definir le bon chemin de fichier)
  list_sectors <<- c('RES','IND','TER','TRA','AGR','NEU','ESU')
  list_type_folder <<- c(Hist = 'Hist', Proj = 'Proj')
  # names(list_type_folder) <<- c('Hist','Proj')
  list_type_file <<- c(Assumptions='Assumptions',Energy_Demand='Energy_Demand',Macro='Macro',Target='Target')
  #names(list_type_file) <<- c('Assumption','Energy','Macro','Target')
  base <<-'../../Data/Inputs/' ### Chemin access fichier inputs
  
  
  # 
  # Hist_Energy_Demand  <<- readRDS("../../Data/temp/Hist_Energy_Demand.Rds")
  # Hist_Assumption  <<- readRDS("../../Data/temp/Hist_Assumption.Rds")
  # Hist_Macro  <<- readRDS("../../Data/temp/Hist_Macro.Rds")
  # Proj_Assumption  <<- readRDS("../../Data/temp/Proj_Assumption.Rds")rm()
  # Proj_Macro  <<- readRDS("../../Data/temp/Proj_Macro.Rds")
  
  Hist_Energy_Demand  <- reactiveFileReader(1000, session, "../../Data/temp/Hist_Energy_Demand.Rds", readRDS)
  Hist_Assumption  <- reactiveFileReader(1000, session, "../../Data/temp/Hist_Assumption.Rds", readRDS)
  Hist_Macro  <- reactiveFileReader(1000, session, "../../Data/temp/Hist_Macro.Rds", readRDS)
  Proj_Assumption  <- reactiveFileReader(1000, session, "../../Data/temp/Proj_Assumption.Rds", readRDS)
  Proj_Macro  <- reactiveFileReader(1000, session, "../../Data/temp/Proj_Macro.Rds", readRDS)
  Proj_Target  <- reactiveFileReader(1000, session, "../../Data/temp/Proj_Target.Rds", readRDS)
  IND_calibration_subsitution <- reactiveFileReader(1000, session, "../../Data/temp/IND_calibration_subsitution.Rds", readRDS)
  Proj_Energy_Demand  <- reactiveFileReader(1000, session, "../../Data/temp/Proj_Energy_Demand.Rds", readRDS)
  # hourly_load_profile_projection  <- reactiveFileReader(1000, session, "../../Data/temp/hourly_load_profile_projection.Rds", readRDS)
  
  ########################################## Onglet INput Visualisation #################################################
  
  #################### Barre laterale de controle option generale ############################
  
  ### Observateur de click sur Bouton declenchant import of the Input data 
  observeEvent(input$go, {                          
    
    showNotification("Input Import started", type = "message" , duration = NULL)
    import_start <- Sys.time()
    
    Hist_Assumption <- Create_Input_Dataframe('Hist', 'Assumption')
    saveRDS(Hist_Assumption, "../../Data/temp/Hist_Assumption.Rds")
    
    Hist_Energy_Demand <- ktoe_to_TWh(Create_Input_Dataframe('Hist','Energy'))
    Hist_Energy_Demand <- calculate_hist_volume_usage_RES(Hist_Energy_Demand, Hist_Assumption)
    Hist_Energy_Demand <- calculate_hist_volume_usage_TER(Hist_Energy_Demand, Hist_Assumption)
    
    Hist_Macro <- Create_Input_Dataframe('Hist', 'Macro')
    Proj_Macro <- Create_Input_Dataframe('Proj', 'Macro')
    Proj_Assumption <- Create_Input_Dataframe('Proj', 'Assumption')
    saveRDS(Proj_Assumption, "../../Data/temp/Proj_Assumption.Rds")
    
    Hist_Macro <- calculate_macro_data(Hist_Macro)
    
    Proj_Macro <- rbind( apply_growth_rate(Hist_Macro, Proj_Macro) , apply_delta_for_shares(Hist_Macro, Proj_Macro ))
    
    Proj_Macro <- calculate_proj_macro_data(Proj_Macro, Hist_Macro, Proj_Assumption)
    
    Proj_Assumption <- calculate_proj_assumption_data(Proj_Assumption, Proj_Macro)
    saveRDS(Proj_Assumption, "../../Data/temp/Proj_Assumption.Rds")
    
    odysee_shares_fixing(Hist_Assumption, Hist_Energy_Demand)
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.Rds")
    
    main_input_Hist_RES(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.Rds")
    
    main_input_Hist_AGR(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.Rds")
    
    main_input_Hist_TER(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.Rds")
    
    main_input_Hist_TRA(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.Rds")
    
    Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.Rds")
    main_input_Proj_TER(Proj_Macro, Hist_Assumption, Proj_Assumption)
    
    Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.Rds")
    Proj_Target <- readRDS("../../Data/temp/Proj_Target.Rds")
    
    main_input_Proj_RES(Proj_Macro, Hist_Assumption, Proj_Assumption, Proj_Target)
    Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.Rds")
    
    main_input_Proj_AGR(Proj_Macro, Proj_Assumption)
    Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.Rds")
    
    saveRDS(Hist_Macro, "../../Data/temp/Hist_Macro.Rds")
    saveRDS(Hist_Energy_Demand, "../../Data/temp/Hist_Energy_Demand.Rds")
    saveRDS(Hist_Assumption, "../../Data/temp/Hist_Assumption.Rds")
    saveRDS(Proj_Macro, "../../Data/temp/Proj_Macro.Rds")
    saveRDS(Proj_Assumption, "../../Data/temp/Proj_Assumption.Rds")
    
    import_end <- Sys.time()
    
    #Message PRojsuccess Import Inputs Data
    showNotification(paste("Input Import suceeded in", as.character(round( import_end - import_start, 0)), "s"), type = "message" , duration = NULL)
    updateNavlistPanel(session = session, inputId = "Output Visualisation", selected = "Energy")
  })
  
  ### Liste Selection Item
  output$Item <- renderUI({                                           
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Choice_Input_File(input$Type_data, 
                                    Hist_Macro(), 
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand())   
    
    list_item_input <- Create_list_input(Input_File, "ID_Item")           
    
    #On creer ensuite the object liste
    selectInput("Item",
                "Item ID:",
                list_item_input,
                multiple = TRUE,
                selected = list_item_input[1])
  })
  
  ### Liste Selection Scenario
  output$Scenario <- renderUI({                                           
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Choice_Input_File(input$Type_data,
                                    Hist_Macro(),
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    list_scenario_input <- Create_list_input(Input_File, "Scenario")           
    
    #On creer ensuite the object liste
    selectInput("Scenario",
                "Scenario:",
                list_scenario_input,
                multiple = TRUE,
                selected = list_scenario_input[1])
  })
  
  ### Liste Selection Energy 
  output$Energy <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante 
    Input_File <- Choice_Input_File(input$Type_data,
                                    Hist_Macro(),
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    list_energy_input <- Create_list_input(Input_File, "Energy")
    
    #On creer ensuite the object liste
    selectInput("Energy",
                "Energy:",
                list_energy_input,
                multiple = TRUE,
                selected = list_energy_input[1])
  })
  
  ### Liste Selection Sector
  output$Sector <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Choice_Input_File(input$Type_data,
                                    Hist_Macro(),
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    list_sector_input <- Create_list_input(Input_File, "Sector")
    
    #On creer ensuite the object liste
    selectInput("Sector",
                "Sector:",
                list_sector_input,
                multiple = TRUE,
                selected = list_sector_input[1])
  })
  
  ### Liste Selection Sub_Sector
  output$Sub_Sector <- renderUI({                                   
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Choice_Input_File(input$Type_data, 
                                    Hist_Macro(),
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    list_sub_sectors_input <- Create_list_input(Input_File, "Sub_Sector")
    
    #On creer ensuite the object liste
    selectInput(
      "Sub_Sector",
      "Sub-Sector:",
      list_sub_sectors_input,
      multiple = TRUE,
      selected = list_sub_sectors_input[1]
    )
  })
  
  ### Liste Selection Usage
  output$Usage <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Choice_Input_File(input$Type_data, 
                                    Hist_Macro(),
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    list_usage_input <- Create_list_input(Input_File, "Usage")
    
    #On creer ensuite the object liste
    selectInput("Usage",
                "Usage:",
                list_usage_input,
                multiple = TRUE,
                selected = list_usage_input[1])
  })
  
  ### Liste Selection Country
  output$Country <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Choice_Input_File(input$Type_data, 
                                    Hist_Macro(),
                                    Proj_Macro(),
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    list_country_input <- Create_list_input(Input_File, "Country")
    
    #On creer ensuite the object liste
    selectInput("Country",
                "Country:",
                list_country_input,
                multiple = TRUE,
                selected = list_country_input[1])
  })
  
  output$Chart_Display <- renderUI ({
    selectInput(
      "Group_Chart",
      "Group by:",
      c(
        "Country" = "Country",
        "Item" = "ID_Item",
        "Energy" = "Energy",
        "Sector" = "Sector",
        "Sub-Sector" = "Sub_Sector",
        "Usage" = "Usage"
      )
    )
  })
  
  #################### Sous Onglet Table Input Data ############################
  
  #Tableau de rendu Input Data selected
  output$dataset <- renderTable({                                     
    
    #On actualise le type de donnees choisies 
    Input_File <- Choice_Input_File(input$Type_data,
                                    Hist_Macro(),
                                    Proj_Macro(), 
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    #On recupere les annees de debut et fin choisies par the user
    Starting_Year <- as.numeric(strftime(input$Date[1], "%Y"))
    End_Year <- as.numeric(strftime(input$Date[2], "%Y"))
    
    #On extrait la table voulue
    data <- Extract_Specific_Data(
      Input_File,
      input$Item,
      input$Scenario,
      input$Country,
      input$Energy,
      Starting_Year,
      End_Year,
      input$Sector,
      input$Sub_Sector,
      input$Usage)
    
    return(data)
  }, align = 'c', digits = 2, striped = T)
  
  #################### Sous Onglet Analysis and Visualisation of Historical Data ############################ 
  
  output$Trend_Line <- renderPlotly({
    
    #On actualise le type de donnees choisies 
    Input_File <- Choice_Input_File(input$Type_data,
                                    Hist_Macro(),
                                    Proj_Macro(), 
                                    Hist_Assumption(),
                                    Proj_Target(),
                                    Proj_Assumption(),
                                    Hist_Energy_Demand()) 
    
    #On recupere les annees de debut et fin choisies par the user
    Starting_Year <- as.numeric(strftime(input$Date[1], "%Y"))
    End_Year <- as.numeric(strftime(input$Date[2], "%Y"))
    
    #On definit le dataset a afficher en fonction des choix of the tuser
    y_dataset <-
      Extract_Specific_Data(
        Input_File,
        input$Item,
        input$Scenario,
        input$Country,
        input$Energy,
        Starting_Year,
        End_Year,
        input$Sector,
        input$Sub_Sector,
        input$Usage
      )
    
    #On le met a plat pour pouvoir utiliser ggplot (year in one column)
    Flat_df <- Transform_to_Flat_Table(y_dataset)
    
    hover_text <- hover_text(input$Group_Chart, Flat_df)
    Flat_df$text <- paste0(input$Group_Chart,": ", hover_text , "\n", "Year: ", round(Flat_df$Year, 0), "\n", "Value: ", round(Flat_df$Value, 2)) 
    
    # on definit differents parametres du graphique (titre...)  
    graph_title <- as.character(y_dataset[1,"ID_Item"])
    graph_y_axis <- as.character(y_dataset[1,"Unit"])
    
    if(min(Flat_df$Value, na.rm = T) < 0 ) { y_min <- min(Flat_df$Value)}
    else{ y_min <- 0}
    
    #on cree the object graphique voulu (avec trendline et application of a theme)
    graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", group = input$Group_Chart, color = input$Group_Chart, text = "text")) 
    graph <- graph + geom_line() + geom_point() + scale_color_hc() + theme_hc() #+ geom_smooth(se = F, span = input$Trend_Smoothness)
    graph <- graph + theme(plot.title = element_text(face = "bold", size = 17)) + labs(color = "") + ylim(c(y_min, max(Flat_df$Value)))
    
    graph <- ggplotly(graph, tooltip = "text")
  })
  
  ######################################## Sous Onglet Energy Intensity ######################################
  
  output$intensity_chart <- renderPlotly({
    
    #On actualise le type de donnees choisies 
    
    Input_File <- Hist_Energy_Demand()
    
    #On recupere les annees de debut et fin choisies par the user
    Starting_Year <- as.numeric(strftime(input$Date[1], "%Y"))
    End_Year <- as.numeric(strftime(input$Date[2], "%Y"))
    
    #On definit le dataset a afficher en fonction des choix of the user
    energy_dataset <-
      Extract_Specific_Data(
        Input_File,
        "Energy Demand",
        levels(Input_File$Scenario),
        input$Country,
        input$Energy,
        Starting_Year,
        End_Year,
        input$Sector,
        input$Sub_Sector,
        input$Usage
      )
    
    if(input$Sector == "IND")
    {
      VA_IND_dataset <- 
        Extract_Specific_Data(
          Proj_Macro(),
          "Value Added",
          levels(Proj_Macro()$Scenario),
          input$Country,
          "na",
          Starting_Year,
          End_Year,
          input$Sector,
          input$Sub_Sector,
          "na"
        )
      
      VA_IND_dataset<- VA_IND_dataset[rep( seq_len( nrow( VA_IND_dataset ) ), length(input$Energy)) , ] 
      
      y_dataset<- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Industry" #paste(input$Energy[i], "Demand intensity")
      y_dataset$Unit <- "kWh/$10"
      y_dataset[,10:length(y_dataset)] <- 1000 * y_dataset[,10:length(y_dataset)] / VA_IND_dataset[,10:length(VA_IND_dataset)] 
    }
    
    if(input$Sector == "TER")
    {
      VA_TER_dataset <- 
        Extract_Specific_Data(
          Proj_Macro(),
          "Value Added",
          levels(Proj_Macro()$Scenario),
          input$Country,
          "na",
          Starting_Year,
          End_Year,
          input$Sector,
          "total",
          "na"
        )
      
      VA_TER_dataset<- VA_TER_dataset[rep( seq_len( nrow( VA_TER_dataset ) ), length(input$Energy)) , ] 
      
      y_dataset <- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Services"
      y_dataset$Unit <- "kWh/$10"
      y_dataset[,10:length(y_dataset)] <- 1000 * energy_dataset[,10:length(energy_dataset)] / VA_TER_dataset[,10:length(VA_TER_dataset)] 
    }
    
    if(input$Sector== "AGR")
    {
      VA_AGR_dataset <- 
        Extract_Specific_Data(
          Proj_Macro(),
          "Value Added",
          levels(Proj_Macro()$Scenario),
          input$Country,
          "na",
          Starting_Year,
          End_Year,
          input$Sector,
          "total",
          "na"
        )
      
      VA_AGR_dataset<- VA_AGR_dataset[rep( seq_len( nrow( VA_AGR_dataset ) ), length(input$Energy)) , ] 
      
      y_dataset <- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Agriculture"
      y_dataset$Unit <- "kWh/$10"
      y_dataset[,10:length(y_dataset)] <- 1000 * energy_dataset[,10:length(energy_dataset)] / VA_AGR_dataset[,10:length(VA_AGR_dataset)] 
    }
    
    if(input$Sector == "RES")
    {
      No_Household_dataset <- 
        Extract_Specific_Data(
          Proj_Macro(),
          "No_Household",
          levels(Proj_Macro()$Scenario),
          input$Country,
          "na",
          Starting_Year,
          End_Year,
          input$Sector,
          "na",
          "na"
        )
      
      No_Household_dataset<- No_Household_dataset[rep( seq_len( nrow( No_Household_dataset ) ), length(input$Energy)) , ] 
      
      y_dataset <- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Residential"
      y_dataset$Unit <- "kWh/Household"
      y_dataset[,10:length(y_dataset)] <- energy_dataset[,10:length(energy_dataset)] / No_Household_dataset[,10:length(No_Household_dataset)] * 1000000
    }
    
    #On le met a plat pour pouvoir utiliser ggplot (year in one column)
    Flat_df <- Transform_to_Flat_Table(y_dataset)
    hover_text <- hover_text("Energy", Flat_df)
    Flat_df$text <- paste0("Energy: ", Flat_df$Energy , "\n", "Year: ", round(Flat_df$Year, 0), "\n", "Value: ", round(Flat_df$Value, 2))
    
    # on definit differents parametres du graphique (titre...)  
    graph_title <- y_dataset$ID_Item[1]
    graph_y_axis <- as.character(y_dataset[1,"Unit"])
    
    #on cree the object graphique voulu (avec trendline et application of a theme)
    graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", color = "Energy", group = "Energy", text = "text") )
    graph <- graph + geom_line() + scale_color_hc() + theme_hc() 
    graph <- graph + theme(plot.title = element_text(face = "bold", size = 17)) + labs(color = "") + ylim(c(0, max(Flat_df$Value)+0.1))
    graph <- graph + scale_color_manual(values= energy_color(input$Energy))
    
    graph <- ggplotly(graph, tooltip = "text")
  })  
  
  ######################################## Sous Onglet Calibration Road Transport ######################################
  
  ### Graphique permettant de display interpolation results
  output$Country_veh_per_cap_chart <- renderUI({
    selectInput(
      "Country_veh_per_cap_chart",
      "Country Selection:",
      levels(Hist_Energy_Demand()$Country),
      multiple = T, 
      selected = levels(Hist_Energy_Demand()$Country)[1:10]
    )
  })
  
  output$veh_per_cap_calibration_Transport_Chart <- renderPlotly({
    
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.RDS")
    Hist_Macro <- readRDS("../../Data/temp/Hist_Macro.RDS")
    Hist_Energy_Demand <- readRDS("../../Data/temp/Hist_Energy_Demand.RDS")
    
    #Extraction dataset contenant uniquement les cibles de projections pour les hypotheses
    data_transport_calibration <- prepare_data_transport_calibration(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
    data_veh_calibration <- transform_data_veh(data_transport_calibration)
    
    par_calibration_veh <- read.csv2("../../Data/temp/veh_per_cap_calibration_parameters.csv", col.names = c("param", "value"))
    par_calibration_veh$param <- c(rep("gamma", 23), "alpha", rep("beta", 23))
    par_calibration_veh$Country <- c(levels(data_veh_calibration$Country), NA, levels(data_veh_calibration$Country))
    
    
    optimum_curves <- NULL
    alpha <- par_calibration_veh[par_calibration_veh$param == "alpha", "value"]
    
    for(i in 1:nlevels(data_veh_calibration$Country))
    {
      country <- levels(data_veh_calibration$Country)[i]
      gamma <- par_calibration_veh[par_calibration_veh$param == "gamma" & par_calibration_veh$Country == country, 2]
      beta <- par_calibration_veh[par_calibration_veh$param == "beta" & par_calibration_veh$Country == country, 2]
      
      data_country_specific <- data_veh_calibration[data_veh_calibration$Country == country,]
      optimum_curves <- rbind(optimum_curves, data.frame("Country" = rep(country, ceiling(max(data_veh_calibration$GDP_per_cap))*10+1), "GDP_per_cap" = seq(0, ceiling(max(data_veh_calibration$GDP_per_cap)),  0.1), "veh_per_cap" = formula.gompertz(seq(0, ceiling(max(data_veh_calibration$GDP_per_cap)), 0.1), gamma, alpha, beta)))
    }
    
    
    data_veh_calibration_chart <- data_veh_calibration[data_veh_calibration$Country %in% input$Country_veh_per_cap_chart,]
    optimum_curves_chart <- optimum_curves[optimum_curves$Country %in% input$Country_veh_per_cap_chart,]
    
    #on cree the object graphique voulu (avec trendline et application of a theme)
    graph <- ggplot(data = data_veh_calibration_chart, aes(x = GDP_per_cap ,y = veh_per_cap, color = Country))  + scale_color_discrete() + theme_hc() + geom_point() + ylim(c(0, 1)) + geom_line(data = optimum_curves_chart, aes(x = GDP_per_cap ,y = veh_per_cap, color = Country)) 
  })
  
  output$pkm_calibration_Transport_Chart <- renderPlotly({
    
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.RDS")
    Hist_Macro <- readRDS("../../Data/temp/Hist_Macro.RDS")
    Hist_Energy_Demand <- readRDS("../../Data/temp/Hist_Energy_Demand.RDS")
    
    #Extraction dataset contenant uniquement les cibles de projections pour les hypotheses
    data_transport_calibration <- prepare_data_transport_calibration(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
    data_pkm_calibration <- transform_data_pkm(data_transport_calibration)
    
    par_calibration_pkm <- read.csv2("../../Data/temp/pkm_calibration_parameters.csv", col.names = c("param", "value"))
    
    gamma <- par_calibration_pkm[par_calibration_pkm$param == "gamma", 2]
    beta <- par_calibration_pkm[par_calibration_pkm$param == "beta", 2]
    alpha <- par_calibration_pkm[par_calibration_pkm$param == "alpha", 2]
    
    optimum_curve <- data.frame("Country" = rep("Optimal", nrow(data_pkm_calibration)), "GDP_per_cap" = seq(0,max(data_pkm_calibration$GDP_per_cap), max(data_pkm_calibration$GDP_per_cap)/(nrow(data_pkm_calibration)-1)), "pkm_per_cap" = formula_logistic(seq(0,max(data_pkm_calibration$GDP_per_cap), max(data_pkm_calibration$GDP_per_cap)/(nrow(data_pkm_calibration)-1)), gamma, beta, alpha))
    #on cree the object graphique voulu (avec trendline et application of a theme)
    graph <- ggplot(data = data_pkm_calibration, aes(x = GDP_per_cap ,y = pkm_per_cap, color = Country))  + scale_color_discrete() + theme_hc() + geom_point() + geom_line(data = optimum_curve) + ylim(c(0, max(data_pkm_calibration$pkm_per_cap)+0.1))
  })
  
  #Objet Button permettant de declencher le chament de valeur de la cible selected
  output$Calibrate_Road_Transport <- renderUI({
    actionButton(
      "Calibrate_Road_Transport",
      "Launch Calibration of Road Transport",
      # class = "btn-info",
      width = "100%"
    )
  })
  
  #Observateur du click button Change Target declenchant the update of the table with the new value
  observeEvent(input$Calibrate_Road_Transport, {
    
    Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.RDS")
    Hist_Macro <- readRDS("../../Data/temp/Hist_Macro.RDS")
    Proj_Macro <- readRDS("../../Data/temp/Proj_Macro.RDS")
    Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.RDS")
    Hist_Energy_Demand <- readRDS("../../Data/temp/Hist_Energy_Demand.RDS")
    
    showNotification("Road transport calibration launched", type = "message", duration = NULL)
    Calibration_pkm_tkm_stock_TRA(Hist_Macro, Hist_Assumption, Proj_Macro, Proj_Assumption, Hist_Energy_Demand)
    
    #Mesage de success of the operation
    showNotification("Road transport calibrated", type = "message", duration = NULL)
  })
  
  ######################################## Sous Onglet Substitution Calibration Industry  ######################################
  
  output$Date_Range_Calibration_Sub <- renderUI({
    dateRangeInput(
      "Date_Calibration_Sub",
      "Date Selection for Calibration:",
      start = "2006-01-01",
      end = "2016-01-01",
      min = "2000-01-01",
      max = "2040-12-31",
      format = "yyyy",
      startview = "decade",
      separator = "To"
    )
  })
  
  output$Launch_Substitution_Calibration <- renderUI({
    actionButton(
      "Launch_Substitution_Calibration",
      "Calibrate industry",
      class = "btn-primary",
      width = "100%"
      # style = "height:40px"
    )
  })
  
  
  label_sub <- reactiveValues(data = NULL)
  class_sub <- reactiveValues(data = NULL)
  
  observeEvent(input$Launch_Substitution_Calibration, {
    
    Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.RDS")
    Hist_Energy_Demand <- readRDS("../../Data/temp/Hist_Energy_Demand.RDS")
    
    calibration_start_year <- as.numeric(strftime(input$Date_Calibration_Sub[1], "%Y"))
    calibration_end_year <- as.numeric(strftime(input$Date_Calibration_Sub[2], "%Y"))
    
    showModal(modalDialog(
      title = "Calibration and Simulation of Substitution effect",
      pre( id = "console"),
      size = "l",
      easyClose = FALSE,
      footer = uiOutput("sub_log_button")
    ))
    
    withCallingHandlers(
      
      calibration_sub_model_result <- Substitution_Calibration_Model(calibration_start_year, calibration_end_year, Proj_Assumption = Proj_Assumption, Hist_Energy_Demand = Hist_Energy_Demand),
      
      # can use "warning" instead/on top of "message" to catch warnings too 
      message = function(m) {
        shinyjs::html("console", m$message, TRUE)
      }
    )
    
    #???On supprime les anciennes valeurs issue dun precedetn run du modele de subsitution
    saveRDS(Proj_Assumption[!(Proj_Assumption$Sector == "IND" & Proj_Assumption$ID_Item == "Substitution Rate"),], "../../Data/temp/Proj_Assumption.RDS")
    
    #On ajoute les nouvelles
    saveRDS(rbind(Proj_Assumption, 
                  Extract_Item(calibration_sub_model_result, "Substitution Rate")),
            "../../Data/temp/Proj_Assumption.RDS")
    
    saveRDS(calibration_sub_model_result, "../../Data/temp/IND_calibration_subsitution.Rds" )
    label_sub$data <- "Model finished: You can close the window"
    class_sub$data <- "btn-success"
    showNotification("Simulation and Calibration of Substitution model completed", type = "message")
    
  })
  
  output$sub_log_button <-  renderUI({actionButton("close", label_sub$data, class = class_sub$data)})
  
  observeEvent(input$close, {
    label_sub$data <- NULL
    class_sub$data <- NULL
    removeModal() })
  
  ### Liste Selection Sub_Sector_Industry
  output$Sub_Sector_Substitution <- renderUI({                                   
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    list_sub_sectors_subtitution <- levels(IND_calibration_subsitution()$Sub_Sector)
    
    #On creer ensuite the object liste
    selectInput(
      "Sub_Sector_Subtitution",
      "Sub-Sector in Industry:",
      list_sub_sectors_subtitution,
      selected = list_sub_sectors_subtitution[1]
    )
  })
  
  output$Country_Substitution <- renderUI({                                   
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    list_country_subtitution <- levels(IND_calibration_subsitution()$Country)
    
    #On creer ensuite the object liste
    selectInput(
      "Country_Subtitution",
      "Country:",
      list_country_subtitution,
      selected = list_country_subtitution[1]
    )
  })
  
  output$Simulated_Demand <- renderPlotly({
    
    simulated_demand <- Extract_Item(IND_calibration_subsitution(), "Simulated Demand")
    simulated_demand <- Extract_Country( Extract_Sub_Sector(simulated_demand, input$Sub_Sector_Subtitution), input$Country_Subtitution)
    simulated_demand <- Transform_to_Flat_Table(simulated_demand)
    
    
    historical_demand <- Extract_Specific_Energy(Extract_Country( Extract_Sub_Sector(Hist_Energy_Demand(), input$Sub_Sector_Subtitution), input$Country_Subtitution), c("coal", "electricity", "gas", "oil"))
    historical_demand <- Transform_to_Flat_Table(historical_demand)
    
    graph <- ggplot(data = simulated_demand, aes_string(x = "Year", y = "Value", color = "Energy", group = "Energy") )
    graph <- graph + geom_line(linetype = "dashed") + scale_color_hc() + theme_hc() + labs(color = "") + ylim(c(0, max(simulated_demand$Value, historical_demand$Value)))
    graph <- graph + geom_line(data = historical_demand)
  })
  
  output$Substitution_Rate <- renderTable({
    
    IND_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand(), "IND")
    col_deb <- min(which(is.na(colSums(IND_Hist_Energy_Demand[,10:length(IND_Hist_Energy_Demand)])))) + 9
    
    substitution_rate <- Extract_Item(Extract_Sector(IND_calibration_subsitution(), "IND"), "Substitution Rate")
    substitution_rate <- Extract_Country( Extract_Sub_Sector(substitution_rate, input$Sub_Sector_Subtitution), input$Country_Subtitution)
    
    substitution_rate <- cbind(substitution_rate[,1:9], substitution_rate[,col_deb:length(substitution_rate)]) 
  })
  
  
  ######################################## Sous Onglet Starting Points Analysis  ######################################
  
  ###Definition of the button which islaunching the starting point analysis
  output$Launch_Start_Point_Analysis <- renderUI({
    actionButton(
      "Start_Point_Analysis",
      "Analyse Starting Points",
      class = "btn-primary",
      width = "100%",
      style = "height:40px"
    )
  })
  
  ###Observateur du click bounton starting point analysis declanchant tha analysis
  observeEvent(input$Start_Point_Analysis, {
    
    Starting_Points_Table <<- Hist_Last_Year_Analysis(Hist_Energy_Demand(), input$No_Year_Starting_Points)
    
    showNotification("Analysis of Starting Points completed", type = "message")
    return(Starting_Points_Table)
  })
  
  #Objet Input nombre years avant la derniere annee pour faire the analysis (average and standard deviation)
  output$No_Year_Starting_Points <- renderUI ({
    numericInput("No_Year_Starting_Points", "Number of Years:", value = 5)
  })
  
  #Valeur de stratig point incoherent que on rend reactif au click du button starting point analysis 
  Starting_Point_Table_Value <- eventReactive(input$Start_Point_Analysis,{Starting_Points_Table})
  
  #Objet Table permettant de display les starting points incoherents 
  output$Starting_Point_Table <- renderTable({Starting_Point_Table_Value()})
  
  #On calcule la part de starting points incoherent sur le total (en la rendant reactive to the button click)
  Share_Incoherent_Start_Points_Value <- eventReactive(input$Start_Point_Analysis,{
    paste(as.character(round(length(rownames(Starting_Points_Table))/length(rownames(Hist_Energy_Demand()))*100),1), "%", sep = "")
  })
  
  #Objet affichage de la valeur de la part de starting points incoherents 
  output$Share_Incoherent_Start_Points <- renderText({Share_Incoherent_Start_Points_Value()})
  
  output$Table_Odysee_shares_checking <- renderTable({ list_anomalie <- odysee_shares_checking(Hist_Assumption(), Hist_Energy_Demand())})
  
  ################################################################################################################################################################
  ########################################## Onglet Energy Demand Projection #####################################################################################
  ################################################################################################################################################################
  
  data("wrld_simpl")
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)
      )
    m <- m %>% setView(0, 20, zoom = 3)
    
    
    country_available <- levels(Hist_Energy_Demand()$Country)
    color_pal_country <- colorFactor(palette = "viridis", domain = NULL, levels =  levels(Hist_Energy_Demand()$Country), na.color = "transparent")
    
    m <- m%>%addPolygons(data = wrld_simpl,
                         group = "country",
                         layerId = wrld_simpl$ISO2,
                         stroke = FALSE,
                         smoothFactor = 0.3, fillOpacity = 1,
                         fillColor = ~color_pal_country(wrld_simpl$ISO2),
                         label = wrld_simpl$NAME,
                         highlight = highlightOptions(
                           weight = 10,
                           color = "white",
                           fillOpacity = 0.75
                         ))
  })
  
  observeEvent(input$Project_Demand, {
    
    test_calibration_IND <- nrow(Proj_Assumption()[Proj_Assumption()$ID_Item == "Substitution Rate",]) == 0
    
    if(test_calibration_IND)
    {
      showModal(modalDialog(
        title = "Energy demand projection",
        "Please, run the calibration of subtitution rate for the industry sector first",
        size = "m",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    else
    {
      showModal(modalDialog(
        title = "Energy demand projection",
        size = "m",
        tags$p(id = "title_end_use", tags$b("Projection of end-uses countries:")),
        progressBar(id = "projection_end_use_progress_bar" ,value =0, display_pct = TRUE, status = "danger"),
        tags$p(id = "title_sector", tags$b("<b>Projection of sector countries:</b>")),
        progressBar(id = "projection_sector_progress_bar" ,value =0, display_pct = TRUE, status = "danger"),
        tags$p(id = "title_load_profile", tags$b("<b>Projection of load profiles:</b>")),
        progressBar(id = "projection_load_profile_progress_bar" ,value =0, display_pct = TRUE, status = "danger"),
        easyClose = TRUE,
        footer = NULL
      ))
      
      shinyjs::hide("title_end_use")
      shinyjs::hide("title_sector")
      shinyjs::hide("title_load_profile")
      
      if(input$project_end_uses_choice)
      {
        shinyjs::show("title_end_use")
        updateProgressBar(session = session, id = "projection_sector_progress_bar" ,value = 100, status = "disabled")
        updateProgressBar(session = session, id = "projection_load_profile_progress_bar" ,value = 100, status = "disabled")
        
        Hist_Energy_Demand <- readRDS("../../Data/temp/Hist_Energy_Demand.RDS")
        Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.RDS")
        Hist_Assumption <- readRDS("../../Data/temp/Hist_Assumption.RDS")
        Proj_Target <- readRDS("../../Data/temp/Proj_Target.RDS")
        Proj_Macro <- readRDS("../../Data/temp/Proj_Macro.RDS")
        
        Proj_Energy_Demand_End_Uses<- NULL
        Proj_Energy_Demand_End_Uses <- main_projection_energy_demand_IND(IND_calibration_subsitution(), Hist_Energy_Demand, Proj_Assumption)
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 20, status = "primary")
        
        Proj_Energy_Demand_End_Uses <- rbind( Proj_Energy_Demand_End_Uses, main_projection_energy_demand_AGR(Hist_Energy_Demand, Proj_Assumption) )
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 30, status = "primary")
        
        Proj_Energy_Demand_End_Uses <- rbind( Proj_Energy_Demand_End_Uses, main_projection_energy_demand_TRA(Hist_Energy_Demand, Proj_Assumption, Hist_Assumption) )
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 50, status = "primary")
        
        Proj_Energy_Demand_End_Uses <- rbind( Proj_Energy_Demand_End_Uses, main_projection_energy_demand_RES(Hist_Energy_Demand, Proj_Assumption, Proj_Target, Proj_Macro, Hist_Assumption) )
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 60, status = "primary")
        
        Proj_Energy_Demand_End_Uses <- rbind( Proj_Energy_Demand_End_Uses, main_projection_energy_demand_TER(Hist_Energy_Demand, Proj_Assumption, Proj_Macro, Proj_Target) )
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 70, status = "primary")
        
        Proj_Energy_Demand_End_Uses <- rbind(Proj_Energy_Demand_End_Uses, main_projection_energy_demand_NEU(Hist_Energy_Demand, Proj_Assumption, Proj_Energy_Demand_End_Uses))
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 90, status = "primary")
        
        #pour secteur ESU and T&D besoin de connaitre le total demand final 
        Proj_Energy_Demand_End_Uses <- main_output(Proj_Energy_Demand_End_Uses, Hist_Energy_Demand)
        updateProgressBar(session = session, id = "projection_end_use_progress_bar" ,value = 100, status = "success")
      }
      
      if(input$project_load_profile_choice)
      {
        showNotification("Load profile", type = "message", duration = NULL)
        hourly_load_projection()
      }
      
      
      #chargement des differents benchmarks 
      column_type <- c(rep("character", times = 9),rep("numeric", times = 51))
      Benchmark_Data <-read.xlsx2("../../Data/Inputs/Benchmarks Data/Benchmark_Energy_Demand.xlsx", sheetIndex = 1 , colClasses = column_type )
      
      saveRDS(Benchmark_Data, "../../Data/temp/Benchmark_Data.RDS")
      saveRDS(Proj_Energy_Demand_End_Uses, "../../Data/temp/Proj_Energy_Demand.RDS")
      
      showNotification("Projection is finished", type = "message", duration = NULL) 
    }
  })
  
  
  
  ################################################################################################################################################################
  ########################################## Output Visualisation #####################################################################################
  ################################################################################################################################################################  
  
  ######################################## Sous Onglet Overview  ######################################  
  observeEvent(input$Import_Benchmark_data, {
    dir_chemin <- tk_choose.files(default = "X:\\02_0124\\01_CEEME\\01_SEER\\02_ActivityFinite\\PSE_0013_Model_Improvement_Demand_Europe\\04. Project Documents\\AMADEUS\\Data\\Outputs\\", caption = "Choose the benchmark you want to import", multi = FALSE )
    
    if(length(dir_chemin)!=0)
    {
      Benchmark_Data <<- rbind(Benchmark_Data, main_IHS_Data(dir_chemin))
      showNotification("Benchmark data Import suceeded ", type = "message" , duration = NULL)
      js$reset()
    }
  })
  
  observeEvent(input$Export_Data_Output, { 
    showModal(modalDialog(title = "Data to save - Scenario name",
                          textInput("scenario_name_save", "Enter a scenario name to save the data"),
                          progressBar(id = "progress_bar_export_data", value = 0, display_pct = TRUE, status = "danger"),
                          size = "l",
                          easyClose = FALSE,
                          footer = uiOutput("export_data_button")
    )
    )
    shinyjs::hide( "progress_bar_export_data")
  })
  
  output$export_data_button <- renderUI({
    actionButton(
      "export_data_button",
      "Export all scenario data",
      class = "btn-primary",
      width = "30%"
    )
    
  })
  
  observeEvent(input$export_data_button, { 
    
    
    if(!is.error(Proj_Energy_Demand()))
    {
      if(!is.na(input$scenario_name_save))
      {
        # showNotification("Scenario data export started ", type = "message" , duration = NULL)
        # insertUI(
        #   selector = "#scenario_name_save",
        #   where = "afterEnd",
        #   ui = progressBar(id = "progress_bar_export_data", value = 50, display_pct = TRUE, status = "warning"),
        #   immediate = TRUE
        # )
        shinyjs::show( "progress_bar_export_data")
        # progress <- shiny::Progress$new(style = style)
        # progress$set(message = "Computing data", value = 0)
        
        dir_chemin <- paste("../../Data/Outputs/", input$scenario_name_save, "/", format(Sys.time(), "%d-%m-%Y %Hh%M"), "/", sep="")
        dir.create(dir_chemin, recursive = T)
        
        Proj_Energy_Demand_to_save <- readRDS("../../Data/temp/Proj_Energy_Demand.RDS")
        Proj_Macro_to_save <- readRDS("../../Data/temp/Proj_Macro.RDS")
        Proj_Assumption_to_save <- readRDS("../../Data/temp/Proj_Assumption.RDS")
        Hist_Assumption_to_save <- readRDS("../../Data/temp/Hist_Assumption.RDS")
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 10, status = "danger")
        
        Proj_Energy_Demand_to_save$Scenario <- as.factor(input$scenario_name_save)
        Proj_Macro_to_save$Scenario <- input$scenario_name_save
        Proj_Assumption_to_save$Scenario <- input$scenario_name_save
        
        saveRDS(Proj_Energy_Demand_to_save, file = paste0(dir_chemin, "Energy Demand Projection.RData") )
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 20, status = "danger")
        saveRDS(Proj_Macro_to_save, file = paste0(dir_chemin, "Projection Macro Data.RData") )
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 30, status = "danger")
        saveRDS(Proj_Assumption_to_save, file = paste0(dir_chemin, "Projection Assumption Data.RData") )
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 40, status = "warning")
        # #
        write.xlsx2(Proj_Energy_Demand_to_save, file = paste0(dir_chemin, "Energy Demand Projection.xlsx") , row.names = F)
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 65, status = "warning")
        gc()
        write.xlsx2(Proj_Macro_to_save, file = paste0(dir_chemin, "Projection Macro Data.xlsx") , row.names = F)
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 80, status = "success")
        gc()
        write.xlsx2(Proj_Assumption_to_save, file = paste0(dir_chemin, "Projection Assumption Data.xlsx") , row.names = F)
        gc()
        write.xlsx2(Hist_Assumption_to_save, file = paste0(dir_chemin, "Historical Assumption Data.xlsx") , row.names = F)
        
        updateProgressBar(session = session, id = "progress_bar_export_data", value = 100, status = "success")
        
        removeModal() 
        showNotification("Scenario data export suceeded ", type = "message" , duration = NULL)
      }
      else
      {
        removeModal() 
        showNotification("You did not specified a scenario name", type = "message" , duration = NULL)
      }
    }
    else
    {
      removeModal() 
      showNotification("You need to project demand first", type = "message" , duration = NULL)
    }
  } )
  
  ### 1. Final 
  output$Final_energy_chart <- renderHighchart({
    
      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "Final"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Final demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>% 
        hc_add_series(name = "plug-in hybrid", data = Flat_df[Flat_df$Energy == "plug-in hybrid",]$Value, color = as.character(pal["plug-in hybrid"])) %>%
        hc_add_series(name = "self-recharge hybrid", data = Flat_df[Flat_df$Energy == "self-recharge hybrid",]$Value, color = as.character(pal["self-recharge hybrid"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")

  })
  
  output$Final_sector_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Specific_Energy(Extract_Sub_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "total"), "total"), "total")
      input <- droplevels(input[input$Sector !="Final",])
      input <- droplevels(input[input$Sector !="ESU",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_sector <- levels(Flat_df$Sector)
      pal <- viridis(length(list_sector), option = "A")
      pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_sector)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Final demand projection breakdown by sector",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Sector == "RES",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "Agriculture", data = Flat_df[Flat_df$Sector == "AGR",]$Value, color = as.character(pal["AGR"])) %>%
        hc_add_series(name = "Industry", data = Flat_df[Flat_df$Sector == "IND",]$Value, color = as.character(pal["IND"])) %>%
        hc_add_series(name = "Non Energy Uses", data = Flat_df[Flat_df$Sector == "NEU",]$Value, color = as.character(pal["NEU"])) %>%
        hc_add_series(name = "Residential", data = Flat_df[Flat_df$Sector == "RES",]$Value, color = as.character(pal["RES"])) %>%
        hc_add_series(name = "Services", data = Flat_df[Flat_df$Sector == "TER",]$Value, color = as.character(pal["TER"])) %>%
        hc_add_series(name = "Transport", data = Flat_df[Flat_df$Sector == "TRA",]$Value, color = as.character(pal["TRA"])) %>%
        hc_add_series(name = "Unspecified", data = Flat_df[Flat_df$Sector == "Unspecified",]$Value, color = as.character(pal["Unspecified"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
    })
  
  ### 2. RES 
  output$RES_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "RES"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy == "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$RES_usage_chart <- renderHighchart({

      input <- Extract_Specific_Energy(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "RES"), "total"), "total")
      input <- droplevels(input[input$Usage !="total",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_usage <- levels(Flat_df$Usage)
      pal <- viridis(length(list_usage), option = "A")
      pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_usage)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by end-uses",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Usage == "space heating",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "cooking", data = Flat_df[Flat_df$Usage == "cooking",]$Value, color = as.character(pal["space heating"]))  %>%
        hc_add_series(name = "large appliances", data = Flat_df[Flat_df$Usage == "large appliances",]$Value, color = as.character(pal["large appliances"])) %>%
        hc_add_series(name = "lighting", data = Flat_df[Flat_df$Usage == "lighting",]$Value, color = as.character(pal["lighting"])) %>%
        hc_add_series(name = "small appliances", data = Flat_df[Flat_df$Usage == "small appliances",]$Value, color = as.character(pal["small appliances"])) %>%
        hc_add_series(name = "space cooling", data = Flat_df[Flat_df$Usage == "space cooling",]$Value, color = as.character(pal["space cooling"])) %>%
        hc_add_series(name = "space heating", data = Flat_df[Flat_df$Usage == "space heating",]$Value, color = as.character(pal["space heating"])) %>%
        hc_add_series(name = "water heating", data = Flat_df[Flat_df$Usage == "water heating",]$Value, color = as.character(pal["water heating"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
      
  })
  
  ### 3. TER   
  output$TER_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "TER"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$TER_usage_chart <- renderHighchart({

      input <- Extract_Specific_Energy(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "TER"), "total"), "total")
      input <- droplevels(input[input$Usage !="total",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_usage <- levels(Flat_df$Usage)
      pal <- viridis(length(list_usage), option = "A")
      pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_usage)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by end-uses",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Usage == "space heating",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "cooking", data = Flat_df[Flat_df$Usage == "cooking",]$Value, color = as.character(pal["space heating"]))  %>%
        hc_add_series(name = "data centers", data = Flat_df[Flat_df$Usage == "data centers",]$Value, color = as.character(pal["data centers"])) %>%
        hc_add_series(name = "lighting", data = Flat_df[Flat_df$Usage == "lighting",]$Value, color = as.character(pal["lighting"])) %>%
        hc_add_series(name = "space cooling", data = Flat_df[Flat_df$Usage == "space cooling",]$Value, color = as.character(pal["space cooling"])) %>%
        hc_add_series(name = "space heating", data = Flat_df[Flat_df$Usage == "space heating",]$Value, color = as.character(pal["space heating"])) %>%
        hc_add_series(name = "specific uses", data = Flat_df[Flat_df$Usage == "specific uses",]$Value, color = as.character(pal["specific uses"])) %>%
        hc_add_series(name = "water heating", data = Flat_df[Flat_df$Usage == "water heating",]$Value, color = as.character(pal["water heating"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  ### 4. IND     
  output$IND_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "IND"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$IND_sub_sector_chart <- renderHighchart({

      input <- Extract_Specific_Energy(Extract_Usage(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "IND"), "total"), "total")
      input <- droplevels(input[input$Sub_Sector !="total",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_sub_sector <- levels(Flat_df$Sub_Sector)
      pal <- brewer_pal(type = "div", "RdBu")(length(list_sub_sector))
      #  pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_sub_sector)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by industrial branch",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Sub_Sector == "Chemical",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "Chemical", data = Flat_df[Flat_df$Sub_Sector == "Chemical",]$Value, color = as.character(pal["Chemical"])) %>%
        hc_add_series(name = "Food", data = Flat_df[Flat_df$Sub_Sector == "Food",]$Value, color = as.character(pal["Food"])) %>%
        hc_add_series(name = "Iron and Steel", data = Flat_df[Flat_df$Sub_Sector == "Iron and Steel",]$Value, color = as.character(pal["Iron and Steel"])) %>%
        hc_add_series(name = "Machinery and Transport", data = Flat_df[Flat_df$Sub_Sector == "Machinery and Transport",]$Value, color = as.character(pal["Machinery and Transport"])) %>%
        hc_add_series(name = "Mining and Construction", data = Flat_df[Flat_df$Sub_Sector == "Mining and Construction",]$Value, color = as.character(pal["Mining and Construction"])) %>%
        hc_add_series(name = "Non-F Metals", data = Flat_df[Flat_df$Sub_Sector == "Non-F Metals",]$Value, color = as.character(pal["Non-F Metals"])) %>%
        hc_add_series(name = "Non-M Mineral", data = Flat_df[Flat_df$Sub_Sector == "Non-M Mineral",]$Value, color = as.character(pal["Non-M Mineral"])) %>%
        hc_add_series(name = "Other Industry", data = Flat_df[Flat_df$Sub_Sector == "Other Industry",]$Value, color = as.character(pal["Other Industry"])) %>%
        hc_add_series(name = "Paper", data = Flat_df[Flat_df$Sub_Sector == "Paper",]$Value, color = as.character(pal["Paper"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$IND_usage_chart <- renderHighchart({

      input <- Extract_Specific_Energy(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "IND"), "total"), "total")
      input <- droplevels(input[input$Usage !="total",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_usage <- levels(Flat_df$Usage)
      pal <- viridis(length(list_usage), option = "A")
      pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_usage)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by end-uses",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Usage == "space heating",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Usage == "others",]$Value, color = as.character(pal["others"])) %>%
        hc_add_series(name = "process heating", data = Flat_df[Flat_df$Usage == "process heating",]$Value, color = as.character(pal["process heating"])) %>%
        hc_add_series(name = "space cooling", data = Flat_df[Flat_df$Usage == "space cooling",]$Value, color = as.character(pal["space cooling"])) %>%
        hc_add_series(name = "space heating", data = Flat_df[Flat_df$Usage == "space heating",]$Value, color = as.character(pal["space heating"])) %>%
        hc_add_series(name = "water heating", data = Flat_df[Flat_df$Usage == "water heating",]$Value, color = as.character(pal["water heating"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  ### 5. TRA   
  output$TRA_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "TRA"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "plug-in hybrid", data = Flat_df[Flat_df$Energy == "plug-in hybrid",]$Value, color = as.character(pal["plug-in hybrid"])) %>%
        hc_add_series(name = "self-recharge hybrid", data = Flat_df[Flat_df$Energy == "self-recharge hybrid",]$Value, color = as.character(pal["self-recharge hybrid"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$TRA_sub_sector_chart <- renderHighchart({

      input <- Extract_Specific_Energy(Extract_Usage(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "TRA"), "total"), "total")
      input <- droplevels(input[input$Sub_Sector !="total",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_sub_sector <- levels(Flat_df$Sub_Sector)
      pal <- brewer_pal(type = "div", "RdBu")(length(list_sub_sector))
      #  pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_sub_sector)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Demand projection breakdown by industrial branch",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Sub_Sector == "Chemical",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "Domestic Aviation", data = Flat_df[Flat_df$Sub_Sector == "Domestic Aviation",]$Value, color = as.character(pal["Domestic Aviation"])) %>%
        hc_add_series(name = "International Aviation", data = Flat_df[Flat_df$Sub_Sector == "International Aviation",]$Value, color = as.character(pal["International Aviation"])) %>%
        hc_add_series(name = "Domestic Navigation", data = Flat_df[Flat_df$Sub_Sector == "Domestic Navigation",]$Value, color = as.character(pal["Domestic Navigation"])) %>%
        hc_add_series(name = "Rail", data = Flat_df[Flat_df$Sub_Sector == "Rail",]$Value, color = as.character(pal["Rail"])) %>%
        hc_add_series(name = "Road", data = Flat_df[Flat_df$Sub_Sector == "Road",]$Value, color = as.character(pal["Road"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$TRA_usage_chart <- renderHighchart({

      input <- Extract_Specific_Energy(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "TRA"), "Road"), "total")
      input <- droplevels(input[input$Usage !="total",])
      Flat_df <- Transform_to_Flat_Table(input)
      
      list_usage <- levels(Flat_df$Usage)
      pal <- viridis(length(list_usage), option = "A")
      pal <- substr(pal, 0, 7)
      pal <- setNames(pal, list_usage)
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Road demand projection breakdown by vehicles",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Usage == "space heating",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "car", data = Flat_df[Flat_df$Usage == "car",]$Value, color = as.character(pal["car"])) %>%
        hc_add_series(name = "bus", data = Flat_df[Flat_df$Usage == "bus",]$Value, color = as.character(pal["bus"])) %>%
        hc_add_series(name = "heavy truck", data = Flat_df[Flat_df$Usage == "heavy truck",]$Value, color = as.character(pal["heavy truck"])) %>%
        hc_add_series(name = "light truck", data = Flat_df[Flat_df$Usage == "light truck",]$Value, color = as.character(pal["light truck"])) %>%
        hc_add_series(name = "2 wheels", data = Flat_df[Flat_df$Usage == "2 wheels",]$Value, color = as.character(pal["2 wheels"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
    
  })
  
  ### 6. Others   
  output$AGR_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "AGR"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Agriculture demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
    
  })
  
  output$ESU_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "ESU"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Energy Sector Uses demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")
  })
  
  output$NEU_energy_chart <- renderHighchart({

      input <- Extract_Usage(Extract_Sub_Sector(Extract_Sector(Extract_Country(Extract_Item(Proj_Energy_Demand(), "Energy Demand"), input$Country), "NEU"), "total"), "total")
      input <- input[input$Energy !="total",]
      Flat_df <- Transform_to_Flat_Table(input)
      
      pal <- c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold")
      pal <- setNames(pal, c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"))
      
      highchart()%>% 
        hc_chart(type = "area") %>% 
        hc_title(text= "Non Energy Uses demand projection breakdown by energy",
                 style = list(fontSize = 14)
        ) %>%
        hc_xAxis(categories = Flat_df[Flat_df$Energy ==  "electricity",]$Year,
                 tickInterval = 10 
        ) %>%
        hc_yAxis(opposite = T) %>%
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} TWh<br/>",
                   shared = TRUE
        ) %>%
        hc_add_series(name = "biomass and waste", data = Flat_df[Flat_df$Energy == "biomass and waste",]$Value, color = as.character(pal["biomass and waste"]))  %>%
        hc_add_series(name = "coal", data = Flat_df[Flat_df$Energy == "coal",]$Value, color = as.character(pal["coal"])) %>%
        hc_add_series(name = "direct hydrogen", data = Flat_df[Flat_df$Energy == "direct hydrogen",]$Value, color = as.character(pal["direct hydrogen"])) %>%
        hc_add_series(name = "electricity", data = Flat_df[Flat_df$Energy == "electricity",]$Value, color = as.character(pal["electricity"])) %>%
        hc_add_series(name = "final heat", data = Flat_df[Flat_df$Energy == "final heat",]$Value, color = as.character(pal["final heat"])) %>%
        hc_add_series(name = "gas", data = Flat_df[Flat_df$Energy == "gas",]$Value, color = as.character(pal["gas"])) %>%
        hc_add_series(name = "oil", data = Flat_df[Flat_df$Energy == "oil",]$Value, color = as.character(pal["oil"])) %>%
        hc_add_series(name = "others", data = Flat_df[Flat_df$Energy == "others",]$Value, color = as.character(pal["biomass and waste"])) %>%
        hc_plotOptions(area = list(
          stacking = "normal", 
          marker =list(
            enabled = FALSE
          )
        )) %>%
        hc_legend("none")

  })
  
  
  
  ######################################## Sous Onglet Load Profile  ######################################  
  
  output$Country_Load_Profile <- renderUI({
    
    names_files_load_profiles <- dir("../../Data/temp/load_profile")
    list_country <- t(as.data.frame(str_split(names_files_load_profiles, "_"), row.names = NULL))
    list_country <- unique(list_country[,1])
    
    selectInput(
      "country_load_profile",
      "Country:",
      list_country
    )
  })
  
  output$Year_Load_Profile <- renderUI({
    selectInput(
      "year_load_profile",
      "Year:",
      seq(2000,2050,5)
    )
  })
  
  output$Info_Load_Profile <- renderUI({
    selectInput(
      "info_load_profile",
      "Data to display:",
      c("Load profile", "Peak/Offpeak evolution", "Peak composition"), 
      selected = "Load profile"
    )
  })
  
  output$Usage_Load_Profile <- renderUI({
    names_files_load_profiles <- dir("../../Data/temp/load_profile")
    names_files_load_profiles <- readRDS(paste0("../../Data/temp/load_profile/", names_files_load_profiles[1]))
    list_usage <- names(names_files_load_profiles)
    list_usage <- c(list_usage, "total")
    
    selectInput(
      "usage_load_profile",
      "End-uses:",
      list_usage,
      multiple = FALSE,
      selected = "total"
    )
  })
  
  output$Temporal_Load_Profile <- renderUI({
    selectInput(
      "temporal_load_profile",
      "Temporal choice:",
      c("Day", "Week", "Month", "Year"), 
      selected = "Week"
    )
  })
  
  output$Year_Time_Load_Profile <- renderUI({
    selectInput(
      "year_time_load_profile",
      "Time of the year:",
      c("Typical winter", "Typical summer", "Peak", "Offpeak"),
      multiple = FALSE,
      selected = "Peak"
    )
  })
  
  
  df_choice_user <- reactive({
    
    choice_user <- paste0(input$country_load_profile,"_",as.character(input$year_load_profile))
    
    
    df_choice_user <- NULL
    
    files <- paste0("../../Data/temp/load_profile/", choice_user, ".rds")  
    
    df_choice_user <- readRDS(files)
    
    return(df_choice_user)
    
    })
  
  df_choice_user_peak <- reactive({
    
    country_choice_user <- as.character(input$country_load_profile)
  
    df_choice_user_peak <- NULL
    
    file <- paste0("../../Data/temp/peak_evolution/", country_choice_user, "_peak_evolution.rds")  
    
    df_choice_user_peak <- readRDS(file)
    
    return(df_choice_user_peak)
  })
  
  df_choice_user_offpeak <- reactive({
    
    country_choice_user <- paste0(input$country_load_profile)
    
    df_choice_user_offpeak <- NULL
    
    file <- paste0("../../Data/temp/peak_evolution/", country_choice_user, "_offpeak_evolution.rds")  
    
    df_choice_user_offpeak <- readRDS(file)
    
    return(df_choice_user_offpeak)
  })
  
  df_choice_user_peak_composition <- reactive({
    
    country_choice_user <- as.character(input$country_load_profile)
    
    df_choice_user_peak_composition <- NULL
    
    file <- paste0("../../Data/temp/peak_composition/", country_choice_user, "_peak_composition.rds")  
    
    df_choice_user_peak_composition <- readRDS(file)
    
    return(df_choice_user_peak_composition)
  })
  
  observeEvent(input$usage_load_profile, 
               {
                if(input$usage_load_profile != "total")
                {
                  shinyjs::hide("Temporal_Load_Profile")
                  shinyjs::hide("Year_Time_Load_Profile")
                }
                if(input$usage_load_profile == "total")
                {
                   shinyjs::show("Temporal_Load_Profile")
                   shinyjs::show("Year_Time_Load_Profile")
                }
            })
  
  observeEvent(input$info_load_profile, 
               {
                 if(input$info_load_profile == "Peak composition")
                 {
                   shinyjs::hide("Temporal_Load_Profile")
                   shinyjs::hide("Year_Time_Load_Profile")
                   shinyjs::hide("Year_Load_Profile")
                   shinyjs::hide("Usage_Load_Profile")
                   shinyjs::show("Country_Load_Profile")
                 }
                 if(input$info_load_profile == "Load profile")
                 {
                   shinyjs::show("Temporal_Load_Profile")
                   shinyjs::show("Year_Time_Load_Profile")
                   shinyjs::show("Year_Load_Profile")
                   shinyjs::show("Usage_Load_Profile")
                   shinyjs::show("Country_Load_Profile")
                 }
                 
                 if(input$info_load_profile == "Peak/Offpeak evolution")
                 {
                   shinyjs::hide("Temporal_Load_Profile")
                   shinyjs::hide("Year_Time_Load_Profile")
                   shinyjs::hide("Year_Load_Profile")
                   shinyjs::show("Usage_Load_Profile")
                   shinyjs::show("Country_Load_Profile")
                 }
               })
  
  output$Load_Profile_Chart <- renderHighchart({
    
    if(input$info_load_profile == "Peak composition")
    {
      test <- highchart()%>% 
        hc_chart(type = "column",
                 zoomType = "x"
        ) %>% 
        
        hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                   pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                   shared = TRUE
        ) %>%
        
        hc_xAxis(categories = seq(2000, 2050),
                 tickInterval = 5 
        ) %>%
        
        hc_boost(enabled = TRUE,
                 useGPUTranslations = TRUE) %>%
        # hc_yAxis(opposite = T) %>%
        hc_add_series(name = "RES space heating", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_space heating", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "RES water heating", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_water heating", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "RES cooking", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_cooking", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "RES lighting", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_lighting", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "RES space cooling", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_space cooling", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "RES small appliances", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_small appliances", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "RES large appliances", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "RES_large appliances", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "TER space heating", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "TER_space heating", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "TER space cooling", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "TER_space cooling", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "Agriculture", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "AGR_total", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "Industry", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "IND_total", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "Energy Uses", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "ESU_total", 2:length(df_choice_user_peak_composition())] ) ) %>%
        hc_add_series(name = "Transport", data = as.numeric(df_choice_user_peak_composition()[df_choice_user_peak_composition()$Usage == "TRA_total", 2:length(df_choice_user_peak_composition())] ) ) %>%

        hc_legend("none") %>%
        
        hc_plotOptions(column = list(
          stacking = "normal"
          )
      ) 
      
      return(test)
    }
    
    if(input$info_load_profile == "Peak/Offpeak evolution")
    {
      if(input$usage_load_profile == "RES_space heating")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_space heating", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_space heating", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_lighting")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_lighting", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_lighting", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "AGR_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "AGR_total", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "AGR_total", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "IND_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "IND_total", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "IND_total", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "ESU_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "ESU_total", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "ESU_total", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "TRA_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "TRA_total", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "TRA_total", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_cooking")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_cooking", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_cooking", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_large appliances")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_large appliances", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_large appliances", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_small appliances")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_small appliances", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_small appliances", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_space cooling")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_space cooling", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_space cooling", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_water heating")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "RES_water heating", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "RES_water heating", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "TER_space cooling")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "TER_space cooling", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "TER_space cooling", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "TER_space heating")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "TER_space heating", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "TER_space heating", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
      if(input$usage_load_profile == "total")
      {
        test <- highchart()%>% 
          hc_chart(type = "line",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_xAxis(categories = seq(2000, 2050),
                   tickInterval = 5 
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Annual peak", data = as.numeric(df_choice_user_peak()[df_choice_user_peak()$Usage == "total", 2:length(df_choice_user_peak())] ) ) %>%
          hc_add_series(name = "Annual offpeak", data = as.numeric(df_choice_user_offpeak()[ df_choice_user_offpeak()$Usage == "total", 2:length(df_choice_user_offpeak())])) %>%
          
          hc_legend("none") 
        
        return(test)
      }
      
    }
    
    
    if(input$info_load_profile == "Load profile")
    {
      
      if(input$usage_load_profile == "total")
      {
        
        if(input$temporal_load_profile == "Day")
        {
          if(input$year_time_load_profile == "Peak")
          {
            df_choice_user_max_position <-  which(rowSums(df_choice_user()) == max(rowSums(df_choice_user()))) 
            
            min_day <- (df_choice_user_max_position %/% 24) * 24
            max_day <- min_day + 23
            df_choice_user_day <- df_choice_user()[min_day:max_day ,]
          }
          
          if(input$year_time_load_profile == "Offpeak")
          {
            df_choice_user_min_position <-  which(rowSums(df_choice_user()) == min(rowSums(df_choice_user()))) 
            
            min_day <- (df_choice_user_min_position %/% 24) * 24
            max_day <- min_day + 23
            df_choice_user_day <- df_choice_user()[min_day:max_day ,]
          }
          
          if(input$year_time_load_profile == "Typical summer")
          {
            min_day <- 4560
            max_day <- min_day + 23
            df_choice_user_day <- df_choice_user()[min_day:max_day ,]
          }
          
          if(input$year_time_load_profile == "Typical winter")
          {
            min_day <- 31*24
            max_day <- min_day + 23
            df_choice_user_day <- df_choice_user()[min_day:max_day ,]
          }
          
          
          test <- highchart()%>% 
            hc_chart(type = "area",
                     zoomType = "x",
                     animation = FALSE
            ) %>% 
            
            hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                       pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                       shared = TRUE
            ) %>%
            
            hc_boost(enabled = TRUE,
                     useGPUTranslations = TRUE,
                     usePreAllocated= TRUE
            ) %>%
            # hc_yAxis(opposite = T) %>%
            hc_add_series(name = "RES space heating", data = df_choice_user_day[, "RES_space heating"] * 1000000) %>%
            hc_add_series(name = "RES lighting", data = df_choice_user_day[, "RES_lighting"] * 1000000) %>%
            hc_add_series(name = "Agriculture", data = df_choice_user_day[, "AGR_total"] * 1000000) %>%
            hc_add_series(name = "Industry", data = df_choice_user_day[,"IND_total"] * 1000000) %>%
            hc_add_series(name = "Energy Uses", data = df_choice_user_day[, "ESU_total"] * 1000000) %>%
            hc_add_series(name = "Transport", data = df_choice_user_day[, "TRA_total"] * 1000000) %>%
            hc_add_series(name = "RES cooking", data = df_choice_user_day[, "RES_cooking"] * 1000000) %>%
            hc_add_series(name = "RES large appliances", data = df_choice_user_day[, "RES_large appliances"] * 1000000) %>%
            hc_add_series(name = "RES small appliances", data = df_choice_user_day[, "RES_small appliances"] * 1000000) %>%
            hc_add_series(name = "RES space cooling", data = df_choice_user_day[, "RES_space cooling"] * 1000000) %>%
            hc_add_series(name = "RES water heating", data = df_choice_user_day[, "RES_water heating"] * 1000000) %>%
            hc_add_series(name = "TER space cooling", data = df_choice_user_day[, "TER_space cooling"] * 1000000) %>%
            hc_add_series(name = "TER space heating", data = df_choice_user_day[,"TER_space heating"] * 1000000) %>%
            
            hc_legend("none") %>%
            
            # hc_xAxis(events = list(setExtremes = c(0,10000, TRUE, FALSE))) %>%
            
            hc_plotOptions(area = list(
              stacking = "normal", 
              marker =list(
                enabled = FALSE
              ),
              series = list(animation = FALSE)
            )) 
          
          return(test)
        }
        
        if(input$temporal_load_profile == "Week")
        {
          if(input$year_time_load_profile == "Peak")
          {
            df_choice_user_max_position <-  which(rowSums(df_choice_user()) == max(rowSums(df_choice_user()))) 
            
            min_week <- max((df_choice_user_max_position %/% 24) * 24 - 84, 0)
            max_week <- min_week + 168
            df_choice_user_week <- df_choice_user()[min_week:max_week ,]
          }
          
          if(input$year_time_load_profile == "Offpeak")
          {
            df_choice_user_min_position <-  which(rowSums(df_choice_user()) == min(rowSums(df_choice_user()))) 
            
            min_week <- max((df_choice_user_min_position %/% 24) * 24 - 84, 0)
            max_week <- min_week + 168
            df_choice_user_week <- df_choice_user()[min_week:max_week ,]
          }
          
          if(input$year_time_load_profile == "Typical summer")
          {
            min_week <- 4560
            max_week <- min_week + 168
            df_choice_user_week <- df_choice_user()[min_week:max_week ,]
          }
          
          if(input$year_time_load_profile == "Typical winter")
          {
            min_week <- 31*24
            max_week <- min_week + 168
            df_choice_user_week <- df_choice_user()[min_week:max_week ,]
          }
          
          test <- highchart()%>% 
            hc_chart(type = "area",
                     zoomType = "x",
                     animation = FALSE
            ) %>% 
            
            hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                       pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                       shared = TRUE
            ) %>%
            
            hc_boost(enabled = TRUE,
                     useGPUTranslations = TRUE,
                     usePreAllocated= TRUE
            ) %>%
            # hc_yAxis(opposite = T) %>%
            hc_add_series(name = "RES space heating", data = df_choice_user_week[, "RES_space heating"] * 1000000) %>%
            hc_add_series(name = "RES lighting", data = df_choice_user_week[, "RES_lighting"] * 1000000) %>%
            hc_add_series(name = "Agriculture", data = df_choice_user_week[, "AGR_total"] * 1000000) %>%
            hc_add_series(name = "Industry", data = df_choice_user_week[,"IND_total"] * 1000000) %>%
            hc_add_series(name = "Energy Uses", data = df_choice_user_week[, "ESU_total"] * 1000000) %>%
            hc_add_series(name = "Transport", data = df_choice_user_week[, "TRA_total"] * 1000000) %>%
            hc_add_series(name = "RES cooking", data = df_choice_user_week[, "RES_cooking"] * 1000000) %>%
            hc_add_series(name = "RES large appliances", data = df_choice_user_week[, "RES_large appliances"] * 1000000) %>%
            hc_add_series(name = "RES small appliances", data = df_choice_user_week[, "RES_small appliances"] * 1000000) %>%
            hc_add_series(name = "RES space cooling", data = df_choice_user_week[, "RES_space cooling"] * 1000000) %>%
            hc_add_series(name = "RES water heating", data = df_choice_user_week[, "RES_water heating"] * 1000000) %>%
            hc_add_series(name = "TER space cooling", data = df_choice_user_week[, "TER_space cooling"] * 1000000) %>%
            hc_add_series(name = "TER space heating", data = df_choice_user_week[,"TER_space heating"] * 1000000) %>%
            
            hc_legend("none") %>%
            
            # hc_xAxis(events = list(setExtremes = c(0,10000, TRUE, FALSE))) %>%
            
            hc_plotOptions(area = list(
              stacking = "normal", 
              marker =list(
                enabled = FALSE
              ),
              series = list(animation = FALSE)
            )) 
          
          return(test)
        }
        
        if(input$temporal_load_profile == "Month")
        {
          if(input$year_time_load_profile == "Peak")
          {
            df_choice_user_max_position <-  which(rowSums(df_choice_user()) == max(rowSums(df_choice_user()))) 
            
            min_month <- max((df_choice_user_max_position %/% 24) * 24 - 15*24, 0)
            max_month <- min_month + 30*24
            df_choice_user_month <- df_choice_user()[min_month:max_month ,]
          }
          
          if(input$year_time_load_profile == "Offpeak")
          {
            df_choice_user_min_position <-  which(rowSums(df_choice_user()) == min(rowSums(df_choice_user()))) 
            
            min_month <- max((df_choice_user_min_position %/% 24) * 24 - 84, 0)
            max_month <- min_month + 30*24
            df_choice_user_month <- df_choice_user()[min_month:max_month ,]
          }
          
          if(input$year_time_load_profile == "Typical summer")
          {
            min_month <- 4560
            max_month <- min_month + 30*24
            df_choice_user_month <- df_choice_user()[min_month:max_month ,]
          }
          
          if(input$year_time_load_profile == "Typical winter")
          {
            min_month <- 31*24
            max_month <- min_month + 30*24
            df_choice_user_month <- df_choice_user()[min_month:max_month ,]
          }
          
          test <- highchart()%>% 
            hc_chart(type = "area",
                     zoomType = "x",
                     animation = FALSE
            ) %>% 
            
            hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                       pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                       shared = TRUE
            ) %>%
            
            hc_boost(enabled = TRUE,
                     useGPUTranslations = TRUE,
                     usePreAllocated= TRUE
            ) %>%
            # hc_yAxis(opposite = T) %>%
            hc_add_series(name = "RES space heating", data = df_choice_user_month[, "RES_space heating"] * 1000000) %>%
            hc_add_series(name = "RES lighting", data = df_choice_user_month[, "RES_lighting"] * 1000000) %>%
            hc_add_series(name = "Agriculture", data = df_choice_user_month[, "AGR_total"] * 1000000) %>%
            hc_add_series(name = "Industry", data = df_choice_user_month[,"IND_total"] * 1000000) %>%
            hc_add_series(name = "Energy Uses", data = df_choice_user_month[, "ESU_total"] * 1000000) %>%
            hc_add_series(name = "Transport", data = df_choice_user_month[, "TRA_total"] * 1000000) %>%
            hc_add_series(name = "RES cooking", data = df_choice_user_month[, "RES_cooking"] * 1000000) %>%
            hc_add_series(name = "RES large appliances", data = df_choice_user_month[, "RES_large appliances"] * 1000000) %>%
            hc_add_series(name = "RES small appliances", data = df_choice_user_month[, "RES_small appliances"] * 1000000) %>%
            hc_add_series(name = "RES space cooling", data = df_choice_user_month[, "RES_space cooling"] * 1000000) %>%
            hc_add_series(name = "RES water heating", data = df_choice_user_month[, "RES_water heating"] * 1000000) %>%
            hc_add_series(name = "TER space cooling", data = df_choice_user_month[, "TER_space cooling"] * 1000000) %>%
            hc_add_series(name = "TER space heating", data = df_choice_user_month[,"TER_space heating"] * 1000000) %>%
            
            hc_legend("none") %>%
            
            # hc_xAxis(events = list(setExtremes = c(0,10000, TRUE, FALSE))) %>%
            
            hc_plotOptions(area = list(
              stacking = "normal", 
              marker =list(
                enabled = FALSE
              ),
              series = list(animation = FALSE)
            )) 
          
          return(test)
        }
        
        if(input$temporal_load_profile == "Year")
        {
          test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x",
                   animation = FALSE
                   ) %>% 
            
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                       pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                       shared = TRUE
           ) %>%
            
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE,
                   usePreAllocated= TRUE
                   ) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES space heating", data = df_choice_user()[, "RES_space heating"] * 1000000) %>%
          hc_add_series(name = "RES lighting", data = df_choice_user()[, "RES_lighting"] * 1000000) %>%
          hc_add_series(name = "Agriculture", data = df_choice_user()[, "AGR_total"] * 1000000) %>%
          hc_add_series(name = "Industry", data = df_choice_user()[,"IND_total"] * 1000000) %>%
          hc_add_series(name = "Energy Uses", data = df_choice_user()[, "ESU_total"] * 1000000) %>%
          hc_add_series(name = "Transport", data = df_choice_user()[, "TRA_total"] * 1000000) %>%
          hc_add_series(name = "RES cooking", data = df_choice_user()[, "RES_cooking"] * 1000000) %>%
          hc_add_series(name = "RES large appliances", data = df_choice_user()[, "RES_large appliances"] * 1000000) %>%
          hc_add_series(name = "RES small appliances", data = df_choice_user()[, "RES_small appliances"] * 1000000) %>%
          hc_add_series(name = "RES space cooling", data = df_choice_user()[, "RES_space cooling"] * 1000000) %>%
          hc_add_series(name = "RES water heating", data = df_choice_user()[, "RES_water heating"] * 1000000) %>%
          hc_add_series(name = "TER space cooling", data = df_choice_user()[, "TER_space cooling"] * 1000000) %>%
          hc_add_series(name = "TER space heating", data = df_choice_user()[,"TER_space heating"] * 1000000) %>%
    
            hc_legend("none") %>%
            
          # hc_xAxis(events = list(setExtremes = c(0,10000, TRUE, FALSE))) %>%
            
          hc_plotOptions(area = list(
              stacking = "normal", 
              marker =list(
                enabled = FALSE
              ),
              series = list(animation = FALSE)
            )) 
          
          return(test)
        }
        
      }
      
      if(input$usage_load_profile == "RES_space heating")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES space heating", data = df_choice_user()[, "RES_space heating"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_lighting")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES lighting", data = df_choice_user()[, "RES_lighting"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "AGR_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Agriculture", data = df_choice_user()[, "AGR_total"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "IND_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Industry", data = df_choice_user()[,"IND_total"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "ESU_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Energy Uses", data = df_choice_user()[, "ESU_total"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "TRA_total")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "Transport", data = df_choice_user()[, "TRA_total"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_cooking")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES cooking", data = df_choice_user()[, "RES_cooking"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_large appliances")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES large appliances", data = df_choice_user()[, "RES_large appliances"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
  
      if(input$usage_load_profile == "RES_small appliances")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES small appliances", data = df_choice_user()[, "RES_small appliances"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
  
      if(input$usage_load_profile == "RES_space cooling")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES space cooling", data = df_choice_user()[, "RES_space cooling"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "RES_water heating")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "RES water heating", data = df_choice_user()[, "RES_water heating"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
  
      if(input$usage_load_profile == "TER_space cooling")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "TER space cooling", data = df_choice_user()[, "TER_space cooling"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
      
      if(input$usage_load_profile == "TER_space heating")
      {
        test <- highchart()%>% 
          hc_chart(type = "area",
                   zoomType = "x"
          ) %>% 
          
          hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
                     pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: {point.y:,.2f} MWh<br/>",
                     shared = TRUE
          ) %>%
          
          hc_boost(enabled = TRUE,
                   useGPUTranslations = TRUE) %>%
          # hc_yAxis(opposite = T) %>%
          hc_add_series(name = "TER space heating", data = df_choice_user()[,"TER_space heating"] * 1000000) %>%
          
          hc_legend("none") %>%
          hc_plotOptions(area = list(
            stacking = "normal", 
            marker =list(
              enabled = FALSE
            ),
            series = list(animation = FALSE)
          )) 
        
        return(test)
      }
    }
  })
  
  ######################################## Sous Onglet Energy Intensity Data  ######################################
  
  ### Liste Selection Sector
  output$Sector_Proj_intensity <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Proj_Energy_Demand
    list_sector <- Create_list_input(Input_File, "Sector")
    
    #On creer ensuite the object liste
    selectInput("Sector_Proj_intensity",
                "Sector:",
                list_sector
                ,
                multiple = TRUE
                ,
                selected = list_sector[1])
  })
  
  ### Liste Selection Sub_Sector
  output$Sub_Sector_Proj_intensity <- renderUI({                                   
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Proj_Energy_Demand
    list_sub_sectors <- Create_list_input(Input_File, "Sub_Sector")
    
    #On creer ensuite the object liste
    selectInput(
      "Sub_Sector_Proj_intensity",
      "Sub-Sector:",
      list_sub_sectors
      ,
      multiple = TRUE
      ,
      selected = list_sub_sectors[1]
    )
  })
  
  ### Liste Selection Energy
  output$Energy_Proj_intensity <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Proj_Energy_Demand
    list_energy <- Create_list_input(Input_File, "Energy")
    
    #On creer ensuite the object liste
    selectInput("Energy_Proj_intensity",
                "Energy:",
                list_energy
                ,
                multiple = TRUE
                ,
                selected = list_energy[1])
  })
  
  ### Liste Selection Usage
  output$Usage_Proj_intensity <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Proj_Energy_Demand
    list_usage <- Create_list_input(Input_File, "Usage")
    
    #On creer ensuite the object liste
    selectInput("Usage_Proj_intensity",
                "Usage:",
                list_usage
                ,
                multiple = TRUE
                ,
                selected = list_usage[1])
  })
  
  ### Liste Selection Country
  output$Country_Proj_intensity <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- Proj_Energy_Demand
    list_country <- Create_list_input(Input_File, "Country")
    
    #On creer ensuite the object liste
    selectInput("Country_Proj_intensity",
                "Country:",
                list_country
                ,
                multiple = TRUE
                ,
                selected = list_country[1])
  })
  
  ### Graphe ligne de enrgy demand 
  output$proj_intensity_chart <- renderPlotly({
    
    #On actualise le type de donnees choisies 
    Input_File <- Extract_Item( Proj_Energy_Demand, "Energy Demand")
    
    #On recupere les annees de debut et fin choisies par the user
    Starting_Year <- as.numeric(strftime(input$Date_Proj_intensity[1], "%Y"))
    End_Year <- as.numeric(strftime(input$Date_Proj_intensity[2], "%Y"))
    
    #On definit le dataset a afficher en fonction des choix of the user
    energy_dataset <-
      Extract_Specific_Data(
        Input_File,
        "Energy Demand",
        levels(Proj_Energy_Demand$Scenario),
        input$Country_Proj_intensity,
        input$Energy_Proj_intensity,
        Starting_Year,
        End_Year,
        input$Sector_Proj_intensity,
        input$Sub_Sector_Proj_intensity,
        input$Usage_Proj_intensity
      )
    
    if(input$Sector_Proj_intensity == "IND")
    {
      VA_IND_dataset <- 
        Extract_Specific_Data(
          Proj_Macro,
          "Value Added",
          levels(Proj_Macro$Scenario),
          input$Country_Proj_intensity,
          "na",
          Starting_Year,
          End_Year,
          input$Sector_Proj_intensity,
          input$Sub_Sector_Proj_intensity,
          "na"
        )
      
      VA_IND_dataset<- VA_IND_dataset[rep( seq_len( nrow( VA_IND_dataset ) ), length(input$Energy_Proj_intensity)) , ] 
      
      y_dataset<- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Industry" #paste(input$Energy[i], "Demand intensity")
      y_dataset$Unit <- "kWh/$10"
      y_dataset[,10:length(y_dataset)] <- 1000 * y_dataset[,10:length(y_dataset)] / VA_IND_dataset[,10:length(VA_IND_dataset)] 
    }
    
    if(input$Sector_Proj_intensity == "TER")
    {
      VA_TER_dataset <- 
        Extract_Specific_Data(
          Proj_Macro,
          "Value Added",
          levels(Proj_Macro$Scenario),
          input$Country_Proj_intensity,
          "na",
          Starting_Year,
          End_Year,
          input$Sector_Proj_intensity,
          "total",
          "na"
        )
      
      VA_TER_dataset<- VA_TER_dataset[rep( seq_len( nrow( VA_TER_dataset ) ), length(input$Energy_Proj_intensity)) , ] 
      
      y_dataset <- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Services"
      y_dataset$Unit <- "kWh/$10"
      y_dataset[,10:length(y_dataset)] <- 1000 * energy_dataset[,10:length(energy_dataset)] / VA_TER_dataset[,10:length(VA_TER_dataset)] 
    }
    
    if(input$Sector_Proj_intensity == "AGR")
    {
      VA_AGR_dataset <- 
        Extract_Specific_Data(
          Proj_Macro,
          "Value Added",
          levels(Proj_Macro$Scenario),
          input$Country_Proj_intensity,
          "na",
          Starting_Year,
          End_Year,
          input$Sector_Proj_intensity,
          "total",
          "na"
        )
      
      VA_AGR_dataset<- VA_AGR_dataset[rep( seq_len( nrow( VA_AGR_dataset ) ), length(input$Energy_Proj_intensity)) , ] 
      
      y_dataset <- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Agriculture"
      y_dataset$Unit <- "kWh/$10"
      y_dataset[,10:length(y_dataset)] <- 1000 * energy_dataset[,10:length(energy_dataset)] / VA_AGR_dataset[,10:length(VA_AGR_dataset)] 
    }
    
    if(input$Sector_Proj_intensity == "RES")
    {
      No_Household_dataset <- 
        Extract_Specific_Data(
          Proj_Macro,
          "No_Household",
          levels(Proj_Macro$Scenario),
          input$Country_Proj_intensity,
          "na",
          Starting_Year,
          End_Year,
          input$Sector_Proj_intensity,
          "na",
          "na"
        )
      
      No_Household_dataset<- No_Household_dataset[rep( seq_len( nrow( No_Household_dataset ) ), length(input$Energy_Proj_intensity)) , ] 
      
      y_dataset <- energy_dataset
      y_dataset$ID_Item <- "Energy demand intensity in Residential"
      y_dataset$Unit <- "kWh/Household"
      y_dataset[,10:length(y_dataset)] <- energy_dataset[,10:length(energy_dataset)] / No_Household_dataset[,10:length(No_Household_dataset)] * 1000000
    }
    
    #On le met a plat pour pouvoir utiliser ggplot (year in one column)
    Flat_df <- Transform_to_Flat_Table(y_dataset)
    
    hover_text <- hover_text(input$Type_data_Proj, Flat_df)
    Flat_df$text <- paste0("Energy: ", Flat_df$Energy , "\n", "Year: ", round(Flat_df$Year, 0), "\n", "Value: ", round(Flat_df$Value, 2))
    
    # on definit differents parametres du graphique (titre...)  
    graph_title <- y_dataset$ID_Item[1]
    graph_y_axis <- as.character(y_dataset[1,"Unit"])
    
    #on cree the object graphique voulu (avec trendline et application of a theme)
    graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", color = "Energy", group = "Energy", text = "text") )
    graph <- graph + geom_line() + scale_color_hc() + theme_hc() 
    graph <- graph + ylab(graph_y_axis) + ggtitle(graph_title) + theme(plot.title = element_text(face = "bold", size = 17)) + labs(color = "") + ylim(c(0, max(Flat_df$Value)))
    graph <- graph + scale_color_manual(values= energy_color(input$Energy_Proj_intensity))
    
    graph <- ggplotly(graph, tooltip = "text")
  })  
  
  output$Table_Eurostat_checking <- renderTable({ diff <- total_eurostat_checking(input$eurostat_diff_accuracy)})
  
  
  #################### Powerpoint chart generation
  
  ###liste selection Energy
  output$Item_Pres <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante 
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_item <- Create_list_input(Input_File, "ID_Item")
    
    #On creer ensuite the object liste
    selectInput("Item_Pres",
                "Item:",
                list_item
                ,
                multiple = FALSE
                ,
                selected = list_item[3])
  })
  
  output$Scenario_Pres <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante 
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_scenario <- Create_list_input(Input_File, "Scenario")
    
    #On creer ensuite the object liste
    selectInput("Scenario_Pres",
                "Scenario:",
                list_scenario
                ,
                multiple = TRUE
                ,
                selected = list_scenario[1])
  })
  
  ###liste selection Energy
  output$Energy_Pres <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante 
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_energy <- Create_list_input(Input_File, "Energy")
    
    #On creer ensuite the object liste
    selectInput("Energy_Pres",
                "Energy:",
                list_energy
                ,
                multiple = TRUE
                ,
                selected = list_energy[1])
  })
  
  ### Liste Selection Sector
  output$Sector_Pres <- renderUI({                                        
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_sector <- Create_list_input(Input_File, "Sector")
    
    #On creer ensuite the object liste
    selectInput("Sector_Pres",
                "Sector:",
                list_sector
                ,
                multiple = TRUE
                ,
                selected = list_sector[1])
  })
  
  ### Liste Selection Sub_Sector
  output$Sub_Sector_Pres <- renderUI({                                   
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_sub_sectors <- Create_list_input(Input_File, "Sub_Sector")
    
    #On creer ensuite the object liste
    selectInput(
      "Sub_Sector_Pres",
      "Sub-Sector:",
      list_sub_sectors
      ,
      multiple = TRUE
      ,
      selected = list_sub_sectors[1]
    )
  })
  
  ### Liste Selection Usage
  output$Usage_Pres <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_usage <- Create_list_input(Input_File, "Usage")
    
    #On creer ensuite the object liste
    selectInput("Usage_Pres",
                "Usage:",
                list_usage
                ,
                multiple = TRUE
                ,
                selected = list_usage[1])
  })
  
  ### Liste Selection Country
  output$Country_Pres <- renderUI({
    
    #On actualise le type de donnees choisies et on creer la liste correspondante
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    list_country <- Create_list_input(Input_File, "Country")
    
    #On creer ensuite the object liste
    selectInput("Country_Pres",
                "Country:",
                list_country
                ,
                multiple = TRUE
                ,
                selected = list_country[1])
  })
  
  ### Graphe ligne de enrgy demand 
  output$Pres_Chart_object_1 <- renderPlotly({
    
    #On actualise le type de donnees choisies 
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    
    #On recupere les annees de debut et fin choisies par the user
    Starting_Year <- as.numeric(strftime(input$Date_Pres[1], "%Y"))
    End_Year <- as.numeric(strftime(input$Date_Pres[2], "%Y"))
    
    #On definit le dataset a afficher en fonction des choix of the user
    y_dataset <-
      Extract_Specific_Data(
        Input_File,
        input$Item_Pres,
        input$Scenario_Pres,
        input$Country_Pres,
        input$Energy_Pres,
        Starting_Year,
        End_Year,
        input$Sector_Pres,
        input$Sub_Sector_Pres,
        input$Usage_Pres
      )
    
    #permet de faire le graphe sur le bon point de depart
    if(input$Type_data_Pres == "Scenario")
    {
      y_dataset <- Benchmark_chart_starting_point(y_dataset)
    }
    
    #On le met a plat pour pouvoir utiliser ggplot (year in one column)
    Flat_df <- Transform_to_Flat_Table(y_dataset)
    
    hover_text <- hover_text(input$Type_data_Pres, Flat_df)
    Flat_df$text <- paste0(input$Type_data_Pres,": ", hover_text , "\n", "Year: ", round(Flat_df$Year, 0), "\n", "Value: ", round(Flat_df$Value, 2)) 
    
    # on definit differents parametres du graphique (titre...)  
    if(input$no_of_chart == 2)
    {
      graph_title <- paste("Chart 1 on slide", input$edit_slide)
    }
    else 
    {
      graph_title <- paste("Chart on slide", input$edit_slide)
    }
    
    graph_y_axis <- as.character(y_dataset[1,"Unit"])
    
    legend_value <- "right"
    
    if(input$type_chart_pres_1 == "Line chart")
    {
      #on cree the object graphique voulu (avec trendline et application of a theme)
      graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", color = input$Type_data_Pres, group = input$Type_data_Pres, text = "text") )
      graph <- graph + geom_line() + geom_point() + theme_hc() 
      graph <- graph + xlab("") + ylab(graph_y_axis) + ggtitle(graph_title) + theme(plot.title = element_text(face = "bold", size = 17), legend.position = legend_value) + labs(color = "") + ylim(c(0, max(Flat_df$Value)+1))
      
      if(input$Type_data_Pres == "Energy") graph <- graph + scale_color_manual(values= energy_color(input$Energy_Pres[order(input$Energy_Pres)]))
      else if(input$Type_data_Pres == "Sector") graph <- graph + scale_color_manual(values= sector_color(input$Sector_Pres[order(input$Sector_Pres)]))
      else graph <- graph + scale_color_hc()  
    }
    
    if(input$type_chart_pres_1 == "Area chart")
    {
      #on cree the object graphique voulu (avec trendline et application of a theme)
      graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", fill = input$Type_data_Pres))
      graph <- graph + geom_area(position = "stack") + scale_color_hc() + theme_hc() 
      graph <- graph + xlab("") + ylab(graph_y_axis) + ggtitle(graph_title) + theme(plot.title = element_text(face = "bold", size = 17), legend.position = legend_value) 
      
      if(input$Type_data_Pres == "Energy") graph <- graph + scale_fill_manual(values= energy_color(input$Energy_Pres[order(input$Energy_Pres)]))
      else if(input$Type_data_Pres == "Sector") graph <- graph + scale_fill_manual(values= sector_color(as.character(input$Sector_Pres[order(as.character(input$Sector_Pres))])))
      else graph <- graph + scale_color_hc()
    }
    
    graph <- ggplotly(graph, tooltip = "text" )
    
  })
  
  
  ############# CHart 2
  
  ###liste selection Energy
  output$Item_Pres_2 <- renderUI({                                        
    if(input$no_of_chart == 2 )
    {  
      #On actualise le type de donnees choisies et on creer la liste correspondante 
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_item <- Create_list_input(Input_File, "ID_Item")
      
      #On creer ensuite the object liste
      selectInput("Item_Pres_2",
                  "Item:",
                  list_item
                  ,
                  multiple = FALSE
                  ,
                  selected = list_item[3])
    }
  })
  
  output$Scenario_Pres_2 <- renderUI({                                        
    if(input$no_of_chart == 2 )
    {
      #On actualise le type de donnees choisies et on creer la liste correspondante 
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_scenario <- Create_list_input(Input_File, "Scenario")
      
      #On creer ensuite the object liste
      selectInput("Scenario_Pres_2",
                  "Scenario:",
                  list_scenario
                  ,
                  multiple = TRUE
                  ,
                  selected = list_scenario[1])
    }
  })
  
  ###liste selection Energy
  output$Energy_Pres_2 <- renderUI({                                        
    if(input$no_of_chart == 2 )
    {
      #On actualise le type de donnees choisies et on creer la liste correspondante 
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_energy <- Create_list_input(Input_File, "Energy")
      
      #On creer ensuite the object liste
      selectInput("Energy_Pres_2",
                  "Energy:",
                  list_energy
                  ,
                  multiple = TRUE
                  ,
                  selected = list_energy[1])
    }
  })
  
  ### Liste Selection Sector
  output$Sector_Pres_2 <- renderUI({                                        
    if(input$no_of_chart == 2 )
    {
      #On actualise le type de donnees choisies et on creer la liste correspondante
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_sector <- Create_list_input(Input_File, "Sector")
      
      #On creer ensuite the object liste
      selectInput("Sector_Pres_2",
                  "Sector:",
                  list_sector
                  ,
                  multiple = TRUE
                  ,
                  selected = list_sector[1])
    }
  })
  
  ### Liste Selection Sub_Sector
  output$Sub_Sector_Pres_2 <- renderUI({                                   
    if(input$no_of_chart == 2 )
    {
      #On actualise le type de donnees choisies et on creer la liste correspondante
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_sub_sectors <- Create_list_input(Input_File, "Sub_Sector")
      
      #On creer ensuite the object liste
      selectInput(
        "Sub_Sector_Pres_2",
        "Sub-Sector:",
        list_sub_sectors
        ,
        multiple = TRUE
        ,
        selected = list_sub_sectors[1]
      )
    }
  })
  
  ### Liste Selection Usage
  output$Usage_Pres_2 <- renderUI({
    if(input$no_of_chart == 2 )
    {
      #On actualise le type de donnees choisies et on creer la liste correspondante
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_usage <- Create_list_input(Input_File, "Usage")
      
      #On creer ensuite the object liste
      selectInput("Usage_Pres_2",
                  "Usage:",
                  list_usage
                  ,
                  multiple = TRUE
                  ,
                  selected = list_usage[1])
    }
  })
  
  ### Liste Selection Country
  output$Country_Pres_2 <- renderUI({
    if(input$no_of_chart == 2 )
    {
      #On actualise le type de donnees choisies et on creer la liste correspondante
      Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
      list_country <- Create_list_input(Input_File, "Country")
      
      #On creer ensuite the object liste
      selectInput("Country_Pres_2",
                  "Country:",
                  list_country
                  ,
                  multiple = TRUE
                  ,
                  selected = list_country[1])
    }
  })
  
  output$Date_Pres_2 <- renderUI({
    if(input$no_of_chart == 2 )
    {
      dateRangeInput(
        "Date_Pres_2",
        "Date Selection",
        start = "2000-01-01",
        end = "2050-12-31",
        min = "2000-01-01",
        max = "2050-12-31",
        format = "yyyy",
        startview = "decade",
        separator = "To"
      )
    }
  })
  
  output$Type_data_Pres_2 <- renderUI({
    if(input$no_of_chart == 2 )
    {
      selectInput(
        "Type_data_Pres_2",
        "Display by:",
        c(
          "Energy" = "Energy",
          "Country" = "Country",
          "Sector" = "Sector",
          "Sub Sector" = "Sub_Sector",
          "Usage" = "Usage",
          "Scenario" = "Scenario" 
        ))
    }
  })
  
  output$Pres_Chart_object_2 <- renderPlotly({
    
    #On actualise le type de donnees choisies 
    Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
    
    #On recupere les annees de debut et fin choisies par the user
    Starting_Year <- as.numeric(strftime(input$Date_Pres_2[1], "%Y"))
    End_Year <- as.numeric(strftime(input$Date_Pres_2[2], "%Y"))
    
    #On definit le dataset a afficher en fonction des choix of the user
    y_dataset <-
      Extract_Specific_Data(
        Input_File,
        input$Item_Pres_2,
        input$Scenario_Pres_2,
        input$Country_Pres_2,
        input$Energy_Pres_2,
        Starting_Year,
        End_Year,
        input$Sector_Pres_2,
        input$Sub_Sector_Pres_2,
        input$Usage_Pres_2
      )
    
    if(input$Type_data_Pres_2 == "Scenario")
    {
      y_dataset <- Benchmark_chart_starting_point(y_dataset)
    }
    
    #On le met a plat pour pouvoir utiliser ggplot (year in one column)
    Flat_df <- Transform_to_Flat_Table(y_dataset)
    
    hover_text <- hover_text(input$Type_data_Pres_2, Flat_df)
    Flat_df$text <- paste0(input$Type_data_Pres_2,": ", hover_text , "\n", "Year: ", round(Flat_df$Year, 0), "\n", "Value: ", round(Flat_df$Value, 2)) 
    
    # on definit differents parametres du graphique (titre...)  
    graph_title <- paste("Chart 2 on slide", input$edit_slide)
    graph_y_axis <- as.character(y_dataset[1,"Unit"])
    
    if(input$type_chart_pres_2 == "Line chart")
    {
      #on cree the object graphique voulu (avec trendline et application of a theme)
      graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", color = input$Type_data_Pres_2, group = input$Type_data_Pres_2, text = "text") )
      graph <- graph + geom_line() + geom_point() + theme_hc() 
      graph <- graph + xlab("") + ylab(graph_y_axis) + ggtitle(graph_title) + theme(plot.title = element_text(face = "bold", size = 17)) + labs(color = "") + ylim(c(0, max(Flat_df$Value)+1))
      
      if(input$Type_data_Pres_2 == "Energy") graph <- graph + scale_color_manual(values= energy_color(input$Energy_Pres_2))
      if(input$Type_data_Pres_2 == "Sector") graph <- graph + scale_color_manual(values= sector_color(input$Sector_Pres_2))
      else graph <- graph + scale_color_hc()  
    }
    
    if(input$type_chart_pres_2 == "Area chart")
    {
      #on cree the object graphique voulu (avec trendline et application of a theme)
      graph <- ggplot(data = Flat_df, aes_string(x = "Year", y = "Value", fill = input$Type_data_Pres_2))
      graph <- graph + geom_area(position = "stack") + scale_color_hc() + theme_hc() 
      graph <- graph + xlab("") + ylab(graph_y_axis) + ggtitle(graph_title) + theme(plot.title = element_text(face = "bold", size = 17), legend.position = "right") 
      
      if(input$Type_data_Pres_2 == "Energy") graph <- graph + scale_fill_manual(values= energy_color(input$Energy_Pres_2))
      if(input$Type_data_Pres_2 == "Sector") graph <- graph + scale_fill_manual(values= sector_color(input$Sector_Pres_2))
      else graph <- graph + scale_color_hc()
    }
    
    graph <- ggplotly(graph, tooltip = "text" )
    
  })
  
  
  
  
  observeEvent(input$Generate_chart, 
               {
                 whole_pres <- read_pptx(paste0("X:\\02_0124\\01_CEEME\\01_SEER\\02_ActivityFinite\\PSE_0013_Model_Improvement_Demand_Europe\\04. Project Documents\\AMADEUS\\Data\\Powerpoint output\\", input$pres_title ,".pptx"))
                 slide_edited <- on_slide(whole_pres, index = as.numeric(input$edit_slide))
                 
                 #creation du chart a mettre dans powerpoint
                 
                 #On recupere les annees de debut et fin choisies par the user
                 Input_File <- rbind(Proj_Energy_Demand, Benchmark_Data)
                 
                 Starting_Year <- as.numeric(strftime(input$Date_Pres[1], "%Y"))
                 End_Year <- as.numeric(strftime(input$Date_Pres[2], "%Y"))
                 
                 dataset_chart <- Extract_Specific_Data(
                   Input_File,
                   input$Item_Pres,
                   input$Scenario_Pres,
                   input$Country_Pres,
                   input$Energy_Pres,
                   Starting_Year,
                   End_Year,
                   input$Sector_Pres,
                   input$Sub_Sector_Pres,
                   input$Usage_Pres
                 )  
                 
                 if(input$Type_data_Pres == "Scenario")
                 {
                   dataset_chart <- Benchmark_chart_starting_point(dataset_chart)
                 }
                 
                 dataset_chart <- Transform_to_Flat_Table(dataset_chart)
                 
                 mytheme <- mschart_theme(
                   axis_title = fp_text(color = "#848484", font.size = 9, bold = FALSE),
                   axis_text = fp_text(color = "#848484", font.size = 9, bold = FALSE),
                   axis_title_y = fp_text(color = "#848484", font.size = 8, bold = FALSE),
                   main_title = fp_text(color = "#848484", font.size = 11),
                   grid_major_line_x = fp_border(style = "none"),
                   grid_minor_line_y = fp_border(style = "none", width = 0),
                   grid_major_line_y = fp_border(color = "#e3e3e3", width = 3/4, style = "solid"),
                   legend_text = fp_text(color = "#848484", font.size = 9, bold = FALSE),
                   legend_position = "b"
                 )
                 
                 if(input$type_chart_pres_1 == "Line chart")
                 {
                   amadeus_chart <- ms_scatterchart(dataset_chart, x= "Year", y = "Value", group = input$Type_data_Pres) 
                   amadeus_chart <- chart_settings(amadeus_chart, scatterstyle = "line")
                   color_line <- energy_color(input$Energy_Pres)
                   names(color_line) <- input$Energy_Pres[order(input$Energy_Pres)]
                   amadeus_chart <- chart_data_line_width(amadeus_chart, values = 2.25)
                   amadeus_chart <- chart_data_stroke(amadeus_chart, values = color_line)
                 }
                 
                 if(input$type_chart_pres_1 == "Area chart")
                 {
                   amadeus_chart <- as_bar_stack(ms_barchart(dataset_chart, x= "Year", y = "Value", group = input$Type_data_Pres)) 
                   amadeus_chart <- chart_settings(amadeus_chart, gap_width = 0)
                   color_line <- energy_color(input$Energy_Pres)
                   names(color_line) <- input$Energy_Pres[order(input$Energy_Pres)]
                   amadeus_chart <- chart_data_fill(amadeus_chart, values = color_line)
                   amadeus_chart <- chart_data_stroke(amadeus_chart, values = color_line)
                   
                 }
                 
                 amadeus_chart <- chart_labels(amadeus_chart, title = paste(input$Sector_Pres, "energy demand breakdown by",tolower(input$Type_data_Pres), "in", input$Country_Pres),
                                               xlab = "", ylab = "TWh")
                 
                 amadeus_chart  <- set_theme(amadeus_chart , mytheme)
                 amadeus_chart <- chart_ax_x(amadeus_chart, major_tick_mark = "none", minor_tick_mark = "none")
                 amadeus_chart <- chart_ax_y(amadeus_chart, limit_min = 0, major_tick_mark = "none", minor_tick_mark = "none")
                 
                 slide_edited <- ph_with_chart_at(slide_edited, amadeus_chart,left = 0.27952755877, top = 1.62992125818, width = 4.64173227873 * (2/input$no_of_chart), height = 3.52755905152)
                 
                 if(input$no_of_chart == 2)
                 {
                   
                   Starting_Year_2 <- as.numeric(strftime(input$Date_Pres[1], "%Y"))
                   End_Year_2 <- as.numeric(strftime(input$Date_Pres[2], "%Y"))
                   
                   dataset_chart_2 <- Extract_Specific_Data(
                     Input_File,
                     input$Item_Pres_2,
                     input$Scenario_Pres_2,
                     input$Country_Pres_2,
                     input$Energy_Pres_2,
                     Starting_Year_2,
                     End_Year_2,
                     input$Sector_Pres_2,
                     input$Sub_Sector_Pres_2,
                     input$Usage_Pres_2
                   )  
                   
                   if(input$Type_data_Pres_2 == "Scenario")
                   {
                     dataset_chart_2 <- Benchmark_chart_starting_point(dataset_chart_2)
                   }
                   
                   dataset_chart_2 <- Transform_to_Flat_Table(dataset_chart_2)
                   
                   if(input$type_chart_pres_2 == "Line chart")
                   {
                     amadeus_chart_2 <- ms_scatterchart(dataset_chart_2, x= "Year", y = "Value", group = input$Type_data_Pres_2) 
                     amadeus_chart_2 <- chart_settings(amadeus_chart_2, scatterstyle = "line")
                     color_line_2 <- energy_color(input$Energy_Pres_2)
                     names(color_line_2) <- input$Energy_Pres_2[order(input$Energy_Pres_2)]
                     amadeus_chart_2 <- chart_data_line_width(amadeus_chart_2, values = 2.25)
                     amadeus_chart_2 <- chart_data_stroke(amadeus_chart_2, values = color_line_2)
                   }
                   
                   if(input$type_chart_pres_2 == "Area chart")
                   {
                     amadeus_chart_2 <- as_bar_stack(ms_barchart(dataset_chart_2, x= "Year", y = "Value", group = input$Type_data_Pres_2)) 
                     amadeus_chart_2 <- chart_settings(amadeus_chart_2, gap_width = 0)
                     
                     if(input$Type_data_Pres_2 == "Energy")
                     {
                       color_line_2 <- energy_color(input$Energy_Pres_2)
                       names(color_line_2) <- input$Energy_Pres_2[order(input$Energy_Pres_2)]
                     }
                     else if(input$Type_data_Pres_2 == "Sector")
                     {
                       color_line_2 <- sector_color(input$Sector_Pres_2)
                       names(color_line_2) <- input$Sector_Pres_2[order(input$Sector_Pres_2)]
                     }
                     
                     amadeus_chart_2 <- chart_data_fill(amadeus_chart_2, values = color_line_2)
                     amadeus_chart_2 <- chart_data_stroke(amadeus_chart_2, values = color_line_2)
                   }
                   
                   amadeus_chart_2 <- chart_labels(amadeus_chart_2, title = paste(input$Sector_Pres_2, "energy demand breakdown by",tolower(input$Type_data_Pres_2), "in", input$Country_Pres_2),
                                                   xlab = "", ylab = "TWh")
                   
                   amadeus_chart_2  <- set_theme(amadeus_chart_2 , mytheme)
                   amadeus_chart_2 <- chart_ax_x(amadeus_chart_2, major_tick_mark = "none", minor_tick_mark = "none")
                   amadeus_chart_2 <- chart_ax_y(amadeus_chart_2, limit_min = 0, major_tick_mark = "none", minor_tick_mark = "none")
                   
                   
                   slide_edited <- ph_with_chart_at(slide_edited, amadeus_chart_2 ,left = (0.27952755877 + 4.64173227873 * (2/input$no_of_chart)), top = 1.62992125818, width = 4.64173227873 * (2/input$no_of_chart), height = 3.52755905152)
                 }
                 
                 
                 
                 print(whole_pres, paste0("X:\\02_0124\\01_CEEME\\01_SEER\\02_ActivityFinite\\PSE_0013_Model_Improvement_Demand_Europe\\04. Project Documents\\AMADEUS\\Data\\Powerpoint output\\", input$pres_title,".pptx"))
               })
  
  observeEvent(input$Generate_powerpoint, 
               {
                 template_engie <- remove_slide(read_pptx("X:\\02_0124\\01_CEEME\\01_SEER\\02_ActivityFinite\\PSE_0013_Model_Improvement_Demand_Europe\\04. Project Documents\\AMADEUS\\Data\\Powerpoint output\\Template\\template_engie.pptx"), index = 1)
                 
                 for(i in 1:input$no_of_slide)
                 {
                   slide_auto <-  add_slide(template_engie, layout = "Titre et contenu B", master = "ENGIE_Bleu")
                 }
                 print(slide_auto, paste0("X:\\02_0124\\01_CEEME\\01_SEER\\02_ActivityFinite\\PSE_0013_Model_Improvement_Demand_Europe\\04. Project Documents\\AMADEUS\\Data\\Powerpoint output\\", input$pres_title,".pptx"))
               }
  )
  
  
  output$edit_slide <- renderUI({                                          
    selectInput("edit_slide", "Slide you are editing:", 1:input$no_of_slide)
  })
  
  output$type_chart_pres_1 <- renderUI({ 
    
    if(input$no_of_chart == 1 )
    {
      HTML("<center><h4>Chart Data selection</h4></center>")
      selectInput("type_chart_pres_1", "Type of chart:", c("Line chart", "Area chart"))
    }
    else
    {
      HTML("<center><h4>Chart 1 Data selection</h4></center>")
      selectInput("type_chart_pres_1", "Type of chart 1:", c("Line chart", "Area chart"))
    }
  })
  
  output$type_chart_pres_2 <- renderUI({  
    
    if(input$no_of_chart == 2 )
    {
      h1("Chart 2 Data selection")
      selectInput("type_chart_pres_2", "Type of chart 2:", c("Line chart", "Area chart"))
    }
  })
  
  
  dynamic_chart_height <- function(){ paste0(680/input$no_of_chart, "px") }
  
  
  output$Pres_Chart_1 <- renderUI({ 
    
    plotlyOutput("Pres_Chart_object_1", height = dynamic_chart_height(), width = "100%" )
    
  })
  
  output$Pres_Chart_2 <- renderUI({
    if(input$no_of_chart == 2)
    {
      plotlyOutput("Pres_Chart_object_2", height = dynamic_chart_height(), width = "100%" )
    }
  })
  
  
}

