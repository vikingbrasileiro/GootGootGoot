############################################## AMADEUS World Projection of modeled countries  ################################################

source("../Functions/Demand Projection/World_non_modeled_countries.R")

options(java.parameters = "- UseGCOverheadLimit")

main_world_energy_demand_projection <- function()
{
  # Import main assumptions 
  main_import_world_model_inputs()
  
  #Project  sector modelled countries
  Proj_Energy_Demand <- NULL
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_RES())
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_IND())
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_TER())
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_AGR_NEU_TRA_GB2018())
    # Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_AGR())
  # Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_NEU())
  # Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_world_TRA())
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, main_proj_Unspecified())
  
  Final_Proj_Eenergy_Demand_value <- aggregate(Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)], by = Proj_Energy_Demand[c("Country", "Energy")], FUN = sum )
  Final_Proj_Eenergy_Demand_header  <- Proj_Energy_Demand[1:nrow(Final_Proj_Eenergy_Demand_value), 1:9]  
  Final_Proj_Eenergy_Demand_header$Energy <- Final_Proj_Eenergy_Demand_value$Energy
  Final_Proj_Eenergy_Demand_header$Country <- Final_Proj_Eenergy_Demand_value$Country
  Final_Proj_Eenergy_Demand_header$Sector <- "Final"
  Final_Proj_Eenergy_Demand_header$Sub_Sector <- "total"
  Final_Proj_Eenergy_Demand_header$Usage <- "total"
  Final_Proj_Eenergy_Demand <- unique(cbind(Final_Proj_Eenergy_Demand_header, Final_Proj_Eenergy_Demand_value[,-(1:2)]))
  Proj_Energy_Demand <- droplevels( rbind(Proj_Energy_Demand, Final_Proj_Eenergy_Demand) )
  
  Proj_Energy_Demand_end_uses <- readRDS("../../Data/temp/Proj_Energy_Demand.RDS") 
  
  #Projection of rest of the world 
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, main_rest_of_world_projection( Proj_Energy_Demand, Proj_Energy_Demand_end_uses ) )
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_end_uses)
    
  return(Proj_Energy_Demand)

}

import_population_UNPop <- function()
{
  # Slect the UNpop file
  files <- dir("../../Data/Inputs/World Data/Raw data/")
  file_UNPop <- files[str_detect(files, "POP")]
  
  #Loading Historical and Medium variant scenario of United nation
  Historical_Pop_data <- read.xlsx2(paste0("../../Data/Inputs/World Data/Raw data/", file_UNPop), sheetIndex = 1)
  Projection_Pop_data <- read.xlsx2(paste0("../../Data/Inputs/World Data/Raw data/", file_UNPop), sheetIndex = 2)
  
  # Slection of useful data
  first_row_useful <- which(Historical_Pop_data[,1] == "Index")
  last_row_useful <- nrow(Historical_Pop_data)
  Historical_Pop_data <- Historical_Pop_data[first_row_useful:last_row_useful,]
  Projection_Pop_data <- Projection_Pop_data[first_row_useful:last_row_useful,]
  
  country_column <- which(Historical_Pop_data[2,] == "WORLD")
  first_year_column_useful <- which(Historical_Pop_data[1,] == 2000)
  last_year_column_useful <- length(Historical_Pop_data)
  Historical_Pop_data <- Historical_Pop_data[, c(country_column,first_year_column_useful:last_year_column_useful) ]
  
  first_year_column_useful <- which(Projection_Pop_data[1,] == 2016)
  last_year_column_useful <- which(Projection_Pop_data[1,] == 2050)
  Projection_Pop_data <- Projection_Pop_data[, c(country_column,first_year_column_useful:last_year_column_useful)]

  # Giving good colums names
  Final_Pop_data <- cbind(Historical_Pop_data,Projection_Pop_data[,-1])
  names_columns <- c("Country_UNPop", paste0("X", as.character(seq(2000,2050))))
  names(Final_Pop_data) <- names_columns
  Final_Pop_data <- Final_Pop_data[-1,]
  
  #Merging with country ISo_code
  nomenclature_UNPop <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/nomenclature_UNPop.xlsx", sheetIndex = 1)
  Final_Pop_data <- merge(nomenclature_UNPop, Final_Pop_data)
  Final_Pop_data <- Final_Pop_data[,-(1:2)]
  Final_Pop_data <- Final_Pop_data[Final_Pop_data$Country != "",]
  
  # Reshape in AMADESU dtabase
  Final_Pop_data$ID_Item <- "Population"
  Final_Pop_data$Scenario <- "Medium variant"
  Final_Pop_data$Energy <- "total"
  Final_Pop_data$Sector <- "total"
  Final_Pop_data$Sub_Sector <- "total"
  Final_Pop_data$Usage <- "total"
  Final_Pop_data$Source <- "UNPop"
  Final_Pop_data$Unit <- "Thousand"
  
  Final_Pop_data <- cbind(Final_Pop_data$ID_Item,
                          Final_Pop_data$Scenario,
                          Final_Pop_data$Country,
                          Final_Pop_data$Energy,
                          Final_Pop_data$Sector,
                          Final_Pop_data$Sub_Sector,
                          Final_Pop_data$Usage,
                          Final_Pop_data$Source,
                          Final_Pop_data$Unit,
                          Final_Pop_data[,2:52])
  
  names(Final_Pop_data)[1:9] <- c("ID_Item",
                                  "Scenario",
                                  "Country",
                                  "Energy",
                                  "Sector",
                                  "Sub_Sector",
                                  "Usage",
                                  "Source",
                                  "Unit"
                                  )
  
  write.xlsx2(Final_Pop_data, file = "../../Data/Inputs/World Data/Pop_update.xlsx", row.names = FALSE)
  
  return(Final_Pop_data)
  
}

import_GDP_Engie <- function()
{
  # Slect the UNpop file
  files <- dir("../../Data/Inputs/World Data/Raw data/")
  file_GDP <- files[str_detect(files, "GDP.xlsx")]
  
  #Loading Historical and Medium variant scenario of United nation
  Raw_GDP_data <- read.xlsx2(paste0("../../Data/Inputs/World Data/Raw data/", file_GDP), sheetName = "Group Baseline")
  
  # Select useful data
  first_row_useful <- which(Raw_GDP_data[,1] == 2000)
  last_row_useful  <- which(Raw_GDP_data[,1] == 2050)
  Raw_GDP_data <- Raw_GDP_data[c(1,first_row_useful:last_row_useful),] 
  Raw_GDP_data <- as.data.frame(t(Raw_GDP_data), row.names = NULL)
  Raw_GDP_data$Country_ISO3 <- rownames(Raw_GDP_data)
  
  nomenclature_GDP <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/Nomenclature_GDP_ENGIE.xlsx", sheetIndex = 1)
  
  GDP_Data <- merge(Raw_GDP_data, nomenclature_GDP)
  
  # Reshape in AMADESU dtabase
  GDP_Data$ID_Item <- "GDP"
  GDP_Data$Scenario <- "GB"
  GDP_Data$Energy <- "total"
  GDP_Data$Sector <- "total"
  GDP_Data$Sub_Sector <- "total"
  GDP_Data$Usage <- "total"
  GDP_Data$Source <- "ENGIE"
  GDP_Data$Unit <- "M$10"
  
  GDP_Data <- cbind(GDP_Data$ID_Item,
                          GDP_Data$Scenario,
                          GDP_Data$Country,
                          GDP_Data$Energy,
                          GDP_Data$Sector,
                          GDP_Data$Sub_Sector,
                          GDP_Data$Usage,
                          GDP_Data$Source,
                          GDP_Data$Unit,
                          GDP_Data[,3:53])
  
  names(GDP_Data)[1:9] <- c("ID_Item",
                                  "Scenario",
                                  "Country",
                                  "Energy",
                                  "Sector",
                                  "Sub_Sector",
                                  "Usage",
                                  "Source",
                                  "Unit"
                          )
  
  names(GDP_Data)[10:length(GDP_Data)] <- paste0("X", as.character(seq(2000, 2050)))
  GDP_Data[10:length(GDP_Data)] <- apply(GDP_Data[10:length(GDP_Data)], 2, function(x) {as.numeric(as.character(x))})
  # GDP_Data <- cbind(GDP_Data, GDP_Data[,10:19])
  # 
  # names(GDP_Data)[(length(GDP_Data)-9):length(GDP_Data)] <- paste0("X", as.character(seq(2041,2050)))
  # GDP_Data[, (length(GDP_Data)-9):length(GDP_Data)] <- NA
  
  
  # Projection to 2050 based on last 5 years trend
  # 
  # for(j in 6:15)
  # {
  #   GDP_last_five_years <- GDP_Data[,(j+40):(j+44)]
  #   last_years <- seq(1,5)
  #   
  #   coef_trend <- apply(GDP_last_five_years, 1, function(x) { coefficients( lm(as.numeric(x) ~ last_years) ) })
  #   coef_trend <- t(coef_trend)
  #   GDP_Data[,j+45] <-  coef_trend[,1] + 6 * coef_trend[,2]   
  # }
  # 
  
  #Save in the Hist_World_Macro file
  write.xlsx2(GDP_Data, file = "../../Data/Inputs/World Data/GDP_update.xlsx", row.names = FALSE)
  
  return(GDP_Data)
}

sankey_diagramm <- function()
{
  sankey_data <- data.frame(from = c('Primary', 'Primary', 'Energy Sector Uses', 'Energy Sector Uses', "Final", "Final", "Final", "Final", "Final"),
                            to = c('Final', 'Energy Sector Uses', 'Losses', 'Refineries', "Residential", "Industry", "Transport", "Services", "Non Energy Uses"),
                            weight = c(80, 20, 15, 5, 15, 20, 20, 20, 5))
  
  highchart() %>%
    hc_chart(type = 'sankey') %>%
    hc_add_series(data = sankey_data)
  
}

main_import_world_model_inputs <- function()
{
  
  #############
  #Value added
  ############# 
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_World_Macro <- read.xlsx2("../../Data/Inputs/World Data/Hist_World_Macro.xlsx", sheetIndex = 1 , colClasses = column_type)
  Proj_World_Macro <- read.xlsx2("../../Data/Inputs/World Data/Proj_World_Macro.xlsx", sheetIndex = 1 , colClasses = column_type)
  
  
  Hist_shares_VA <- droplevels( Hist_World_Macro[Hist_World_Macro$ID_Item == "Value added shares in GDP",] )
  Hist_shares_VA <- Hist_shares_VA[order(Hist_shares_VA$Country),]
  Hist_shares_VA <- Hist_shares_VA[order(Hist_shares_VA$Sector),]
  country_by_sector <- levels(as.factor(Hist_shares_VA$Country))
  
  
  # Calculation of Hiistorical Value added values 
  GDP <- droplevels( Hist_World_Macro[Hist_World_Macro$ID_Item == "GDP",] )
  GDP_for_target_countries <-  droplevels( GDP[GDP$Country %in% country_by_sector,] )
  GDP_for_target_countries <- GDP_for_target_countries[order(GDP_for_target_countries$Country) ,]
  GDP_for_target_countries_temp <-  GDP_for_target_countries[ rep( seq_len( nrow( GDP_for_target_countries ) ), (nlevels(as.factor(Hist_shares_VA$Sector)) )), ]
  
  Hist_Value_added <- Hist_shares_VA
  Hist_Value_added[, 10:length(Hist_Value_added)] <- GDP_for_target_countries_temp[, 10:length(GDP_for_target_countries_temp)] * Hist_shares_VA[, 10:length(Hist_shares_VA)]
  Hist_Value_added$ID_Item <- "Value added"
  Hist_Value_added$Unit <- "M$10"
  
  #Projectino IND and AGR based on elasticity old CEEME
  VA_Elasticity_to_GDP <- droplevels( Proj_World_Macro[Proj_World_Macro$ID_Item == "Value added elasticity to GDP",] )
  VA_Elasticity_to_GDP <- VA_Elasticity_to_GDP[order(VA_Elasticity_to_GDP$Country) ,]
  VA_Elasticity_to_GDP <- VA_Elasticity_to_GDP[order(VA_Elasticity_to_GDP$Sector) ,]
  list_sector_elasticity_VA <-  levels(as.factor(VA_Elasticity_to_GDP$Sector))
  
  Hist_Value_added_IND_AGR <- Hist_Value_added[Hist_Value_added$Sector %in% list_sector_elasticity_VA ,]
  Proj_Value_added_IND_AGR <- Hist_Value_added_IND_AGR
  
  Year_Proj_col = min(which(is.nan(colSums(Hist_Value_added_IND_AGR[10:length(Hist_Value_added_IND_AGR)])))) + 9
  for(j in Year_Proj_col:length(Proj_Value_added_IND_AGR))
  {
    Proj_Value_added_IND_AGR[, j] <- Proj_Value_added_IND_AGR[, j-1] * (  1 + VA_Elasticity_to_GDP[, j] * (GDP_for_target_countries[, j] / GDP_for_target_countries[, j-1] - 1) )  
  }

  #Projection TER based on inelasticity assumption of tax (old CEEME)
  
  Proj_Value_added_TER <- Hist_Value_added[Hist_Value_added$Sector == "TER",]
  sum_Value_added_IND_AGR <- aggregate(Proj_Value_added_IND_AGR[,10:length(Proj_Value_added_IND_AGR)], 
                                       by = Proj_Value_added_IND_AGR["Country"],
                                       FUN = sum)
  
  Hist_shares_VA_tax <- droplevels( Hist_shares_VA[Hist_shares_VA$Sector == "tax",] )
  Hist_shares_VA_tax <- Hist_shares_VA_tax[order(Hist_shares_VA_tax$Country) ,] 
  Last_Year_col = min(which(is.nan(colSums(Hist_shares_VA_tax[10:length(Hist_shares_VA_tax)])))) + 9 - 1
  Hist_shares_VA_tax[, (Last_Year_col+1):length(Hist_shares_VA_tax)] <- Hist_shares_VA_tax[, Last_Year_col] 
  
  for(j in Year_Proj_col:length(Proj_Value_added_TER))
  {
    Proj_Value_added_TER[, j] <- GDP_for_target_countries[, j] * ( 1 - Hist_shares_VA_tax[, j] ) - sum_Value_added_IND_AGR[, j-8]
  }
  
  Proj_Value_added <- rbind(Proj_Value_added_IND_AGR, Proj_Value_added_TER)
 
  #############
  #Economical efficiency effect
  #############
  Proj_World_Assumption <- read.xlsx2("../../Data/Inputs/World Data/Proj_World_Assumptions.xlsx", sheetIndex = 1 , colClasses = column_type)
  
  TFP <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "TFP" ,] )
  TFP <- TFP[order(TFP$Country) ,]
  TFP <- TFP[order(TFP$Sector) ,]
  
  TFP_IND_TER <- TFP[TFP$Sector %in% c("IND", "TER") ,]
  TFP_RES <- TFP[TFP$Sector == "RES" ,]
  Proj_Value_added_IND_TER <- Proj_Value_added[Proj_Value_added$Sector %in% c("IND", "TER") ,]
  Economical_Efficiency_IND_TER <- TFP_IND_TER 
  
  Population <-  droplevels( Proj_World_Macro[Proj_World_Macro$ID_Item == "Population",] )
  Population <- Population[order(Population$Country) ,]
  Population_for_target_countries <- droplevels( Population[Population$Country %in% country_by_sector,] )
  
  GDP_per_cap  <- GDP_for_target_countries
  GDP_per_cap[, 10:length(GDP_per_cap)] <- GDP_for_target_countries[, 10:length(GDP_for_target_countries)] / Population_for_target_countries[, 10:length(Population_for_target_countries)] * 1000 
  GDP_per_cap$ID_Item <- "GDP per capita"
  GDP_per_cap$Unit <- "$10/cap"
  Economical_Efficiency_RES <- TFP_RES
  
  for(j in 11:length(Economical_Efficiency_IND_TER))
  {
    Economical_Efficiency_IND_TER[, j] <-   - ( TFP_IND_TER[, j] * (Proj_Value_added_IND_TER[, j] / Proj_Value_added_IND_TER[, j-1] - 1 ) )
    Economical_Efficiency_RES[, j] <-  (TFP_RES[, j] * (GDP_per_cap[, j] / GDP_per_cap[, j-1] -1 ) )
  }
  
  Economical_Efficiency <- rbind(Economical_Efficiency_RES, Economical_Efficiency_IND_TER)
  Economical_Efficiency <- Economical_Efficiency[order(Economical_Efficiency$Sector) ,]
  Economical_Efficiency$ID_Item <- "Economical Efficiency Rate"
  
  Hist_Macro_data_to_save <- rbind(Hist_World_Macro, Hist_Value_added)
  Proj_Macro_data_to_save <- rbind(Proj_World_Macro, Proj_Value_added, GDP_per_cap)
  Proj_Assumption_data_to_save <- rbind(Proj_World_Assumption, Economical_Efficiency)
  
  saveRDS(Hist_Macro_data_to_save, "../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  saveRDS(Proj_Macro_data_to_save, "../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  saveRDS(Proj_Assumption_data_to_save, "../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")
  
}

main_proj_world_RES <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(Proj_World_Assumption$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "RES",]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  GDP_per_cap <- droplevels( Proj_World_Macro[Proj_World_Macro$ID_Item == "Population"  & Proj_World_Macro$Country %in% list_country_sector_modeling,])
  GDP_per_cap <- GDP_per_cap[order(GDP_per_cap$Country) ,] 
  Eco_Efficiency <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Economical Efficiency Rate" & Proj_World_Assumption$Sector == "RES",])
  Eco_Efficiency <- Eco_Efficiency[order(Eco_Efficiency$Country) ,]
  Tech_Efficiency <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Technical Efficiency Rate" & Proj_World_Assumption$Sector == "RES",])
  Tech_Efficiency <- Tech_Efficiency[order(Tech_Efficiency$Country) ,]
  Substition_rate <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Substitution Rate" & Proj_World_Assumption$Sector == "RES",])
  Substition_rate <- Substition_rate[order(Substition_rate$Country) ,]
  
  Substition_rate_elec <- droplevels( Substition_rate[Substition_rate$Energy == "electricity",])
  Substition_rate_gas <- droplevels( Substition_rate[Substition_rate$Energy == "gas",])
  
  climatic_correction_gas <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Climatic correction" & Proj_World_Assumption$Sector == "RES",])
  climatic_correction_gas <- climatic_correction_gas[order(climatic_correction_gas$Country) ,]
  
  #Projection of electricity and gas first
  
  Year_Proj_col = min(which(is.nan(colSums(Hist_Energy_Demand[10:length(Hist_Energy_Demand)])))) + 9
  
  Hist_Energy_Demand_elec <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "electricity" ,]
  Hist_Energy_Demand_gas <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "gas" ,]
  Hist_Energy_Demand_total <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total" ,]
  
  Proj_Energy_Demand_elec <- Hist_Energy_Demand_elec
  Proj_Energy_Demand_gas <- Hist_Energy_Demand_gas
  Proj_Energy_Demand_total <- Hist_Energy_Demand_total
  
  for(i in Year_Proj_col:length(Hist_Energy_Demand))
  {
    Proj_Energy_Demand_elec[, i] <- Proj_Energy_Demand_elec[, i-1] * (1 + (GDP_per_cap[, i] / GDP_per_cap[, i-1] - 1) + Substition_rate_elec[, i] + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
    Proj_Energy_Demand_gas[, i] <- Proj_Energy_Demand_gas[, i-1] * ( 1 - climatic_correction_gas[, i] ) * (1 + (GDP_per_cap[, i] / GDP_per_cap[, i-1] - 1) + Substition_rate_gas[, i] + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
    Proj_Energy_Demand_total[, i] <- Proj_Energy_Demand_total[, i-1] * (1 + (GDP_per_cap[, i] / GDP_per_cap[, i-1] - 1) + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
  }
  
  # Projection of others energies
  #####
  
  ## Calculation of historical market shares
  
  Hist_market_shares_Energy_Demand <- Hist_Energy_Demand 
  Hist_Energy_Demand_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total",] )
  
  for(i in 1:nrow(Hist_market_shares_Energy_Demand))
  {
    country_selected <- as.character(Hist_Energy_Demand$Country[i]) 
    Hist_market_shares_Energy_Demand[i,10:length(Hist_market_shares_Energy_Demand)] <- Hist_Energy_Demand[i,10:length(Hist_Energy_Demand)] / Hist_Energy_Demand_total[Hist_Energy_Demand_total$Country == country_selected, 10:length(Hist_Energy_Demand)] 
  }

  Hist_market_shares_Energy_Demand$ID_Item <- "Energy market shares"
  Hist_market_shares_Energy_Demand$Unit <- "%"
  
  ## Calculation of projected market shares for electricity and gas
  
  Proj_Energy_Demand_elec_gas <- rbind(Proj_Energy_Demand_elec, Proj_Energy_Demand_gas)
  Proj_market_shares_elec_gas <- Proj_Energy_Demand_elec_gas
  
  for(i in 1:nrow(Proj_market_shares_elec_gas))
  {
    country_selected <- as.character(Proj_Energy_Demand_elec_gas$Country[i]) 
    Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] <- Proj_Energy_Demand_elec_gas[i,10:length(Proj_Energy_Demand_elec_gas)] / Proj_Energy_Demand_total[Proj_Energy_Demand_total$Country == country_selected, 10:length(Proj_Energy_Demand_total)] 
   
    col_above_one <- which( Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] > 1) + 9
    if(length(col_above_one) > 0 )
    {
      Proj_market_shares_elec_gas[i,col_above_one] <- 1 
    }
  }
  
  Proj_market_shares_elec_gas$ID_Item <- "Energy market shares"
  Proj_market_shares_elec_gas$Unit <- "%"
  
  ## application of evolution of others energy except the one that it is use to loop
  Proj_diff_market_shares_last_hist_year <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Market shares difference with last historical year" &
                                                                              Proj_World_Assumption$Sector == "RES" 
                                                                                ,] )
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Country) ,]
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Energy) ,]
  
  Proj_market_shares_Energy_Demand <- Hist_market_shares_Energy_Demand
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "electricity" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "electricity" ,]
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "gas" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "gas" ,]
  
  Year_Proj_MS_col <- min(which(is.nan(colSums(Hist_market_shares_Energy_Demand[10:length(Hist_market_shares_Energy_Demand)])))) + 9  
  
  for(i in 1:nrow(Proj_diff_market_shares_last_hist_year))
  {
    country_selected <- as.character( Proj_diff_market_shares_last_hist_year[i, "Country"] )
    energy_selected <-  as.character( Proj_diff_market_shares_last_hist_year[i, "Energy"] )
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, Year_Proj_MS_col-1] - Proj_diff_market_shares_last_hist_year[i, j]
    
      ##Check if some market shares are below 0 or above 1
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] < 0)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j]  <- 0
      }
      
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- 1
      }
    }
  }
  
  
  #Reallocation of delta pdm where sum is above 1
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  sum_MS_others <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy %in% c("biomass and waste", "coal", "final heat", "oil") ,]
  sum_MS_others <- aggregate(sum_MS_others[,10:length(sum_MS_others)], by = sum_MS_others["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  max_sum_MS <- apply(sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)], 1, FUN = function(x) { max(x, na.rm = TRUE ) } ) 
  sum_MS_not_good <- sum_MS[which(max_sum_MS > 1) ,]
  sum_MS_others_not_good <- sum_MS_others[which(max_sum_MS > 1) ,]
  
  for (i in 1:nrow(sum_MS_not_good))
  {
    country_selected <- as.character( sum_MS_not_good[i, "Country"] )
    energy_selected <- c("biomass and waste", "coal", "final heat", "oil")
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      if( sum( Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected, j], na.rm = TRUE ) > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] + ( ( 1 - sum_MS_not_good[i, j-8] ) *  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] / sum_MS_others_not_good[i, j-8] )
      }
    }
  }
  
  # Loop with last energy not defined
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  row_missing <- which(is.na(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy!= "total",Year_Proj_MS_col]))
  MS_missing <- droplevels( Proj_market_shares_Energy_Demand[row_missing,] )
  MS_missing <- MS_missing[order( as.character( MS_missing$Country) ) ,]
  MS_missing[, Year_Proj_MS_col:length(MS_missing)] <- 1 - sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)]
  
  for(i in 1:length(row_missing))
  {
    country_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Country"] )
    energy_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Energy"] )
    Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected ,] <- MS_missing[MS_missing$Country == country_selected & MS_missing$Energy == energy_selected ,]
  }
  
  #####
  
  Proj_Energy_Demand <- Proj_Energy_Demand_total[ rep( seq_len( nrow( Proj_Energy_Demand_total ) ), nlevels(as.factor(Proj_market_shares_Energy_Demand$Energy)) ), ]
  Proj_Energy_Demand$Energy <- Proj_market_shares_Energy_Demand$Energy
  Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] <- Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] * Proj_market_shares_Energy_Demand[, 10:length(Proj_market_shares_Energy_Demand)]
  Proj_Energy_Demand <- Proj_Energy_Demand[Proj_Energy_Demand$Energy != "total" ,]
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_total)
  
  return(Proj_Energy_Demand)
}

main_proj_world_IND <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(Proj_World_Assumption$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "IND",]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Value_added_IND <- droplevels( Proj_World_Macro[Proj_World_Macro$ID_Item == "Value added"  & Proj_World_Macro$Sector == "IND",])
  Eco_Efficiency <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Economical Efficiency Rate" & Proj_World_Assumption$Sector == "IND",])
  Eco_Efficiency <- Eco_Efficiency[order(Eco_Efficiency$Country) ,]
  Tech_Efficiency <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Technical Efficiency Rate" & Proj_World_Assumption$Sector == "IND",])
  Tech_Efficiency <- Tech_Efficiency[order(Tech_Efficiency$Country) ,]
  Substition_rate <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Substitution Rate" & Proj_World_Assumption$Sector == "IND",])
  Substition_rate <- Substition_rate[order(Substition_rate$Country) ,]
  
  Substition_rate_elec <- droplevels( Substition_rate[Substition_rate$Energy == "electricity",])
  Substition_rate_gas <- droplevels( Substition_rate[Substition_rate$Energy == "gas",])
  
  climatic_correction_gas <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Climatic correction" & Proj_World_Assumption$Sector == "IND",])
  climatic_correction_gas <- climatic_correction_gas[order(climatic_correction_gas$Country) ,]
  
  #Projection of electricity, gas and total first
  
  Year_Proj_col = min(which(is.nan(colSums(Hist_Energy_Demand[10:length(Hist_Energy_Demand)])))) + 9
  
  Hist_Energy_Demand_elec <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "electricity" ,]
  Hist_Energy_Demand_gas <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "gas" ,]
  Hist_Energy_Demand_total <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total" ,]
  
  Proj_Energy_Demand_elec <- Hist_Energy_Demand_elec
  Proj_Energy_Demand_gas <- Hist_Energy_Demand_gas
  Proj_Energy_Demand_total <- Hist_Energy_Demand_total
  
  for(i in Year_Proj_col:length(Hist_Energy_Demand))
  {
    Proj_Energy_Demand_elec[, i] <- Proj_Energy_Demand_elec[, i-1] * (1 + (Value_added_IND[, i] / Value_added_IND[, i-1] - 1) + Substition_rate_elec[, i] + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
    Proj_Energy_Demand_gas[, i] <- Proj_Energy_Demand_gas[, i-1] * (1 - climatic_correction_gas[, i] ) * (1 + (Value_added_IND[, i] / Value_added_IND[, i-1] - 1) + Substition_rate_gas[, i] + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
    Proj_Energy_Demand_total[, i] <- Proj_Energy_Demand_total[, i-1] * (1 + (Value_added_IND[, i] / Value_added_IND[, i-1] - 1)  + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
  }
  
  # Projection of others energies
  #####
  
  ## Calculation of historical market shares
  
  Hist_market_shares_Energy_Demand <- Hist_Energy_Demand 
  Hist_Energy_Demand_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total",] )
  
  for(i in 1:nrow(Hist_market_shares_Energy_Demand))
  {
    country_selected <- as.character(Hist_Energy_Demand$Country[i]) 
    Hist_market_shares_Energy_Demand[i,10:length(Hist_market_shares_Energy_Demand)] <- Hist_Energy_Demand[i,10:length(Hist_Energy_Demand)] / Hist_Energy_Demand_total[Hist_Energy_Demand_total$Country == country_selected, 10:length(Hist_Energy_Demand)] 
  }
  
  Hist_market_shares_Energy_Demand$ID_Item <- "Energy market shares"
  Hist_market_shares_Energy_Demand$Unit <- "%"
  
  ## Calculation of projected market shares for electricity and gas
  
  Proj_Energy_Demand_elec_gas <- rbind(Proj_Energy_Demand_elec, Proj_Energy_Demand_gas)
  Proj_market_shares_elec_gas <- Proj_Energy_Demand_elec_gas
  
  for(i in 1:nrow(Proj_market_shares_elec_gas))
  {
    country_selected <- as.character(Proj_Energy_Demand_elec_gas$Country[i]) 
    Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] <- Proj_Energy_Demand_elec_gas[i,10:length(Proj_Energy_Demand_elec_gas)] / Proj_Energy_Demand_total[Proj_Energy_Demand_total$Country == country_selected, 10:length(Proj_Energy_Demand_total)] 
    
    col_above_one <- which( Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] > 1) + 9
    if(length(col_above_one) > 0 )
    {
      Proj_market_shares_elec_gas[i,col_above_one] <- 1 
    }
  }
  
  Proj_market_shares_elec_gas$ID_Item <- "Energy market shares"
  Proj_market_shares_elec_gas$Unit <- "%"
  
  ## application of evolution of others energy except the one that it is use to loop
  Proj_diff_market_shares_last_hist_year <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Market shares difference with last historical year" &
                                                                                Proj_World_Assumption$Sector == "IND" 
                                                                              ,] )
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Country) ,]
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Energy) ,]
  
  Proj_market_shares_Energy_Demand <- Hist_market_shares_Energy_Demand
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "electricity" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "electricity" ,]
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "gas" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "gas" ,]
  
  Year_Proj_MS_col <- min(which(is.nan(colSums(Hist_market_shares_Energy_Demand[10:length(Hist_market_shares_Energy_Demand)])))) + 9  
  
  for(i in 1:nrow(Proj_diff_market_shares_last_hist_year))
  {
    country_selected <- as.character( Proj_diff_market_shares_last_hist_year[i, "Country"] )
    energy_selected <-  as.character( Proj_diff_market_shares_last_hist_year[i, "Energy"] )
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, Year_Proj_MS_col-1] - Proj_diff_market_shares_last_hist_year[i, j]
      
      ##Check if some market shares are below 0 or above 1
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] < 0)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j]  <- 0
      }
      
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- 1
      }
    }
  }
  
  
  #Reallocation of delta pdm where sum is above 1
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  sum_MS_others <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy %in% c("biomass and waste", "coal", "final heat", "oil") ,]
  sum_MS_others <- aggregate(sum_MS_others[,10:length(sum_MS_others)], by = sum_MS_others["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  max_sum_MS <- apply(sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)], 1, FUN = function(x) { max(x, na.rm = TRUE ) } ) 
  sum_MS_not_good <- sum_MS[which(max_sum_MS > 1) ,]
  sum_MS_others_not_good <- sum_MS_others[which(max_sum_MS > 1) ,]
  
  for (i in 1:nrow(sum_MS_not_good))
  {
    country_selected <- as.character( sum_MS_not_good[i, "Country"] )
    energy_selected <- c("biomass and waste", "coal", "final heat", "oil")
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      if( sum( Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected, j], na.rm = TRUE ) > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] + ( ( 1 - sum_MS_not_good[i, j-8] ) *  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] / sum_MS_others_not_good[i, j-8] )
      }
    }
  }
  
  # Loop with last energy not defined
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  row_missing <- which(is.na(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy!= "total",Year_Proj_MS_col]))
  MS_missing <- droplevels( Proj_market_shares_Energy_Demand[row_missing,] )
  MS_missing <- MS_missing[order( as.character( MS_missing$Country) ) ,]
  MS_missing[, Year_Proj_MS_col:length(MS_missing)] <- 1 - sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)]
  
  for(i in 1:length(row_missing))
  {
    country_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Country"] )
    energy_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Energy"] )
    Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected ,] <- MS_missing[MS_missing$Country == country_selected & MS_missing$Energy == energy_selected ,]
  }
  
  #####
  
  Proj_Energy_Demand <- Proj_Energy_Demand_total[ rep( seq_len( nrow( Proj_Energy_Demand_total ) ), nlevels(as.factor(Proj_market_shares_Energy_Demand$Energy)) ), ]
  Proj_Energy_Demand$Energy <- Proj_market_shares_Energy_Demand$Energy
  Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] <- Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] * Proj_market_shares_Energy_Demand[, 10:length(Proj_market_shares_Energy_Demand)]
  Proj_Energy_Demand <- Proj_Energy_Demand[Proj_Energy_Demand$Energy != "total" ,]
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_total)
  
  return(Proj_Energy_Demand)
}

main_proj_world_TER <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(Proj_World_Assumption$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "TER",]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Value_added_TER <- droplevels( Proj_World_Macro[Proj_World_Macro$ID_Item == "Value added"  & Proj_World_Macro$Sector == "TER",])
  Eco_Efficiency <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Economical Efficiency Rate" & Proj_World_Assumption$Sector == "TER",])
  Eco_Efficiency <- Eco_Efficiency[order(Eco_Efficiency$Country) ,]
  Tech_Efficiency <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Technical Efficiency Rate" & Proj_World_Assumption$Sector == "TER",])
  Tech_Efficiency <- Tech_Efficiency[order(Tech_Efficiency$Country) ,]
  Substition_rate <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Substitution Rate" & Proj_World_Assumption$Sector == "TER",])
  Substition_rate <- Substition_rate[order(Substition_rate$Country) ,]
  
  Substition_rate_elec <- droplevels( Substition_rate[Substition_rate$Energy == "electricity",])
  Substition_rate_gas <- droplevels( Substition_rate[Substition_rate$Energy == "gas",])
  
  climatic_correction_gas <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Climatic correction" & Proj_World_Assumption$Sector == "TER",])
  climatic_correction_gas <- climatic_correction_gas[order(climatic_correction_gas$Country) ,]
  
  #Projection of electricity and gas first
  
  Year_Proj_col = min(which(is.nan(colSums(Hist_Energy_Demand[10:length(Hist_Energy_Demand)])))) + 9
  
  Hist_Energy_Demand_elec <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "electricity" ,]
  Hist_Energy_Demand_gas <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "gas" ,]
  Hist_Energy_Demand_total <- Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total" ,]
  
  Proj_Energy_Demand_elec <- Hist_Energy_Demand_elec
  Proj_Energy_Demand_gas <- Hist_Energy_Demand_gas
  Proj_Energy_Demand_total <- Hist_Energy_Demand_total
  
  for(i in Year_Proj_col:length(Hist_Energy_Demand))
  {
    Proj_Energy_Demand_elec[, i] <- Proj_Energy_Demand_elec[, i-1] * (1 + (Value_added_TER[, i] / Value_added_TER[, i-1] - 1) + Substition_rate_elec[, i] + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
    Proj_Energy_Demand_gas[, i] <- Proj_Energy_Demand_gas[, i-1] * ( 1 - climatic_correction_gas[, i] ) * (1 + (Value_added_TER[, i] / Value_added_TER[, i-1] - 1) + Substition_rate_gas[, i] + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
    Proj_Energy_Demand_total[, i] <- Proj_Energy_Demand_total[, i-1] * (1 + (Value_added_TER[, i] / Value_added_TER[, i-1] - 1) + Eco_Efficiency[, i] + Tech_Efficiency[, i] )
  }
  
  # Projection of others energies
  #####
  
  ## Calculation of historical market shares
  
  Hist_market_shares_Energy_Demand <- Hist_Energy_Demand 
  Hist_Energy_Demand_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total",] )
  
  for(i in 1:nrow(Hist_market_shares_Energy_Demand))
  {
    country_selected <- as.character(Hist_Energy_Demand$Country[i]) 
    Hist_market_shares_Energy_Demand[i,10:length(Hist_market_shares_Energy_Demand)] <- Hist_Energy_Demand[i,10:length(Hist_Energy_Demand)] / Hist_Energy_Demand_total[Hist_Energy_Demand_total$Country == country_selected, 10:length(Hist_Energy_Demand)] 
  }
  
  Hist_market_shares_Energy_Demand$ID_Item <- "Energy market shares"
  Hist_market_shares_Energy_Demand$Unit <- "%"
  
  ## Calculation of projected market shares for electricity and gas
  
  Proj_Energy_Demand_elec_gas <- rbind(Proj_Energy_Demand_elec, Proj_Energy_Demand_gas)
  Proj_market_shares_elec_gas <- Proj_Energy_Demand_elec_gas
  
  for(i in 1:nrow(Proj_market_shares_elec_gas))
  {
    country_selected <- as.character(Proj_Energy_Demand_elec_gas$Country[i]) 
    Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] <- Proj_Energy_Demand_elec_gas[i,10:length(Proj_Energy_Demand_elec_gas)] / Proj_Energy_Demand_total[Proj_Energy_Demand_total$Country == country_selected, 10:length(Proj_Energy_Demand_total)] 
    
    col_above_one <- which( Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] > 1) + 9
    if(length(col_above_one) > 0 )
    {
      Proj_market_shares_elec_gas[i,col_above_one] <- 1 
    }
  }
  
  Proj_market_shares_elec_gas$ID_Item <- "Energy market shares"
  Proj_market_shares_elec_gas$Unit <- "%"
  
  ## application of evolution of others energy except the one that it is use to loop
  Proj_diff_market_shares_last_hist_year <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Market shares difference with last historical year" &
                                                                                Proj_World_Assumption$Sector == "TER" 
                                                                              ,] )
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Country) ,]
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Energy) ,]
  
  Proj_market_shares_Energy_Demand <- Hist_market_shares_Energy_Demand
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "electricity" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "electricity" ,]
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "gas" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "gas" ,]
  
  Year_Proj_MS_col <- min(which(is.nan(colSums(Hist_market_shares_Energy_Demand[10:length(Hist_market_shares_Energy_Demand)])))) + 9  
  
  for(i in 1:nrow(Proj_diff_market_shares_last_hist_year))
  {
    country_selected <- as.character( Proj_diff_market_shares_last_hist_year[i, "Country"] )
    energy_selected <-  as.character( Proj_diff_market_shares_last_hist_year[i, "Energy"] )
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, Year_Proj_MS_col-1] - Proj_diff_market_shares_last_hist_year[i, j]
      
      ##Check if some market shares are below 0 or above 1
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] < 0)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j]  <- 0
      }
      
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- 1
      }
    }
  }
  
  
  #Reallocation of delta pdm where sum is above 1
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  sum_MS_others <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy %in% c("biomass and waste", "coal", "final heat", "oil") ,]
  sum_MS_others <- aggregate(sum_MS_others[,10:length(sum_MS_others)], by = sum_MS_others["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  max_sum_MS <- apply(sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)], 1, FUN = function(x) { max(x, na.rm = TRUE ) } ) 
  sum_MS_not_good <- sum_MS[which(max_sum_MS > 1) ,]
  sum_MS_others_not_good <- sum_MS_others[which(max_sum_MS > 1) ,]
  
  for (i in 1:nrow(sum_MS_not_good))
  {
    country_selected <- as.character( sum_MS_not_good[i, "Country"] )
    energy_selected <- c("biomass and waste", "coal", "final heat", "oil")
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      if( sum( Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected, j], na.rm = TRUE ) > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] + ( ( 1 - sum_MS_not_good[i, j-8] ) *  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] / sum_MS_others_not_good[i, j-8] )
      }
    }
  }
  
  # Loop with last energy not defined
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  row_missing <- which(is.na(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy!= "total",Year_Proj_MS_col]))
  MS_missing <- droplevels( Proj_market_shares_Energy_Demand[row_missing,] )
  MS_missing <- MS_missing[order( as.character( MS_missing$Country) ) ,]
  MS_missing[, Year_Proj_MS_col:length(MS_missing)] <- 1 - sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)]
  
  for(i in 1:length(row_missing))
  {
    country_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Country"] )
    energy_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Energy"] )
    Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected ,] <- MS_missing[MS_missing$Country == country_selected & MS_missing$Energy == energy_selected ,]
  }
  
  #####
  
  Proj_Energy_Demand <- Proj_Energy_Demand_total[ rep( seq_len( nrow( Proj_Energy_Demand_total ) ), nlevels(as.factor(Proj_market_shares_Energy_Demand$Energy)) ), ]
  Proj_Energy_Demand$Energy <- Proj_market_shares_Energy_Demand$Energy
  Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] <- Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] * Proj_market_shares_Energy_Demand[, 10:length(Proj_market_shares_Energy_Demand)]
  Proj_Energy_Demand <- Proj_Energy_Demand[Proj_Energy_Demand$Energy != "total" ,]
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_total)
  
  return(Proj_Energy_Demand)
}

main_proj_world_AGR_NEU_TRA_GB2018 <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")

  AGR_NEU_TRA_GB2018 <- read.xlsx2("../../Data/Inputs/World Data/AGR_NEU_TRA_GB2018.xlsx", sheetIndex = 1)
  AGR_NEU_TRA_GB2018 <- AGR_NEU_TRA_GB2018[order(AGR_NEU_TRA_GB2018$id_sector) ,]
  AGR_NEU_TRA_GB2018 <- AGR_NEU_TRA_GB2018[order(AGR_NEU_TRA_GB2018$code) ,]
  AGR_NEU_TRA_GB2018 <- AGR_NEU_TRA_GB2018[order(AGR_NEU_TRA_GB2018$id_energy) ,]
  AGR_NEU_TRA_GB2018 <- AGR_NEU_TRA_GB2018[,1:55] 
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(AGR_NEU_TRA_GB2018 $code))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector %in% c("AGR", "NEU", "TRA") ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Sector) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Hist_Energy_Demand_elec_gas <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy %in% c("electricity", "gas") ,] )
  Proj_Energy_Demand_elec_gas <- Hist_Energy_Demand_elec_gas
  
  Year_Proj_col <- min(which(is.nan(colSums(Hist_Energy_Demand_elec_gas[10:length(Hist_Energy_Demand_elec_gas)])))) + 9
  
  for(i in Year_Proj_col:length(Proj_Energy_Demand_elec_gas))
  {
    for(j in 1:nrow(Proj_Energy_Demand_elec_gas))
    {
      if (as.numeric(as.character(AGR_NEU_TRA_GB2018[j, i-6])) != 0) 
      {
        Proj_Energy_Demand_elec_gas[j, i] <- Proj_Energy_Demand_elec_gas[j, i-1] * as.numeric(as.character(AGR_NEU_TRA_GB2018[j, i-5])) / as.numeric(as.character(AGR_NEU_TRA_GB2018[j, i-6]))
      }
      if (as.numeric(as.character(AGR_NEU_TRA_GB2018[j, i-6])) == 0) 
      {
        Proj_Energy_Demand_elec_gas[j, i] <- 0
      }
      if (Proj_Energy_Demand_elec_gas[j, i] < 0) 
      {
        Proj_Energy_Demand_elec_gas[j, i] <- 0
      }
    }
  }
  
  return(Proj_Energy_Demand_elec_gas)
  
}

main_proj_world_AGR <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  # First projection of elec and gas and total
  AGR_NEU_rules_projection <- read.xlsx2("../../Data/Inputs/World Data/AGR_NEU_TRA_projection_rules.xlsx", sheetIndex = 1)
  AGR_NEU_rules_projection <- AGR_NEU_rules_projection[AGR_NEU_rules_projection$Sector == "AGR" ,]
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(AGR_NEU_rules_projection$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "AGR" ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Sector) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Hist_Energy_Demand_elec_gas_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy %in% c("electricity", "gas", "total") ,] )
  Proj_Energy_Demand_elec_gas_total <- Hist_Energy_Demand_elec_gas_total
  Year_Proj_col <- min(which(is.nan(colSums(Hist_Energy_Demand_elec_gas_total[10:length(Hist_Energy_Demand_elec_gas_total)])))) + 9
  
  #####
  ##projection rules : last historical year constant
  #####
  AGR_NEU_rules_projection_constant <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "N(-1)" ,] )
  
  if(nrow(AGR_NEU_rules_projection_constant) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_constant))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Energy"])
      
      Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , Year_Proj_col:length(Hist_Energy_Demand_elec_gas_total)] <- Hist_Energy_Demand_elec_gas_total[Hist_Energy_Demand_elec_gas_total$Country == country_selected & Hist_Energy_Demand_elec_gas_total$Sector == sector_selected & Hist_Energy_Demand_elec_gas_total$Energy == energy_selected , (Year_Proj_col - 1)]
    }
  }
  
  #####
  ##projection rules : constant growth
  #####
  AGR_NEU_rules_projection_growth <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Growth" ,] )
  
  if(nrow(AGR_NEU_rules_projection_growth) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_growth))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Energy"])
      growth_selected <- as.numeric( as.character( AGR_NEU_rules_projection_growth[i, "param_growth"] ) ) / 100
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( 1 + growth_selected ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  #####
  #Projection rules : variation according to IHS rivalry 
  #####
  AGR_NEU_rules_projection_var_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var" & AGR_NEU_rules_projection$Regle2 == "" ,] )
  IHS_Rivalry_world <- read.xlsx2("../../Data/Inputs/Benchmarks Data/IHS Rivalry World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  IHS_Rivalry_world <- IHS_Rivalry_world[IHS_Rivalry_world$Sector %in% c("AGR", "NEU") ,]
  IHS_Rivalry_world <- IHS_Rivalry_world[IHS_Rivalry_world$Country %in% list_country_sector_modeling ,]
  
  if(nrow(AGR_NEU_rules_projection_var_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  #####
  #Projection rules : TCAM on a constant number of year 
  #####
  AGR_NEU_rules_projection_TCAM <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "TCAM " ,] )
  
  if(nrow(AGR_NEU_rules_projection_TCAM) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_TCAM))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Energy"])
      number_of_year_TCAM <- as.numeric( as.character( AGR_NEU_rules_projection_TCAM[i, "param1"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  * ( ( Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1] / Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-number_of_year_TCAM] ) ^ (1/number_of_year_TCAM) )
      }
    }
  }

  #####
  # Projection rules : Var + root 
  ######
  AGR_NEU_rules_projection_Var_root <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var + root" ,] )
  
  if(nrow(AGR_NEU_rules_projection_Var_root) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_Var_root))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Energy"])
      param_root <- as.numeric( as.character( AGR_NEU_rules_projection_Var_root[i, "param_root"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( ( IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1] ) ^ param_root ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  
  #####
  #Projection rules : Delta  
  #####
  AGR_NEU_rules_projection_delta_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Delta",] )
  
  if(nrow(AGR_NEU_rules_projection_delta_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_delta_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] - IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) + Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  #####
  #Projection rules : Var and Var + Root on choosen year
  #####
  AGR_NEU_rules_projection_var_compo_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var" & AGR_NEU_rules_projection$Regle2 == "Var + root" ,] )
  
  if(nrow(AGR_NEU_rules_projection_var_compo_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_compo_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Energy"])
      param_year_root <- as.numeric( str_extract( as.character( AGR_NEU_rules_projection_var_compo_IHS[i, "param1"] ), "\\d+((,|\\.)\\d+)?" ) )
      param_root <- as.numeric( as.character( AGR_NEU_rules_projection_var_compo_IHS[i, "param_root"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        if( j <= (param_year_root - 1990))
        {
          Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
        }
        else
        {
          Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1] ) ^ (param_root) ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
        }
      }
    }
  }
 
  #####
  #Projection rules : Variation Gas Industry projection model  
  #####
  AGR_NEU_rules_projection_var_gas_industry_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var + Gas demand" ,] )
  
  #Besoin de la projection de l'industrie avant de pouvoir effectuer ce calcul
  
  # for(i in 1:nrow(AGR_NEU_rules_projection_var_IHS))
  # {
  #   country_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Country"])
  #   sector_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Sector"])
  #   energy_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Energy"])
  #   
  #   for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
  #   {
  #     Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == "IND" & IHS_Rivalry_world$Energy == "gas" , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == "IND" & IHS_Rivalry_world$Energy == "gas" , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
  #   }
  # }
  
  
  
  #####
  
  # Projection of others energies
  #####
  
  ## Calculation of historical market shares
  
  Hist_market_shares_Energy_Demand <- Hist_Energy_Demand 
  Hist_Energy_Demand_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total",] )
  
  for(i in 1:nrow(Hist_market_shares_Energy_Demand))
  {
    country_selected <- as.character(Hist_Energy_Demand$Country[i]) 
    Hist_market_shares_Energy_Demand[i,10:length(Hist_market_shares_Energy_Demand)] <- Hist_Energy_Demand[i,10:length(Hist_Energy_Demand)] / Hist_Energy_Demand_total[Hist_Energy_Demand_total$Country == country_selected, 10:length(Hist_Energy_Demand)] 
  }
  
  Hist_market_shares_Energy_Demand$ID_Item <- "Energy market shares"
  Hist_market_shares_Energy_Demand$Unit <- "%"
  
  ## Calculation of projected market shares for electricity and gas
  
  Proj_Energy_Demand_total <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Energy == "total" ,]
  Proj_Energy_Demand_elec_gas <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Energy != "total" ,]
  Proj_market_shares_elec_gas <- Proj_Energy_Demand_elec_gas
  
  for(i in 1:nrow(Proj_market_shares_elec_gas))
  {
    country_selected <- as.character(Proj_Energy_Demand_elec_gas$Country[i]) 
    Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] <- Proj_Energy_Demand_elec_gas[i,10:length(Proj_Energy_Demand_elec_gas)] / Proj_Energy_Demand_total[Proj_Energy_Demand_total$Country == country_selected, 10:length(Proj_Energy_Demand_total)] 
    
    col_above_one <- which( Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] > 1) + 9
    if(length(col_above_one) > 0 )
    {
      Proj_market_shares_elec_gas[i,col_above_one] <- 1 
    }
  }
  
  Proj_market_shares_elec_gas$ID_Item <- "Energy market shares"
  Proj_market_shares_elec_gas$Unit <- "%"
  
  ## application of evolution of others energy except the one that it is use to loop
  Proj_diff_market_shares_last_hist_year <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Market shares difference with last historical year" &
                                                                                Proj_World_Assumption$Sector == "AGR" 
                                                                              ,] )
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Country) ,]
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Energy) ,]
  
  Proj_market_shares_Energy_Demand <- Hist_market_shares_Energy_Demand
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "electricity" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "electricity" ,]
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "gas" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "gas" ,]
  
  Year_Proj_MS_col <- min(which(is.nan(colSums(Hist_market_shares_Energy_Demand[10:length(Hist_market_shares_Energy_Demand)])))) + 9  
  
  for(i in 1:nrow(Proj_diff_market_shares_last_hist_year))
  {
    country_selected <- as.character( Proj_diff_market_shares_last_hist_year[i, "Country"] )
    energy_selected <-  as.character( Proj_diff_market_shares_last_hist_year[i, "Energy"] )
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, Year_Proj_MS_col-1] - Proj_diff_market_shares_last_hist_year[i, j]
      
      ##Check if some market shares are below 0 or above 1
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] < 0)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j]  <- 0
      }
      
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- 1
      }
    }
  }
  
  
  #Reallocation of delta pdm where sum is above 1
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  sum_MS_others <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy %in% c("biomass and waste", "coal", "final heat", "oil") ,]
  sum_MS_others <- aggregate(sum_MS_others[,10:length(sum_MS_others)], by = sum_MS_others["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  max_sum_MS <- apply(sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)], 1, FUN = function(x) { max(x, na.rm = TRUE ) } ) 
  sum_MS_not_good <- sum_MS[which(max_sum_MS > 1) ,]
  sum_MS_others_not_good <- sum_MS_others[which(max_sum_MS > 1) ,]
  
  for (i in 1:nrow(sum_MS_not_good))
  {
    country_selected <- as.character( sum_MS_not_good[i, "Country"] )
    energy_selected <- c("biomass and waste", "coal", "final heat", "oil")
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      if( sum( Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected, j], na.rm = TRUE ) > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] + ( ( 1 - sum_MS_not_good[i, j-8] ) *  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] / sum_MS_others_not_good[i, j-8] )
      }
    }
  }
  
  # Loop with last energy not defined
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  row_missing <- which(is.na(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy!= "total",Year_Proj_MS_col]))
  MS_missing <- droplevels( Proj_market_shares_Energy_Demand[row_missing,] )
  MS_missing <- MS_missing[order( as.character( MS_missing$Country) ) ,]
  MS_missing[, Year_Proj_MS_col:length(MS_missing)] <- 1 - sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)]
  
  for(i in 1:length(row_missing))
  {
    country_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Country"] )
    energy_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Energy"] )
    Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected ,] <- MS_missing[MS_missing$Country == country_selected & MS_missing$Energy == energy_selected ,]
  }
  
  #####
  

  Proj_Energy_Demand <- Proj_Energy_Demand_total[ rep( seq_len( nrow( Proj_Energy_Demand_total ) ), nlevels(as.factor(Proj_market_shares_Energy_Demand$Energy)) ), ]
  Proj_Energy_Demand$Energy <- Proj_market_shares_Energy_Demand$Energy
  Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] <- Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] * Proj_market_shares_Energy_Demand[, 10:length(Proj_market_shares_Energy_Demand)]
  Proj_Energy_Demand <- Proj_Energy_Demand[Proj_Energy_Demand$Energy != "total" ,]
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_total)
  
  return(Proj_Energy_Demand)
}

main_proj_world_NEU <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  # First projection of elec and gas and total
  AGR_NEU_rules_projection <- read.xlsx2("../../Data/Inputs/World Data/AGR_NEU_TRA_projection_rules.xlsx", sheetIndex = 1)
  AGR_NEU_rules_projection <- AGR_NEU_rules_projection[AGR_NEU_rules_projection$Sector == "NEU" ,]
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(AGR_NEU_rules_projection$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "NEU" ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Sector) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Hist_Energy_Demand_elec_gas_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy %in% c("electricity", "gas", "total") ,] )
  Proj_Energy_Demand_elec_gas_total <- Hist_Energy_Demand_elec_gas_total
  Year_Proj_col <- min(which(is.nan(colSums(Hist_Energy_Demand_elec_gas_total[10:length(Hist_Energy_Demand_elec_gas_total)])))) + 9
  
  #####
  ##projection rules : last historical year constant
  #####
  AGR_NEU_rules_projection_constant <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "N(-1)" ,] )
  
  if(nrow(AGR_NEU_rules_projection_constant) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_constant))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Energy"])
      
      Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , Year_Proj_col:length(Hist_Energy_Demand_elec_gas_total)] <- Hist_Energy_Demand_elec_gas_total[Hist_Energy_Demand_elec_gas_total$Country == country_selected & Hist_Energy_Demand_elec_gas_total$Sector == sector_selected & Hist_Energy_Demand_elec_gas_total$Energy == energy_selected , (Year_Proj_col - 1)]
    }
  }
  
  #####
  ##projection rules : constant growth
  #####
  AGR_NEU_rules_projection_growth <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Growth" ,] )
  
  if(nrow(AGR_NEU_rules_projection_growth) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_growth))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Energy"])
      growth_selected <- as.numeric( as.character( AGR_NEU_rules_projection_growth[i, "param_growth"] ) ) / 100
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( 1 + growth_selected ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  #####
  #Projection rules : variation according to IHS rivalry 
  #####
  AGR_NEU_rules_projection_var_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var" & AGR_NEU_rules_projection$Regle2 == "" ,] )
  IHS_Rivalry_world <- read.xlsx2("../../Data/Inputs/Benchmarks Data/IHS Rivalry World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  IHS_Rivalry_world <- IHS_Rivalry_world[IHS_Rivalry_world$Sector %in% c("AGR", "NEU") ,]
  IHS_Rivalry_world <- IHS_Rivalry_world[IHS_Rivalry_world$Country %in% list_country_sector_modeling ,]
  
  if(nrow(AGR_NEU_rules_projection_var_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  #####
  #Projection rules : TCAM on a constant number of year 
  #####
  AGR_NEU_rules_projection_TCAM <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "TCAM " ,] )
  
  if(nrow(AGR_NEU_rules_projection_TCAM) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_TCAM))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Energy"])
      number_of_year_TCAM <- as.numeric( as.character( AGR_NEU_rules_projection_TCAM[i, "param1"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  * ( ( Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1] / Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-number_of_year_TCAM] ) ^ (1/number_of_year_TCAM) )
      }
    }
  }
  
  #####
  # Projection rules : Var + root 
  ######
  AGR_NEU_rules_projection_Var_root <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var + root" ,] )
  
  if(nrow(AGR_NEU_rules_projection_Var_root) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_Var_root))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Energy"])
      param_root <- as.numeric( as.character( AGR_NEU_rules_projection_Var_root[i, "param_root"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( ( IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1] ) ^ param_root ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  
  #####
  #Projection rules : Delta  
  #####
  AGR_NEU_rules_projection_delta_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Delta",] )
  
  if(nrow(AGR_NEU_rules_projection_delta_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_delta_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] - IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) + Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  #####
  #Projection rules : Var and Var + Root on choosen year
  #####
  AGR_NEU_rules_projection_var_compo_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var" & AGR_NEU_rules_projection$Regle2 == "Var + root" ,] )
  
  if(nrow(AGR_NEU_rules_projection_var_compo_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_compo_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Energy"])
      param_year_root <- as.numeric( str_extract( as.character( AGR_NEU_rules_projection_var_compo_IHS[i, "param1"] ), "\\d+((,|\\.)\\d+)?" ) )
      param_root <- as.numeric( as.character( AGR_NEU_rules_projection_var_compo_IHS[i, "param_root"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        if( j <= (param_year_root - 1990))
        {
          Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
        }
        else
        {
          Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1] ) ^ (param_root) ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
        }
      }
    }
  }
  
  #####
  #Projection rules : Variation Gas Industry projection model  
  #####
  AGR_NEU_rules_projection_var_gas_industry_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var + Gas demand" ,] )
  
  #Besoin de la projection de l'industrie avant de pouvoir effectuer ce calcul
  if(nrow(AGR_NEU_rules_projection_var_compo_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_gas_industry_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Energy"])
  
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , Year_Proj_col-1]
      }
    }
  }
  
  
  #####
  
  # Projection of others energies
  #####
  
  ## Calculation of historical market shares
  
  Hist_market_shares_Energy_Demand <- Hist_Energy_Demand 
  Hist_Energy_Demand_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total",] )
  
  for(i in 1:nrow(Hist_market_shares_Energy_Demand))
  {
    country_selected <- as.character(Hist_Energy_Demand$Country[i]) 
    Hist_market_shares_Energy_Demand[i,10:length(Hist_market_shares_Energy_Demand)] <- Hist_Energy_Demand[i,10:length(Hist_Energy_Demand)] / Hist_Energy_Demand_total[Hist_Energy_Demand_total$Country == country_selected, 10:length(Hist_Energy_Demand)] 
  }
  
  Hist_market_shares_Energy_Demand$ID_Item <- "Energy market shares"
  Hist_market_shares_Energy_Demand$Unit <- "%"
  
  ## Calculation of projected market shares for electricity and gas
  
  Proj_Energy_Demand_total <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Energy == "total" ,]
  Proj_Energy_Demand_elec_gas <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Energy != "total" ,]
  Proj_market_shares_elec_gas <- Proj_Energy_Demand_elec_gas
  
  for(i in 1:nrow(Proj_market_shares_elec_gas))
  {
    country_selected <- as.character(Proj_Energy_Demand_elec_gas$Country[i]) 
    Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] <- Proj_Energy_Demand_elec_gas[i,10:length(Proj_Energy_Demand_elec_gas)] / Proj_Energy_Demand_total[Proj_Energy_Demand_total$Country == country_selected, 10:length(Proj_Energy_Demand_total)] 
    
    col_above_one <- which( Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] > 1) + 9
    if(length(col_above_one) > 0 )
    {
      Proj_market_shares_elec_gas[i,col_above_one] <- 1 
    }
  }
  
  Proj_market_shares_elec_gas$ID_Item <- "Energy market shares"
  Proj_market_shares_elec_gas$Unit <- "%"
  
  ## application of evolution of others energy except the one that it is use to loop
  Proj_diff_market_shares_last_hist_year <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Market shares difference with last historical year" &
                                                                                Proj_World_Assumption$Sector == "NEU" 
                                                                              ,] )
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Country) ,]
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Energy) ,]
  
  Proj_market_shares_Energy_Demand <- Hist_market_shares_Energy_Demand
  # Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "electricity" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "electricity" ,]
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "gas" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "gas" ,]
  
  Year_Proj_MS_col <- min(which(is.nan(colSums(Hist_market_shares_Energy_Demand[10:length(Hist_market_shares_Energy_Demand)])))) + 9  
  
  for(i in 1:nrow(Proj_diff_market_shares_last_hist_year))
  {
    country_selected <- as.character( Proj_diff_market_shares_last_hist_year[i, "Country"] )
    energy_selected <-  as.character( Proj_diff_market_shares_last_hist_year[i, "Energy"] )
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, Year_Proj_MS_col-1] - Proj_diff_market_shares_last_hist_year[i, j]
      
      ##Check if some market shares are below 0 or above 1
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] < 0)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j]  <- 0
      }
      
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- 1
      }
    }
  }
  
  
  #Reallocation of delta pdm where sum is above 1
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  sum_MS_others <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy %in% c("coal", "oil") ,]
  sum_MS_others <- aggregate(sum_MS_others[,10:length(sum_MS_others)], by = sum_MS_others["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  max_sum_MS <- apply(sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)], 1, FUN = function(x) { max(x, na.rm = TRUE ) } ) 
  sum_MS_not_good <- sum_MS[which(max_sum_MS > 1) ,]
  sum_MS_others_not_good <- sum_MS_others[which(max_sum_MS > 1) ,]
  
  for (i in 1:nrow(sum_MS_not_good))
  {
    country_selected <- as.character( sum_MS_not_good[i, "Country"] )
    energy_selected <- c("coal", "oil")
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      if( sum( Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected, j], na.rm = TRUE ) > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] + ( ( 1 - sum_MS_not_good[i, j-8] ) *  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] / sum_MS_others_not_good[i, j-8] )
      }
    }
  }
  
  # Loop with last energy not defined
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  row_missing <- which(is.na(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy!= "total",Year_Proj_MS_col]))
  MS_missing <- droplevels( Proj_market_shares_Energy_Demand[row_missing,] )
  MS_missing <- MS_missing[order( as.character( MS_missing$Country) ) ,]
  MS_missing[, Year_Proj_MS_col:length(MS_missing)] <- 1 - sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)]
  
  for(i in 1:length(row_missing))
  {
    country_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Country"] )
    energy_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Energy"] )
    Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected ,] <- MS_missing[MS_missing$Country == country_selected & MS_missing$Energy == energy_selected ,]
  }
  
  #####
  
  
  Proj_Energy_Demand <- Proj_Energy_Demand_total[ rep( seq_len( nrow( Proj_Energy_Demand_total ) ), nlevels( as.factor(droplevels (Proj_market_shares_Energy_Demand$Energy)) ) ), ]
  Proj_Energy_Demand$Energy <- Proj_market_shares_Energy_Demand$Energy
  Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] <- Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] * Proj_market_shares_Energy_Demand[, 10:length(Proj_market_shares_Energy_Demand)]
  Proj_Energy_Demand <- Proj_Energy_Demand[Proj_Energy_Demand$Energy != "total" ,]
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_total)
  
  return(Proj_Energy_Demand)
}

main_proj_world_TRA <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  # First projection of elec and gas and total
  AGR_NEU_rules_projection <- read.xlsx2("../../Data/Inputs/World Data/AGR_NEU_TRA_projection_rules.xlsx", sheetIndex = 1)
  AGR_NEU_rules_projection <- AGR_NEU_rules_projection[AGR_NEU_rules_projection$Sector == "TRA" ,]
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(AGR_NEU_rules_projection$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "TRA" ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Sector) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Hist_Energy_Demand_elec_gas_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy %in% c("electricity", "gas", "total") ,] )
  Proj_Energy_Demand_elec_gas_total <- Hist_Energy_Demand_elec_gas_total
  Year_Proj_col <- min(which(is.nan(colSums(Hist_Energy_Demand_elec_gas_total[10:length(Hist_Energy_Demand_elec_gas_total)])))) + 9
  
  #####
  ##projection rules : last historical year constant
  #####
  AGR_NEU_rules_projection_constant <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "N(-1)" ,] )
  
  if(nrow(AGR_NEU_rules_projection_constant) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_constant))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_constant[i, "Energy"])
      
      Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , Year_Proj_col:length(Hist_Energy_Demand_elec_gas_total)] <- Hist_Energy_Demand_elec_gas_total[Hist_Energy_Demand_elec_gas_total$Country == country_selected & Hist_Energy_Demand_elec_gas_total$Sector == sector_selected & Hist_Energy_Demand_elec_gas_total$Energy == energy_selected , (Year_Proj_col - 1)]
    }
  }
  
  #####
  ##projection rules : constant growth
  #####
  AGR_NEU_rules_projection_growth <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Growth" ,] )
  
  if(nrow(AGR_NEU_rules_projection_growth) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_growth))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_growth[i, "Energy"])
      growth_selected <- as.numeric( as.character( AGR_NEU_rules_projection_growth[i, "param_growth"] ) ) / 100
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( 1 + growth_selected ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  #####
  #Projection rules : variation according to IHS rivalry 
  #####
  AGR_NEU_rules_projection_var_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var" & AGR_NEU_rules_projection$Regle2 == "" ,] )
  IHS_Rivalry_world <- read.xlsx2("../../Data/Inputs/Benchmarks Data/IHS Rivalry World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  IHS_Rivalry_world <- IHS_Rivalry_world[IHS_Rivalry_world$Sector == "TRA" ,]
  IHS_Rivalry_world <- IHS_Rivalry_world[IHS_Rivalry_world$Country %in% list_country_sector_modeling ,]
  
  if(nrow(AGR_NEU_rules_projection_var_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  #####
  #Projection rules : TCAM on a constant number of year 
  #####
  AGR_NEU_rules_projection_TCAM <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "TCAM " ,] )
  
  if(nrow(AGR_NEU_rules_projection_TCAM) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_TCAM))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_TCAM[i, "Energy"])
      number_of_year_TCAM <- as.numeric( as.character( AGR_NEU_rules_projection_TCAM[i, "param1"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  * ( ( Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1] / Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-number_of_year_TCAM] ) ^ (1/number_of_year_TCAM) )
      }
    }
  }
  
  #####
  # Projection rules : Var + root 
  ######
  AGR_NEU_rules_projection_Var_root <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var + root" ,] )
  
  if(nrow(AGR_NEU_rules_projection_Var_root) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_Var_root))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_Var_root[i, "Energy"])
      param_root <- as.numeric( as.character( AGR_NEU_rules_projection_Var_root[i, "param_root"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( ( IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1] ) ^ param_root ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  
  #####
  #Projection rules : Delta  
  #####
  AGR_NEU_rules_projection_delta_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Delta",] )
  
  if(nrow(AGR_NEU_rules_projection_delta_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_delta_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_delta_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] - IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) + Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
      }
    }
  }
  
  
  #####
  #Projection rules : Var and Var + Root on choosen year
  #####
  AGR_NEU_rules_projection_var_compo_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var" & AGR_NEU_rules_projection$Regle2 == "Var + root" ,] )
  
  if(nrow(AGR_NEU_rules_projection_var_compo_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_compo_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_compo_IHS[i, "Energy"])
      param_year_root <- as.numeric( str_extract( as.character( AGR_NEU_rules_projection_var_compo_IHS[i, "param1"] ), "\\d+((,|\\.)\\d+)?" ) )
      param_root <- as.numeric( as.character( AGR_NEU_rules_projection_var_compo_IHS[i, "param_root"] ) )
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        if( j <= (param_year_root - 1990))
        {
          Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1]  ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
        }
        else
        {
          Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- ( (IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j] / IHS_Rivalry_world[IHS_Rivalry_world$Country == country_selected & IHS_Rivalry_world$Sector == sector_selected & IHS_Rivalry_world$Energy == energy_selected , j-1] ) ^ (param_root) ) * Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j-1]  
        }
      }
    }
  }
  
  #####
  #Projection rules : Variation Gas Industry projection model  
  #####
  AGR_NEU_rules_projection_var_gas_industry_IHS <- droplevels( AGR_NEU_rules_projection[AGR_NEU_rules_projection$Regle1 == "Var + Gas demand" ,] )
  
  #Besoin de la projection de l'industrie avant de pouvoir effectuer ce calcul
  if(nrow(AGR_NEU_rules_projection_var_compo_IHS) > 0)
  {
    for(i in 1:nrow(AGR_NEU_rules_projection_var_gas_industry_IHS))
    {
      country_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Country"])
      sector_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Sector"])
      energy_selected <- as.character(AGR_NEU_rules_projection_var_gas_industry_IHS[i, "Energy"])
      
      for(j in Year_Proj_col:length(Proj_Energy_Demand_elec_gas_total))
      {
        Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , j] <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Country == country_selected & Proj_Energy_Demand_elec_gas_total$Sector == sector_selected & Proj_Energy_Demand_elec_gas_total$Energy == energy_selected , Year_Proj_col-1]
      }
    }
  }
  
  
  #####
  
  # Projection of others energies
  #####
  
  ## Calculation of historical market shares
  
  Hist_market_shares_Energy_Demand <- Hist_Energy_Demand 
  Hist_Energy_Demand_total <- droplevels( Hist_Energy_Demand[Hist_Energy_Demand$Energy == "total",] )
  
  for(i in 1:nrow(Hist_market_shares_Energy_Demand))
  {
    country_selected <- as.character(Hist_Energy_Demand$Country[i]) 
    Hist_market_shares_Energy_Demand[i,10:length(Hist_market_shares_Energy_Demand)] <- Hist_Energy_Demand[i,10:length(Hist_Energy_Demand)] / Hist_Energy_Demand_total[Hist_Energy_Demand_total$Country == country_selected, 10:length(Hist_Energy_Demand)] 
  }
  
  Hist_market_shares_Energy_Demand$ID_Item <- "Energy market shares"
  Hist_market_shares_Energy_Demand$Unit <- "%"
  
  ## Calculation of projected market shares for electricity and gas
  
  Proj_Energy_Demand_total <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Energy == "total" ,]
  Proj_Energy_Demand_elec_gas <- Proj_Energy_Demand_elec_gas_total[Proj_Energy_Demand_elec_gas_total$Energy != "total" ,]
  Proj_market_shares_elec_gas <- Proj_Energy_Demand_elec_gas
  
  for(i in 1:nrow(Proj_market_shares_elec_gas))
  {
    country_selected <- as.character(Proj_Energy_Demand_elec_gas$Country[i]) 
    Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] <- Proj_Energy_Demand_elec_gas[i,10:length(Proj_Energy_Demand_elec_gas)] / Proj_Energy_Demand_total[Proj_Energy_Demand_total$Country == country_selected, 10:length(Proj_Energy_Demand_total)] 
    
    col_above_one <- which( Proj_market_shares_elec_gas[i,10:length(Proj_market_shares_elec_gas)] > 1) + 9
    if(length(col_above_one) > 0 )
    {
      Proj_market_shares_elec_gas[i,col_above_one] <- 1 
    }
  }
  
  Proj_market_shares_elec_gas$ID_Item <- "Energy market shares"
  Proj_market_shares_elec_gas$Unit <- "%"
  
  ## application of evolution of others energy except the one that it is use to loop
  Proj_diff_market_shares_last_hist_year <- droplevels( Proj_World_Assumption[Proj_World_Assumption$ID_Item == "Market shares difference with last historical year" &
                                                                                Proj_World_Assumption$Sector == "TRA" 
                                                                              ,] )
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Country) ,]
  Proj_diff_market_shares_last_hist_year <- Proj_diff_market_shares_last_hist_year[order(Proj_diff_market_shares_last_hist_year$Energy) ,]
  
  Proj_market_shares_Energy_Demand <- Hist_market_shares_Energy_Demand
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "electricity" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "electricity" ,]
  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy == "gas" ,] <- Proj_market_shares_elec_gas[Proj_market_shares_elec_gas$Energy == "gas" ,]
  
  Year_Proj_MS_col <- min(which(is.nan(colSums(Hist_market_shares_Energy_Demand[10:length(Hist_market_shares_Energy_Demand)])))) + 9  
  
  for(i in 1:nrow(Proj_diff_market_shares_last_hist_year))
  {
    country_selected <- as.character( Proj_diff_market_shares_last_hist_year[i, "Country"] )
    energy_selected <-  as.character( Proj_diff_market_shares_last_hist_year[i, "Energy"] )
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, Year_Proj_MS_col-1] - Proj_diff_market_shares_last_hist_year[i, j]
      
      ##Check if some market shares are below 0 or above 1
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] < 0)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j]  <- 0
      }
      
      if(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected, j] <- 1
      }
    }
  }
  
  
  #Reallocation of delta pdm where sum is above 1
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  sum_MS_others <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy %in% c("biomass and waste", "coal", "oil") ,]
  sum_MS_others <- aggregate(sum_MS_others[,10:length(sum_MS_others)], by = sum_MS_others["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  max_sum_MS <- apply(sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)], 1, FUN = function(x) { max(x, na.rm = TRUE ) } ) 
  sum_MS_not_good <- sum_MS[which(max_sum_MS > 1) ,]
  sum_MS_others_not_good <- sum_MS_others[which(max_sum_MS > 1) ,]
  
  for (i in 1:nrow(sum_MS_not_good))
  {
    country_selected <- as.character( sum_MS_not_good[i, "Country"] )
    energy_selected <- c("biomass and waste", "coal", "oil")
    
    for(j in Year_Proj_MS_col:length(Proj_market_shares_Energy_Demand))
    {
      if( sum( Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected, j], na.rm = TRUE ) > 1)
      {
        Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] <- Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] + ( ( 1 - sum_MS_not_good[i, j-8] ) *  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy %in% energy_selected, j] / sum_MS_others_not_good[i, j-8] )
      }
    }
  }
  
  # Loop with last energy not defined
  sum_MS <-  Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy != "total",]
  sum_MS <- aggregate(sum_MS[,10:length(sum_MS)], by = sum_MS["Country"], FUN = function(x) {sum(x, na.rm = T)})
  
  row_missing <- which(is.na(Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Energy!= "total",Year_Proj_MS_col]))
  MS_missing <- droplevels( Proj_market_shares_Energy_Demand[row_missing,] )
  MS_missing <- MS_missing[order( as.character( MS_missing$Country) ) ,]
  MS_missing[, Year_Proj_MS_col:length(MS_missing)] <- 1 - sum_MS[, (Year_Proj_MS_col-8):length(sum_MS)]
  
  for(i in 1:length(row_missing))
  {
    country_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Country"] )
    energy_selected <- as.character( Proj_market_shares_Energy_Demand[row_missing[i], "Energy"] )
    Proj_market_shares_Energy_Demand[Proj_market_shares_Energy_Demand$Country == country_selected & Proj_market_shares_Energy_Demand$Energy == energy_selected ,] <- MS_missing[MS_missing$Country == country_selected & MS_missing$Energy == energy_selected ,]
  }
  
  #####
  
  
  Proj_Energy_Demand <- Proj_Energy_Demand_total[ rep( seq_len( nrow( Proj_Energy_Demand_total ) ), nlevels( as.factor(droplevels (Proj_market_shares_Energy_Demand$Energy) )) ), ]
  Proj_Energy_Demand$Energy <- Proj_market_shares_Energy_Demand$Energy
  Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] <- Proj_Energy_Demand[, 10:length(Proj_Energy_Demand)] * Proj_market_shares_Energy_Demand[, 10:length(Proj_market_shares_Energy_Demand)]
  Proj_Energy_Demand <- Proj_Energy_Demand[Proj_Energy_Demand$Energy != "total" ,]
  Proj_Energy_Demand <- rbind( Proj_Energy_Demand, Proj_Energy_Demand_total)
  
  return(Proj_Energy_Demand)
}

main_proj_Unspecified <- function()
{
  #Import all data mandatory for projectino
  Proj_World_Assumption <- readRDS("../../Data/temp/sector_country_data/Proj_World_Assumptions.rds")  
  Hist_World_Macro <- readRDS("../../Data/temp/sector_country_data/Hist_World_Macro.rds")
  Proj_World_Macro <- readRDS("../../Data/temp/sector_country_data/Proj_World_Macro.rds")
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Hist_Energy_Demand <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_country_sector_modeling <- levels(as.factor(Proj_World_Assumption$Country))
  Hist_Energy_Demand <- Hist_Energy_Demand[Hist_Energy_Demand$Country %in% list_country_sector_modeling  & Hist_Energy_Demand$Sector == "Unspecified",]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Country) ,]
  Hist_Energy_Demand <- Hist_Energy_Demand[order(Hist_Energy_Demand$Energy) ,]
  
  Year_Proj_col = min(which(is.nan(colSums(Hist_Energy_Demand[10:length(Hist_Energy_Demand)])))) + 9
  Proj_Energy_Demand <- Hist_Energy_Demand
  
  Proj_Energy_Demand[, Year_Proj_col:length(Proj_Energy_Demand)] <- Hist_Energy_Demand[, Year_Proj_col-1]
  
  return(Proj_Energy_Demand)
}

main_rest_of_world_projection <- function( world_modeled_countries , end_uses_countries)
{
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  enerdata_starting_point <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  country_classification <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/Country Classification.xlsx", sheetIndex = 1)
  country_mapping <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/Country Mapping.xlsx", sheetIndex = 1)
  IHS_World_projection <- read.xlsx2("../../Data/Inputs/Benchmarks Data/IHS Rivalry World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  All_countries_modeled <- rbind(world_modeled_countries , end_uses_countries)
  All_countries_modeled <- All_countries_modeled[All_countries_modeled$Country != "EU23" & All_countries_modeled$Energy %in% c("electricity", "gas") ,]
  
  IHS_World_projection_elec_gas <- droplevels( IHS_World_projection[IHS_World_projection$Energy %in% c("electricity", "gas") & IHS_World_projection$Sector != "Primary" ,] )
  
  IHS_country_available <- levels(as.factor(IHS_World_projection_elec_gas$Country))
  world_modeled_countries_available <- levels(as.factor(world_modeled_countries$Country))
  end_uses_countries_available <- levels(as.factor(end_uses_countries$Country))
  
  countries_to_kept <- IHS_country_available[!(IHS_country_available %in% world_modeled_countries_available)]
  countries_to_kept <- countries_to_kept[!(countries_to_kept %in% end_uses_countries_available)]
  
  zone_IHS <- countries_to_kept[nchar(countries_to_kept) == 3]
  countries_lacking <- countries_to_kept[nchar(countries_to_kept) == 2] 
    
  IHS_World_projection_zone <- IHS_World_projection_elec_gas[IHS_World_projection_elec_gas$Country %in% zone_IHS ,]
  IHS_World_projection_zone <- IHS_World_projection_elec_gas[IHS_World_projection_elec_gas$Country %in% zone_IHS ,]
  
  Final_IHS_World_projection_zone <- aggregate(IHS_World_projection_zone[, 10:length(IHS_World_projection_zone)], by = IHS_World_projection_zone[c("Country", "Energy")], FUN = sum)
  Final_All_countries_modeled <- All_countries_modeled[All_countries_modeled$Sector == "Final" ,]

  Final_All_countries_modeled <- merge(Final_All_countries_modeled, country_mapping)
  Final_All_countries_modeled <- aggregate(Final_All_countries_modeled[, 10:(length(Final_All_countries_modeled)-2)], by = Final_All_countries_modeled[c("Region", "Energy")], FUN = sum)
  
  Final_Rest_of_world <- Final_IHS_World_projection_zone
  for(i in 1:nrow(Final_All_countries_modeled))
  {
    country_selected <- as.character( Final_All_countries_modeled$Region[i])
    energy_selected <- as.character( Final_All_countries_modeled$Energy[i])
    Final_Rest_of_world[Final_Rest_of_world$Country == country_selected & Final_Rest_of_world$Energy == energy_selected, 3:length(Final_Rest_of_world)] <- Final_Rest_of_world[Final_Rest_of_world$Country == country_selected & Final_Rest_of_world$Energy == energy_selected, 3:length(Final_Rest_of_world)] - Final_All_countries_modeled[i, 3:length(Final_All_countries_modeled)]
  }
  
  
  Final_Rest_of_world <- droplevels( Final_Rest_of_world )

  enerdata_starting_point <- enerdata_total_energy_computation(enerdata_starting_point)
  enerdata_starting_point <- enerdata_reshaping(enerdata_starting_point, country_classification, country_mapping, "Zone sector")
  enerdata_starting_point <- enerdata_starting_point[enerdata_starting_point$Energy %in% c("electricity", "gas") ,] 
  enerdata_starting_point <- enerdata_starting_point[order(enerdata_starting_point$Country) ,]
  enerdata_starting_point <- enerdata_starting_point[order(enerdata_starting_point$Energy) ,]
  
  Proj_countries_rest_of_world <- enerdata_starting_point
  for(i in 1:nrow(enerdata_starting_point))
  {
    zone_selected <- as.character( enerdata_starting_point$Region[i] )
    energy_selected <- as.character( enerdata_starting_point$Energy[i] )
    
    Final_Rest_of_world_selected <- droplevels( Final_Rest_of_world[Final_Rest_of_world$Country == zone_selected & Final_Rest_of_world$Energy == energy_selected ,] )
    Year_Proj_col <- min(which(is.nan(colSums(Proj_countries_rest_of_world[12:length(Proj_countries_rest_of_world)])))) + 11
    
    for(j in Year_Proj_col:length(Proj_countries_rest_of_world))
    {
      Proj_countries_rest_of_world[i,j] <- Proj_countries_rest_of_world[i,j-1] *  Final_Rest_of_world_selected[1, j- 9] / Final_Rest_of_world_selected[1, j-10] 
    }
  }
  
  Final_Proj_countries_rest_of_world <- aggregate(Proj_countries_rest_of_world[,12:length(Proj_countries_rest_of_world)], by = Proj_countries_rest_of_world[c("Country", "Energy")], FUN = sum )
  Final_Proj_countries_rest_of_world$Sector <- "Final"
  Final_Proj_countries_rest_of_world$Sub_Sector <- "total"
  Final_Proj_countries_rest_of_world$Unit <- "TWh"
  Final_Proj_countries_rest_of_world$Usage <- "total"
  Final_Proj_countries_rest_of_world$Source <- "AMADEUS"
  Final_Proj_countries_rest_of_world$ID_Item <- "Energy Demand"
  Final_Proj_countries_rest_of_world$Scenario <- "GB"
  
  Final_Proj_countries_rest_of_world <- cbind(Final_Proj_countries_rest_of_world$ID_Item,
                                                                           Final_Proj_countries_rest_of_world$Scenario,
                                                                           Final_Proj_countries_rest_of_world$Country,
                                                                           Final_Proj_countries_rest_of_world$Energy,
                                                                           Final_Proj_countries_rest_of_world$Sector,
                                                                           Final_Proj_countries_rest_of_world$Sub_Sector,
                                                                           Final_Proj_countries_rest_of_world$Usage,
                                                                           Final_Proj_countries_rest_of_world$Source,
                                                                           Final_Proj_countries_rest_of_world$Unit,
                                                                           Final_Proj_countries_rest_of_world[, 3:(length(Final_Proj_countries_rest_of_world)-7)
                                                                                                              ]
  )
  
  names(Final_Proj_countries_rest_of_world)[1:9] <- c("ID_Item",
                                                      "Scenario",
                                                      "Country",
                                                      "Energy",
                                                      "Sector",
                                                      "Sub_Sector",
                                                      "Usage",
                                                      "Source",
                                                      "Unit"
                                                      )
  
  return(Final_Proj_countries_rest_of_world)
}

enerdata_reshaping <- function(enerdata_raw_data, country_classification, country_mapping, choice_country_model)
{
  #### Get country classification and mapping (region and modelisation way)
  enerdata_raw_data <- merge(country_classification, enerdata_raw_data)
  enerdata_raw_data <- droplevels(enerdata_raw_data[enerdata_raw_data$Country_Modelling ==  choice_country_model,])
  enerdata_raw_data <- droplevels(enerdata_raw_data[enerdata_raw_data$Sector !=  "Final",])
  enerdata_raw_data <- enerdata_raw_data[,-(2:3)]
  enerdata_raw_data <- merge(country_mapping, enerdata_raw_data)
  
  # ### Aggregation of biomass, final heat and direct hydrogen in other
  # enerdata_others <- enerdata_raw_data[enerdata_raw_data$Energy %in% c("final heat", "direct hydrogen", "biomass and waste"),]
  # enerdata_others_value <-  aggregate(x = enerdata_others[,12:length(enerdata_others)], by = enerdata_others[c("Sector", "Country")], FUN = sum)
  # enerdata_others_header  <- enerdata_others[1:nrow(enerdata_others_value), 1:11]  
  # enerdata_others_header$Country <- enerdata_others_value$Country
  # enerdata_others_header$Sector <- enerdata_others_value$Sector
  # enerdata_others_header$Energy <- "others"
  # enerdata_others <- cbind(enerdata_others_header, enerdata_others_value[,-(1:2)])
  # 
  # enerdata_main_energy <- enerdata_raw_data[enerdata_raw_data$Energy %in% c("coal", "total", "electricity", "gas", "oil"),]
  # 
  # enerdata_reshaped <- rbind(enerdata_main_energy, enerdata_others)
  
  return(enerdata_raw_data)
}

enerdata_total_energy_computation <- function(enerdata_raw_data)
{
  enerdata_without_total <- enerdata_raw_data[enerdata_raw_data$Energy != "total",]
  
  ### Aggregation of energy for total energy calculation
  enerdata_with_total_value <-  aggregate(x = enerdata_without_total[,10:length(enerdata_without_total)], by = enerdata_without_total[c("Sector", "Country")], FUN = sum)
  enerdata_with_total_header  <- enerdata_without_total[1:nrow(enerdata_with_total_value), 1:9]  
  enerdata_with_total_header$Country <- enerdata_with_total_value$Country
  enerdata_with_total_header$Sector <- enerdata_with_total_value$Sector
  enerdata_with_total_header$Energy <- "total"
  enerdata_with_total <- cbind(enerdata_with_total_header, enerdata_with_total_value[,-(1:2)])
  
  final_enerdata <- rbind(enerdata_without_total, enerdata_with_total)
  
  return(final_enerdata)
}


