source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the Transport sector 
############################################################################################################################################################################

main_projection_energy_demand_TRA <- function()
{
  #1. Data input extraction 
  TRA_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "TRA")
  TRA_Hist_Energy_Demand <- Sort_By_Country(TRA_Hist_Energy_Demand)
  TRA_Hist_Energy_Demand <- TRA_Hist_Energy_Demand[order(TRA_Hist_Energy_Demand$Energy),]
  
  #Porjection issue du modele SAS Europe
  column_type <- c(rep("character", times = 9),rep("numeric", times = 51))
  TRA_SAS_Energy_Demand <- read.xlsx2("X:\\02_0124\\01_CEEME\\01_SEER\\02_ActivityFinite\\PSE_0013_Model_Improvement_Demand_Europe\\04. Project Documents\\AMADEUS\\Data\\Inputs\\Historical Data\\TRA\\TRA_SAS_Demand.xlsx", sheetIndex = 1 , colClasses = column_type )
  TRA_SAS_Energy_Demand <- Sort_By_Country(ktoe_to_TWh(TRA_SAS_Energy_Demand))
  TRA_SAS_Energy_Demand <- TRA_SAS_Energy_Demand[order(TRA_SAS_Energy_Demand$Energy),]
  
  Year_Proj_col = min(which(is.nan(colSums(TRA_Hist_Energy_Demand[10:length(TRA_Hist_Energy_Demand)])))) + 9
  TRA_Proj_Demand <- TRA_Hist_Energy_Demand
  
  for(i in Year_Proj_col:length(TRA_Hist_Energy_Demand))
  {
    TRA_Proj_Demand[,i] <- TRA_Proj_Demand[,i-1] + (TRA_SAS_Energy_Demand[,i] - TRA_SAS_Energy_Demand[,i-1])  
  }
  
  for(i in Year_Proj_col:length(TRA_Hist_Energy_Demand))
  {
    for(j in 1:length(rownames(TRA_Hist_Energy_Demand)))
    {
       if (TRA_Proj_Demand[j,i] < 0) {TRA_Proj_Demand[j,i] <- 0}
    }
  }
  
  
  TRA_Proj_Demand$Sub_Sector <- as.factor("total")
  TRA_Proj_Demand$Usage <- as.factor("total")
  TRA_Proj_Demand <- calculate_total_sector_TRA(TRA_Proj_Demand) 
  
  return(TRA_Proj_Demand)
}

calculate_total_sector_TRA <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  #on calcule le total secteur pour chaques energies (somme des usages)
  TRA_Proj_Demand_total_sector_value <- sapply(input[,10:length(input)], function(x){ tapply(x, input$Country, sum)})
  TRA_Proj_Demand_total_sector_header  <- input[1:nrow(TRA_Proj_Demand_total_sector_value), 1:9]
  TRA_Proj_Demand_total_sector_header$Energy <- "total"
  TRA_Proj_Demand_total_sector_header$Country <- rep(levels(as.factor(input$Country)))
  
  #on calcule le total EU23 pour chaques energy
  TRA_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  TRA_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(TRA_Proj_Demand_Country_Energy_total_value), 1:9]  
  TRA_Proj_Demand_Country_Energy_total_header$Energy <- TRA_Proj_Demand_Country_Energy_total_value$Energy
  TRA_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  TRA_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  TRA_Proj_Demand_Country_Energy_total <- unique(cbind(TRA_Proj_Demand_Country_Energy_total_header, TRA_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  TRA_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  TRA_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(TRA_Proj_Demand_Country_Sector_total_value), 1:9]  
  TRA_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  TRA_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  TRA_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  TRA_Proj_Demand_Country_Sector_total_header$Sector <- TRA_Proj_Demand_Country_Sector_total_value$Sector
  TRA_Proj_Demand_Country_Sector_total <- unique(cbind(TRA_Proj_Demand_Country_Sector_total_header, TRA_Proj_Demand_Country_Sector_total_value[,-1]))
  
  
  input <- rbind(input, unique(cbind( TRA_Proj_Demand_total_sector_header, TRA_Proj_Demand_total_sector_value)))
  input <- rbind(input, TRA_Proj_Demand_Country_Energy_total, TRA_Proj_Demand_Country_Sector_total)
  
  return(input)
}                                                 
                                                   