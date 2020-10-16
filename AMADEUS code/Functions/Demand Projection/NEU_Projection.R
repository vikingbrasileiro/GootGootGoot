source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the Non Energy Uses sector 
############################################################################################################################################################################

main_projection_energy_demand_NEU <- function(Hist_Energy_Demand, Proj_Assumption, Proj_Energy_Demand)
{
  #1. Data input extraction 
  NEU_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "NEU")
  NEU_Hist_Energy_Demand <- Sort_By_Country(NEU_Hist_Energy_Demand)
  NEU_Hist_Energy_Demand <- NEU_Hist_Energy_Demand[order(NEU_Hist_Energy_Demand$Energy),]
  
  #Based on chemical secrtor in industry
  Consumption_Chemical_IND <- Extract_Usage( Extract_Item( Extract_Sub_Sector(Proj_Energy_Demand, "Chemical"), "Energy Demand"), "total")
  Consumption_Chemical_IND <- Consumption_Chemical_IND[Consumption_Chemical_IND$Country != "EU23",]
  Consumption_Chemical_IND <- Sort_By_Country(Consumption_Chemical_IND)
  Consumption_Chemical_IND <- Consumption_Chemical_IND[order(Consumption_Chemical_IND$Energy),]
  
  Share_Organic_Chemical <- Extract_Item(Proj_Assumption, "Share of Organic Chemical")
  Share_Organic_Chemical <- Share_Organic_Chemical[rep( seq_len( nrow( Share_Organic_Chemical ) ), nrow(Consumption_Chemical_IND)),]
  Share_Organic_Chemical <- Sort_By_Country(Share_Organic_Chemical)
  Share_Organic_Chemical <- Share_Organic_Chemical[order(Share_Organic_Chemical$Energy),]

  NEU_Activity_Rate <- Consumption_Chemical_IND
  NEU_Activity_Rate$ID_Item <- "Activity Rate"
  NEU_Activity_Rate$Unit <- "%"
  NEU_Activity_Rate[, 10:length(NEU_Activity_Rate)] = Share_Organic_Chemical[,10:length(Share_Organic_Chemical)] * Consumption_Chemical_IND[, 10:length(Consumption_Chemical_IND)]
  NEU_Activity_Rate <- derivate_dataframe(NEU_Activity_Rate)
  
  for(j in 1:length(NEU_Activity_Rate))
  {
    for(i in 1:nrow(NEU_Activity_Rate))
    {
      
      if(NEU_Activity_Rate[i,j] == Inf) NEU_Activity_Rate[i,j] <- 0
          
    }
  }
  
  #Ajout de NEU Activity Rate au dataframe pour visualisation dans interface
  NEU_Activity_Rate$Sub_Sector <- "na"
  NEU_Activity_Rate$Sector <- "NEU"
  NEU_Activity_Rate <- unique(NEU_Activity_Rate)
  Proj_Assumption <<- unique(rbind(Proj_Assumption, NEU_Activity_Rate)) 
  
  #2. Projection 
  
  NEU_Proj_Demand <- NEU_Hist_Energy_Demand
  
  Year_Proj_col = min(which(is.nan(colSums(NEU_Hist_Energy_Demand[10:length(NEU_Hist_Energy_Demand)])))) + 9
  
  for(i in Year_Proj_col:length(NEU_Hist_Energy_Demand))
  {
    NEU_Proj_Demand[,i] <- NEU_Proj_Demand[,i-1] * ( 1 + NEU_Activity_Rate[,i] )  
  }
  
  NEU_Proj_Demand$Sub_Sector <- as.factor("total")
  NEU_Proj_Demand$Usage <- as.factor("total")
  NEU_Proj_Demand <- calculate_total_sector_NEU(NEU_Proj_Demand)
  
  return(NEU_Proj_Demand)
} 

calculate_total_sector_NEU <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  #on calcule le total secteur pour chaques energies (somme des usages)
  NEU_Proj_Demand_total_sector_value <- sapply(input[,10:length(input)], function(x){ tapply(x, input$Country, sum)})
  NEU_Proj_Demand_total_sector_header  <- input[1:nrow(NEU_Proj_Demand_total_sector_value), 1:9]
  NEU_Proj_Demand_total_sector_header$Energy <- "total"
  NEU_Proj_Demand_total_sector_header$Country <- rep(levels(as.factor(input$Country)))
  
  #on calcule le total EU23 pour chaques energy
  NEU_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  NEU_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(NEU_Proj_Demand_Country_Energy_total_value), 1:9]  
  NEU_Proj_Demand_Country_Energy_total_header$Energy <- NEU_Proj_Demand_Country_Energy_total_value$Energy
  NEU_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  NEU_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  NEU_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  NEU_Proj_Demand_Country_Energy_total <- unique(cbind(NEU_Proj_Demand_Country_Energy_total_header, NEU_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  NEU_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  NEU_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(NEU_Proj_Demand_Country_Sector_total_value), 1:9]  
  NEU_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  NEU_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  NEU_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  NEU_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  NEU_Proj_Demand_Country_Sector_total_header$Sector <- NEU_Proj_Demand_Country_Sector_total_value$Sector
  NEU_Proj_Demand_Country_Sector_total <- unique(cbind(NEU_Proj_Demand_Country_Sector_total_header, NEU_Proj_Demand_Country_Sector_total_value[,-1]))
  
  input <- rbind(input, unique(cbind( NEU_Proj_Demand_total_sector_header, NEU_Proj_Demand_total_sector_value))) 
  input <- rbind(input, NEU_Proj_Demand_Country_Energy_total, NEU_Proj_Demand_Country_Sector_total) 
  
  return(input)
}