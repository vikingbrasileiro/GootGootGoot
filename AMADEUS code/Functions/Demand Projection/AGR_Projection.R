source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the Agriculture sector 
############################################################################################################################################################################

main_projection_energy_demand_AGR <- function(Hist_Energy_Demand, Proj_Assumption)
{
  # browser()
  #1. Data input extraction 
  AGR_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "AGR")
  AGR_Hist_Energy_Demand <- Sort_By_Country(AGR_Hist_Energy_Demand)
  AGR_Hist_Energy_Demand <- AGR_Hist_Energy_Demand[order(AGR_Hist_Energy_Demand$Energy),]
  AGR_Hist_Energy_Demand <- droplevels(subset(AGR_Hist_Energy_Demand, Energy != "total"))
  
  AGR_Activity_Rate <- Extract_Sector(Extract_Item(Proj_Assumption, "Activity Rate"), "AGR")
  AGR_Activity_Rate = Sort_By_Country(AGR_Activity_Rate)
  AGR_Activity_Rate = AGR_Activity_Rate[ rep( seq_len( nrow( AGR_Activity_Rate ) ), nlevels(as.factor(AGR_Hist_Energy_Demand$Energy)) ), ]
  AGR_Activity_Rate$Energy = as.factor( rep( levels( as.factor( AGR_Hist_Energy_Demand$Energy ) ), each = nlevels( as.factor( AGR_Activity_Rate$Country ) ) ) )
  AGR_Activity_Rate = AGR_Activity_Rate[order(AGR_Activity_Rate$Energy),]
  AGR_Activity_Rate <- droplevels(subset(AGR_Activity_Rate, Energy != "total"))
  
  AGR_Intensity_Rate <- Extract_Sector(Extract_Item(Proj_Assumption, "Energy Intensity"), "AGR")
  AGR_Intensity_Rate = Sort_By_Country(AGR_Intensity_Rate)
  AGR_Activity_Rate$Energy <- as.factor(AGR_Activity_Rate$Energy)
  AGR_Intensity_Rate = AGR_Intensity_Rate[order(AGR_Intensity_Rate$Energy),]
  
  #2. Projection 
  
  AGR_Proj_Demand <- AGR_Hist_Energy_Demand
  
  Year_Proj_col = min(which(is.nan(colSums(AGR_Hist_Energy_Demand[10:length(AGR_Hist_Energy_Demand)])))) + 9
  
  for(i in Year_Proj_col:length(AGR_Hist_Energy_Demand))
  {
    AGR_Proj_Demand[,i] <- AGR_Proj_Demand[,i-1]*(1+AGR_Activity_Rate[,i])  * ( 1+ AGR_Intensity_Rate[,i] )
  }
  
  AGR_Proj_Demand$Sub_Sector <- as.factor("total")
  AGR_Proj_Demand$Usage <- as.factor("total")
  AGR_Proj_Demand <- calculate_total_sector_AGR(AGR_Proj_Demand)
  
  return(AGR_Proj_Demand)
} 

calculate_total_sector_AGR <- function(input)
{
  #input <- droplevels(subset(input, Energy != "total"))
  
  #on calcule le total secteur pour chaques energies (somme des usages)
  AGR_Proj_Demand_total_sector_value <- sapply(input[,10:length(input)], function(x){ tapply(x, input$Country, sum)})
  AGR_Proj_Demand_total_sector_header  <- input[1:nrow(AGR_Proj_Demand_total_sector_value), 1:9]
  AGR_Proj_Demand_total_sector_header$Usage <- "total"
  AGR_Proj_Demand_total_sector_header$Energy <- "total"
  AGR_Proj_Demand_total_sector_header$Country <- rep(levels(as.factor(input$Country)))
  
  #on calcule le total EU23 pour chaques energy
  AGR_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  AGR_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(AGR_Proj_Demand_Country_Energy_total_value), 1:9]  
  AGR_Proj_Demand_Country_Energy_total_header$Energy <- AGR_Proj_Demand_Country_Energy_total_value$Energy
  AGR_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  AGR_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  AGR_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  AGR_Proj_Demand_Country_Energy_total <- unique(cbind(AGR_Proj_Demand_Country_Energy_total_header, AGR_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  AGR_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  AGR_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(AGR_Proj_Demand_Country_Sector_total_value), 1:9]  
  AGR_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  AGR_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  AGR_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  AGR_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  AGR_Proj_Demand_Country_Sector_total_header$Sector <- AGR_Proj_Demand_Country_Sector_total_value$Sector
  AGR_Proj_Demand_Country_Sector_total <- unique(cbind(AGR_Proj_Demand_Country_Sector_total_header, AGR_Proj_Demand_Country_Sector_total_value[,-1]))
  
  input <- rbind(input, unique(cbind( AGR_Proj_Demand_total_sector_header, AGR_Proj_Demand_total_sector_value)))
  input <- rbind(input, AGR_Proj_Demand_Country_Energy_total, AGR_Proj_Demand_Country_Sector_total)
  
  return(input)
}