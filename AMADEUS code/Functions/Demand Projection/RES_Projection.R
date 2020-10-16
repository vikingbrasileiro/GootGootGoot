source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the Residential sector 
############################################################################################################################################################################
main_projection_energy_demand_RES <- function(Hist_Energy_Demand, Proj_Assumption, Proj_Target, Proj_Macro, Hist_Assumption)
{
  #1. Data input extraction 
  # browser()
  RES_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "RES")
  #IND_Hist_Energy_Demand <- droplevels( subset(IND_Hist_Energy_Demand, Sub_Sector != "Industry - energy") )
  RES_Hist_Energy_Demand <- RES_Hist_Energy_Demand[order(RES_Hist_Energy_Demand$Usage),]
  RES_Hist_Energy_Demand <- Sort_By_Country(RES_Hist_Energy_Demand)
  RES_Hist_Energy_Demand <- RES_Hist_Energy_Demand[order(RES_Hist_Energy_Demand$Energy),]
  
  RES_Proj_Assumption <- Extract_Sector(Proj_Assumption, "RES")
  RES_Proj_Target <- Extract_Sector(Proj_Target, "RES")
  #RES_Proj_Assumption <- droplevels( subset(IND_Proj_Assumption, Sub_Sector != "Industry - energy") ) 
  
  RES_Proj_Macro <- Extract_Item(Proj_Macro, c("Floor Area of residential", "Population"))
  
  #2. Defintion of variables compulsory to the projection
  
  #a. Space Heating
  RES_Proj_Demand_Heating <- Extract_Usage(RES_Hist_Energy_Demand, "space heating")
  
  ### Average Heating need
  RES_Average_Heating_need <- Extract_Item(RES_Proj_Assumption, "Heating need")
  RES_Average_Heating_need <- RES_Average_Heating_need[ rep( seq_len( nrow( RES_Average_Heating_need ) ), nlevels(as.factor(RES_Proj_Demand_Heating$Energy)) ), ]
  RES_Average_Heating_need$Energy <- RES_Proj_Demand_Heating$Energy
  RES_Average_Heating_need <- Sort_By_Country(RES_Average_Heating_need)
  RES_Average_Heating_need <- RES_Average_Heating_need[order(RES_Average_Heating_need$Energy),]

  ### Market Shares (Si/S)
  RES_Market_Shares_Heating <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Market shares"), "space heating" )
  RES_Market_Shares_Heating <- Sort_By_Country(RES_Market_Shares_Heating)
  RES_Market_Shares_Heating <- RES_Market_Shares_Heating[order(RES_Market_Shares_Heating$Energy),]

  ### Total Residential Surface (S)
  RES_Total_Surface <- Extract_Item(RES_Proj_Macro, "Floor Area of residential")
  RES_Total_Surface <- RES_Total_Surface[ rep( seq_len( nrow( RES_Total_Surface ) ), nlevels(as.factor(RES_Proj_Demand_Heating$Energy)) ), ]
  RES_Total_Surface$Energy <- RES_Proj_Demand_Heating$Energy
  RES_Total_Surface <-  Sort_By_Country(RES_Total_Surface)
  RES_Total_Surface <- RES_Total_Surface[order(RES_Total_Surface$Energy),]

  ### Equipment Efficiency
  RES_Equipment_Efficiency_Heating <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "System efficiency"), "space heating" )
  RES_Equipment_Efficiency_Heating <- Sort_By_Country(RES_Equipment_Efficiency_Heating)
  RES_Equipment_Efficiency_Heating <- RES_Equipment_Efficiency_Heating[order(RES_Equipment_Efficiency_Heating$Energy),]

  #b. Domestic Hot Water
  
  RES_Proj_Demand_DWH <- Extract_Usage(RES_Hist_Energy_Demand, "water heating")
  
  ### Average DWH need
  RES_Average_DWH_need <- Extract_Item(RES_Proj_Assumption, "Water heating need")
  RES_Average_DWH_need <- RES_Average_DWH_need[ rep( seq_len( nrow( RES_Average_DWH_need ) ), nlevels(as.factor(RES_Proj_Demand_DWH$Energy)) ), ]
  RES_Average_DWH_need$Energy <- RES_Proj_Demand_DWH$Energy
  RES_Average_DWH_need <- Sort_By_Country(RES_Average_DWH_need)
  RES_Average_DWH_need <- RES_Average_DWH_need[order(RES_Average_DWH_need$Energy),]

  ### Market Shares (Pi/P)
  RES_Market_Shares_DWH <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Market shares"), "water heating" )
  RES_Market_Shares_DWH <- Sort_By_Country(RES_Market_Shares_DWH)
  RES_Market_Shares_DWH <- RES_Market_Shares_DWH[order(RES_Market_Shares_DWH$Energy),]

  ### Population (P)
  Population <- Extract_Item(RES_Proj_Macro, "Population")
  Population <- Population[ rep( seq_len( nrow( Population ) ), nlevels(as.factor(RES_Proj_Demand_DWH$Energy)) ), ]
  Population$Energy <- RES_Proj_Demand_DWH$Energy
  Population <-  Sort_By_Country(Population)
  Population <- Population[order(Population$Energy),]

  ### Equipment Efficiency
  RES_Equipment_Efficiency_DWH <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "System efficiency"), "water heating" )
  RES_Equipment_Efficiency_DWH <- Sort_By_Country(RES_Equipment_Efficiency_DWH)
  RES_Equipment_Efficiency_DWH <- RES_Equipment_Efficiency_DWH[order(RES_Equipment_Efficiency_DWH$Energy),]

  #c. Cooking
  RES_Proj_Demand_Cooking <- Extract_Usage(RES_Hist_Energy_Demand, "cooking")

  ### Substitution Rate
  RES_Substitution_Rate_Cooking <- Extract_Specific_Energy( Extract_Usage( Extract_Item(RES_Proj_Assumption, "Substitution Rate"), "cooking" ), levels(as.factor(RES_Proj_Demand_Cooking$Energy)))
  RES_Substitution_Rate_Cooking <- Sort_By_Country(RES_Substitution_Rate_Cooking)
  RES_Substitution_Rate_Cooking <- RES_Substitution_Rate_Cooking[order(RES_Substitution_Rate_Cooking$Energy),]

  ### Efficiency Rate
  RES_Efficiency_Rate_Cooking <- Extract_Specific_Energy( Extract_Usage( Extract_Item(RES_Proj_Assumption, "Efficiency Rate"), "cooking" ), levels(as.factor(RES_Proj_Demand_Cooking$Energy)))
  RES_Efficiency_Rate_Cooking <- Sort_By_Country(RES_Efficiency_Rate_Cooking)
  RES_Efficiency_Rate_Cooking <- RES_Efficiency_Rate_Cooking[order(RES_Efficiency_Rate_Cooking$Energy),]

  ### Activity Rate
  RES_Activity_Rate_Cooking <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Activity Rate"), "cooking" )
  RES_Activity_Rate_Cooking <- Sort_By_Country(RES_Activity_Rate_Cooking)
  RES_Activity_Rate_Cooking <-  RES_Activity_Rate_Cooking[ rep( seq_len( nrow( RES_Activity_Rate_Cooking ) ), nlevels(as.factor(RES_Proj_Demand_Cooking$Energy)) ), ]
  RES_Activity_Rate_Cooking$Energy = as.factor( rep( levels( as.factor( RES_Proj_Demand_Cooking$Energy ) ), each = nlevels( as.factor( RES_Activity_Rate_Cooking$Country) ) ) )

  #d. Lighting
  RES_Proj_Demand_Lighting <- Extract_Usage(RES_Hist_Energy_Demand, "lighting")
  
  ### Efficiency Rate
  RES_Efficiency_Rate_Lighting <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Efficiency Rate"), "lighting" )
  RES_Efficiency_Rate_Lighting <- Sort_By_Country(RES_Efficiency_Rate_Lighting)

  ### Activity Rate
  RES_Activity_Rate_Lighting <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Activity Rate"), "lighting" )
  RES_Activity_Rate_Lighting <- Sort_By_Country(RES_Activity_Rate_Lighting)

  #e. Specific Uses
  
  #large appliances
  RES_Hist_Demand_large_app <- Extract_Usage(RES_Hist_Energy_Demand, "large appliances")
  RES_Proj_Demand_large_app <- RES_Hist_Demand_large_app
  
  ### Efficiency Rate
  RES_Efficiency_Rate_large_app <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Efficiency Rate"), "large appliances" )
  RES_Efficiency_Rate_large_app <- Sort_By_Country(RES_Efficiency_Rate_large_app)
  
  ### Activity Rate
  RES_Activity_Rate_large_app <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Activity Rate"), "large appliances" )
  RES_Activity_Rate_large_app <- Sort_By_Country(RES_Activity_Rate_large_app)
  
  #small appliances
  RES_Hist_Demand_small_app <- Sort_By_Country( Extract_Usage(RES_Hist_Energy_Demand, "small appliances") )
  RES_Proj_Demand_small_app <- RES_Hist_Demand_small_app
  
  RES_Proj_indice_small_app <- Sort_By_Country( Extract_Item(RES_Proj_Target, "Indice small appliances") ) 
  RES_Proj_indice_small_app <- derivate_dataframe(RES_Proj_indice_small_app)
  
  RES_Hist_Demand_per_HH_small_app <- RES_Hist_Demand_small_app
  RES_No_Household <- Sort_By_Country( Extract_Item(Proj_Macro, "No_Household") )
  RES_Hist_Demand_per_HH_small_app[,10:length(RES_Hist_Demand_per_HH_small_app)] <- RES_Hist_Demand_per_HH_small_app[,10:length(RES_Hist_Demand_per_HH_small_app)] / RES_No_Household[,10:length(RES_No_Household)] 
  
  #Definition of a small appliances equipment rate
  Equipment_Rate_small_app <- RES_Hist_Demand_per_HH_small_app
  Equipment_Rate_small_app$ID_Item <- "Equipment Rate"
  Equipment_Rate_small_app$Unit <- "%"
  
  RES_Hist_Demand_per_HH_small_app_FR <- Extract_Country(Equipment_Rate_small_app, "FR")
  RES_Hist_Demand_per_HH_small_app_FR <- RES_Hist_Demand_per_HH_small_app_FR[ rep( seq_len( nrow( RES_Hist_Demand_per_HH_small_app_FR ) ), nlevels(as.factor(RES_Hist_Demand_per_HH_small_app$Country)) ), ]
  Equipment_Rate_small_app[,10:length(Equipment_Rate_small_app)] <- RES_Hist_Demand_per_HH_small_app[,10:length(RES_Hist_Demand_per_HH_small_app)] /  RES_Hist_Demand_per_HH_small_app_FR[,10:length(RES_Hist_Demand_per_HH_small_app_FR)]
  
  Last_year_col = min(which(is.nan(colSums(RES_Hist_Energy_Demand[10:length(RES_Hist_Energy_Demand)])))) + 9 - 1
  Equipment_Rate_small_app$X2040 <- 1
  Equipment_Rate_small_app$X2050 <- 1
  Equipment_Rate_small_app[Equipment_Rate_small_app[,Last_year_col]>1,]$X2040 <- Equipment_Rate_small_app[Equipment_Rate_small_app[,Last_year_col]>1,]$X2015
  Equipment_Rate_small_app[Equipment_Rate_small_app[,Last_year_col]>1,]$X2050 <- Equipment_Rate_small_app[Equipment_Rate_small_app[,Last_year_col]>1,]$X2015
  Equipment_Rate_small_app[Equipment_Rate_small_app$Country == "DE",]$X2040 <- Equipment_Rate_small_app[Equipment_Rate_small_app$Country == "DE",]$X2015
  Equipment_Rate_small_app[Equipment_Rate_small_app$Country == "DE",]$X2050 <- Equipment_Rate_small_app[Equipment_Rate_small_app$Country == "DE",]$X2015
  Equipment_Rate_small_app <- Interpolate_Missing_Values(Equipment_Rate_small_app)
  
  saveRDS(rbind(Hist_Assumption, Equipment_Rate_small_app), "../../Data/temp/Hist_Assumption.RDS")
  
  Activity_Rate_small_app_per_HH <- derivate_dataframe(Equipment_Rate_small_app)
  
  
  
  
  #f. cooling
  
  RES_Proj_Demand_Cooling <- Sort_By_Country(Extract_Usage(RES_Hist_Energy_Demand, "space cooling"))
  RES_Proj_Demand_Cooling <- RES_Proj_Demand_Cooling[order(RES_Proj_Demand_Cooling$Energy),]
  
  ### Average Cooling need
  RES_Average_Cooling_need <- Extract_Item(RES_Proj_Assumption, "Cooling need")
  RES_Average_Cooling_need <- RES_Average_Cooling_need[ rep( seq_len( nrow( RES_Average_Cooling_need ) ), nlevels(as.factor(RES_Proj_Demand_Cooling$Energy)) ), ]
  RES_Average_Cooling_need$Energy <- RES_Proj_Demand_Cooling$Energy
  RES_Average_Cooling_need <- Sort_By_Country(RES_Average_Cooling_need)
  RES_Average_Cooling_need <- RES_Average_Cooling_need[order(RES_Average_Cooling_need$Energy),]

  ### Equipment Rate
  RES_Equipment_Rate_Cooling <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "average_equipment_rate"), "space cooling" )
  RES_Equipment_Rate_Cooling <- RES_Equipment_Rate_Cooling[ rep( seq_len( nrow( RES_Equipment_Rate_Cooling ) ), nlevels(as.factor(RES_Proj_Demand_Cooling$Energy)) ), ]
  RES_Equipment_Rate_Cooling$Energy <- RES_Proj_Demand_Cooling$Energy
  RES_Equipment_Rate_Cooling <- Sort_By_Country(RES_Equipment_Rate_Cooling)
  RES_Equipment_Rate_Cooling <- RES_Equipment_Rate_Cooling[order(RES_Equipment_Rate_Cooling$Energy),]

  ### Total Residential Surface (S) (already extract for heating)
  ### Total Residential Surface (S)
  RES_Total_Surface_cooling <- Extract_Item(RES_Proj_Macro, "Floor Area of residential")
  RES_Total_Surface_cooling <- RES_Total_Surface_cooling[ rep( seq_len( nrow( RES_Total_Surface_cooling ) ), nlevels(as.factor(RES_Proj_Demand_Cooling$Energy)) ), ]
  RES_Total_Surface_cooling$Energy <- RES_Proj_Demand_Cooling$Energy
  RES_Total_Surface_cooling <-  Sort_By_Country(RES_Total_Surface_cooling)
  RES_Total_Surface_cooling <- RES_Total_Surface_cooling[order(RES_Total_Surface_cooling$Energy),]
  
  ### Equipment Efficiency
  RES_Equipment_Efficiency_Cooling <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "System efficiency"), "space cooling" )
  RES_Equipment_Efficiency_Cooling <- Sort_By_Country(RES_Equipment_Efficiency_Cooling)
  RES_Equipment_Efficiency_Cooling <- RES_Equipment_Efficiency_Cooling[order(RES_Equipment_Efficiency_Cooling$Energy),]
  
  RES_Market_Shares_Cooling <- Extract_Usage( Extract_Item(RES_Proj_Assumption, "Market shares"), "space cooling" )
  RES_Market_Shares_Cooling <- Sort_By_Country(RES_Market_Shares_Cooling)
  RES_Market_Shares_Cooling <- RES_Market_Shares_Cooling[order(RES_Market_Shares_Cooling$Energy),]

  #3. Projection of energy demand in industry sector 

  Year_Proj_col = min(which(is.nan(colSums(RES_Hist_Energy_Demand[10:length(RES_Hist_Energy_Demand)])))) + 9
  
  for(i in Year_Proj_col:length(RES_Hist_Energy_Demand))
  {
    RES_Proj_Demand_Heating[,i] <- RES_Market_Shares_Heating[,i] * RES_Total_Surface[,i] * RES_Average_Heating_need[,i] / (RES_Equipment_Efficiency_Heating[,i] * 1000) 
    RES_Proj_Demand_DWH[,i] <- RES_Market_Shares_DWH[,i] * Population[,i] * RES_Average_DWH_need[,i] / (RES_Equipment_Efficiency_DWH[,i] * 1000000)
    RES_Proj_Demand_Cooking[,i] <- RES_Proj_Demand_Cooking[,i-1] * (1 + RES_Efficiency_Rate_Cooking[,i]) * (1 + RES_Activity_Rate_Cooking[,i]) * (1 + RES_Substitution_Rate_Cooking[,i])
    RES_Proj_Demand_Lighting[,i] <- RES_Proj_Demand_Lighting[,i-1] * (1 + RES_Efficiency_Rate_Lighting[,i]) * (1 + RES_Activity_Rate_Lighting[,i])
    RES_Proj_Demand_large_app[,i] <- RES_Proj_Demand_large_app[,i-1] * (1 + RES_Efficiency_Rate_large_app[,i]) * (1 + RES_Activity_Rate_large_app[,i])
    
    RES_Hist_Demand_per_HH_small_app[,i] <- RES_Hist_Demand_per_HH_small_app[,i-1] * ( 1 + RES_Proj_indice_small_app[,i]) * (1 + Activity_Rate_small_app_per_HH[,i])
    RES_Proj_Demand_small_app[,i] <- RES_Hist_Demand_per_HH_small_app[,i] * RES_No_Household[,i]
    
    RES_Proj_Demand_Cooling[,i] <- RES_Market_Shares_Cooling[,i] * RES_Equipment_Rate_Cooling[,i] * RES_Total_Surface_cooling[,i] * RES_Average_Cooling_need[,i] / (RES_Equipment_Efficiency_Cooling[,i] * 1000)
    
    # IND_Proj_Demand_Efficiency[,i] <- IND_Proj_Demand_Efficiency[,i-1] * (1+IND_Efficiency_Rate[,i]) 
    # IND_Proj_Demand_Activity[,i] <- IND_Proj_Demand_Activity[,i-1] * (1+IND_Activity_Rate[,i]) 
    # IND_Proj_Demand_Substitution[,i] <- IND_Proj_Demand_Substitution[,i-1] * (1+IND_Substitution_Rate[,i])  
  }
  
  # IND_Proj_Demand_Efficiency <- calculate_total_sector(IND_Proj_Demand_Efficiency)
  # IND_Proj_Demand_Activity <- calculate_total_sector(IND_Proj_Demand_Activity)
  # IND_Proj_Demand_Substitution <- calculate_total_sector(IND_Proj_Demand_Substitution)
  
  RES_Proj_Demand <- rbind(RES_Proj_Demand_Cooling, RES_Proj_Demand_Heating, RES_Proj_Demand_DWH, RES_Proj_Demand_Cooking, RES_Proj_Demand_Lighting, RES_Proj_Demand_large_app, RES_Proj_Demand_small_app)
  RES_Proj_Demand$Sub_Sector <- as.factor("total")
  RES_Proj_Demand <- calculate_total_sector_RES(RES_Proj_Demand)
  
  return(RES_Proj_Demand) 
}

calculate_total_sector_RES <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  #on calcule le total secteur pour chaques Usages
  RES_Proj_Demand_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Usage")], FUN = sum)
  RES_Proj_Demand_Usage_total_header  <- input[1:nrow(RES_Proj_Demand_Usage_total_value), 1:9]  
  RES_Proj_Demand_Usage_total_header$Energy <- "total"
  RES_Proj_Demand_Usage_total_header$Country <- RES_Proj_Demand_Usage_total_value$Country
  RES_Proj_Demand_Usage_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Usage_total_header$Usage <- RES_Proj_Demand_Usage_total_value$Usage
  RES_Proj_Demand_Usage_total <- unique(cbind(RES_Proj_Demand_Usage_total_header, RES_Proj_Demand_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energys
  RES_Proj_Demand_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy")], FUN = sum)
  RES_Proj_Demand_Energy_total_header  <- input[1:nrow(RES_Proj_Demand_Energy_total_value), 1:9]  
  RES_Proj_Demand_Energy_total_header$Energy <- RES_Proj_Demand_Energy_total_value$Energy
  RES_Proj_Demand_Energy_total_header$Country <- RES_Proj_Demand_Energy_total_value$Country
  RES_Proj_Demand_Energy_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Energy_total_header$Usage <- "total"
  RES_Proj_Demand_Energy_total <- unique(cbind(RES_Proj_Demand_Energy_total_header, RES_Proj_Demand_Energy_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sectors
  RES_Proj_Demand_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sector")], FUN = sum)
  RES_Proj_Demand_Sector_total_header  <- input[1:nrow(RES_Proj_Demand_Sector_total_value), 1:9]  
  RES_Proj_Demand_Sector_total_header$Energy <- "total"
  RES_Proj_Demand_Sector_total_header$Country <- RES_Proj_Demand_Sector_total_value$Country
  RES_Proj_Demand_Sector_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Sector_total_header$Usage <- "total"
  RES_Proj_Demand_Sector_total <- unique(cbind(RES_Proj_Demand_Sector_total_header, RES_Proj_Demand_Sector_total_value[,-(1:2)]))
  
  #on calcule le total EU23 pour chaques energy
  RES_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  RES_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(RES_Proj_Demand_Country_Energy_total_value), 1:9]  
  RES_Proj_Demand_Country_Energy_total_header$Energy <- RES_Proj_Demand_Country_Energy_total_value$Energy
  RES_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  RES_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  RES_Proj_Demand_Country_Energy_total <- unique(cbind(RES_Proj_Demand_Country_Energy_total_header, RES_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques usages
  RES_Proj_Demand_Country_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Usage")], FUN = sum)
  RES_Proj_Demand_Country_Usage_total_header  <- input[1:nrow(RES_Proj_Demand_Country_Usage_total_value), 1:9]  
  RES_Proj_Demand_Country_Usage_total_header$Energy <- "total"
  RES_Proj_Demand_Country_Usage_total_header$Country <- "EU23"
  RES_Proj_Demand_Country_Usage_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Country_Usage_total_header$Usage <- RES_Proj_Demand_Country_Usage_total_value$Usage
  RES_Proj_Demand_Country_Usage_total <- unique(cbind(RES_Proj_Demand_Country_Usage_total_header, RES_Proj_Demand_Country_Usage_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  RES_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  RES_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(RES_Proj_Demand_Country_Sector_total_value), 1:9]  
  RES_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  RES_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  RES_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  RES_Proj_Demand_Country_Sector_total_header$Sector <- RES_Proj_Demand_Country_Sector_total_value$Sector
  RES_Proj_Demand_Country_Sector_total <- unique(cbind(RES_Proj_Demand_Country_Sector_total_header, RES_Proj_Demand_Country_Sector_total_value[,-1]))
  
  #on calcule le total secteur pour chaques Energy/Usage
  RES_Proj_Demand_Country_Energy_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Usage")], FUN = sum)
  RES_Proj_Demand_Country_Energy_Usage_total_header  <- input[1:nrow(RES_Proj_Demand_Country_Energy_Usage_total_value), 1:9]  
  RES_Proj_Demand_Country_Energy_Usage_total_header$Energy <- RES_Proj_Demand_Country_Energy_Usage_total_value$Energy
  RES_Proj_Demand_Country_Energy_Usage_total_header$Country <- "EU23"
  RES_Proj_Demand_Country_Energy_Usage_total_header$Sub_Sector <- "total"
  RES_Proj_Demand_Country_Energy_Usage_total_header$Usage <- RES_Proj_Demand_Country_Energy_Usage_total_value$Usage
  RES_Proj_Demand_Country_Energy_Usage_total <- unique(cbind(RES_Proj_Demand_Country_Energy_Usage_total_header, RES_Proj_Demand_Country_Energy_Usage_total_value[,-(1:2)]))
  
  input <- rbind(input, RES_Proj_Demand_Energy_total, RES_Proj_Demand_Usage_total, RES_Proj_Demand_Sector_total, RES_Proj_Demand_Country_Energy_total,
                 RES_Proj_Demand_Country_Usage_total, RES_Proj_Demand_Country_Sector_total, RES_Proj_Demand_Country_Energy_Usage_total) 
  
  return(input)
}