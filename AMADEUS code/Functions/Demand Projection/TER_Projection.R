source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the Tertiary sector 
############################################################################################################################################################################
main_projection_energy_demand_TER <- function(Hist_Energy_Demand, Proj_Assumption, Proj_Macro, Proj_Target)
{
  #1. Data input extraction

  TER_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "TER")
  TER_Hist_Energy_Demand <- TER_Hist_Energy_Demand[order(TER_Hist_Energy_Demand$Usage),]
  TER_Hist_Energy_Demand <- Sort_By_Country(TER_Hist_Energy_Demand)
  TER_Hist_Energy_Demand <- TER_Hist_Energy_Demand[order(TER_Hist_Energy_Demand$Energy),]

  TER_Proj_Assumption <- Extract_Sector(Proj_Assumption, "TER")
  TER_Proj_Macro <- Extract_Item(Proj_Macro, "Floor area of services")

  #2. Defintion of variables compulsory to the projection

  #a. Space Heating
  TER_Proj_Demand_Heating <- Extract_Usage(TER_Hist_Energy_Demand, "space heating")

  ### Average Heating need
  TER_Average_Heating_need <- Extract_Item(TER_Proj_Assumption, "Heating need")
  TER_Average_Heating_need <- TER_Average_Heating_need[ rep( seq_len( nrow( TER_Average_Heating_need ) ), nlevels( as.factor( TER_Proj_Demand_Heating$Energy) ) ), ]
  TER_Average_Heating_need$Energy <- TER_Proj_Demand_Heating$Energy
  TER_Average_Heating_need <- Sort_By_Country(TER_Average_Heating_need)
  TER_Average_Heating_need <- TER_Average_Heating_need[order(TER_Average_Heating_need$Energy),]

  ### Market Shares (Si/S)
  TER_Market_Shares_Heating <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Market shares"), "space heating" )
  TER_Market_Shares_Heating <- Sort_By_Country(TER_Market_Shares_Heating)
  TER_Market_Shares_Heating <- TER_Market_Shares_Heating[order(TER_Market_Shares_Heating$Energy),]

  ### Total tertiary Surface (S)
  TER_Total_Surface <- Extract_Item(TER_Proj_Macro, "Floor area of services")
  TER_Total_Surface <- TER_Total_Surface[ rep( seq_len( nrow( TER_Total_Surface ) ), nlevels(as.factor(TER_Proj_Demand_Heating$Energy)) ), ]
  TER_Total_Surface$Energy <- TER_Proj_Demand_Heating$Energy
  TER_Total_Surface <-  Sort_By_Country(TER_Total_Surface)
  TER_Total_Surface <- TER_Total_Surface[order(TER_Total_Surface$Energy),]

  ### Equipment Efficiency
  TER_Equipment_Efficiency_Heating <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "System efficiency"), "space heating" )
  TER_Equipment_Efficiency_Heating <- Sort_By_Country(TER_Equipment_Efficiency_Heating)
  TER_Equipment_Efficiency_Heating <- TER_Equipment_Efficiency_Heating[order(TER_Equipment_Efficiency_Heating$Energy),]

  # on supprime hydrogen for the moment
  TER_Equipment_Efficiency_Heating <- droplevels(subset(TER_Equipment_Efficiency_Heating, Energy!="sd hydrogen"))

  #b. Domestic Hot Water

  TER_Proj_Demand_DWH <- Extract_Usage(TER_Hist_Energy_Demand, "water heating")

  ### Average DWH need
  TER_Average_DWH_need <- Extract_Item(TER_Proj_Assumption, "Water heating need")
  TER_Average_DWH_need <- TER_Average_DWH_need[ rep( seq_len( nrow( TER_Average_DWH_need ) ), nlevels(as.factor(TER_Proj_Demand_DWH$Energy)) ), ]
  TER_Average_DWH_need$Energy <- TER_Proj_Demand_DWH$Energy
  TER_Average_DWH_need <- Sort_By_Country(TER_Average_DWH_need)
  TER_Average_DWH_need <- TER_Average_DWH_need[order(TER_Average_DWH_need$Energy),]

  ### Market Shares (Si/S)
  TER_Market_Shares_DWH <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Market shares"), "water heating" )
  TER_Market_Shares_DWH <- Sort_By_Country(TER_Market_Shares_DWH)
  TER_Market_Shares_DWH <- TER_Market_Shares_DWH[order(TER_Market_Shares_DWH$Energy),]

  ### Equipment Efficiency
  TER_Equipment_Efficiency_DWH <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "System efficiency"), "water heating" )
  TER_Equipment_Efficiency_DWH <- Sort_By_Country(TER_Equipment_Efficiency_DWH)
  TER_Equipment_Efficiency_DWH <- TER_Equipment_Efficiency_DWH[order(TER_Equipment_Efficiency_DWH$Energy),]

  #??? on supprime hydrogen for the moment
  TER_Equipment_Efficiency_DWH <- droplevels(subset(TER_Equipment_Efficiency_DWH, Energy!="SD Hydrogen"))

  #c. Cooking
  TER_Proj_Demand_Cooking <- Extract_Usage(TER_Hist_Energy_Demand, "cooking")

  ### Substitution Rate
  TER_Substitution_Rate_Cooking <- Extract_Specific_Energy( Extract_Usage( Extract_Item(TER_Proj_Assumption, "Substitution Rate"), "cooking" ), levels(as.factor(TER_Proj_Demand_Cooking$Energy)))
  TER_Substitution_Rate_Cooking <- Sort_By_Country(TER_Substitution_Rate_Cooking)
  TER_Substitution_Rate_Cooking <- TER_Substitution_Rate_Cooking[order(TER_Substitution_Rate_Cooking$Energy),]

  ### Efficiency Rate
  TER_Efficiency_Rate_Cooking <- Extract_Specific_Energy( Extract_Usage( Extract_Item(TER_Proj_Assumption, "Efficiency Rate"), "cooking" ), levels(as.factor(TER_Proj_Demand_Cooking$Energy)))
  TER_Efficiency_Rate_Cooking <- Sort_By_Country(TER_Efficiency_Rate_Cooking)
  TER_Efficiency_Rate_Cooking <- TER_Efficiency_Rate_Cooking[order(TER_Efficiency_Rate_Cooking$Energy),]

  ### Activity Rate
  TER_Activity_Rate_Cooking <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Activity Rate"), "cooking" )
  TER_Activity_Rate_Cooking <- Sort_By_Country(TER_Activity_Rate_Cooking)
  TER_Activity_Rate_Cooking <-  TER_Activity_Rate_Cooking[ rep( seq_len( nrow( TER_Activity_Rate_Cooking ) ), nlevels(as.factor(TER_Proj_Demand_Cooking$Energy)) ), ]
  TER_Activity_Rate_Cooking$Energy = as.factor( rep( levels( as.factor( TER_Proj_Demand_Cooking$Energy ) ), each = nlevels( as.factor( TER_Activity_Rate_Cooking$Country ) ) ) ) 
  
  #d. Lighting
  
  TER_Proj_Demand_Lighting <- Extract_Usage(TER_Hist_Energy_Demand, "lighting")
  
  ### Efficiency Rate
  TER_Efficiency_Rate_Lighting <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Efficiency Rate"), "lighting" )
  TER_Efficiency_Rate_Lighting <- Sort_By_Country(TER_Efficiency_Rate_Lighting)
  
  ### Activity Rate
  TER_Activity_Rate_Lighting <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Activity Rate"), "lighting" )
  TER_Activity_Rate_Lighting <- Sort_By_Country(TER_Activity_Rate_Lighting)
  
  #e. Specific Uses
  
  TER_Hist_Demand_SU <- Extract_Usage(TER_Hist_Energy_Demand, "specific uses")
  TER_Proj_Demand_SU <- TER_Hist_Demand_SU

  ### Efficiency Rate
  TER_Efficiency_Rate_SU <- Extract_Usage( Extract_Item(Proj_Target, "Efficiency_elec_spe_indice"), "specific uses" )
  TER_Efficiency_Rate_SU <- Sort_By_Country(TER_Efficiency_Rate_SU)
  TER_Efficiency_Rate_SU <- derivate_dataframe(TER_Efficiency_Rate_SU)

  ### Activity Rate
  TER_Activity_Rate_SU <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Activity Rate"), "specific uses" )
  TER_Activity_Rate_SU <- Sort_By_Country(TER_Activity_Rate_SU)
  
  # ### Equipment rate

  TER_Equipment_Rate_SU <- TER_Activity_Rate_SU
  TER_Equipment_Rate_SU[,10:length(TER_Equipment_Rate_SU)] <- NA
  TER_Equipment_Rate_SU[,10:25] <- 0
  TER_Equipment_Rate_SU$ID_Item <- "Equipment Rate"
  TER_Equipment_Rate_SU$Unit <- "equipment/m²"
  TER_Equipment_Rate_SU$X2016 <- 1

  TER_Total_Surface_SU <- Extract_Item(TER_Proj_Macro, "Floor area of services")
  TER_Total_Surface_SU_FR <- Extract_Country(TER_Total_Surface_SU, "FR")
  TER_Demand_SU_FR <- Extract_Country(TER_Hist_Demand_SU, "FR")

  #Cibles definit dans le BP RTE 2017
  TER_Demand_SU_FR$X2035 <- TER_Demand_SU_FR$X2016 * (15.9/21)

  TER_Unitary_Consumption <- TER_Activity_Rate_SU
  TER_Unitary_Consumption[,10:length(TER_Unitary_Consumption)] <- NA
  TER_Unitary_Consumption[,10:25] <- 0
  TER_Unitary_Consumption$ID_Item <- "Unitary Consumption"
  TER_Unitary_Consumption$Unit <- "kWh/equipment"
  TER_Unitary_Consumption[TER_Unitary_Consumption$Country == "FR",]$X2016 <- TER_Demand_SU_FR$X2016 /  TER_Total_Surface_SU_FR$X2016 * 1000
  TER_Unitary_Consumption[TER_Unitary_Consumption$Country == "FR",]$X2035 <- TER_Demand_SU_FR$X2035 /  ( 1.3 * TER_Total_Surface_SU_FR$X2016 ) * 1000
  TER_Unitary_Consumption$X2016 <- TER_Unitary_Consumption[TER_Unitary_Consumption$Country == "FR",]$X2016
  TER_Unitary_Consumption$X2035 <- TER_Unitary_Consumption[TER_Unitary_Consumption$Country == "FR",]$X2035
  
  CAGR_Unitary_Consumption_2016_2035_FR <- ( TER_Unitary_Consumption[TER_Unitary_Consumption$Country == "FR",]$X2035 / TER_Unitary_Consumption[TER_Unitary_Consumption$Country == "FR",]$X2016)^(1/(2035-2016)) - 1
  CAGR_Unitary_Consumption_2016_2035_FR <- (2/3) * CAGR_Unitary_Consumption_2016_2035_FR
  
  TER_Unitary_Consumption$X2035 <- TER_Unitary_Consumption$X2016 * (1 + CAGR_Unitary_Consumption_2016_2035_FR)^(2035-2016)  
  TER_Unitary_Consumption$X2050 <- TER_Unitary_Consumption$X2035 * (1 + CAGR_Unitary_Consumption_2016_2035_FR)^(2050-2035)
  TER_Unitary_Consumption <- Interpolate_Missing_Values(TER_Unitary_Consumption)

  TER_Equipment_Rate_SU$X2016 <-  TER_Hist_Demand_SU$X2016 / ( TER_Unitary_Consumption$X2016 * TER_Total_Surface_SU$X2016 ) * 1000
  TER_Equipment_Rate_SU[TER_Equipment_Rate_SU$Country == "FR",]$X2035 <- 1.3 * TER_Equipment_Rate_SU[TER_Equipment_Rate_SU$Country == "FR",]$X2016  * TER_Total_Surface_SU_FR$X2016 / TER_Total_Surface_SU_FR$X2035 
  CAGR_Equipment_Rate_2016_2035_FR <- ( TER_Equipment_Rate_SU[TER_Equipment_Rate_SU$Country == "FR",]$X2035 / TER_Equipment_Rate_SU[TER_Equipment_Rate_SU$Country == "FR",]$X2016)^(1/(2035-2016)) - 1
  TER_Equipment_Rate_SU[TER_Equipment_Rate_SU$Country == "FR",]$X2050 <- TER_Equipment_Rate_SU[TER_Equipment_Rate_SU$Country == "FR",]$X2035 * (1 + CAGR_Equipment_Rate_2016_2035_FR)^(2050-2035)
  TER_Equipment_Rate_SU$X2050 <- TER_Equipment_Rate_SU$X2016 * (1 + CAGR_Equipment_Rate_2016_2035_FR)^(2050-2016)
  TER_Equipment_Rate_SU <- Interpolate_Missing_Values(TER_Equipment_Rate_SU)
  
  saveRDS(rbind(Proj_Assumption, TER_Unitary_Consumption, TER_Equipment_Rate_SU), "../../Proj_Assumption.RDS")

  #f. cooling
  
  TER_Proj_Demand_Cooling <- Sort_By_Country(Extract_Usage(TER_Hist_Energy_Demand, "space cooling"))
  TER_Proj_Demand_Cooling <- TER_Proj_Demand_Cooling[order(TER_Proj_Demand_Cooling$Energy),]
  
  ### Average Cooling need
  TER_Average_Cooling_need <- Extract_Item(TER_Proj_Assumption, "Cooling need")
  TER_Average_Cooling_need <- TER_Average_Cooling_need[ rep( seq_len( nrow( TER_Average_Cooling_need ) ), nlevels(as.factor(TER_Proj_Demand_Cooling$Energy)) ), ]
  TER_Average_Cooling_need$Energy <- TER_Proj_Demand_Cooling$Energy
  TER_Average_Cooling_need <- Sort_By_Country(TER_Average_Cooling_need)
  TER_Average_Cooling_need <- TER_Average_Cooling_need[order(TER_Average_Cooling_need$Energy),]
  
  ### Equipment Rate
  TER_Equipment_Rate_Cooling <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "average_equipment_rate"), "space cooling" )
  TER_Equipment_Rate_Cooling <- TER_Equipment_Rate_Cooling[ rep( seq_len( nrow( TER_Equipment_Rate_Cooling ) ), nlevels(as.factor(TER_Proj_Demand_Cooling$Energy)) ), ]
  TER_Equipment_Rate_Cooling$Energy <- TER_Proj_Demand_Cooling$Energy
  TER_Equipment_Rate_Cooling <- Sort_By_Country(TER_Equipment_Rate_Cooling)
  TER_Equipment_Rate_Cooling <- TER_Equipment_Rate_Cooling[order(TER_Equipment_Rate_Cooling$Energy),]
  
  ### Total tertiary Surface (S) (already extract for heating)
  ### Total tertiary Surface (S)
  TER_Total_Surface_cooling <- Extract_Item(TER_Proj_Macro, "Floor area of services")
  TER_Total_Surface_cooling <- TER_Total_Surface_cooling[ rep( seq_len( nrow( TER_Total_Surface_cooling ) ), nlevels(as.factor(TER_Proj_Demand_Cooling$Energy)) ), ]
  TER_Total_Surface_cooling$Energy <- TER_Proj_Demand_Cooling$Energy
  TER_Total_Surface_cooling <-  Sort_By_Country(TER_Total_Surface_cooling)
  TER_Total_Surface_cooling <- TER_Total_Surface_cooling[order(TER_Total_Surface_cooling$Energy),]
  
  ### Equipment Efficiency
  TER_Equipment_Efficiency_Cooling <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "System efficiency"), "space cooling" )
  TER_Equipment_Efficiency_Cooling <- Sort_By_Country(TER_Equipment_Efficiency_Cooling)
  TER_Equipment_Efficiency_Cooling <- TER_Equipment_Efficiency_Cooling[order(TER_Equipment_Efficiency_Cooling$Energy),]
  
  ### Market shares
  TER_Market_Shares_Cooling <- Extract_Usage( Extract_Item(TER_Proj_Assumption, "Market shares"), "space cooling" )
  TER_Market_Shares_Cooling <- Sort_By_Country(TER_Market_Shares_Cooling)
  TER_Market_Shares_Cooling <- TER_Market_Shares_Cooling[order(TER_Market_Shares_Cooling$Energy),]
  
  #f. Data centers
  
  TER_Proj_Demand_Data_centers <- Sort_By_Country(Extract_Usage(TER_Hist_Energy_Demand, "data centers"))
  
  TER_Proj_Indice_Data_centers <- Sort_By_Country(Extract_Usage(Extract_Item(Proj_Target, "indice_data_center"), "data centers"))
  TER_Proj_Indice_Data_centers <- derivate_dataframe(TER_Proj_Indice_Data_centers)
  
  #3. Projection of energy demand in industry sector 
  
  Year_Proj_col = min(which(is.nan(colSums(TER_Hist_Energy_Demand[10:length(TER_Hist_Energy_Demand)])))) + 9
  
  for(i in Year_Proj_col:length(TER_Hist_Energy_Demand))
  {
    TER_Proj_Demand_Heating[,i] <- TER_Market_Shares_Heating[,i] * TER_Total_Surface[,i] * TER_Average_Heating_need[,i] / (TER_Equipment_Efficiency_Heating[,i] * 1000) 
    TER_Proj_Demand_DWH[,i] <- TER_Market_Shares_DWH[,i] * TER_Total_Surface[,i] * TER_Average_DWH_need[,i] / (TER_Equipment_Efficiency_DWH[,i] * 1000)
    TER_Proj_Demand_Cooking[,i] <- TER_Proj_Demand_Cooking[,i-1] * (1 + TER_Efficiency_Rate_Cooking[,i]) * (1 + TER_Activity_Rate_Cooking[,i]) * (1 + TER_Substitution_Rate_Cooking[,i])
    TER_Proj_Demand_Lighting[,i] <- TER_Proj_Demand_Lighting[,i-1] * (1 + TER_Efficiency_Rate_Lighting[,i]) * (1 + TER_Activity_Rate_Lighting[,i])
    TER_Proj_Demand_SU[,i] <- TER_Total_Surface_SU[,i] * TER_Unitary_Consumption[,i] * TER_Equipment_Rate_SU[,i] / 1000
    TER_Proj_Demand_Cooling[,i] <- TER_Market_Shares_Cooling[,i] * TER_Equipment_Rate_Cooling[,i] * TER_Total_Surface_cooling[,i] * TER_Average_Cooling_need[,i] / (TER_Equipment_Efficiency_Cooling[,i] * 1000)
    TER_Proj_Demand_Data_centers[,i] <- TER_Proj_Demand_Data_centers[,i-1] * ( 1 +  TER_Proj_Indice_Data_centers[,i] )
    
  }

  
  TER_Proj_Demand <- rbind(TER_Proj_Demand_Cooling, TER_Proj_Demand_Heating, TER_Proj_Demand_DWH, TER_Proj_Demand_Cooking, TER_Proj_Demand_Lighting, TER_Proj_Demand_SU, TER_Proj_Demand_Data_centers)
  
  TER_Proj_Demand <- calculate_total_sector_TER(TER_Proj_Demand)
  
  return(TER_Proj_Demand) 
}

calculate_total_sector_TER <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  #on recalcule le total pour chaque branche industrielle 
  TER_Proj_Demand_Sub_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sub_Sector")], FUN = sum)
  TER_Proj_Demand_Sub_Sector_total_header  <- input[1:nrow(TER_Proj_Demand_Sub_Sector_total_value), 1:9]  
  TER_Proj_Demand_Sub_Sector_total_header$Energy <- "total"
  TER_Proj_Demand_Sub_Sector_total_header$Country <- TER_Proj_Demand_Sub_Sector_total_value$Country
  TER_Proj_Demand_Sub_Sector_total_header$Sub_Sector <- TER_Proj_Demand_Sub_Sector_total_value$Sub_Sector
  TER_Proj_Demand_Sub_Sector_total_header$Usage <- "total"
  TER_Proj_Demand_Sub_Sector_total <- unique(cbind(TER_Proj_Demand_Sub_Sector_total_header, TER_Proj_Demand_Sub_Sector_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Usages
  TER_Proj_Demand_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Usage")], FUN = sum)
  TER_Proj_Demand_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Usage_total_value), 1:9]  
  TER_Proj_Demand_Usage_total_header$Energy <- "total"
  TER_Proj_Demand_Usage_total_header$Country <- TER_Proj_Demand_Usage_total_value$Country
  TER_Proj_Demand_Usage_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Usage_total_header$Usage <- TER_Proj_Demand_Usage_total_value$Usage
  TER_Proj_Demand_Usage_total <- unique(cbind(TER_Proj_Demand_Usage_total_header, TER_Proj_Demand_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energys
  TER_Proj_Demand_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy")], FUN = sum)
  TER_Proj_Demand_Energy_total_header  <- input[1:nrow(TER_Proj_Demand_Energy_total_value), 1:9]  
  TER_Proj_Demand_Energy_total_header$Energy <- TER_Proj_Demand_Energy_total_value$Energy
  TER_Proj_Demand_Energy_total_header$Country <- TER_Proj_Demand_Energy_total_value$Country
  TER_Proj_Demand_Energy_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Energy_total_header$Usage <- "total"
  TER_Proj_Demand_Energy_total <- unique(cbind(TER_Proj_Demand_Energy_total_header, TER_Proj_Demand_Energy_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sectors
  TER_Proj_Demand_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sector")], FUN = sum)
  TER_Proj_Demand_Sector_total_header  <- input[1:nrow(TER_Proj_Demand_Sector_total_value), 1:9]  
  TER_Proj_Demand_Sector_total_header$Energy <- "total"
  TER_Proj_Demand_Sector_total_header$Country <- TER_Proj_Demand_Sector_total_value$Country
  TER_Proj_Demand_Sector_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Sector_total_header$Usage <- "total"
  TER_Proj_Demand_Sector_total <- unique(cbind(TER_Proj_Demand_Sector_total_header, TER_Proj_Demand_Sector_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energy/Sub_Sector
  TER_Proj_Demand_Energy_Sub_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy", "Sub_Sector")], FUN = sum)
  TER_Proj_Demand_Energy_Sub_total_header  <- input[1:nrow(TER_Proj_Demand_Energy_Sub_total_value), 1:9]  
  TER_Proj_Demand_Energy_Sub_total_header$Energy <- TER_Proj_Demand_Energy_Sub_total_value$Energy
  TER_Proj_Demand_Energy_Sub_total_header$Country <- TER_Proj_Demand_Energy_Sub_total_value$Country
  TER_Proj_Demand_Energy_Sub_total_header$Sub_Sector <- TER_Proj_Demand_Energy_Sub_total_value$Sub_Sector
  TER_Proj_Demand_Energy_Sub_total_header$Usage <- "total"
  TER_Proj_Demand_Energy_Sub_total <- unique(cbind(TER_Proj_Demand_Energy_Sub_total_header, TER_Proj_Demand_Energy_Sub_total_value[,-(1:3)]))
  
  #on calcule le total secteur pour chaques Energy/Usage
  TER_Proj_Demand_Energy_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy", "Usage")], FUN = sum)
  TER_Proj_Demand_Energy_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Energy_Usage_total_value), 1:9]  
  TER_Proj_Demand_Energy_Usage_total_header$Energy <- TER_Proj_Demand_Energy_Usage_total_value$Energy
  TER_Proj_Demand_Energy_Usage_total_header$Country <- TER_Proj_Demand_Energy_Usage_total_value$Country
  TER_Proj_Demand_Energy_Usage_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Energy_Usage_total_header$Usage <- TER_Proj_Demand_Energy_Usage_total_value$Usage
  TER_Proj_Demand_Energy_Usage_total <- unique(cbind(TER_Proj_Demand_Energy_Usage_total_header, TER_Proj_Demand_Energy_Usage_total_value[,-(1:3)]))
  
  #on calcule le total secteur pour chaques Sub_Sector/Usage
  TER_Proj_Demand_Sub_Sector_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sub_Sector", "Usage")], FUN = sum)
  TER_Proj_Demand_Sub_Sector_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Sub_Sector_Usage_total_value), 1:9]  
  TER_Proj_Demand_Sub_Sector_Usage_total_header$Energy <- "total"
  TER_Proj_Demand_Sub_Sector_Usage_total_header$Country <- TER_Proj_Demand_Sub_Sector_Usage_total_value$Country
  TER_Proj_Demand_Sub_Sector_Usage_total_header$Sub_Sector <- TER_Proj_Demand_Sub_Sector_Usage_total_value$Sub_Sector
  TER_Proj_Demand_Sub_Sector_Usage_total_header$Usage <- TER_Proj_Demand_Sub_Sector_Usage_total_value$Usage
  TER_Proj_Demand_Sub_Sector_Usage_total <- unique(cbind(TER_Proj_Demand_Sub_Sector_Usage_total_header, TER_Proj_Demand_Sub_Sector_Usage_total_value[,-(1:3)]))
  
  #on calcule le total EU23 pour chaques energy
  TER_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  TER_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Energy_total_value), 1:9]  
  TER_Proj_Demand_Country_Energy_total_header$Energy <- TER_Proj_Demand_Country_Energy_total_value$Energy
  TER_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  TER_Proj_Demand_Country_Energy_total <- unique(cbind(TER_Proj_Demand_Country_Energy_total_header, TER_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques usages
  TER_Proj_Demand_Country_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Usage")], FUN = sum)
  TER_Proj_Demand_Country_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Usage_total_value), 1:9]  
  TER_Proj_Demand_Country_Usage_total_header$Energy <- "total"
  TER_Proj_Demand_Country_Usage_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Usage_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Country_Usage_total_header$Usage <- TER_Proj_Demand_Country_Usage_total_value$Usage
  TER_Proj_Demand_Country_Usage_total <- unique(cbind(TER_Proj_Demand_Country_Usage_total_header, TER_Proj_Demand_Country_Usage_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  TER_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  TER_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Sector_total_value), 1:9]  
  TER_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  TER_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  TER_Proj_Demand_Country_Sector_total_header$Sector <- TER_Proj_Demand_Country_Sector_total_value$Sector
  TER_Proj_Demand_Country_Sector_total <- unique(cbind(TER_Proj_Demand_Country_Sector_total_header, TER_Proj_Demand_Country_Sector_total_value[,-1]))
  
  #on calcule le total secteur pour chaques Energy/Sub_Sector
  TER_Proj_Demand_Country_Energy_Sub_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Sub_Sector")], FUN = sum)
  TER_Proj_Demand_Country_Energy_Sub_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Energy_Sub_total_value), 1:9]  
  TER_Proj_Demand_Country_Energy_Sub_total_header$Energy <- TER_Proj_Demand_Country_Energy_Sub_total_value$Energy
  TER_Proj_Demand_Country_Energy_Sub_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Energy_Sub_total_header$Sub_Sector <- TER_Proj_Demand_Country_Energy_Sub_total_value$Sub_Sector
  TER_Proj_Demand_Country_Energy_Sub_total_header$Usage <- "total"
  TER_Proj_Demand_Country_Energy_Sub_total <- unique(cbind(TER_Proj_Demand_Country_Energy_Sub_total_header, TER_Proj_Demand_Country_Energy_Sub_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energy/Usage
  TER_Proj_Demand_Country_Energy_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Usage")], FUN = sum)
  TER_Proj_Demand_Country_Energy_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Energy_Usage_total_value), 1:9]  
  TER_Proj_Demand_Country_Energy_Usage_total_header$Energy <- TER_Proj_Demand_Country_Energy_Usage_total_value$Energy
  TER_Proj_Demand_Country_Energy_Usage_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Energy_Usage_total_header$Sub_Sector <- "total"
  TER_Proj_Demand_Country_Energy_Usage_total_header$Usage <- TER_Proj_Demand_Country_Energy_Usage_total_value$Usage
  TER_Proj_Demand_Country_Energy_Usage_total <- unique(cbind(TER_Proj_Demand_Country_Energy_Usage_total_header, TER_Proj_Demand_Country_Energy_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sub_Sector/Usage
  TER_Proj_Demand_Country_Sub_Sector_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sub_Sector", "Usage")], FUN = sum)
  TER_Proj_Demand_Country_Sub_Sector_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Sub_Sector_Usage_total_value), 1:9]  
  TER_Proj_Demand_Country_Sub_Sector_Usage_total_header$Energy <- "total"
  TER_Proj_Demand_Country_Sub_Sector_Usage_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Sub_Sector_Usage_total_header$Sub_Sector <- TER_Proj_Demand_Country_Sub_Sector_Usage_total_value$Sub_Sector
  TER_Proj_Demand_Country_Sub_Sector_Usage_total_header$Usage <- TER_Proj_Demand_Country_Sub_Sector_Usage_total_value$Usage
  TER_Proj_Demand_Country_Sub_Sector_Usage_total <- unique(cbind(TER_Proj_Demand_Country_Sub_Sector_Usage_total_header, TER_Proj_Demand_Country_Sub_Sector_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sub_Sector/Usage
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Sub_Sector", "Usage")], FUN = sum)
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header  <- input[1:nrow(TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value), 1:9]  
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Energy <- TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value$Energy
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Country <- "EU23"
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Sub_Sector <- TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value$Sub_Sector
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Usage <- TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value$Usage
  TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total <- unique(cbind(TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header, TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value[,-(1:3)]))
  
  input <- rbind(input, TER_Proj_Demand_Sub_Sector_total, TER_Proj_Demand_Usage_total, TER_Proj_Demand_Energy_total, TER_Proj_Demand_Energy_Sub_total,
                 TER_Proj_Demand_Energy_Usage_total, TER_Proj_Demand_Sub_Sector_Usage_total, TER_Proj_Demand_Sector_total, TER_Proj_Demand_Country_Energy_total, 
                 TER_Proj_Demand_Country_Usage_total, TER_Proj_Demand_Country_Sector_total, TER_Proj_Demand_Country_Energy_Sub_total, TER_Proj_Demand_Country_Energy_Usage_total,
                 TER_Proj_Demand_Country_Sub_Sector_Usage_total, TER_Proj_Demand_Country_Energy_Sub_Sector_Usage_total)
 
   return(input)
}