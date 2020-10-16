source("../Functions/Input/General_Inputs_Functions.R")

############################################################### Main Function ######################################################################################

main_input_Hist_RES <- function(Hist_Macro, Hist_Assumption, Hist_Energy_Demand) 
{
  RES_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "RES")
  RES_Hist_Energy_Demand <- RES_Hist_Energy_Demand[order(RES_Hist_Energy_Demand$Usage),]
  RES_Hist_Energy_Demand <- Sort_By_Country(RES_Hist_Energy_Demand)
  RES_Hist_Energy_Demand <- RES_Hist_Energy_Demand[order(as.character(RES_Hist_Energy_Demand$Energy)),]
  
  RES_Hist_Assumption <- Extract_Sector(Hist_Assumption, "RES")
  
  RES_Total_Surface <- Sort_By_Country(Extract_Item( Extract_Sector(Hist_Macro, "RES"), "Floor Area of residential"))
  Population <- Sort_By_Country(Extract_Item(Hist_Macro, "Population"))
  
  #a. Space Heating 
  
  RES_Hist_Energy_Demand_Heating <- Extract_Usage(RES_Hist_Energy_Demand, "space heating")
  
  ### Equipment Efficiency 
  RES_Equipment_Efficiency_Heating <- Extract_Usage( Extract_Item(RES_Hist_Assumption, "System efficiency"), "space heating" )
  RES_Equipment_Efficiency_Heating <- Sort_By_Country(RES_Equipment_Efficiency_Heating)
  RES_Equipment_Efficiency_Heating <- RES_Equipment_Efficiency_Heating[order(RES_Equipment_Efficiency_Heating$Energy),]
  
  #??? on supprime hydrogen for the moment 
 # RES_Equipment_Efficiency_Heating <- droplevels(subset(RES_Equipment_Efficiency_Heating, Energy!="SD Hydrogen"))
  
  ### Average Heating need
  RES_Heating_need_by_energy <- RES_Hist_Energy_Demand_Heating
  RES_Heating_need_by_energy$ID_Item <- "Heating need by energy"
  
  # First total heating need by energy (Bi) 
  RES_Heating_need_by_energy[,10:length(RES_Heating_need_by_energy)] <- RES_Equipment_Efficiency_Heating[,10:length(RES_Equipment_Efficiency_Heating)] * RES_Hist_Energy_Demand_Heating[,10:length(RES_Hist_Energy_Demand_Heating)] 
  # Then total heating need (B)
  RES_Total_Heating_need_value <- sapply(RES_Heating_need_by_energy[,10:length(RES_Heating_need_by_energy)], function(x){ tapply(x, RES_Heating_need_by_energy$Country, sum)})  
  RES_Total_Heating_need_header  <- RES_Heating_need_by_energy[1:nlevels(as.factor(RES_Hist_Energy_Demand_Heating$Country)),1:9]
  RES_Total_Heating_need_header$Country <- levels(as.factor(RES_Hist_Energy_Demand_Heating$Country))
  RES_Total_Heating_need_header$Energy <- "na"
  RES_Total_Heating_need_header$ID_Item <- "Total heating need"
  RES_Total_Heating_need <- cbind(RES_Total_Heating_need_header, RES_Total_Heating_need_value) 
  # Finallly average heating need (b)
  
  RES_Average_Heating_need <- RES_Total_Heating_need
  RES_Average_Heating_need$ID_Item <- "Heating need"
  RES_Average_Heating_need$Unit <- "kwh/m²"
  RES_Average_Heating_need[,10:length(RES_Average_Heating_need)] <- RES_Total_Heating_need[,10:length(RES_Total_Heating_need)] / RES_Total_Surface[,10:length(RES_Total_Surface)] * 1000 #pour mettre en kWh/m2
  
  ### Market Shares (Si/S)
  RES_Average_Heating_need_MS <- RES_Average_Heating_need[ rep( seq_len( nrow( RES_Average_Heating_need ) ), nlevels(as.factor(RES_Heating_need_by_energy$Energy) )), ]
  RES_Average_Heating_need_MS$Energy = RES_Heating_need_by_energy$Energy
  
  RES_Total_Surface_MS <- RES_Total_Surface[ rep( seq_len( nrow( RES_Total_Surface ) ), nlevels(as.factor(RES_Heating_need_by_energy$Energy)) ), ]
  RES_Total_Surface_MS$Energy = RES_Heating_need_by_energy$Energy
  
  RES_Market_Shares_Heating <- RES_Heating_need_by_energy
  RES_Market_Shares_Heating$ID_Item <- "Market shares"
  RES_Market_Shares_Heating$Unit <- "%"
  RES_Market_Shares_Heating[,10:length(RES_Market_Shares_Heating)] <- RES_Heating_need_by_energy[,10:length(RES_Heating_need_by_energy)] / (RES_Average_Heating_need_MS[,10:length(RES_Average_Heating_need_MS)] * RES_Total_Surface_MS[,10:length(RES_Total_Surface_MS)]) *1000
  RES_Market_Shares_Heating <- Sort_By_Country(RES_Market_Shares_Heating)
  RES_Market_Shares_Heating <- RES_Market_Shares_Heating[order(as.character(RES_Market_Shares_Heating$Energy)),]
  
  #b. Water heating need
  RES_Hist_Energy_Demand_DWH <- Extract_Usage(RES_Hist_Energy_Demand, "water heating")
  
  ### Equipment Efficiency 
  RES_Equipment_Efficiency_DWH <- Extract_Usage( Extract_Item(RES_Hist_Assumption, "System efficiency"), "water heating" )
  RES_Equipment_Efficiency_DWH <- Sort_By_Country(RES_Equipment_Efficiency_DWH)
  RES_Equipment_Efficiency_DWH <- RES_Equipment_Efficiency_DWH[order(RES_Equipment_Efficiency_DWH$Energy),]
  
  #RES_Equipment_Efficiency_DWH <- droplevels(subset(RES_Equipment_Efficiency_DWH, Energy!="SD Hydrogen"))
  
  ### Average DWH need
  RES_DWH_need_by_energy <- RES_Hist_Energy_Demand_DWH
  RES_DWH_need_by_energy$ID_Item <- "Water heating need by energy"
  
  # First total water heating need by energy (Bi) 
  RES_DWH_need_by_energy[,10:length(RES_DWH_need_by_energy)] <- RES_Equipment_Efficiency_DWH[,10:length(RES_Equipment_Efficiency_DWH)] * RES_Hist_Energy_Demand_DWH[,10:length(RES_Hist_Energy_Demand_DWH)] 
  # Then total water heating need (B)
  RES_Total_DWH_need_value <- sapply(RES_DWH_need_by_energy[,10:length(RES_DWH_need_by_energy)], function(x){ tapply(x, RES_DWH_need_by_energy$Country, sum)})  
  RES_Total_DWH_need_header  <- RES_DWH_need_by_energy[1:nlevels(as.factor(RES_Hist_Energy_Demand_DWH$Country)),1:9]
  RES_Total_DWH_need_header$Country <- levels(as.factor(RES_Hist_Energy_Demand_DWH$Country))
  RES_Total_DWH_need_header$Energy <- "na"
  RES_Total_DWH_need_header$ID_Item <- "Total water heating need"
  RES_Total_DWH_need <- cbind(RES_Total_DWH_need_header, RES_Total_DWH_need_value) 
  # Finallly average water heating need (b)
  
  RES_Average_DWH_need <- RES_Total_DWH_need
  RES_Average_DWH_need$ID_Item <- "Water heating need"
  RES_Average_DWH_need$Unit <- "kwh/person"
  RES_Average_DWH_need[,10:length(RES_Average_DWH_need)] <- RES_Total_DWH_need[,10:length(RES_Total_DWH_need)] / Population[,10:length(Population)] * 1000000 #??? pour mettre en kWh/person
  
  ### Market Shares (Pi/P)
  RES_Average_DWH_need_MS <- RES_Average_DWH_need[ rep( seq_len( nrow( RES_Average_DWH_need ) ), nlevels(as.factor(RES_DWH_need_by_energy$Energy)) ), ]
  RES_Average_DWH_need_MS$Energy = RES_DWH_need_by_energy$Energy
  
  Population_MS <- Population[ rep( seq_len( nrow( Population ) ), nlevels(as.factor(RES_DWH_need_by_energy$Energy)) ), ]
  Population_MS$Energy = RES_DWH_need_by_energy$Energy
  
  RES_Market_Shares_DWH <- RES_DWH_need_by_energy
  RES_Market_Shares_DWH$ID_Item <- "Market shares"
  RES_Market_Shares_DWH$Unit <- "%"
  RES_Market_Shares_DWH[,10:length(RES_Market_Shares_DWH)] <- RES_DWH_need_by_energy[,10:length(RES_DWH_need_by_energy)] / (RES_Average_DWH_need_MS[,10:length(RES_Average_DWH_need_MS)] * Population_MS[,10:length(Population_MS)]) * 1000000
  RES_Market_Shares_DWH <- Sort_By_Country(RES_Market_Shares_DWH)
  RES_Market_Shares_DWH <- RES_Market_Shares_DWH[order(as.character(RES_Market_Shares_DWH$Energy)),]
  
  #c. Cooking (nothing to do)
  
  #d. Lighting (nothing to do)
  
  #e. Specific Uses (nothing to do)
  
  # #f. cooling

  RES_Hist_Energy_Demand_Cooling <- Sort_By_Country( Extract_Usage(RES_Hist_Energy_Demand, "space cooling") )
  RES_Hist_Energy_Demand_Cooling <- RES_Hist_Energy_Demand_Cooling[order(as.character(RES_Hist_Energy_Demand_Cooling$Energy)),]

  ### Surface cooling 
  RES_Total_Surface_Cooling <- RES_Total_Surface[ rep( seq_len( nrow( RES_Total_Surface ) ), nlevels(as.factor(RES_Hist_Energy_Demand_Cooling$Energy)) ), ]
  RES_Total_Surface_Cooling$Energy <- RES_Hist_Energy_Demand_Cooling$Energy
  ### Equipment Efficiency
  RES_Equipment_Efficiency_Cooling <- Extract_Usage( Extract_Item(RES_Hist_Assumption, "System efficiency"), "space cooling" )
  RES_Equipment_Efficiency_Cooling <- Sort_By_Country(RES_Equipment_Efficiency_Cooling)
  RES_Equipment_Efficiency_Cooling <- RES_Equipment_Efficiency_Cooling[order(as.character(RES_Equipment_Efficiency_Cooling$Energy)),]

  ### Equipment Rate
  RES_Equipment_Rate_Cooling <- Extract_Usage( Extract_Item(RES_Hist_Assumption, "average_equipment_rate"), "space cooling" )
  RES_Equipment_Rate_Cooling <- RES_Equipment_Rate_Cooling[ rep( seq_len( nrow( RES_Equipment_Rate_Cooling ) ), nlevels(as.factor(RES_Hist_Energy_Demand_Cooling$Energy)) ), ]
  RES_Equipment_Rate_Cooling$Energy = RES_Hist_Energy_Demand_Cooling$Energy
  RES_Equipment_Rate_Cooling <- Sort_By_Country(RES_Equipment_Rate_Cooling)
  RES_Equipment_Rate_Cooling <- RES_Equipment_Rate_Cooling[order(as.character(RES_Equipment_Rate_Cooling$Energy)),]
  
  ### Average Cooling need
  RES_Cooling_need_by_energy <- RES_Hist_Energy_Demand_Cooling
  RES_Cooling_need_by_energy$ID_Item <- "Cooling need by energy"
  
  RES_Cooling_need_by_energy[,10:length(RES_Cooling_need_by_energy)] <- RES_Hist_Energy_Demand_Cooling[,10:length(RES_Hist_Energy_Demand_Cooling)] * RES_Equipment_Efficiency_Cooling[,10:length(RES_Equipment_Efficiency_Cooling)] / (RES_Equipment_Rate_Cooling[,10:length(RES_Equipment_Rate_Cooling)] )
  
  RES_Total_Cooling_need_value <- aggregate(x = RES_Cooling_need_by_energy[,10:length(RES_Cooling_need_by_energy)], by = RES_Cooling_need_by_energy[c("Country")], FUN = sum)
  RES_Total_Cooling_need_header  <- RES_Hist_Energy_Demand_Cooling[1:nrow(RES_Total_Cooling_need_value), 1:9]  
  RES_Total_Cooling_need_header$Energy <- "na"
  RES_Total_Cooling_need_header$ID_Item <- "Total cooling need"
  RES_Total_Cooling_need <- unique(cbind(RES_Total_Cooling_need_header, RES_Total_Cooling_need_value[,-1]))
  
  RES_Average_Cooling_need <- RES_Total_Cooling_need
  RES_Average_Cooling_need$ID_Item <- "Cooling need"
  RES_Average_Cooling_need$Unit <- "kWh/m²" 
  RES_Average_Cooling_need[,10:length(RES_Average_Cooling_need)] <- RES_Total_Cooling_need[,10:length(RES_Total_Cooling_need)] / RES_Total_Surface[,10:length(RES_Total_Surface)] * 1000
  
  #On remplace les valeurs NaN par zero. le besoin en cooling est nul dans ces pays
  Year_Proj_col <- min(which(is.nan(colSums(RES_Hist_Energy_Demand_Cooling[10:length(RES_Hist_Energy_Demand_Cooling)])))) + 9
  RES_Average_Cooling_need[,10:Year_Proj_col-1][is.na(RES_Average_Cooling_need[,10:Year_Proj_col-1])] <- 0
  
  
  RES_Average_Cooling_need_MS <- RES_Average_Cooling_need[ rep( seq_len( nrow( RES_Average_Cooling_need ) ), nlevels(as.factor(RES_Cooling_need_by_energy$Energy)) ), ]
  RES_Average_Cooling_need_MS$Energy = RES_Cooling_need_by_energy$Energy
  
  RES_Market_Shares_Cooling <- RES_Cooling_need_by_energy
  RES_Market_Shares_Cooling$ID_Item <- "Market shares"
  RES_Market_Shares_Cooling$Unit <- "%"
  RES_Market_Shares_Cooling[,10:length(RES_Market_Shares_Cooling)] <- RES_Cooling_need_by_energy[,10:length(RES_Cooling_need_by_energy)] / (RES_Average_Cooling_need_MS[,10:length(RES_Average_Cooling_need_MS)] * RES_Total_Surface_Cooling[,10:length(RES_Total_Surface_Cooling)]) *1000
  RES_Market_Shares_Cooling <- Sort_By_Country(RES_Market_Shares_Cooling)
  RES_Market_Shares_Cooling <- RES_Market_Shares_Cooling[order(as.character(RES_Market_Shares_Cooling$Energy)),]
  
  saveRDS(rbind(Hist_Assumption, 
                RES_Average_Heating_need, 
                RES_Average_DWH_need, 
                RES_Average_Cooling_need, 
                RES_Market_Shares_Heating, 
                RES_Market_Shares_DWH, 
                RES_Market_Shares_Cooling),
          "../../Data/temp/Hist_Assumption.Rds")

}





main_input_Proj_RES <- function(Proj_Macro, Hist_Assumption, Proj_Assumption, Proj_Target) 
{
  Hist_Assumption_RES <- Extract_Sector(Hist_Assumption, "RES")
  Proj_Assumption_RES <- Extract_Sector(Proj_Assumption, "RES")
  
  #cooking
  Cooking_Activity_Rate <- Sort_By_Country( Extract_Item(Proj_Macro, "No_Household") )
  Cooking_Activity_Rate$ID_Item <- "Activity Rate"
  Cooking_Activity_Rate$Usage <- "cooking"
  Cooking_Activity_Rate$Unit <- "%"
  Cooking_Activity_Rate <- derivate_dataframe(Cooking_Activity_Rate)
  
  #lighting
  Lighting_Activity_Rate <- Sort_By_Country( Extract_Item(Proj_Macro, "Floor Area of residential") )
  Lighting_Activity_Rate$ID_Item <- "Activity Rate"
  Lighting_Activity_Rate$Usage <- "lighting"
  Lighting_Activity_Rate$Unit <- "%"
  Lighting_Activity_Rate <- derivate_dataframe(Lighting_Activity_Rate)
  
  Lighting_Efficiency_Rate <- Sort_By_Country( Extract_Item(Proj_Assumption_RES, "Efficiency_lighting_indice") )
  Lighting_Efficiency_Rate$ID_Item <- "Efficiency Rate"
  Lighting_Efficiency_Rate$Unit <- "%"
  Lighting_Efficiency_Rate <- derivate_dataframe(Lighting_Efficiency_Rate)
  
  #Space heating
  RES_Hist_Average_Heating_need <- Sort_By_Country( Extract_Item(Hist_Assumption_RES, "Heating need") )
  RES_Target_Average_Heating_need <- Sort_By_Country( Extract_Item(Proj_Assumption_RES, "Heating need target") )
  RES_Target_Average_Heating_need <- derivate_dataframe(RES_Target_Average_Heating_need)
  
  Year_Proj_col <- min(which(is.nan(colSums(RES_Hist_Average_Heating_need[10:length(RES_Hist_Average_Heating_need)])))) + 9
  
  RES_Proj_Average_Heating_need <- RES_Hist_Average_Heating_need
  for(i in Year_Proj_col:length(RES_Proj_Average_Heating_need))
  {
    RES_Proj_Average_Heating_need[,i] <- RES_Proj_Average_Heating_need[,i-1] * ( 1 + RES_Target_Average_Heating_need[,i] )
  }
  
  #Water heating
  RES_Hist_Average_DWH_need <- Sort_By_Country( Extract_Item(Hist_Assumption_RES, "Water heating need") )
  RES_Target_Average_DWH_need <- Sort_By_Country( Extract_Item(Proj_Assumption_RES, "Water heating need target") )
  RES_Target_Average_DWH_need <- derivate_dataframe(RES_Target_Average_DWH_need)
  
  Year_Proj_col <- min(which(is.nan(colSums(RES_Hist_Average_DWH_need[10:length(RES_Hist_Average_DWH_need)])))) + 9
  
  RES_Proj_Average_DWH_need <- RES_Hist_Average_DWH_need
  for(i in Year_Proj_col:length(RES_Proj_Average_DWH_need))
  {
    RES_Proj_Average_DWH_need[,i] <- RES_Proj_Average_DWH_need[,i-1] * ( 1 + RES_Target_Average_DWH_need[,i] )
  }
  
  
  #Specific Uses
  RES_Hist_Efficiency_Rate_SU <- Extract_Item(Hist_Assumption_RES, "Unitary consumption")
  RES_Target_Efficiency_Rate_SU <- Extract_Item(Proj_Assumption_RES, "Unitary consumption target")
  RES_Efficiency_Rate_SU <- apply_growth_rate_equipment_rate(RES_Hist_Efficiency_Rate_SU, RES_Target_Efficiency_Rate_SU)
  RES_Efficiency_Rate_SU <- derivate_dataframe(RES_Efficiency_Rate_SU)
  RES_Efficiency_Rate_SU$ID_Item <- "Efficiency Rate"
  RES_Efficiency_Rate_SU$Unit <- "%" 
  
  RES_Target_Equipment_Rate_SU <- Extract_Usage( Extract_Item(Proj_Assumption_RES, "average_equipment_rate target"), "large appliances")
  RES_Hist_Equipment_Rate_SU <- Extract_Usage(Extract_Item(Hist_Assumption_RES, "average_equipment_rate"), "large appliances")
  RES_Proj_Equipment_Rate_SU <- apply_growth_rate_equipment_rate(RES_Hist_Equipment_Rate_SU, RES_Target_Equipment_Rate_SU)
  RES_Proj_Equipment_Rate_SU$ID_Item <- "average_equipment_rate"
  
  RES_Activity_Rate_SU <- RES_Proj_Equipment_Rate_SU
  RES_Activity_Rate_SU$ID_Item <- "Activity Rate"
  RES_Activity_Rate_SU$Unit <- "%"
  No_Household <- Sort_By_Country(Extract_Item(Proj_Macro, "No_Household"))
  RES_Activity_Rate_SU[,10:length(RES_Activity_Rate_SU)] <- No_Household[,10:length(No_Household)] * RES_Proj_Equipment_Rate_SU[,10:length(RES_Proj_Equipment_Rate_SU)] 
  RES_Activity_Rate_SU <- derivate_dataframe(RES_Activity_Rate_SU)
  
  #Space cooling
  RES_Hist_Average_Cooling_need <- Sort_By_Country( Extract_Item(Hist_Assumption_RES, "Cooling need") )
  RES_Target_Average_Cooling_need <- Sort_By_Country( Extract_Item(Proj_Assumption_RES, "Cooling need target") )
  RES_Target_Average_Cooling_need <- derivate_dataframe(RES_Target_Average_Cooling_need)
  
  Year_Proj_col <- min(which(is.nan(colSums(RES_Hist_Average_Cooling_need[10:length(RES_Hist_Average_Cooling_need)])))) + 9
  
  RES_Proj_Average_Cooling_need <- RES_Hist_Average_Cooling_need
  for(i in Year_Proj_col:length(RES_Proj_Average_Cooling_need))
  {
    RES_Proj_Average_Cooling_need[,i] <- RES_Proj_Average_Cooling_need[,i-1] * ( 1 + RES_Target_Average_Cooling_need[,i] )
  }
  
  
  #Interpolation des cibles de projections pour les hypotheses residentielles
  RES_Hist_Equipment_Efficiency <- Extract_Item(Proj_Assumption_RES, "System efficiency target")
  RES_Proj_Equipment_Efficiency <- Interpolate_Missing_Values(RES_Hist_Equipment_Efficiency)
  RES_Proj_Equipment_Efficiency$ID_Item <- "System efficiency"
  
  RES_Proj_Market_shares_target <- Sort_By_Country( Extract_Item(Proj_Assumption_RES, "Market shares target") )
  RES_Proj_Market_shares_target <- droplevels(subset(RES_Proj_Market_shares_target, Energy!= "total"))
  #RES_Proj_Market_shares_target <- Interpolate_Missing_Values(RES_Proj_Market_shares_target)
  RES_Hist_Market_shares <- Sort_By_Country( Extract_Item(Hist_Assumption_RES, "Market shares") )
  RES_Proj_Market_shares <- apply_growth_rate_market_shares(RES_Hist_Market_shares, RES_Proj_Market_shares_target)
 
  RES_Proj_Equipment_Rate_target <- Extract_Usage( Extract_Item(Proj_Assumption_RES, "average_equipment_rate target"), "space cooling")
  RES_Proj_Equipment_Rate_target <- Interpolate_Missing_Values(RES_Proj_Equipment_Rate_target)
  RES_Hist_Equipment_Rate <- Extract_Usage(Extract_Item(Hist_Assumption_RES, "average_equipment_rate"), "space cooling")
  RES_Proj_Equipment_Rate <- apply_growth_rate_equipment_rate(RES_Hist_Equipment_Rate, RES_Proj_Equipment_Rate_target)
  RES_Proj_Equipment_Rate$ID_Item <- "average_equipment_rate"
  
  ### Creation du dataframe contenant les targets defnit par les experts du CEEME 
  saveRDS(rbind(Proj_Target, 
                Extract_Item(Proj_Assumption_RES, 
                             c("Indice small appliances",
                               "Unitary consumption target",
                               "average_equipment_rate target", 
                               "Heating need target", 
                               "Water heating need target",
                               "System efficiency target", 
                               "Cooling need target",
                               "Market shares target"))),
          "../../Data/temp/Proj_Target.Rds")
  
  # On efface les targets du fichier de proj (pour TER et RES) en les remplacant par les interpolations ou projection avec TC
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Heating need target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="System efficiency target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Water heating need target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Cooling need target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Market shares target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="average_equipment_rate target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Unitary consumption target"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="indice_data_center"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Efficiency_lighting_indice"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Indice small appliances"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="floor_area_per_VA"))
  Proj_Assumption <- droplevels(subset(Proj_Assumption, ID_Item!="Efficiency_elec_spe_indice"))

  # On met tout dans Proj_assumption
  saveRDS(rbind(Proj_Assumption, 
                RES_Activity_Rate_SU,
                RES_Proj_Average_Heating_need,
                RES_Proj_Average_DWH_need,
                RES_Efficiency_Rate_SU,
                RES_Proj_Equipment_Rate_SU,
                RES_Proj_Average_Cooling_need,
                RES_Proj_Equipment_Efficiency, 
                Lighting_Activity_Rate, 
                Lighting_Efficiency_Rate,
                Cooking_Activity_Rate,
                RES_Proj_Market_shares,
                RES_Proj_Equipment_Rate),
          "../../Data/temp/Proj_Assumption.Rds")
}


calculate_hist_volume_usage_RES <- function(input_energy, input_assumption)
{
  share_usage <- Extract_Sector(Extract_Item(input_assumption, "share_usage"), "RES")
  total_energy_demand_sector <- Extract_Sector(input_energy, "RES")
  
  list_energy <- levels(as.factor(share_usage$Energy))
  
  usage_energy_demand <- NULL
  for(i in 1:length(list_energy))
  {
    total_energy_demand_sector_tmp <- Sort_By_Country( Extract_Specific_Energy(total_energy_demand_sector, list_energy[i]))
    share_usage_tmp <- Sort_By_Country(Extract_Specific_Energy(share_usage, list_energy[i]))
    
    total_energy_demand_sector_tmp = total_energy_demand_sector_tmp[ rep( seq_len( nrow( total_energy_demand_sector_tmp ) ), nlevels( as.factor(share_usage_tmp$Usage )) ), ]
    total_energy_demand_sector_tmp = total_energy_demand_sector_tmp[ order( total_energy_demand_sector_tmp$Country ), ]
    total_energy_demand_sector_tmp$Usage = share_usage_tmp$Usage
    
    usage_energy_demand_tmp_header <- total_energy_demand_sector_tmp[,1:9]
    usage_energy_demand_tmp_value <- share_usage_tmp[,10:length(share_usage_tmp)]*total_energy_demand_sector_tmp[,10:length(total_energy_demand_sector_tmp)]
    usage_energy_demand_tmp <- cbind(usage_energy_demand_tmp_header, usage_energy_demand_tmp_value)
    
    usage_energy_demand = rbind(usage_energy_demand, usage_energy_demand_tmp)
  }
  
  input_energy <- droplevels(subset(input_energy, Sector != "RES"))
  usage_energy_demand <- rbind(input_energy, usage_energy_demand)
  
  return(usage_energy_demand)
}
