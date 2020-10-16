

source("../Functions/Input/General_Inputs_Functions.R")

############################################################### Main Function ######################################################################################

main_input_Hist_TER <- function(Hist_Macro, Hist_Assumption, Hist_Energy_Demand) 
{
  TER_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "TER")
  TER_Hist_Energy_Demand <- TER_Hist_Energy_Demand[order(TER_Hist_Energy_Demand$Usage),]
  TER_Hist_Energy_Demand <- Sort_By_Country(TER_Hist_Energy_Demand)
  TER_Hist_Energy_Demand <- TER_Hist_Energy_Demand[order(TER_Hist_Energy_Demand$Energy),]
  
  TER_Hist_Assumption <- Extract_Sector(Hist_Assumption, "TER")
  
  TER_Total_Surface <- Sort_By_Country(Extract_Item( Extract_Sector(Hist_Macro, "TER"), "Floor area of services"))
  
  #a. Space Heating 
  
  TER_Hist_Energy_Demand_Heating <- Extract_Usage(TER_Hist_Energy_Demand, "space heating")
  
  ### Equipment Efficiency 
  TER_Equipment_Efficiency_Heating <- Extract_Usage( Extract_Item(TER_Hist_Assumption, "System efficiency"), "space heating" )
  TER_Equipment_Efficiency_Heating <- Sort_By_Country(TER_Equipment_Efficiency_Heating)
  TER_Equipment_Efficiency_Heating <- TER_Equipment_Efficiency_Heating[order(TER_Equipment_Efficiency_Heating$Energy),]
  
  ### Average Heating need
  TER_Heating_need_by_energy <- TER_Hist_Energy_Demand_Heating
  TER_Heating_need_by_energy$ID_Item <- "Heating need by energy"
  
  # First total heating need by energy (Bi) 
  TER_Heating_need_by_energy[,10:length(TER_Heating_need_by_energy)] <- TER_Equipment_Efficiency_Heating[,10:length(TER_Equipment_Efficiency_Heating)] * TER_Hist_Energy_Demand_Heating[,10:length(TER_Hist_Energy_Demand_Heating)] 
  # Then total heating need (B)
  TER_Total_Heating_need_value <- sapply(TER_Heating_need_by_energy[,10:length(TER_Heating_need_by_energy)], function(x){ tapply(x, TER_Heating_need_by_energy$Country, sum)})  
  TER_Total_Heating_need_header  <- TER_Heating_need_by_energy[1:nlevels(as.factor(TER_Hist_Energy_Demand_Heating$Country)),1:9]
  TER_Total_Heating_need_header$Country <- levels(as.factor(TER_Hist_Energy_Demand_Heating$Country))
  TER_Total_Heating_need_header$Energy <- "na"
  TER_Total_Heating_need_header$ID_Item <- "Total heating need"
  TER_Total_Heating_need <- cbind(TER_Total_Heating_need_header, TER_Total_Heating_need_value) 
  # Finallly average heating need (b)
  
  TER_Average_Heating_need <- TER_Total_Heating_need
  TER_Average_Heating_need$ID_Item <- "Heating need"
  TER_Average_Heating_need$Unit <- "kwh/m²"
  TER_Average_Heating_need[,10:length(TER_Average_Heating_need)] <- TER_Total_Heating_need[,10:length(TER_Total_Heating_need)] / TER_Total_Surface[,10:length(TER_Total_Surface)] * 1000 #pour mettre en kWh/m2
  
  ### Market ShaTER (Si/S)
  TER_Average_Heating_need_MS <- TER_Average_Heating_need[ rep( seq_len( nrow( TER_Average_Heating_need ) ), nlevels(as.factor(TER_Heating_need_by_energy$Energy)) ), ]
  TER_Average_Heating_need_MS$Energy = TER_Heating_need_by_energy$Energy
  
  TER_Total_Surface_MS <- TER_Total_Surface[ rep( seq_len( nrow( TER_Total_Surface ) ), nlevels(as.factor(TER_Heating_need_by_energy$Energy)) ), ]
  TER_Total_Surface_MS$Energy = TER_Heating_need_by_energy$Energy
  
  TER_Market_Shares_Heating <- TER_Heating_need_by_energy
  TER_Market_Shares_Heating$ID_Item <- "Market shares"
  TER_Market_Shares_Heating$Unit <- "%"
  TER_Market_Shares_Heating[,10:length(TER_Market_Shares_Heating)] <- TER_Heating_need_by_energy[,10:length(TER_Heating_need_by_energy)] / (TER_Average_Heating_need_MS[,10:length(TER_Average_Heating_need_MS)] * TER_Total_Surface_MS[,10:length(TER_Total_Surface_MS)]) *1000
  TER_Market_Shares_Heating <- Sort_By_Country(TER_Market_Shares_Heating)
  TER_Market_Shares_Heating <- TER_Market_Shares_Heating[order(TER_Market_Shares_Heating$Energy),]
  
  #b. Water heating need
  TER_Hist_Energy_Demand_DWH <- Extract_Usage(TER_Hist_Energy_Demand, "water heating")
  
  ### Equipment Efficiency 
  TER_Equipment_Efficiency_DWH <- Extract_Usage( Extract_Item(TER_Hist_Assumption, "System efficiency"), "water heating" )
  TER_Equipment_Efficiency_DWH <- Sort_By_Country(TER_Equipment_Efficiency_DWH)
  TER_Equipment_Efficiency_DWH <- TER_Equipment_Efficiency_DWH[order(TER_Equipment_Efficiency_DWH$Energy),]
  
  ### Average DWH need
  TER_DWH_need_by_energy <- TER_Hist_Energy_Demand_DWH
  TER_DWH_need_by_energy$ID_Item <- "Water heating need by energy"
  
  # First total water heating need by energy (Bi) 
  TER_DWH_need_by_energy[,10:length(TER_DWH_need_by_energy)] <- TER_Equipment_Efficiency_DWH[,10:length(TER_Equipment_Efficiency_DWH)] * TER_Hist_Energy_Demand_DWH[,10:length(TER_Hist_Energy_Demand_DWH)] 
  # Then total water heating need (B)
  TER_Total_DWH_need_value <- sapply(TER_DWH_need_by_energy[,10:length(TER_DWH_need_by_energy)], function(x){ tapply(x, TER_DWH_need_by_energy$Country, sum)})  
  TER_Total_DWH_need_header  <- TER_DWH_need_by_energy[1:nlevels(as.factor(TER_Hist_Energy_Demand_DWH$Country)),1:9]
  TER_Total_DWH_need_header$Country <- levels(as.factor(TER_Hist_Energy_Demand_DWH$Country))
  TER_Total_DWH_need_header$Energy <- "na"
  TER_Total_DWH_need_header$ID_Item <- "Total water heating need"
  TER_Total_DWH_need <- cbind(TER_Total_DWH_need_header, TER_Total_DWH_need_value) 
  # Finallly average water heating need (b)
  
  TER_Average_DWH_need <- TER_Total_DWH_need
  TER_Average_DWH_need$ID_Item <- "Water heating need"
  TER_Average_DWH_need$Unit <- "kwh/m²"
  TER_Average_DWH_need[,10:length(TER_Average_DWH_need)] <- TER_Total_DWH_need[,10:length(TER_Total_DWH_need)] / TER_Total_Surface[,10:length(TER_Total_Surface)] * 1000 #??? pour mettre en kWh/person
  
  ### Market ShaTER (Si/S)
  TER_Average_DWH_need_MS <- TER_Average_DWH_need[ rep( seq_len( nrow( TER_Average_DWH_need ) ), nlevels(as.factor(TER_DWH_need_by_energy$Energy) )), ]
  TER_Average_DWH_need_MS$Energy = TER_DWH_need_by_energy$Energy
  
  TER_Total_Surface_MS <- TER_Total_Surface[ rep( seq_len( nrow( TER_Total_Surface ) ), nlevels(as.factor(TER_DWH_need_by_energy$Energy)) ), ]
  TER_Total_Surface_MS$Energy = TER_DWH_need_by_energy$Energy
  
  TER_Market_Shares_DWH <- TER_DWH_need_by_energy
  TER_Market_Shares_DWH$ID_Item <- "Market shares"
  TER_Market_Shares_DWH$Unit <- "%"
  TER_Market_Shares_DWH[,10:length(TER_Market_Shares_DWH)] <- TER_DWH_need_by_energy[,10:length(TER_DWH_need_by_energy)] / (TER_Average_DWH_need_MS[,10:length(TER_Average_DWH_need_MS)] * TER_Total_Surface_MS[,10:length(TER_Total_Surface_MS)]) * 1000
  TER_Market_Shares_DWH <- Sort_By_Country(TER_Market_Shares_DWH)
  TER_Market_Shares_DWH <- TER_Market_Shares_DWH[order(TER_Market_Shares_DWH$Energy),]
  
  #c. Cooking (nothing to do)
  
  #d. Lighting (nothing to do)
  
  #e. Specific Uses (nothing to do)
  
  # #f. cooling
  
  TER_Hist_Energy_Demand_Cooling <- Sort_By_Country( Extract_Usage(TER_Hist_Energy_Demand, "space cooling") )
  TER_Hist_Energy_Demand_Cooling <- TER_Hist_Energy_Demand_Cooling[order(as.character(TER_Hist_Energy_Demand_Cooling$Energy)),]
  
  ### Surface cooling 
  TER_Total_Surface_Cooling <- TER_Total_Surface[ rep( seq_len( nrow( TER_Total_Surface ) ), nlevels(as.factor(TER_Hist_Energy_Demand_Cooling$Energy)) ), ]
  TER_Total_Surface_Cooling$Energy <- TER_Hist_Energy_Demand_Cooling$Energy
  ### Equipment Efficiency
  TER_Equipment_Efficiency_Cooling <- Extract_Usage( Extract_Item(TER_Hist_Assumption, "System efficiency"), "space cooling" )
  TER_Equipment_Efficiency_Cooling <- Sort_By_Country(TER_Equipment_Efficiency_Cooling)
  TER_Equipment_Efficiency_Cooling <- TER_Equipment_Efficiency_Cooling[order(as.character(TER_Equipment_Efficiency_Cooling$Energy)),]
  
  ### Equipment Rate
  TER_Equipment_Rate_Cooling <- Extract_Usage( Extract_Item(TER_Hist_Assumption, "average_equipment_rate"), "space cooling" )
  TER_Equipment_Rate_Cooling <- TER_Equipment_Rate_Cooling[ rep( seq_len( nrow( TER_Equipment_Rate_Cooling ) ), nlevels(as.factor(TER_Hist_Energy_Demand_Cooling$Energy)) ), ]
  TER_Equipment_Rate_Cooling$Energy = TER_Hist_Energy_Demand_Cooling$Energy
  TER_Equipment_Rate_Cooling <- Sort_By_Country(TER_Equipment_Rate_Cooling)
  TER_Equipment_Rate_Cooling <- TER_Equipment_Rate_Cooling[order(as.character(TER_Equipment_Rate_Cooling$Energy)),]
  
  ### Average Cooling need
  TER_Cooling_need_by_energy <- TER_Hist_Energy_Demand_Cooling
  TER_Cooling_need_by_energy$ID_Item <- "Cooling need by energy"
  
  TER_Cooling_need_by_energy[,10:length(TER_Cooling_need_by_energy)] <- TER_Hist_Energy_Demand_Cooling[,10:length(TER_Hist_Energy_Demand_Cooling)] * TER_Equipment_Efficiency_Cooling[,10:length(TER_Equipment_Efficiency_Cooling)] / (TER_Equipment_Rate_Cooling[,10:length(TER_Equipment_Rate_Cooling)] )
  
  TER_Total_Cooling_need_value <- aggregate(x = TER_Cooling_need_by_energy[,10:length(TER_Cooling_need_by_energy)], by = TER_Cooling_need_by_energy[c("Country")], FUN = sum)
  TER_Total_Cooling_need_header  <- TER_Hist_Energy_Demand_Cooling[1:nrow(TER_Total_Cooling_need_value), 1:9]  
  TER_Total_Cooling_need_header$Energy <- "na"
  TER_Total_Cooling_need_header$ID_Item <- "Total cooling need"
  TER_Total_Cooling_need <- unique(cbind(TER_Total_Cooling_need_header, TER_Total_Cooling_need_value[,-1]))
  
  TER_Average_Cooling_need <- TER_Total_Cooling_need
  TER_Average_Cooling_need$ID_Item <- "Cooling need"
  TER_Average_Cooling_need$Unit <- "kWh/m²" 
  TER_Average_Cooling_need[,10:length(TER_Average_Cooling_need)] <- TER_Total_Cooling_need[,10:length(TER_Total_Cooling_need)] / TER_Total_Surface[,10:length(TER_Total_Surface)] * 1000
  
  #On remplace les valeurs NaN par zero. le besoin en cooling est nul dans ces pays
  Year_Proj_col <- min(which(is.nan(colSums(TER_Hist_Energy_Demand_Cooling[10:length(TER_Hist_Energy_Demand_Cooling)])))) + 9
  TER_Average_Cooling_need[,10:Year_Proj_col-1][is.na(TER_Average_Cooling_need[,10:Year_Proj_col-1])] <- 0
  
  
  TER_Average_Cooling_need_MS <- TER_Average_Cooling_need[ rep( seq_len( nrow( TER_Average_Cooling_need ) ), nlevels(as.factor(TER_Cooling_need_by_energy$Energy)) ), ]
  TER_Average_Cooling_need_MS$Energy = TER_Cooling_need_by_energy$Energy
  
  TER_Market_Shares_Cooling <- TER_Cooling_need_by_energy
  TER_Market_Shares_Cooling$ID_Item <- "Market shares"
  TER_Market_Shares_Cooling$Unit <- "%"
  TER_Market_Shares_Cooling[,10:length(TER_Market_Shares_Cooling)] <- TER_Cooling_need_by_energy[,10:length(TER_Cooling_need_by_energy)] / (TER_Average_Cooling_need_MS[,10:length(TER_Average_Cooling_need_MS)] * TER_Total_Surface_Cooling[,10:length(TER_Total_Surface_Cooling)]) *1000
  TER_Market_Shares_Cooling <- Sort_By_Country(TER_Market_Shares_Cooling)
  TER_Market_Shares_Cooling <- TER_Market_Shares_Cooling[order(as.character(TER_Market_Shares_Cooling$Energy)),]
  
  
  saveRDS(rbind(Hist_Assumption,
                TER_Average_Heating_need, 
                TER_Average_DWH_need, 
                TER_Average_Cooling_need, 
                TER_Market_Shares_Heating, 
                TER_Market_Shares_DWH, 
                TER_Market_Shares_Cooling),
          "../../Data/temp/Hist_Assumption.Rds")
  

}



main_input_Proj_TER <- function(Proj_Macro, Hist_Assumption, Proj_Assumption) 
{
  Hist_Assumption_TER <- Extract_Sector(Hist_Assumption, "TER")
  Proj_Assumption_TER <- Extract_Sector(Proj_Assumption, "TER")
  
  #specific uses
  SU_Activity_Rate <- Sort_By_Country( Extract_Item( Extract_Sector(Proj_Macro, "TER"), "Value Added") )
  SU_Activity_Rate$ID_Item <- "Activity Rate"
  SU_Activity_Rate$Usage <- "specific uses"
  SU_Activity_Rate$Unit <- "%"
  SU_Activity_Rate <- derivate_dataframe(SU_Activity_Rate)
  
  #cooking
  Cooking_Activity_Rate <- Sort_By_Country( Extract_Item(Proj_Macro, "Floor area of services") )
  Cooking_Activity_Rate$ID_Item <- "Activity Rate"
  Cooking_Activity_Rate$Usage <- "cooking"
  Cooking_Activity_Rate$Unit <- "%"
  Cooking_Activity_Rate <- derivate_dataframe(Cooking_Activity_Rate)
  
  #lighting
  Lighting_Activity_Rate <- Sort_By_Country( Extract_Item(Proj_Macro, "Floor area of services") )
  Lighting_Activity_Rate$ID_Item <- "Activity Rate"
  Lighting_Activity_Rate$Usage <- "lighting"
  Lighting_Activity_Rate$Unit <- "%"
  Lighting_Activity_Rate <- derivate_dataframe(Lighting_Activity_Rate)
  
  Lighting_Efficiency_Rate <- Sort_By_Country( Extract_Item(Proj_Assumption_TER, "Efficiency_lighting_indice") )
  Lighting_Efficiency_Rate$ID_Item <- "Efficiency Rate"
  Lighting_Efficiency_Rate$Unit <- "%"
  Lighting_Efficiency_Rate <- derivate_dataframe(Lighting_Efficiency_Rate)
  
  #Space heating
  TER_Hist_Average_Heating_need <- Sort_By_Country( Extract_Item(Hist_Assumption_TER, "Heating need") )
  TER_Target_Average_Heating_need <- Sort_By_Country( Extract_Item(Proj_Assumption_TER, "Heating need target") )
  TER_Target_Average_Heating_need <- derivate_dataframe(TER_Target_Average_Heating_need)
  
  Year_Proj_col <- min(which(is.nan(colSums(TER_Hist_Average_Heating_need[10:length(TER_Hist_Average_Heating_need)])))) + 9
  
  TER_Proj_Average_Heating_need <- TER_Hist_Average_Heating_need
  for(i in Year_Proj_col:length(TER_Proj_Average_Heating_need))
  {
    TER_Proj_Average_Heating_need[,i] <- TER_Proj_Average_Heating_need[,i-1] * ( 1 + TER_Target_Average_Heating_need[,i] )
  }
  
  #Water heating
  TER_Hist_Average_DWH_need <- Sort_By_Country( Extract_Item(Hist_Assumption_TER, "Water heating need") )
  TER_Target_Average_DWH_need <- Sort_By_Country( Extract_Item(Proj_Assumption_TER, "Water heating need target") )
  TER_Target_Average_DWH_need <- derivate_dataframe(TER_Target_Average_DWH_need)
  
  Year_Proj_col <- min(which(is.nan(colSums(TER_Hist_Average_DWH_need[10:length(TER_Hist_Average_DWH_need)])))) + 9
  
  TER_Proj_Average_DWH_need <- TER_Hist_Average_DWH_need
  for(i in Year_Proj_col:length(TER_Proj_Average_DWH_need))
  {
    TER_Proj_Average_DWH_need[,i] <- TER_Proj_Average_DWH_need[,i-1] * ( 1 + TER_Target_Average_DWH_need[,i] )
  }
  
  #Space cooling
  TER_Hist_Average_Cooling_need <- Sort_By_Country( Extract_Item(Hist_Assumption_TER, "Cooling need") )
  TER_Target_Average_Cooling_need <- Sort_By_Country( Extract_Item(Proj_Assumption_TER, "Cooling need target") )
  TER_Target_Average_Cooling_need <- derivate_dataframe(TER_Target_Average_Cooling_need)
  
  Year_Proj_col <- min(which(is.nan(colSums(TER_Hist_Average_Cooling_need[10:length(TER_Hist_Average_Cooling_need)])))) + 9
  
  TER_Proj_Average_Cooling_need <- TER_Hist_Average_Cooling_need
  for(i in Year_Proj_col:length(TER_Proj_Average_Cooling_need))
  {
    TER_Proj_Average_Cooling_need[,i] <- TER_Proj_Average_Cooling_need[,i-1] * ( 1 + TER_Target_Average_Cooling_need[,i] )
  }
  
  
  #Interpolation des cibles de projections pour les hypotheses residentielles
  TER_Hist_Equipment_Efficiency <- Extract_Item(Proj_Assumption_TER, "System efficiency target")
  TER_Proj_Equipment_Efficiency <- Interpolate_Missing_Values(TER_Hist_Equipment_Efficiency)
  TER_Proj_Equipment_Efficiency$ID_Item <- "System efficiency"
  
  TER_Proj_Equipment_Rate_target <- Extract_Usage( Extract_Item(Proj_Assumption_TER, "average_equipment_rate target"), "space cooling")
  TER_Proj_Equipment_Rate_target <- Interpolate_Missing_Values(TER_Proj_Equipment_Rate_target)
  TER_Hist_Equipment_Rate <- Extract_Usage(Extract_Item(Hist_Assumption_TER, "average_equipment_rate"), "space cooling")
  TER_Proj_Equipment_Rate <- apply_growth_rate_equipment_rate(TER_Hist_Equipment_Rate, TER_Proj_Equipment_Rate_target)
  TER_Proj_Equipment_Rate$ID_Item <- "average_equipment_rate"

  TER_Proj_Market_shares_target <- Sort_By_Country( Extract_Item(Proj_Assumption_TER, "Market shares target") )
  TER_Proj_Market_shares_target <- droplevels(subset(TER_Proj_Market_shares_target, Energy!= "total"))
 # TER_Proj_Market_shares_target <- Interpolate_Missing_Values(TER_Proj_Market_shares_target)
  
  TER_Hist_Market_shares <- Sort_By_Country( Extract_Item(Hist_Assumption_TER, "Market shares") )
  TER_Proj_Market_shares <- apply_growth_rate_market_shares(TER_Hist_Market_shares, TER_Proj_Market_shares_target)
  
  ### Creation du datafrme contenant les targets defnit par les experts du CEEME 
  saveRDS(Extract_Item(Proj_Assumption_TER, 
                              c("Efficiency_elec_spe_indice", 
                                "average_equipment_rate target", 
                                "Efficiency_lighting_indice", 
                                "indice_data_center", 
                                "Heating need target", 
                                "Water heating need target", 
                                "System efficiency target", 
                                "Cooling need target",
                                "Market shares target")),
                     "../../Data/temp/Proj_Target.Rds")
  
  saveRDS(rbind( Proj_Macro, 
                 Extract_Item(Proj_Assumption, "floor_area_per_VA" )),
           "../../Data/temp/Proj_Macro.Rds")
          
  
  # On met tout dans Proj_assumption
  saveRDS(rbind(Proj_Assumption, 
                SU_Activity_Rate, 
                TER_Proj_Equipment_Rate,
                TER_Proj_Average_Heating_need, 
                TER_Proj_Average_DWH_need, 
                TER_Proj_Average_Cooling_need, 
                TER_Proj_Equipment_Efficiency, 
                Lighting_Activity_Rate, 
                Lighting_Efficiency_Rate,
                Cooking_Activity_Rate,
                TER_Proj_Market_shares),
           "../../Data/temp/Proj_Assumption.Rds")
}



calculate_hist_volume_usage_TER <- function(input_energy, input_assumption)
{
  share_usage <- Extract_Sector(Extract_Item(input_assumption, "share_usage"), "TER")
  total_energy_demand_sector <- Extract_Sector(input_energy, "TER")
  
  list_energy <- levels(as.factor(share_usage$Energy))
  
  usage_energy_demand <- NULL
  for(i in 1:length(list_energy))
  {
    total_energy_demand_sector_tmp <- Sort_By_Country( Extract_Specific_Energy(total_energy_demand_sector, list_energy[i]))
    share_usage_tmp <- Sort_By_Country(Extract_Specific_Energy(share_usage, list_energy[i]))
    
    total_energy_demand_sector_tmp = total_energy_demand_sector_tmp[ rep( seq_len( nrow( total_energy_demand_sector_tmp ) ), nlevels( as.factor(share_usage_tmp$Usage )) ), ]
    total_energy_demand_sector_tmp = total_energy_demand_sector_tmp[ order( total_energy_demand_sector_tmp$Country ), ]
    total_energy_demand_sector_tmp$Sub_Sector = share_usage_tmp$Sub_Sector
    total_energy_demand_sector_tmp$Usage = share_usage_tmp$Usage
    
    usage_energy_demand_tmp_header <- total_energy_demand_sector_tmp[,1:9]
    usage_energy_demand_tmp_value <- share_usage_tmp[,10:length(share_usage_tmp)]*total_energy_demand_sector_tmp[,10:length(total_energy_demand_sector_tmp)]
    usage_energy_demand_tmp <- cbind(usage_energy_demand_tmp_header, usage_energy_demand_tmp_value)
    
    usage_energy_demand = rbind(usage_energy_demand, usage_energy_demand_tmp)
  }
  
  input_energy <- droplevels(subset(input_energy, Sector != "TER"))
  usage_energy_demand <- rbind(input_energy, usage_energy_demand)
  
  return(usage_energy_demand)
}