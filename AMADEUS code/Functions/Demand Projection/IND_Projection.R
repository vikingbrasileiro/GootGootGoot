source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the industry sector 
############################################################################################################################################################################
main_projection_energy_demand_IND <- function(IND_calibration_subsitution, Hist_Energy_Demand, Proj_Assumption)
{
  #1. Data input extraction 
  
  IND_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "IND")
  IND_Hist_Energy_Demand <- droplevels( subset(IND_Hist_Energy_Demand, Sub_Sector != "Industry - energy") )
  IND_Hist_Energy_Demand <- IND_Hist_Energy_Demand[order(IND_Hist_Energy_Demand$Sub_Sector),]
  IND_Hist_Energy_Demand <- Sort_By_Country(IND_Hist_Energy_Demand)
  IND_Hist_Energy_Demand <- IND_Hist_Energy_Demand[order(IND_Hist_Energy_Demand$Energy),]
  
  IND_Proj_Assumption <- Extract_Sector(Proj_Assumption, "IND")
  IND_Proj_Assumption <- droplevels( subset(IND_Proj_Assumption, Sub_Sector != "Industry - energy") ) 
  
  #2. Defintion of variables compulsory to the projection
  
  ### Substitution Rate
  IND_Substitution_Rate <- Extract_Item(IND_Proj_Assumption, "Substitution Rate")
  IND_Substitution_Rate <- Sort_By_Country(IND_Substitution_Rate)
  IND_Substitution_Rate <- IND_Substitution_Rate[order(IND_Substitution_Rate$Energy),]
  
  ### Activity Rate
  IND_Activity_Rate <- Extract_Item(IND_Proj_Assumption, "Activity Rate")
  IND_Activity_Rate = Sort_By_Country(IND_Activity_Rate)
  IND_Activity_Rate = IND_Activity_Rate[ rep( seq_len( nrow( IND_Activity_Rate ) ), nlevels(as.factor(IND_Hist_Energy_Demand$Energy)) ), ]
  IND_Activity_Rate$Energy = as.factor(rep(levels(as.factor(IND_Hist_Energy_Demand$Energy)), each = nlevels(as.factor(IND_Activity_Rate$Sub_Sector))*nlevels(as.factor(IND_Activity_Rate$Country)) ))
  
  ### Economic Efficiency Rate
  IND_Economic_Eff <- Extract_Item(IND_Proj_Assumption, "Economic efficiency")
  IND_Economic_Eff = Sort_By_Country(IND_Economic_Eff)
  IND_Economic_Eff = IND_Economic_Eff[ rep( seq_len( nrow( IND_Economic_Eff ) ), nlevels(as.factor(IND_Hist_Energy_Demand$Energy)) ), ]
  IND_Economic_Eff$Energy = as.factor(rep(levels(as.factor(IND_Hist_Energy_Demand$Energy)), each = nlevels(as.factor(IND_Economic_Eff$Sub_Sector))*nlevels(as.factor(IND_Economic_Eff$Country)) ))
  IND_Economic_Eff <- IND_Economic_Eff[order(IND_Economic_Eff$Sub_Sector),]
  IND_Economic_Eff <- Sort_By_Country(IND_Economic_Eff)
  IND_Economic_Eff <- IND_Economic_Eff[order(IND_Economic_Eff$Energy),]
  
  ### Technical Efficiency Rate
  IND_Technical_Eff <- Extract_Item(IND_Proj_Assumption, "Technical efficiency")
  IND_Technical_Eff <- IND_Technical_Eff[order(IND_Technical_Eff$Sub_Sector),]
  IND_Technical_Eff <- Sort_By_Country(IND_Technical_Eff)
  IND_Technical_Eff <- IND_Technical_Eff[order(IND_Technical_Eff$Energy),]
  
  ###Global Efficiency Rate
  IND_Efficiency_Rate <- IND_Technical_Eff
  IND_Efficiency_Rate$ID_Item <- "Efficiency Rate"
  IND_Efficiency_Rate[,10:length(IND_Efficiency_Rate)] <- ( 1 + IND_Technical_Eff[,10:length(IND_Technical_Eff)] ) * ( 1 + IND_Economic_Eff[,10:length(IND_Economic_Eff)] ) - 1  
  
  #3. Projection of energy demand in industry sector 
  IND_Proj_Demand <- IND_Hist_Energy_Demand
  
  IND_Proj_Demand_Efficiency <- IND_Hist_Energy_Demand
  IND_Proj_Demand_Efficiency$ID_Item <- "Efficiency Energy Demand" 
  
  IND_Proj_Demand_Activity <- IND_Hist_Energy_Demand
  IND_Proj_Demand_Activity$ID_Item <- "Activity Energy Demand"
  
  IND_Proj_Demand_Substitution <- IND_Hist_Energy_Demand
  IND_Proj_Demand_Substitution$ID_Item <- "Substitution Energy Demand"
  
  IND_wanted_Proj_Energy_Demand <- Extract_Item(IND_calibration_subsitution, "Simulated Demand")
  IND_wanted_Proj_Energy_Demand <- IND_wanted_Proj_Energy_Demand[order(IND_wanted_Proj_Energy_Demand$Sub_Sector),]
  IND_wanted_Proj_Energy_Demand <- Sort_By_Country(IND_wanted_Proj_Energy_Demand)
  IND_wanted_Proj_Energy_Demand <- IND_wanted_Proj_Energy_Demand[order(IND_wanted_Proj_Energy_Demand$Energy),]
  
  Year_Proj_col = min(which(is.nan(colSums(IND_Hist_Energy_Demand[10:length(IND_Hist_Energy_Demand)])))) + 9
  
  for(i in Year_Proj_col:length(IND_Hist_Energy_Demand))
  {
    IND_Proj_Demand[,i] <- IND_Proj_Demand[,i-1] * (1+IND_Activity_Rate[,i]) * (1+IND_Substitution_Rate[,i]) * (1+IND_Efficiency_Rate[,i]) 
    IND_Proj_Demand_Efficiency[,i] <- IND_Proj_Demand_Efficiency[,i-1] * (1+IND_Efficiency_Rate[,i]) 
    IND_Proj_Demand_Activity[,i] <- IND_Proj_Demand_Activity[,i-1] * (1+IND_Activity_Rate[,i]) 
    IND_Proj_Demand_Substitution[,i] <- IND_Proj_Demand_Substitution[,i-1] * (1+IND_Substitution_Rate[,i])  
  }
  
  IND_Proj_Demand_not_simulated <- Extract_Specific_Energy(IND_Proj_Demand, c("biomass and waste", "direct hydrogen", "final heat", "total"))
  IND_Proj_Demand_simulated <- Extract_Specific_Energy(IND_Proj_Demand, c("coal", "electricity", "gas", "oil" ))
  
  for(i in Year_Proj_col:length(IND_Hist_Energy_Demand))
  {
    IND_Proj_Demand_simulated[,i] <- IND_wanted_Proj_Energy_Demand[,i] 
  }
  
  IND_Proj_Demand <- rbind(IND_Proj_Demand_simulated, IND_Proj_Demand_not_simulated)
  IND_Proj_Demand_hydrogen <- Extract_Specific_Energy(IND_Proj_Demand, "direct hydrogen")
  #IND_Proj_Demand <- droplevels(subset(IND_Proj_Demand, Energy!="direct hydrogen"))
  
  IND_Proj_Demand <- IND_Proj_Demand[order(as.character(IND_Proj_Demand$Energy)),]

  # On calcul les taux de substitution correspondant
  IND_Substitution_Rate <- IND_Proj_Demand

  for(i in 11:length(IND_Substitution_Rate))
  {
    IND_Substitution_Rate[,i] <-  IND_Proj_Demand[,i] / ( IND_Proj_Demand[,i-1] * (1+IND_Activity_Rate[,i]) * (1+IND_Efficiency_Rate[,i])) - 1
  }
  
  IND_Substitution_Rate$ID_Item <- "Ex-Post Substitution Rate"
  IND_Substitution_Rate$Unit <- "%"
  IND_Substitution_Rate[,10:(Year_Proj_col-1)] <- 0
  IND_Substitution_Rate[is.na(IND_Substitution_Rate$X2017), Year_Proj_col] <- Inf
  IND_Substitution_Rate[IND_Substitution_Rate[,Year_Proj_col] == Inf ,10:length(IND_Substitution_Rate)] <- 0  
  
  Proj_Assumption <<- rbind(Proj_Assumption, IND_Substitution_Rate)
  
  #On projette une seconde fois avec les taux de substitution corriges par les experts du CEEME
  
  IND_Substitution_Offset <- Extract_Item(Proj_Assumption, "Substitution Offset")
  IND_Substitution_Offset <- droplevels(subset(IND_Substitution_Offset, Energy!="direct hydrogen"))
  IND_Substitution_Offset <- IND_Substitution_Offset[order(IND_Substitution_Offset$Energy),]
  IND_Proj_Demand <- droplevels(subset(IND_Proj_Demand, Energy!="direct hydrogen"))
  IND_Activity_Rate_hydrogen <- Extract_Specific_Energy(IND_Activity_Rate, "direct hydrogen")
  IND_Efficiency_Rate_hydrogen <- Extract_Specific_Energy(IND_Efficiency_Rate, "direct hydrogen")
  IND_Activity_Rate <- droplevels(subset(IND_Activity_Rate, Energy!="direct hydrogen"))
  IND_Efficiency_Rate <- droplevels(subset(IND_Efficiency_Rate, Energy!="direct hydrogen"))
  #
  for(i in Year_Proj_col:length(IND_Hist_Energy_Demand))
  {
    IND_Proj_Demand[,i] <- IND_Proj_Demand[,i-1] * (1+IND_Activity_Rate[,i]) * (1+IND_Substitution_Offset[,i]) * (1+IND_Efficiency_Rate[,i])
    IND_Proj_Demand[,i] <- IND_Proj_Demand[,i-1] * (1+IND_Activity_Rate[,i]) * (1+IND_Efficiency_Rate[,i])
  }
  
  IND_Proj_Demand <- rbind(IND_Proj_Demand, IND_Proj_Demand_hydrogen)
  IND_Activity_Rate <- rbind(IND_Activity_Rate_hydrogen, IND_Proj_Demand_hydrogen)
  IND_Efficiency_Rate <- rbind(IND_Efficiency_Rate_hydrogen, IND_Proj_Demand_hydrogen)
   
  IND_Proj_Demand <- calculate_usage_split_IND(IND_Proj_Demand)
  IND_Proj_Demand <- calculate_total_sector_IND(IND_Proj_Demand)
  
  IND_Proj_Demand_Efficiency <- calculate_usage_split_IND(IND_Proj_Demand_Efficiency)
  IND_Proj_Demand_Efficiency <- calculate_total_sector_IND(IND_Proj_Demand_Efficiency)

  IND_Proj_Demand_Activity <- calculate_usage_split_IND(IND_Proj_Demand_Activity)
  IND_Proj_Demand_Activity <- calculate_total_sector_IND(IND_Proj_Demand_Activity)

  IND_Proj_Demand_Substitution <- calculate_usage_split_IND(IND_Proj_Demand_Substitution)
  IND_Proj_Demand_Substitution <- calculate_total_sector_IND(IND_Proj_Demand_Substitution)
  
  IND_Proj_Demand <- rbind(IND_Proj_Demand, IND_Proj_Demand_Efficiency, IND_Proj_Demand_Activity, IND_Proj_Demand_Substitution)
  IND_Proj_Demand$Energy <- as.factor(IND_Proj_Demand$Energy)
  
  return(IND_Proj_Demand) 
}

calculate_usage_split_IND <- function(input)
{
  #Extraction du split heating and cooling vs others
  IND_Usage_split_HC_vs_Others <- Extract_Item( Extract_Sector( Proj_Assumption, "IND" ), "share_usage_macro")
  IND_Usage_split_HC_vs_Others <- IND_Usage_split_HC_vs_Others[order(IND_Usage_split_HC_vs_Others$Sub_Sector),]
  IND_Usage_split_HC_vs_Others <- IND_Usage_split_HC_vs_Others[order(IND_Usage_split_HC_vs_Others$Usage),]
  IND_Usage_split_HC_vs_Others <- Sort_By_Country(IND_Usage_split_HC_vs_Others)
  IND_Usage_split_HC_vs_Others <- IND_Usage_split_HC_vs_Others[order(IND_Usage_split_HC_vs_Others$Energy),]
  IND_Usage_split_HC_vs_Others <- droplevels( subset( IND_Usage_split_HC_vs_Others, Sub_Sector!="Industry - energy" ) )
  
  #Extraction du split within heating and cooling
  IND_Usage_split_within_HC <- Extract_Item( Extract_Sector( Proj_Assumption, "IND" ), "Share_usage within heating and cooling")
  IND_Usage_split_within_HC <- IND_Usage_split_within_HC[order(IND_Usage_split_within_HC$Sub_Sector),]
  IND_Usage_split_within_HC <- IND_Usage_split_within_HC[order(IND_Usage_split_within_HC$Usage),]
  IND_Usage_split_within_HC <- Sort_By_Country(IND_Usage_split_within_HC)
  IND_Usage_split_within_HC <- IND_Usage_split_within_HC[order(IND_Usage_split_within_HC$Energy),]
  
  input <- input[ rep( seq_len( nrow( input ) ), nlevels(as.factor(IND_Usage_split_HC_vs_Others$Usage)) ), ]
  input <- Sort_By_Country(input)
  input$Energy  <- as.character(input$Energy)
  input <- input[order(input$Energy),]
  input <- input[order(input$Energy),]
  input$Usage <- IND_Usage_split_HC_vs_Others$Usage
  input[,10:length(input)] <- input[,10:length(input)] * IND_Usage_split_HC_vs_Others[,10:length(IND_Usage_split_HC_vs_Others)] 
  
  input_others <- Extract_Usage(input, "others")
  
  input_HC <- Extract_Usage(input, "heating and cooling")
  input_HC <- input_HC[ rep( seq_len( nrow( input_HC ) ), nlevels(as.factor(IND_Usage_split_within_HC$Usage)) ), ]
  input_HC <- Sort_By_Country(input_HC)
  input_HC <- input_HC[order(input_HC$Energy),]
  input_HC$Usage <- IND_Usage_split_within_HC$Usage
  input_HC[,10:length(input_HC)] <- input_HC[,10:length(input_HC)] * IND_Usage_split_within_HC[,10:length(IND_Usage_split_within_HC)]
  
  input <- rbind(input_HC, input_others)
  
  return(input)
}

calculate_total_sector_IND <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  #on recalcule le total pour chaque branche industrielle 
  IND_Proj_Demand_Sub_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sub_Sector")], FUN = sum)
  IND_Proj_Demand_Sub_Sector_total_header  <- input[1:nrow(IND_Proj_Demand_Sub_Sector_total_value), 1:9]  
  IND_Proj_Demand_Sub_Sector_total_header$Energy <- "total"
  IND_Proj_Demand_Sub_Sector_total_header$Country <- IND_Proj_Demand_Sub_Sector_total_value$Country
  IND_Proj_Demand_Sub_Sector_total_header$Sub_Sector <- IND_Proj_Demand_Sub_Sector_total_value$Sub_Sector
  IND_Proj_Demand_Sub_Sector_total_header$Usage <- "total"
  IND_Proj_Demand_Sub_Sector_total <- unique(cbind(IND_Proj_Demand_Sub_Sector_total_header, IND_Proj_Demand_Sub_Sector_total_value[,-(1:2)]))
                                             
  #on calcule le total secteur pour chaques Usages
  IND_Proj_Demand_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Usage")], FUN = sum)
  IND_Proj_Demand_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Usage_total_value), 1:9]  
  IND_Proj_Demand_Usage_total_header$Energy <- "total"
  IND_Proj_Demand_Usage_total_header$Country <- IND_Proj_Demand_Usage_total_value$Country
  IND_Proj_Demand_Usage_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Usage_total_header$Usage <- IND_Proj_Demand_Usage_total_value$Usage
  IND_Proj_Demand_Usage_total <- unique(cbind(IND_Proj_Demand_Usage_total_header, IND_Proj_Demand_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energys
  IND_Proj_Demand_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy")], FUN = sum)
  IND_Proj_Demand_Energy_total_header  <- input[1:nrow(IND_Proj_Demand_Energy_total_value), 1:9]  
  IND_Proj_Demand_Energy_total_header$Energy <- IND_Proj_Demand_Energy_total_value$Energy
  IND_Proj_Demand_Energy_total_header$Country <- IND_Proj_Demand_Energy_total_value$Country
  IND_Proj_Demand_Energy_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Energy_total_header$Usage <- "total"
  IND_Proj_Demand_Energy_total <- unique(cbind(IND_Proj_Demand_Energy_total_header, IND_Proj_Demand_Energy_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sectors
  IND_Proj_Demand_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sector")], FUN = sum)
  IND_Proj_Demand_Sector_total_header  <- input[1:nrow(IND_Proj_Demand_Sector_total_value), 1:9]  
  IND_Proj_Demand_Sector_total_header$Energy <- "total"
  IND_Proj_Demand_Sector_total_header$Country <- IND_Proj_Demand_Sector_total_value$Country
  IND_Proj_Demand_Sector_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Sector_total_header$Usage <- "total"
  IND_Proj_Demand_Sector_total <- unique(cbind(IND_Proj_Demand_Sector_total_header, IND_Proj_Demand_Sector_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energy/Sub_Sector
  IND_Proj_Demand_Energy_Sub_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy", "Sub_Sector")], FUN = sum)
  IND_Proj_Demand_Energy_Sub_total_header  <- input[1:nrow(IND_Proj_Demand_Energy_Sub_total_value), 1:9]  
  IND_Proj_Demand_Energy_Sub_total_header$Energy <- IND_Proj_Demand_Energy_Sub_total_value$Energy
  IND_Proj_Demand_Energy_Sub_total_header$Country <- IND_Proj_Demand_Energy_Sub_total_value$Country
  IND_Proj_Demand_Energy_Sub_total_header$Sub_Sector <- IND_Proj_Demand_Energy_Sub_total_value$Sub_Sector
  IND_Proj_Demand_Energy_Sub_total_header$Usage <- "total"
  IND_Proj_Demand_Energy_Sub_total <- unique(cbind(IND_Proj_Demand_Energy_Sub_total_header, IND_Proj_Demand_Energy_Sub_total_value[,-(1:3)]))
  
  #on calcule le total secteur pour chaques Energy/Usage
  IND_Proj_Demand_Energy_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy", "Usage")], FUN = sum)
  IND_Proj_Demand_Energy_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Energy_Usage_total_value), 1:9]  
  IND_Proj_Demand_Energy_Usage_total_header$Energy <- IND_Proj_Demand_Energy_Usage_total_value$Energy
  IND_Proj_Demand_Energy_Usage_total_header$Country <- IND_Proj_Demand_Energy_Usage_total_value$Country
  IND_Proj_Demand_Energy_Usage_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Energy_Usage_total_header$Usage <- IND_Proj_Demand_Energy_Usage_total_value$Usage
  IND_Proj_Demand_Energy_Usage_total <- unique(cbind(IND_Proj_Demand_Energy_Usage_total_header, IND_Proj_Demand_Energy_Usage_total_value[,-(1:3)]))
  
  #on calcule le total secteur pour chaques Sub_Sector/Usage
  IND_Proj_Demand_Sub_Sector_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sub_Sector", "Usage")], FUN = sum)
  IND_Proj_Demand_Sub_Sector_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Sub_Sector_Usage_total_value), 1:9]  
  IND_Proj_Demand_Sub_Sector_Usage_total_header$Energy <- "total"
  IND_Proj_Demand_Sub_Sector_Usage_total_header$Country <- IND_Proj_Demand_Sub_Sector_Usage_total_value$Country
  IND_Proj_Demand_Sub_Sector_Usage_total_header$Sub_Sector <- IND_Proj_Demand_Sub_Sector_Usage_total_value$Sub_Sector
  IND_Proj_Demand_Sub_Sector_Usage_total_header$Usage <- IND_Proj_Demand_Sub_Sector_Usage_total_value$Usage
  IND_Proj_Demand_Sub_Sector_Usage_total <- unique(cbind(IND_Proj_Demand_Sub_Sector_Usage_total_header, IND_Proj_Demand_Sub_Sector_Usage_total_value[,-(1:3)]))
  
  #on calcule le total EU23 pour chaques energy
  IND_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  IND_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Energy_total_value), 1:9]  
  IND_Proj_Demand_Country_Energy_total_header$Energy <- IND_Proj_Demand_Country_Energy_total_value$Energy
  IND_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  IND_Proj_Demand_Country_Energy_total <- unique(cbind(IND_Proj_Demand_Country_Energy_total_header, IND_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques usages
  IND_Proj_Demand_Country_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Usage")], FUN = sum)
  IND_Proj_Demand_Country_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Usage_total_value), 1:9]  
  IND_Proj_Demand_Country_Usage_total_header$Energy <- "total"
  IND_Proj_Demand_Country_Usage_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Usage_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Country_Usage_total_header$Usage <- IND_Proj_Demand_Country_Usage_total_value$Usage
  IND_Proj_Demand_Country_Usage_total <- unique(cbind(IND_Proj_Demand_Country_Usage_total_header, IND_Proj_Demand_Country_Usage_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  IND_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  IND_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Sector_total_value), 1:9]  
  IND_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  IND_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  IND_Proj_Demand_Country_Sector_total_header$Sector <- IND_Proj_Demand_Country_Sector_total_value$Sector
  IND_Proj_Demand_Country_Sector_total <- unique(cbind(IND_Proj_Demand_Country_Sector_total_header, IND_Proj_Demand_Country_Sector_total_value[,-1]))
  
  #on calcule le total secteur pour chaques Energy/Sub_Sector
  IND_Proj_Demand_Country_Energy_Sub_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Sub_Sector")], FUN = sum)
  IND_Proj_Demand_Country_Energy_Sub_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Energy_Sub_total_value), 1:9]  
  IND_Proj_Demand_Country_Energy_Sub_total_header$Energy <- IND_Proj_Demand_Country_Energy_Sub_total_value$Energy
  IND_Proj_Demand_Country_Energy_Sub_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Energy_Sub_total_header$Sub_Sector <- IND_Proj_Demand_Country_Energy_Sub_total_value$Sub_Sector
  IND_Proj_Demand_Country_Energy_Sub_total_header$Usage <- "total"
  IND_Proj_Demand_Country_Energy_Sub_total <- unique(cbind(IND_Proj_Demand_Country_Energy_Sub_total_header, IND_Proj_Demand_Country_Energy_Sub_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Energy/Usage
  IND_Proj_Demand_Country_Energy_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Usage")], FUN = sum)
  IND_Proj_Demand_Country_Energy_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Energy_Usage_total_value), 1:9]  
  IND_Proj_Demand_Country_Energy_Usage_total_header$Energy <- IND_Proj_Demand_Country_Energy_Usage_total_value$Energy
  IND_Proj_Demand_Country_Energy_Usage_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Energy_Usage_total_header$Sub_Sector <- "total"
  IND_Proj_Demand_Country_Energy_Usage_total_header$Usage <- IND_Proj_Demand_Country_Energy_Usage_total_value$Usage
  IND_Proj_Demand_Country_Energy_Usage_total <- unique(cbind(IND_Proj_Demand_Country_Energy_Usage_total_header, IND_Proj_Demand_Country_Energy_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sub_Sector/Usage
  IND_Proj_Demand_Country_Sub_Sector_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sub_Sector", "Usage")], FUN = sum)
  IND_Proj_Demand_Country_Sub_Sector_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Sub_Sector_Usage_total_value), 1:9]  
  IND_Proj_Demand_Country_Sub_Sector_Usage_total_header$Energy <- "total"
  IND_Proj_Demand_Country_Sub_Sector_Usage_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Sub_Sector_Usage_total_header$Sub_Sector <- IND_Proj_Demand_Country_Sub_Sector_Usage_total_value$Sub_Sector
  IND_Proj_Demand_Country_Sub_Sector_Usage_total_header$Usage <- IND_Proj_Demand_Country_Sub_Sector_Usage_total_value$Usage
  IND_Proj_Demand_Country_Sub_Sector_Usage_total <- unique(cbind(IND_Proj_Demand_Country_Sub_Sector_Usage_total_header, IND_Proj_Demand_Country_Sub_Sector_Usage_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sub_Sector/Usage
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Sub_Sector", "Usage")], FUN = sum)
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header  <- input[1:nrow(IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value), 1:9]  
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Energy <- IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value$Energy
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Country <- "EU23"
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Sub_Sector <- IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value$Sub_Sector
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header$Usage <- IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value$Usage
  IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total <- unique(cbind(IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_header, IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total_value[,-(1:3)]))
  
  input <- rbind(input, IND_Proj_Demand_Sub_Sector_total, IND_Proj_Demand_Usage_total, IND_Proj_Demand_Energy_total, IND_Proj_Demand_Energy_Sub_total,IND_Proj_Demand_Energy_Usage_total, 
                 IND_Proj_Demand_Sub_Sector_Usage_total, IND_Proj_Demand_Sector_total, IND_Proj_Demand_Country_Energy_total, IND_Proj_Demand_Country_Usage_total,
                 IND_Proj_Demand_Country_Sector_total, IND_Proj_Demand_Country_Energy_Sub_total, IND_Proj_Demand_Country_Energy_Usage_total,
                 IND_Proj_Demand_Country_Sub_Sector_Usage_total, IND_Proj_Demand_Country_Energy_Sub_Sector_Usage_total) 
  
  
  return(input)
}