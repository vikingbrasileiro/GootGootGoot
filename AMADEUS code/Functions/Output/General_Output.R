source("../Functions/Input/General_Inputs_Functions.R")

main_output <- function(Proj_Energy_Demand, Hist_Energy_Demand)
{
  #On ajoute la parite unspecified de Eurostat, pour que les totaux finaux collent
  
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, add_unspecified_sector(Proj_Energy_Demand))
  Proj_Energy_Demand$Energy <- as.factor(Proj_Energy_Demand$Energy)
  
  #Final Energy Demand 
  input <- Extract_Sector( Extract_Item(Proj_Energy_Demand, "Energy Demand"), c("IND", "RES", "TER", "TRA", "AGR", "NEU", "Unspecified"))
  input <- Extract_Sub_Sector( Extract_Usage(input, "total"), "total" )
  input$Energy <- as.factor(input$Energy)
  
  Final_Energy_Demand_value <- sapply(input[,10:length(input)], function(x){ tapply(x, list(input$Country, input$Energy), sum)})
  Final_Energy_Demand_header  <- input[1:nrow(Final_Energy_Demand_value), 1:9]
  Final_Energy_Demand_header$Sector <- "Final"
  Final_Energy_Demand_header$Country <- rep(levels(as.factor(input$Country)), nlevels(as.factor(input$Energy)))
  Final_Energy_Demand_header$Energy <- rep(levels(as.factor(input$Energy)), each = nlevels(as.factor(input$Country)))
  Final_Energy_Demand <- unique(cbind(Final_Energy_Demand_header,Final_Energy_Demand_value))
  
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, Final_Energy_Demand) 
  
  #ESU Projection
  Final_Energy_Demand <- Sort_By_Country(Final_Energy_Demand) 
  Final_Energy_Demand <- Final_Energy_Demand[order(Final_Energy_Demand$Energy),]
  Final_Energy_Demand <- droplevels(subset(Final_Energy_Demand, Country !="EU23"))
  Final_Energy_Demand <- droplevels(subset(Final_Energy_Demand, Energy !="plug-in hybrid"))
  Final_Energy_Demand <- droplevels(subset(Final_Energy_Demand, Energy !="self-recharge hybrid"))
  
  #Losses
  Losses_Hist_Energy_Demand <- Extract_Sub_Sector( Extract_Sector( Hist_Energy_Demand, "ESU" ), "Losses" )
  Losses_Hist_Energy_Demand <- Sort_By_Country(Losses_Hist_Energy_Demand)
  Losses_Hist_Energy_Demand <- Losses_Hist_Energy_Demand[order(Losses_Hist_Energy_Demand$Energy),]
  
  Ratio_Losses_Final_Demand <- Losses_Hist_Energy_Demand 
  Ratio_Losses_Final_Demand[,10:length(Ratio_Losses_Final_Demand)] <- Losses_Hist_Energy_Demand[,10:length(Losses_Hist_Energy_Demand)] / Final_Energy_Demand[,10:length(Final_Energy_Demand)]
  Ratio_Losses_Final_Demand$ID_Item <- "Ratio Losses/Final Demand"
  Ratio_Losses_Final_Demand$Unit <- "%"
  Proj_Assumption <- rbind(Proj_Assumption, Ratio_Losses_Final_Demand) 
  saveRDS(Proj_Assumption, "../../Data/temp/Proj_Assumption.RDS")
  
  # last_hist_year_col <- min(which(is.nan(colSums(Ratio_Losses_Final_Demand[,10:length(Ratio_Losses_Final_Demand)])))) + 9
  # Ratio_Losses_Final_Demand <- cbind( Ratio_Losses_Final_Demand[,1:9], rowMeans(Ratio_Losses_Final_Demand[,(last_hist_year_col-6):(last_hist_year_col-1)])) 
  
  #Refineries
  Refineries_Hist_Energy_Demand <- Extract_Sub_Sector( Extract_Sector( Hist_Energy_Demand, "ESU" ), "Refineries" )
  Refineries_Hist_Energy_Demand <- Sort_By_Country(Refineries_Hist_Energy_Demand)
  Refineries_Hist_Energy_Demand <- Refineries_Hist_Energy_Demand[order(Refineries_Hist_Energy_Demand$Energy),]
  
  Ratio_Refineries_Final_Demand <- Refineries_Hist_Energy_Demand 
  Ratio_Refineries_Final_Demand[,10:length(Ratio_Refineries_Final_Demand)] <- Refineries_Hist_Energy_Demand[,10:length(Refineries_Hist_Energy_Demand)] / Final_Energy_Demand[,10:length(Final_Energy_Demand)]
  Ratio_Refineries_Final_Demand$ID_Item <- "Ratio Refineries/Final Demand"
  Ratio_Refineries_Final_Demand$Unit <- "%"
  Proj_Assumption <- rbind(Proj_Assumption, Ratio_Refineries_Final_Demand) 
  saveRDS(Proj_Assumption, "../../Data/temp/Proj_Assumption.RDS")
  # 
  # last_hist_year_col_refineries <- min(which(is.nan(colSums(Ratio_Refineries_Final_Demand[,10:length(Ratio_Refineries_Final_Demand)])))) + 9
  # Ratio_Refineries_Final_Demand <- cbind( Ratio_Refineries_Final_Demand[,1:9], rowMeans(Ratio_Refineries_Final_Demand[,(last_hist_year_col_refineries-6):(last_hist_year_col_refineries-1)])) 
  # 
  #Projection
  Losses_Proj_Energy_Demand <- Losses_Hist_Energy_Demand
  Refineries_Proj_Energy_Demand <- Refineries_Hist_Energy_Demand
  tc_Final_Energy_Demand <- derivate_dataframe(Final_Energy_Demand)
  
  year_proj_col <- min(which(is.nan(colSums(Losses_Hist_Energy_Demand[,10:length(Losses_Hist_Energy_Demand)])))) + 9
  
  for(i in year_proj_col:length(Losses_Hist_Energy_Demand))
  {
    Losses_Proj_Energy_Demand[,i] <- Losses_Proj_Energy_Demand[,i-1] * ( 1 + tc_Final_Energy_Demand[,i] )
    Refineries_Proj_Energy_Demand[,i] <- Refineries_Proj_Energy_Demand[,i-1] * ( 1 + tc_Final_Energy_Demand[,i] )
  }
  
  Losses_Proj_Energy_Demand$Usage <- "total"
  Refineries_Proj_Energy_Demand$Usage <- "total"
  ESU_Proj_Energy_Demand <- rbind( Losses_Proj_Energy_Demand, Refineries_Proj_Energy_Demand )
  ESU_Proj_Energy_Demand <- calculate_total_sector_ESU(ESU_Proj_Energy_Demand)
  Proj_Energy_Demand <- rbind(Proj_Energy_Demand, ESU_Proj_Energy_Demand)

  return(Proj_Energy_Demand)  
}

calculate_total_sector_ESU <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  #on calcule le total secteur pour chaques Energys
  ESU_Proj_Demand_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy")], FUN = sum)
  ESU_Proj_Demand_Energy_total_header  <- input[1:nrow(ESU_Proj_Demand_Energy_total_value), 1:9]  
  ESU_Proj_Demand_Energy_total_header$Energy <- ESU_Proj_Demand_Energy_total_value$Energy
  ESU_Proj_Demand_Energy_total_header$Country <- ESU_Proj_Demand_Energy_total_value$Country
  ESU_Proj_Demand_Energy_total_header$Sub_Sector <- "total"
  ESU_Proj_Demand_Energy_total_header$Usage <- "total"
  ESU_Proj_Demand_Energy_total <- unique(cbind(ESU_Proj_Demand_Energy_total_header, ESU_Proj_Demand_Energy_total_value[,-(1:2)]))
  
  #on calcule le total secteur pour chaques Sectors
  ESU_Proj_Demand_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sector")], FUN = sum)
  ESU_Proj_Demand_Sector_total_header  <- input[1:nrow(ESU_Proj_Demand_Sector_total_value), 1:9]  
  ESU_Proj_Demand_Sector_total_header$Energy <- "total"
  ESU_Proj_Demand_Sector_total_header$Country <- ESU_Proj_Demand_Sector_total_value$Country
  ESU_Proj_Demand_Sector_total_header$Sub_Sector <- "total"
  ESU_Proj_Demand_Sector_total_header$Usage <- "total"
  ESU_Proj_Demand_Sector_total <- unique(cbind(ESU_Proj_Demand_Sector_total_header, ESU_Proj_Demand_Sector_total_value[,-(1:2)]))
  
  #on calcule le total EU23 pour chaques energy
  ESU_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  ESU_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(ESU_Proj_Demand_Country_Energy_total_value), 1:9]  
  ESU_Proj_Demand_Country_Energy_total_header$Energy <- ESU_Proj_Demand_Country_Energy_total_value$Energy
  ESU_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
  ESU_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  ESU_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  ESU_Proj_Demand_Country_Energy_total <- unique(cbind(ESU_Proj_Demand_Country_Energy_total_header, ESU_Proj_Demand_Country_Energy_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques Sectors
  ESU_Proj_Demand_Country_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  ESU_Proj_Demand_Country_Sector_total_header  <- input[1:nrow(ESU_Proj_Demand_Country_Sector_total_value), 1:9]  
  ESU_Proj_Demand_Country_Sector_total_header$Energy <- "total"
  ESU_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
  ESU_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
  ESU_Proj_Demand_Country_Sector_total_header$Usage <- "total"
  ESU_Proj_Demand_Country_Sector_total_header$Sector <- ESU_Proj_Demand_Country_Sector_total_value$Sector
  ESU_Proj_Demand_Country_Sector_total <- unique(cbind(ESU_Proj_Demand_Country_Sector_total_header, ESU_Proj_Demand_Country_Sector_total_value[,-1]))
  
  input <- rbind(input, ESU_Proj_Demand_Energy_total, ESU_Proj_Demand_Sector_total, ESU_Proj_Demand_Country_Energy_total, ESU_Proj_Demand_Country_Sector_total) 
  
  
  return(input)
}

total_eurostat_checking <- function(diff_accurracy, Proj_Energy_Demand)
{
  #On recupere les totaux secteur Eurostat pour comparaison 
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Total_Eurostat_Data <-read.xlsx2("../../Data/Inputs/Benchmarks Data/Eurostat_Total_Energy_Demand.xlsx", sheetIndex = 2 , colClasses = column_type )
  Total_Eurostat_Data <-ktoe_to_TWh(Total_Eurostat_Data)
  Total_Eurostat_Data <- Sort_By_Country(Total_Eurostat_Data)
  
  Total_Eurostat_Data_unspecified <- Extract_Sector(Total_Eurostat_Data, "non-spe")
  Total_Eurostat_Data_unspecified <- Total_Eurostat_Data_unspecified[order(Total_Eurostat_Data_unspecified$Energy),]
  Total_Eurostat_Data_unspecified <- droplevels(subset(Total_Eurostat_Data_unspecified, Energy !="direct hydrogen"))
  
  Total_Eurostat_Data <- Extract_Sector(Total_Eurostat_Data, c("agr","ind","res", "ter", "trans", "Final"))
  Total_Eurostat_Data <- Total_Eurostat_Data[order(Total_Eurostat_Data$Energy),]
  Total_Eurostat_Data <- Total_Eurostat_Data[order(Total_Eurostat_Data$Sector),]
  
  Output_to_compare <- Extract_Sector( Extract_Item(Proj_Energy_Demand, "Energy Demand"), c("IND", "RES", "TER", "AGR", "TRA", "Final"))
  Output_to_compare <- Extract_Sub_Sector( Extract_Usage(Output_to_compare, "total"), "total" )
  Output_to_compare <- Extract_Scenario(Output_to_compare, "GB")
  Output_to_compare <- droplevels(subset(Output_to_compare, Country !="EU23"))
  Output_to_compare <- droplevels(subset(Output_to_compare, Energy !="direct hydrogen"))
  Output_to_compare <- droplevels(subset(Output_to_compare, Energy !="plug-in hybrid"))
  Output_to_compare_final_heat <- Extract_Sector(droplevels(subset(Output_to_compare, Energy =="self-recharge hybrid")), "TRA")
  Output_to_compare <- droplevels(subset(Output_to_compare, Energy !="self-recharge hybrid"))
  Output_to_compare_final_heat$Energy <- "oil"
  Output_to_compare_final_heat[,10:length(Output_to_compare_final_heat)] <- 0 
  Output_to_compare <- rbind(Output_to_compare, Output_to_compare_final_heat)
  
  Output_to_compare$Sector <- as.character(Output_to_compare$Sector) 
  Output_to_compare <- Sort_By_Country(Output_to_compare)
  Output_to_compare <- Output_to_compare[order(Output_to_compare$Energy),]
  Output_to_compare <- Output_to_compare[order(Output_to_compare$Sector),]
  Output_to_compare <- Output_to_compare[order(Output_to_compare$Sector),]
  
  
  #Present dans final definit de notre maniere (AGR+RES+TER+IND+NEU+TRA) mais pas dans final Eurostat
  Output_to_compare_NEU <- Extract_Sector( Extract_Item(Proj_Energy_Demand, "Energy Demand"), "NEU")
  Output_to_compare_NEU <- droplevels(subset(Output_to_compare_NEU, Country !="EU23"))
  Output_to_compare_NEU <- droplevels(subset(Output_to_compare_NEU, Energy !="direct hydrogen"))
  Output_to_compare_NEU <- Sort_By_Country(Output_to_compare_NEU)
  Output_to_compare_NEU <- Output_to_compare_NEU[order(Output_to_compare_NEU$Energy),]
  
  Diff <- Output_to_compare
  Diff[,10:length(Diff)] <- Total_Eurostat_Data[,10:length(Total_Eurostat_Data)] - Output_to_compare[,10:length(Output_to_compare)]
  Diff[Diff$Sector == "Final",10:length(Diff)] <- Diff[Diff$Sector == "Final",10:length(Diff)] +  Output_to_compare_NEU[,10:length(Output_to_compare_NEU)] # - Total_Eurostat_Data_unspecified[,10:length(Total_Eurostat_Data_unspecified)]
  Diff[,10] <- rowMeans(Diff[,10:25], na.rm = T)
  Diff <- Diff[,1:10]

  Diff <- Diff[abs(Diff$X2000) >= diff_accurracy,]
  
  return(Diff)

}

add_unspecified_sector <- function(Proj_Energy_Demand)
{
  #list_unspecified_to_add <- Extract_Sector(total_eurostat_checking(0), "Final")
  
  #On recupere les totaux secteur Eurostat pour comparaison 
    column_type = c(rep("character", times = 9),rep("numeric", times = 51))
    Total_Eurostat_Data <-read.xlsx2("../../Data/Inputs/Benchmarks Data/Eurostat_Total_Energy_Demand.xlsx", sheetIndex = 2 , colClasses = column_type )
    Total_Eurostat_Data <-ktoe_to_TWh(Total_Eurostat_Data)
    Total_Eurostat_Data <- Sort_By_Country(Total_Eurostat_Data)
    
    Total_Eurostat_Data_unspecified <- Extract_Sector(Total_Eurostat_Data, "non-spe")
    Total_Eurostat_Data_unspecified <- Total_Eurostat_Data_unspecified[order(Total_Eurostat_Data_unspecified$Energy),]
    Total_Eurostat_Data_unspecified <- droplevels(subset(Total_Eurostat_Data_unspecified, Energy !="direct hydrogen"))
    
    RES_TER_Amadeus_Model <- Extract_Sector(Proj_Energy_Demand, c("RES","TER"))
    RES_TER_Amadeus_Model <- Extract_Sub_Sector( Extract_Usage(RES_TER_Amadeus_Model, "total"), "total" )
    RES_TER_Amadeus_Model <- Extract_Scenario(RES_TER_Amadeus_Model, "GB")
    RES_TER_Amadeus_Model <- droplevels(subset(RES_TER_Amadeus_Model, Country !="EU23"))
    RES_TER_Amadeus_Model <- droplevels(subset(RES_TER_Amadeus_Model, Energy !="direct hydrogen"))
    
    RES_TER_Amadeus_Model$Sector <- as.character(RES_TER_Amadeus_Model$Sector) 
    RES_TER_Amadeus_Model <- Sort_By_Country(RES_TER_Amadeus_Model)
    RES_TER_Amadeus_Model <- RES_TER_Amadeus_Model[order(RES_TER_Amadeus_Model$Energy),]
    RES_TER_Amadeus_Model <- RES_TER_Amadeus_Model[order(RES_TER_Amadeus_Model$Sector),]
    RES_TER_Amadeus_Model <- RES_TER_Amadeus_Model[order(RES_TER_Amadeus_Model$Sector),]
    
    RES_TER_Amadeus_Model_value <- aggregate(x = RES_TER_Amadeus_Model[,10:length(RES_TER_Amadeus_Model)], by = RES_TER_Amadeus_Model[c("Country", "Energy")], FUN = sum)
    RES_TER_Amadeus_Model_header  <- RES_TER_Amadeus_Model[1:nrow(RES_TER_Amadeus_Model_value), 1:9]  
    RES_TER_Amadeus_Model_header$Energy <- RES_TER_Amadeus_Model_value$Energy
    RES_TER_Amadeus_Model_header$Country <- RES_TER_Amadeus_Model_value$Country
    RES_TER_Amadeus_Model_header$Sub_Sector <- "total"
    RES_TER_Amadeus_Model_header$Usage <- "total"
    RES_TER_Amadeus_Model <- unique(cbind(RES_TER_Amadeus_Model_header, RES_TER_Amadeus_Model_value[,-(1:2)]))
    
    RES_TER_Amadeus_Model <- derivate_dataframe(RES_TER_Amadeus_Model)
    
    Year_Proj_col = min(which(is.nan(colSums(Total_Eurostat_Data_unspecified[10:length(Total_Eurostat_Data_unspecified)])))) + 9
    
    for(i in Year_Proj_col:length(Total_Eurostat_Data_unspecified))
    {
      Total_Eurostat_Data_unspecified[,i] <- Total_Eurostat_Data_unspecified[,i-1] * ( 1 + RES_TER_Amadeus_Model[,i])
    }
    
    Total_Eurostat_Data_unspecified$Scenario <- "GB"
    Total_Eurostat_Data_unspecified$Sector <- "Unspecified"
    Total_Eurostat_Data_unspecified$ID_Item <- "Energy Demand"
    Total_Eurostat_Data_unspecified$Sub_Sector <- "total"
    Total_Eurostat_Data_unspecified$Usage <- "total"
    
    #on calcule le total EU23 pour chaques energy
    UNS_Proj_Demand_Country_Energy_total_value <- aggregate(x = Total_Eurostat_Data_unspecified[,10:length(Total_Eurostat_Data_unspecified)], by = Total_Eurostat_Data_unspecified[c("Energy")], FUN = sum)
    UNS_Proj_Demand_Country_Energy_total_header  <- Total_Eurostat_Data_unspecified[1:nrow(UNS_Proj_Demand_Country_Energy_total_value), 1:9]  
    UNS_Proj_Demand_Country_Energy_total_header$Energy <- UNS_Proj_Demand_Country_Energy_total_value$Energy
    UNS_Proj_Demand_Country_Energy_total_header$Country <- "EU23"
    UNS_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
    UNS_Proj_Demand_Country_Energy_total_header$Usage <- "total"
    UNS_Proj_Demand_Country_Energy_total <- unique(cbind(UNS_Proj_Demand_Country_Energy_total_header, UNS_Proj_Demand_Country_Energy_total_value[,-1]))
    
    # #on calcule le total EU23 pour chaques Sectors
    # UNS_Proj_Demand_Country_Sector_total_value <- aggregate(x = Total_Eurostat_Data_unspecified[,10:length(Total_Eurostat_Data_unspecified)], by = Total_Eurostat_Data_unspecified[c("Sector")], FUN = sum)
    # UNS_Proj_Demand_Country_Sector_total_header  <- Total_Eurostat_Data_unspecified[1:nrow(UNS_Proj_Demand_Country_Sector_total_value), 1:9]  
    # UNS_Proj_Demand_Country_Sector_total_header$Energy <- "total"
    # UNS_Proj_Demand_Country_Sector_total_header$Country <- "EU23"
    # UNS_Proj_Demand_Country_Sector_total_header$Sub_Sector <- "total"
    # UNS_Proj_Demand_Country_Sector_total_header$Usage <- "total"
    # UNS_Proj_Demand_Country_Sector_total_header$Sector <- UNS_Proj_Demand_Country_Sector_total_value$Sector
    # UNS_Proj_Demand_Country_Sector_total <- unique(cbind(UNS_Proj_Demand_Country_Sector_total_header, UNS_Proj_Demand_Country_Sector_total_value[,-1]))  
    
    Total_Eurostat_Data_unspecified <- rbind(Total_Eurostat_Data_unspecified, UNS_Proj_Demand_Country_Energy_total)
    
    return(Total_Eurostat_Data_unspecified) 
}
