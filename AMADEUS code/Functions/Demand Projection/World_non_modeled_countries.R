############################################## AMADEUS World Projection Non-modeled countries  ################################################

main_wolrd_non_modeled_countries_projection <- function()
{
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  enerdata_starting_point <- read.xlsx2("../../Data/Inputs/Benchmarks Data/Enerdata Historical World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  country_classification <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/Country Classification.xlsx", sheetIndex = 1)
  country_mapping <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/Country Mapping.xlsx", sheetIndex = 1)
  IHS_World_projection <- read.xlsx2("../../Data/Inputs/Benchmarks Data/IHS Rivalry World data.xlsx", sheetIndex = 1 , colClasses = column_type)
  
  enerdata_starting_point_base <- enerdata_starting_point
  
  enerdata_starting_point <- enerdata_total_energy_computation(enerdata_starting_point) 
  enerdata_starting_point <- enerdata_reshaping(enerdata_starting_point, country_classification, country_mapping, "Zone sector")
  
  
  IHS_World_projection <- droplevels(IHS_World_projection[IHS_World_projection$Energy %in%  c("coal", "gas", "oil", "electricity", "others"),])
  IHS_World_projection <- droplevels(IHS_World_projection[IHS_World_projection$Sector !=  "Primary",])
  
  country_available <- levels(as.factor(IHS_World_projection$Country))
  energy_available <- levels(as.factor(enerdata_starting_point$Energy))
  region_available <- levels(as.factor(enerdata_starting_point$Region))
  sector_available <- levels(as.factor(IHS_World_projection$Sector))
  
  energy_IHS <- energy_available
  energy_IHS <- data.frame("enerdata" = energy_IHS, "IHS" = c("others", "coal", "electricity", "others", "gas", "oil", "total"))

  energy_demand_projection <- NULL
  final_energy_demand_projection <- NULL
  region_energy_demand_projection <- NULL
  Year_Proj <- 29
  
  for(i in 1:length(region_available))
  {
    for(j in 1:length(sector_available))
    {
        for(k in 1:length(energy_available)) 
        {
            current_energy_IHS <- as.character(energy_IHS[energy_IHS$enerdata == energy_available[k],2])
            energy_demand_projection <- enerdata_starting_point[enerdata_starting_point$Region == region_available[i] & enerdata_starting_point$Sector == sector_available[j] & enerdata_starting_point$Energy == energy_available[k],]
            region_energy_demand_projection <- IHS_World_projection[IHS_World_projection$Country == region_available[i] & IHS_World_projection$Sector == sector_available[j] & IHS_World_projection$Energy == current_energy_IHS,]
            
            for(l in Year_Proj:length(enerdata_starting_point)) 
            {
              energy_demand_projection[,l] <- energy_demand_projection[,l-1] * region_energy_demand_projection[,l-2] / region_energy_demand_projection[,l-3] 
            }
            final_energy_demand_projection <- rbind(final_energy_demand_projection, energy_demand_projection)
            l <- Year_Proj
        }
        k <- 1
    }
    j <- 1
  }
  
  final_energy_demand_projection <- droplevels(final_energy_demand_projection[,-(2:3)])
  final_energy_demand_projection <- cbind(final_energy_demand_projection[,(2:3)], final_energy_demand_projection[,1], final_energy_demand_projection[,(4:length(final_energy_demand_projection))])
  names(final_energy_demand_projection)[3] <- "Country"
  
  return(final_energy_demand_projection)
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

enerdata_market_shares <- function(enerdata_raw_data)
{
  country_available <- levels(as.factor(enerdata_raw_data$Country))
  sector_available <- levels(as.factor(enerdata_raw_data$Sector))
  last_common_year <- min(which(is.nan(colSums(enerdata_raw_data[10:length(enerdata_raw_data)])))) + 9 - 1
  
  enerdata_historical_market_shares <- NULL
  enerdata_country_market_shares <- NULL
  for(j in 1:length(country_available))
  {
    for(k in 1:length(sector_available))
    {
      enerdata_country <- enerdata_raw_data[enerdata_raw_data$Country == country_available[j] & enerdata_raw_data$Sector == sector_available[k],]
      enerdata_country_total <- enerdata_country[enerdata_country$Energy == "total",]
      enerdata_country_market_shares <- enerdata_country
      for(i in 10:last_common_year)
      {
        if(enerdata_country_total[,i] == 0)
        {
          enerdata_country_market_shares[,i] <- 0
        }
        else
        {
          enerdata_country_market_shares[,i] <- enerdata_country[,i] / enerdata_country_total[,i] 
        }
      }
      enerdata_historical_market_shares <- rbind(enerdata_historical_market_shares, enerdata_country_market_shares)
    }
  }
  
  enerdata_historical_market_shares[,10] <- enerdata_historical_market_shares[, last_common_year]
  enerdata_historical_market_shares <- enerdata_historical_market_shares[,1:10]
  names(enerdata_historical_market_shares)[10] <- "Avg_hist_market_shares"
  
  return(enerdata_historical_market_shares)
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

energy_others_projection <- function(energy_demand_projection, Year_Proj, enerdata_hist_energy_data, country_classification, country_mapping)
{
  energy_demand_projection_others <- energy_demand_projection[energy_demand_projection$Energy == "others" ,]
  
  
  enerdata_hist_energy_data_others <- merge(country_classification, enerdata_hist_energy_data)
  enerdata_hist_energy_data_others <- droplevels(enerdata_hist_energy_data_others[enerdata_hist_energy_data_others$Country_Modelling ==  "Zone sector",])
  enerdata_hist_energy_data_others <- droplevels(enerdata_hist_energy_data_others[enerdata_hist_energy_data_others$Sector !=  "Final",])
  enerdata_hist_energy_data_others <- enerdata_hist_energy_data_others[,-(2:3)]
  enerdata_hist_energy_data_others <- droplevels(enerdata_hist_energy_data_others[enerdata_hist_energy_data_others$Energy %in% c("biomass and waste", "direct hydrogen", "final heat"), ])
  enerdata_hist_energy_data_others <- enerdata_hist_energy_data_others[order(as.character(enerdata_hist_energy_data_others$Energy)),]
  enerdata_hist_energy_data_others <- enerdata_hist_energy_data_others[order(as.character(enerdata_hist_energy_data_others$Sector)),]
  enerdata_hist_energy_data_others <- enerdata_hist_energy_data_others[order(as.character(enerdata_hist_energy_data_others$Country)),]
  
  
  energy_demand_projection_others <- energy_demand_projection_others[ rep( seq_len( nrow( energy_demand_projection_others ) ), (nlevels(as.factor(enerdata_hist_energy_data_others$Energy))) ), ]
  energy_demand_projection_others$Energy <- as.factor( rep(c("biomass and waste", "final heat"), nrow(energy_demand_projection_others)/2) )
  energy_demand_projection_others_TRA <- energy_demand_projection_others[energy_demand_projection_others$Sector == "TRA" & energy_demand_projection_others$Energy == "biomass and waste",] 
  energy_demand_projection_others <- rbind(energy_demand_projection_others[energy_demand_projection_others$Sector != "TRA",], energy_demand_projection_others_TRA)
  
  energy_demand_projection_others <- energy_demand_projection_others[order(as.character(energy_demand_projection_others$Energy)),]
  energy_demand_projection_others <- energy_demand_projection_others[order(as.character(energy_demand_projection_others$Sector)),]
  energy_demand_projection_others <- energy_demand_projection_others[order(as.character(energy_demand_projection_others$Country)),]
  
  enerdata_proj_energy_data_others <- enerdata_hist_energy_data_others
  for(i in Year_Proj:length(energy_demand_projection_others))
  {
     enerdata_proj_energy_data_others[,i] <- enerdata_proj_energy_data_others[,i-1] * energy_demand_projection_others[,i] / energy_demand_projection_others[,i-1]  
  }
  
  final_energy_demand_projection <- rbind(energy_demand_projection[energy_demand_projection$Energy != "others" ,] ,enerdata_proj_energy_data_others)
  return(final_energy_demand_projection)
}
