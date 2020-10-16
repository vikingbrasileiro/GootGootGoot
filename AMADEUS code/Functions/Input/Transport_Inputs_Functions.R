
source("../Functions/Input/General_Inputs_Functions.R")

############################################################### Main Function ######################################################################################

main_input_Hist_TRA <- function(Hist_Macro, Hist_Assumption, Hist_Energy_Demand) 
{
  #1. Data input extraction 
  
  TRA_Hist_Assumption <- Extract_Sector(Hist_Assumption, "TRA")
  TRA_Hist_Assumption <- Sort_By_Country(TRA_Hist_Assumption)
  TRA_Hist_Assumption <- TRA_Hist_Assumption[order(TRA_Hist_Assumption$Usage),]
  TRA_Hist_Assumption <- TRA_Hist_Assumption[order(TRA_Hist_Assumption$Energy),]
  TRA_Hist_Assumption <- TRA_Hist_Assumption[order(TRA_Hist_Assumption$ID_Item),]
  
  TRA_Hist_Macro <- Extract_Item(Hist_Macro, c("GDP","Population"))
  TRA_Hist_Macro <- Sort_By_Country(TRA_Hist_Macro)
  TRA_Hist_Macro <- TRA_Hist_Macro[order(TRA_Hist_Macro$ID_Item),]
  
  TRA_Hist_Stock <- Extract_Item(TRA_Hist_Assumption, "Vehicle Stock")
  TRA_Hist_Stock <- TRA_Hist_Stock[order(as.character(TRA_Hist_Stock$Usage)),]
  TRA_Hist_Stock <- Sort_By_Country(TRA_Hist_Stock) 
  
  TRA_Hist_Annual_Distance <- Extract_Item(TRA_Hist_Assumption, "Annual distance")
  TRA_Hist_Annual_Distance <- TRA_Hist_Annual_Distance[order(as.character(TRA_Hist_Annual_Distance$Usage)),]
  TRA_Hist_Annual_Distance <- Sort_By_Country(TRA_Hist_Annual_Distance) 
  
  TRA_Hist_Pkm <- Extract_Item(TRA_Hist_Assumption, "Passenger traffic")
  TRA_Hist_Pkm <- TRA_Hist_Pkm[order(as.character(TRA_Hist_Pkm$Usage)),]
  TRA_Hist_Pkm <- TRA_Hist_Pkm[order(as.character(TRA_Hist_Pkm$Sub_Sector)),]
  TRA_Hist_Pkm <- Sort_By_Country(TRA_Hist_Pkm)
  
  TRA_Hist_tkm <- Extract_Item(TRA_Hist_Assumption, "Goods traffic")
  TRA_Hist_tkm <- TRA_Hist_tkm[order(as.character(TRA_Hist_tkm$Usage)),]
  TRA_Hist_tkm <- TRA_Hist_tkm[order(as.character(TRA_Hist_tkm$Sub_Sector)),]
  TRA_Hist_tkm <- Sort_By_Country(TRA_Hist_tkm)
  
  #Passeneger per vehicle (Road sub_sector)
  
  TRA_Hist_Stock_Road <- Extract_Usage(Extract_Specific_Energy(TRA_Hist_Stock, "total"), c("bus", "car"))
  TRA_Hist_Annual_Distance_Road <- Extract_Usage(TRA_Hist_Annual_Distance, c("bus", "car"))
  TRA_Hist_Pkm_Road <- Extract_Usage(TRA_Hist_Pkm, c("bus", "car"))
  
  TRA_Hist_Passenger_per_vehicle <- TRA_Hist_Stock_Road
  TRA_Hist_Passenger_per_vehicle$ID_Item <- "Passenger per vehicle" 
  TRA_Hist_Passenger_per_vehicle$Unit <- "Number"
  TRA_Hist_Passenger_per_vehicle[,10:length(TRA_Hist_Passenger_per_vehicle)] <- (TRA_Hist_Pkm_Road[,10:length(TRA_Hist_Pkm_Road)] / (TRA_Hist_Annual_Distance_Road[,10:length(TRA_Hist_Annual_Distance_Road)] * (TRA_Hist_Stock_Road[,10:length(TRA_Hist_Stock_Road)]))) * 1000000 
  
  #Tons per vehicle (Road sub_sector)
  
  TRA_Hist_Stock_Road <- Extract_Usage(Extract_Specific_Energy(TRA_Hist_Stock, "total"), c("light truck", "heavy truck"))
  TRA_Hist_Annual_Distance_Road <- Extract_Usage(TRA_Hist_Annual_Distance, c("light truck", "heavy truck"))
  TRA_Hist_tkm_Road <- Extract_Usage(TRA_Hist_tkm, c("light truck", "heavy truck"))
  
  TRA_Hist_tons_per_vehicle <- TRA_Hist_Stock_Road
  TRA_Hist_tons_per_vehicle$ID_Item <- "Tons per vehicle" 
  TRA_Hist_tons_per_vehicle$Unit <- "Number"
  TRA_Hist_tons_per_vehicle[,10:length(TRA_Hist_tons_per_vehicle)] <- (TRA_Hist_tkm_Road[,10:length(TRA_Hist_tkm_Road)] / (TRA_Hist_Annual_Distance_Road[,10:length(TRA_Hist_Annual_Distance_Road)] * (TRA_Hist_Stock_Road[,10:length(TRA_Hist_Stock_Road)]))) * 1000000 
  
  saveRDS(rbind(Hist_Assumption,
                TRA_Hist_Passenger_per_vehicle, 
                TRA_Hist_tons_per_vehicle),
          "../../Data/temp/Hist_Assumption.Rds")
}