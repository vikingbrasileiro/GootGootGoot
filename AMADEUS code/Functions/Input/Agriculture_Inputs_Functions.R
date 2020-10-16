source("../Functions/Input/General_Inputs_Functions.R")

############################################################### Main Function ######################################################################################

main_input_Hist_AGR <- function(Hist_Macro, Hist_Assumption, Hist_Energy_Demand) 
{
  AGR_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "AGR")
  AGR_Hist_Energy_Demand <- Sort_By_Country(AGR_Hist_Energy_Demand)
  AGR_Hist_Energy_Demand <- AGR_Hist_Energy_Demand[order(AGR_Hist_Energy_Demand$Energy),]
  
  Value_Added_AGR <- Extract_Sector(Extract_Item(Hist_Macro, "Value Added"), "AGR")
  
  VA_Growth_Rate <- derivate_dataframe(Value_Added_AGR)
  VA_Growth_Rate <- VA_Growth_Rate[ rep( seq_len( nrow( VA_Growth_Rate ) ), nlevels( as.factor(AGR_Hist_Energy_Demand$Energy) ) ), ]
  #VA_Growth_Rate <- Sort_By_Country(VA_Growth_Rate)
  VA_Growth_Rate$Energy <- AGR_Hist_Energy_Demand$Energy
  VA_Growth_Rate <- VA_Growth_Rate[order(VA_Growth_Rate$Energy),]
  VA_Growth_Rate$ID_Item <- "Activity Rate"
  VA_Growth_Rate$Unit <- "%"
  
  Year_Proj_col = min(which(is.nan(colSums(AGR_Hist_Energy_Demand[10:length(AGR_Hist_Energy_Demand)])))) + 9
  
  AGR_Simulated_Hist_Energy_Demand <- AGR_Hist_Energy_Demand
  
  EI_Perct <- AGR_Hist_Energy_Demand
  EI_Perct$Unit <- "% "
  EI_Perct$ID_Item <- "Energy Intensity"
  EI_Perct[,10:length(EI_Perct)] <- NA
  for(i in 11:(Year_Proj_col-1))
  {
      AGR_Simulated_Hist_Energy_Demand[,i] <- AGR_Simulated_Hist_Energy_Demand[,i-1] * ( 1 + VA_Growth_Rate[,i])
      EI_Perct[,i] <- (AGR_Hist_Energy_Demand[,i] - AGR_Simulated_Hist_Energy_Demand[,i]) / AGR_Hist_Energy_Demand[,i-1]
  }
  
  saveRDS(rbind(Hist_Assumption, VA_Growth_Rate, EI_Perct), "../../Data/temp/Hist_Assumption.Rds")
  
  return(Hist_Assumption)
}

main_input_Proj_AGR <- function(Proj_Macro, Proj_Assumption) 
{

  Value_Added_AGR <- Extract_Sector(Extract_Item(Proj_Macro, "Value Added"), "AGR")
  
  VA_Growth_Rate <- derivate_dataframe(Value_Added_AGR)
  VA_Growth_Rate$ID_Item <- "Activity Rate"
  VA_Growth_Rate$Unit <- "%"
  
  saveRDS(rbind(Proj_Assumption, VA_Growth_Rate), "../../Data/temp/Proj_Assumption.Rds")
  
  return(Proj_Assumption)
}