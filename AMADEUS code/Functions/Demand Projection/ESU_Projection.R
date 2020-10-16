source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# Projection of energy demand for the Energy Sector Uses sector 
############################################################################################################################################################################

main_projection_energy_demand_TRA <- function()
{
  #1. Data input extraction 
  ESU_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "ESU")
  ESU_Hist_Energy_Demand <- Sort_By_Country(ESU_Hist_Energy_Demand)
  ESU_Hist_Energy_Demand <- ESU_Hist_Energy_Demand[order(ESU_Hist_Energy_Demand$Energy),]
} 
  