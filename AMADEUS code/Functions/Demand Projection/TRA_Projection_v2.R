source("../Functions/Input/General_Inputs_Functions.R")

############################################################################################################################################################################
# 
############################################################################################################################################################################

main_projection_energy_demand_TRA <- function(Hist_Energy_Demand, Proj_Assumption, Hist_Assumption)
{
  # browser()
  #1.car
  hist_car_energy_demand <- Extract_Usage(Hist_Energy_Demand, "car")
  hist_car_energy_demand <- Sort_By_Country(hist_car_energy_demand)
  hist_car_energy_demand <- hist_car_energy_demand[order(as.character(hist_car_energy_demand$Energy)),]
  hist_car_energy_demand <- hist_car_energy_demand[hist_car_energy_demand$Energy != "total",]
  
  proj_car_average_efficiency <- Extract_Item(Extract_Usage(Proj_Assumption, "car"), "Unitary consumption")
  
  proj_car_fleet_stock <- Extract_Item(Extract_Usage(Proj_Assumption, "car"), "Vehicle Stock")
  
  proj_car_annual_distance <- Extract_Item(Extract_Usage(Proj_Assumption, "car"), "Annual distance")
  proj_car_annual_distance <- proj_car_annual_distance[ rep( seq_len( nrow( proj_car_annual_distance ) ), nlevels( as.factor( hist_car_energy_demand$Energy ) ) ), ]
  
  Year_Proj <- min(which(is.nan(colSums(hist_car_energy_demand[10:length(hist_car_energy_demand)])))) + 9
  
  proj_car_energy_demand <- hist_car_energy_demand
  proj_car_energy_demand[,Year_Proj:length(proj_car_energy_demand)] <- (proj_car_fleet_stock[,Year_Proj:length(proj_car_fleet_stock)] * 1000) * (proj_car_average_efficiency[,Year_Proj:length(proj_car_average_efficiency)] * (0.9/1000/100 *11.628/1000000) ) * (proj_car_annual_distance[,Year_Proj:length(proj_car_annual_distance)])
  
  #2.bus
  hist_bus_energy_demand <- Extract_Usage(Hist_Energy_Demand, "bus")
  hist_bus_energy_demand <- Sort_By_Country(hist_bus_energy_demand)
  hist_bus_energy_demand <- hist_bus_energy_demand[order(as.character(hist_bus_energy_demand$Energy)),]
  hist_bus_energy_demand <- hist_bus_energy_demand[hist_bus_energy_demand$Energy != "total",]
  
  proj_bus_average_efficiency <- Extract_Item(Extract_Usage(Proj_Assumption, "bus"), "Unitary consumption")
  
  proj_bus_fleet_stock <- Extract_Item(Extract_Usage(Proj_Assumption, "bus"), "Vehicle Stock")
  
  proj_bus_annual_distance <- Extract_Item(Extract_Usage(Proj_Assumption, "bus"), "Annual distance")
  proj_bus_annual_distance <- proj_bus_annual_distance[ rep( seq_len( nrow( proj_bus_annual_distance ) ), nlevels( as.factor( hist_bus_energy_demand$Energy) ) ), ]
  
  Year_Proj <- min(which(is.nan(colSums(hist_bus_energy_demand[10:length(hist_bus_energy_demand)])))) + 9
  
  proj_bus_energy_demand <- hist_bus_energy_demand
  proj_bus_energy_demand[,Year_Proj:length(proj_bus_energy_demand)] <- (proj_bus_fleet_stock[,Year_Proj:length(proj_bus_fleet_stock)] * 1000) * (proj_bus_average_efficiency[,Year_Proj:length(proj_bus_average_efficiency)] * (0.9/1000/100 *11.628/1000000) ) * (proj_bus_annual_distance[,Year_Proj:length(proj_bus_annual_distance)])
  
  #3. 2_wheels
  hist_2_wheels_energy_demand <- Extract_Usage(Hist_Energy_Demand, "2 wheels")
  hist_2_wheels_energy_demand <- Sort_By_Country(hist_2_wheels_energy_demand)
  hist_2_wheels_energy_demand <- hist_2_wheels_energy_demand[order(as.character(hist_2_wheels_energy_demand$Energy)),]
  hist_2_wheels_energy_demand <- hist_2_wheels_energy_demand[hist_2_wheels_energy_demand$Energy != "total",]
  
  proj_2_wheels_average_efficiency <- Extract_Item(Extract_Usage(Proj_Assumption, "2 wheels"), "Unitary consumption")
  
  proj_2_wheels_fleet_stock <- Extract_Item(Extract_Usage(Proj_Assumption, "2 wheels"), "Vehicle Stock")
  
  proj_2_wheels_annual_distance <- Extract_Item(Extract_Usage(Proj_Assumption, "2 wheels"), "Annual distance")
  proj_2_wheels_annual_distance <- proj_2_wheels_annual_distance[ rep( seq_len( nrow( proj_2_wheels_annual_distance ) ), nlevels( as.factor( hist_2_wheels_energy_demand$Energy ) ) ), ]
  
  Year_Proj <- min(which(is.nan(colSums(hist_2_wheels_energy_demand[10:length(hist_2_wheels_energy_demand)])))) + 9
  
  proj_2_wheels_energy_demand <- hist_2_wheels_energy_demand
  proj_2_wheels_energy_demand[,Year_Proj:length(proj_2_wheels_energy_demand)] <- (proj_2_wheels_fleet_stock[,Year_Proj:length(proj_2_wheels_fleet_stock)] * 1000) * (proj_2_wheels_average_efficiency[,Year_Proj:length(proj_2_wheels_average_efficiency)] * (0.9/1000/100 *11.628/1000000) ) * (proj_2_wheels_annual_distance[,Year_Proj:length(proj_2_wheels_annual_distance)])
  
  #4.light_truck
  hist_light_truck_energy_demand <- Extract_Usage(Hist_Energy_Demand, "light truck")
  hist_light_truck_energy_demand <- Sort_By_Country(hist_light_truck_energy_demand)
  hist_light_truck_energy_demand <- hist_light_truck_energy_demand[order(as.character(hist_light_truck_energy_demand$Energy)),]
  hist_light_truck_energy_demand <- hist_light_truck_energy_demand[hist_light_truck_energy_demand$Energy != "total",]

  proj_light_truck_average_efficiency <- Extract_Item(Extract_Usage(Proj_Assumption, "light truck"), "Unitary consumption")

  proj_light_truck_fleet_stock <- Extract_Item(Extract_Usage(Proj_Assumption, "light truck"), "Vehicle Stock")

  proj_light_truck_annual_distance <- Extract_Item(Extract_Usage(Proj_Assumption, "light truck"), "Annual distance")
  proj_light_truck_annual_distance <- proj_light_truck_annual_distance[ rep( seq_len( nrow( proj_light_truck_annual_distance ) ), nlevels( as.factor( hist_light_truck_energy_demand$Energy ) ) ), ]

  Year_Proj <- min(which(is.nan(colSums(hist_light_truck_energy_demand[10:length(hist_light_truck_energy_demand)])))) + 9

  proj_light_truck_energy_demand <- hist_light_truck_energy_demand
  proj_light_truck_energy_demand[,Year_Proj:length(proj_light_truck_energy_demand)] <- (proj_light_truck_fleet_stock[,Year_Proj:length(proj_light_truck_fleet_stock)] * 1000) * (proj_light_truck_average_efficiency[,Year_Proj:length(proj_light_truck_average_efficiency)] * (0.9/1000/100 *11.628/1000000) ) * (proj_light_truck_annual_distance[,Year_Proj:length(proj_light_truck_annual_distance)])

  #5. heavy truck
  hist_heavy_truck_energy_demand <- Extract_Usage(Hist_Energy_Demand, "heavy truck")
  hist_heavy_truck_energy_demand <- Sort_By_Country(hist_heavy_truck_energy_demand)
  hist_heavy_truck_energy_demand <- hist_heavy_truck_energy_demand[order(as.character(hist_heavy_truck_energy_demand$Energy)),]
  hist_heavy_truck_energy_demand <- hist_heavy_truck_energy_demand[hist_heavy_truck_energy_demand$Energy != "total",]

  proj_heavy_truck_average_efficiency <- Extract_Item(Extract_Usage(Proj_Assumption, "heavy truck"), "Unitary consumption")

  proj_heavy_truck_fleet_stock <- Extract_Item(Extract_Usage(Proj_Assumption, "heavy truck"), "Vehicle Stock")

  proj_heavy_truck_annual_distance <- Extract_Item(Extract_Usage(Proj_Assumption, "heavy truck"), "Annual distance")
  proj_heavy_truck_annual_distance <- proj_heavy_truck_annual_distance[ rep( seq_len( nrow( proj_heavy_truck_annual_distance ) ), nlevels( as.factor( hist_heavy_truck_energy_demand$Energy ) ) ), ]

  Year_Proj <- min(which(is.nan(colSums(hist_heavy_truck_energy_demand[10:length(hist_heavy_truck_energy_demand)])))) + 9

  proj_heavy_truck_energy_demand <- hist_heavy_truck_energy_demand
  proj_heavy_truck_energy_demand[,Year_Proj:length(proj_heavy_truck_energy_demand)] <- (proj_heavy_truck_fleet_stock[,Year_Proj:length(proj_heavy_truck_fleet_stock)] * 1000) * (proj_heavy_truck_average_efficiency[,Year_Proj:length(proj_heavy_truck_average_efficiency)] * (0.9/1000/100 *11.628/1000000) ) * (proj_heavy_truck_annual_distance[,Year_Proj:length(proj_heavy_truck_annual_distance)])
  
  #6. Air
  
  #6.1 Domestic
  
  hist_domestic_aviation_energy_demand <- Sort_By_Country( Extract_Sub_Sector(Hist_Energy_Demand, "Domestic Aviation") )
  hist_domestic_aviation_energy_demand <- hist_domestic_aviation_energy_demand[hist_domestic_aviation_energy_demand$Energy != "total",]
  hist_domestic_aviation_energy_demand <- hist_domestic_aviation_energy_demand[order(hist_domestic_aviation_energy_demand$Energy),]
  
  hist_unitary_consumption_aviation <- Extract_Item(Extract_Sub_Sector(Hist_Assumption, "Domestic Aviation"), "Unitary consumption for Passenger")
  proj_unitary_consumption_aviation <- hist_unitary_consumption_aviation
  proj_unitary_consumption_aviation[,Year_Proj:length(proj_unitary_consumption_aviation)] <- proj_unitary_consumption_aviation[,(Year_Proj-1)] 
  proj_unitary_consumption_aviation <- Sort_By_Country(proj_unitary_consumption_aviation)
  proj_unitary_consumption_aviation <- proj_unitary_consumption_aviation[order(proj_unitary_consumption_aviation$Energy),]
  
  proj_pkm_aviation <- Extract_Item(Extract_Sub_Sector(Proj_Assumption, "Domestic Aviation"), "Passenger traffic")
  proj_pkm_aviation <- proj_pkm_aviation[ rep( seq_len( nrow( proj_pkm_aviation ) ), nlevels( as.factor( hist_domestic_aviation_energy_demand$Energy) ) ), ]
  
  proj_domestic_aviation_energy_demand <- hist_domestic_aviation_energy_demand
  proj_domestic_aviation_energy_demand[,Year_Proj:length(proj_domestic_aviation_energy_demand)] <- proj_pkm_aviation[,Year_Proj:length(proj_pkm_aviation)] * proj_unitary_consumption_aviation[,Year_Proj:length(proj_unitary_consumption_aviation)] / 1000 * 11.628
  
  #6.2 INternational
  
  hist_international_aviation_energy_demand <- Sort_By_Country( Extract_Sub_Sector(Hist_Energy_Demand, "International Aviation") )
  hist_international_aviation_energy_demand <- hist_international_aviation_energy_demand[hist_international_aviation_energy_demand$Energy != "total",]
  hist_international_aviation_energy_demand <- hist_international_aviation_energy_demand[order(hist_international_aviation_energy_demand$Energy),]
  
  proj_international_aviation_energy_demand <- hist_international_aviation_energy_demand
  
  for(i in Year_Proj:length(proj_international_aviation_energy_demand))
  {
    proj_international_aviation_energy_demand[,i] <- proj_international_aviation_energy_demand[,i-1] * proj_domestic_aviation_energy_demand[,i] / proj_domestic_aviation_energy_demand[,i-1]
    proj_international_aviation_energy_demand[which(is.na(proj_international_aviation_energy_demand[,i])),i] <- 0
  }

  #7. Domestic Navigation
  
  hist_domestic_navigation_energy_demand <- Sort_By_Country( Extract_Sub_Sector(Hist_Energy_Demand, "Domestic Navigation") )
  hist_domestic_navigation_energy_demand <- hist_domestic_navigation_energy_demand[hist_domestic_navigation_energy_demand$Energy != "total",]
  hist_domestic_navigation_energy_demand <- hist_domestic_navigation_energy_demand[order(hist_domestic_navigation_energy_demand$Energy),]
  
  hist_unitary_consumption_navigation <- Extract_Item(Extract_Sub_Sector(Hist_Assumption, "Domestic Navigation"), "Unitary consumption for Goods")
  proj_unitary_consumption_navigation <- hist_unitary_consumption_navigation
  proj_unitary_consumption_navigation[,Year_Proj:length(proj_unitary_consumption_navigation)] <- proj_unitary_consumption_navigation[,(Year_Proj-1)] 
  proj_unitary_consumption_navigation <- Sort_By_Country(proj_unitary_consumption_navigation)
  proj_unitary_consumption_navigation <- proj_unitary_consumption_navigation[order(proj_unitary_consumption_navigation$Energy),]
  
  proj_tkm_navigation <- Extract_Item(Extract_Sub_Sector(Proj_Assumption, "Domestic Navigation"), "Goods traffic")
  proj_tkm_navigation <- proj_tkm_navigation[ rep( seq_len( nrow( proj_tkm_navigation ) ), nlevels( as.factor( hist_domestic_navigation_energy_demand$Energy ) ) ), ]
  
  proj_domestic_navigation_energy_demand <- hist_domestic_navigation_energy_demand
  proj_domestic_navigation_energy_demand[,Year_Proj:length(proj_domestic_navigation_energy_demand)] <- proj_tkm_navigation[,Year_Proj:length(proj_tkm_navigation)] * proj_unitary_consumption_navigation[,Year_Proj:length(proj_unitary_consumption_navigation)] / 1000 * 11.628
  
  #8. Rail
  hist_rail_energy_demand <- Sort_By_Country( Extract_Sub_Sector(Hist_Energy_Demand, "Rail") )
  hist_rail_energy_demand <- hist_rail_energy_demand[hist_rail_energy_demand$Energy != "total",]
  hist_rail_energy_demand <- hist_rail_energy_demand[order(hist_rail_energy_demand$Energy),]
  
  hist_unitary_consumption_rail_goods <- Extract_Item(Extract_Sub_Sector(Hist_Assumption, "Rail"), "Unitary consumption for Goods")
  proj_unitary_consumption_rail_goods <- hist_unitary_consumption_rail_goods
  proj_unitary_consumption_rail_goods[,Year_Proj:length(proj_unitary_consumption_rail_goods)] <- proj_unitary_consumption_rail_goods[,(Year_Proj-1)] 
  proj_unitary_consumption_rail_goods <- Sort_By_Country(proj_unitary_consumption_rail_goods)
  proj_unitary_consumption_rail_goods <- proj_unitary_consumption_rail_goods[order(proj_unitary_consumption_rail_goods$Energy),]
  
  hist_share_consumption_rail_goods <- Extract_Item(Extract_Sub_Sector(Hist_Assumption, "Rail"), "Share Consumption for Goods traffic in Rail")
  proj_share_consumption_rail_goods <- hist_share_consumption_rail_goods
  proj_share_consumption_rail_goods[,Year_Proj:length(proj_share_consumption_rail_goods)] <- proj_share_consumption_rail_goods[,(Year_Proj-1)] 
  proj_share_consumption_rail_goods <- Sort_By_Country(proj_share_consumption_rail_goods)
  proj_share_consumption_rail_goods <- proj_share_consumption_rail_goods[order(proj_share_consumption_rail_goods$Energy),]
  proj_share_consumption_rail_goods <- proj_share_consumption_rail_goods[ rep( seq_len( nrow( proj_share_consumption_rail_goods ) ), nlevels( as.factor( hist_rail_energy_demand$Energy ) ) ), ]
  
  hist_unitary_consumption_rail_passenger <- Extract_Item(Extract_Sub_Sector(Hist_Assumption, "Rail"), "Unitary consumption for Passenger")
  proj_unitary_consumption_rail_passenger <- hist_unitary_consumption_rail_passenger
  proj_unitary_consumption_rail_passenger[,Year_Proj:length(proj_unitary_consumption_rail_passenger)] <- proj_unitary_consumption_rail_passenger[,(Year_Proj-1)] 
  proj_unitary_consumption_rail_passenger <- Sort_By_Country(proj_unitary_consumption_rail_passenger)
  proj_unitary_consumption_rail_passenger <- proj_unitary_consumption_rail_passenger[order(proj_unitary_consumption_rail_passenger$Energy),]
  
  hist_share_consumption_rail_passenger <- Extract_Item(Extract_Sub_Sector(Hist_Assumption, "Rail"), "Share Consumption for Passenger traffic in Rail")
  proj_share_consumption_rail_passenger <- hist_share_consumption_rail_passenger
  proj_share_consumption_rail_passenger[,Year_Proj:length(proj_share_consumption_rail_passenger)] <- proj_share_consumption_rail_passenger[,(Year_Proj-1)] 
  proj_share_consumption_rail_passenger <- Sort_By_Country(proj_share_consumption_rail_passenger)
  proj_share_consumption_rail_passenger <- proj_share_consumption_rail_passenger[order(proj_share_consumption_rail_passenger$Energy),]
  proj_share_consumption_rail_passenger <- proj_share_consumption_rail_passenger[ rep( seq_len( nrow( proj_share_consumption_rail_passenger ) ), nlevels( as.factor( hist_rail_energy_demand$Energy ) ) ), ]
  
  proj_tkm_rail <- Extract_Item(Extract_Sub_Sector(Proj_Assumption, "Rail"), "Goods traffic")
  proj_tkm_rail <- proj_tkm_rail[ rep( seq_len( nrow( proj_tkm_rail ) ), nlevels( as.factor( hist_rail_energy_demand$Energy ) ) ), ]
  
  proj_pkm_rail <- Extract_Item(Extract_Sub_Sector(Proj_Assumption, "Rail"), "Passenger traffic")
  proj_pkm_rail <- proj_pkm_rail[ rep( seq_len( nrow( proj_pkm_rail ) ), nlevels( as.factor( hist_rail_energy_demand$Energy) ) ), ]
  
  proj_rail_energy_demand <- hist_rail_energy_demand
  proj_rail_energy_demand[,Year_Proj:length(proj_rail_energy_demand)] <- ( proj_tkm_rail[,Year_Proj:length(proj_tkm_rail)] * proj_unitary_consumption_rail_goods[,Year_Proj:length(proj_unitary_consumption_rail_goods)] * proj_share_consumption_rail_goods[,Year_Proj,length(proj_share_consumption_rail_goods)] + proj_pkm_rail[,Year_Proj:length(proj_pkm_rail)] * proj_unitary_consumption_rail_passenger[,Year_Proj:length(proj_unitary_consumption_rail_passenger)] * proj_share_consumption_rail_passenger[,Year_Proj,length(proj_share_consumption_rail_passenger)]) / 1000 * 11.628
  
  
  # Ajout et calcul du total TRA
  TRA_Proj_Energy_Demand_Road <- rbind(proj_car_energy_demand, proj_bus_energy_demand, proj_2_wheels_energy_demand, proj_light_truck_energy_demand, proj_heavy_truck_energy_demand)
  
  TRA_Proj_Energy_Demand_Other <- rbind(proj_domestic_navigation_energy_demand, proj_domestic_aviation_energy_demand, proj_international_aviation_energy_demand, proj_rail_energy_demand) 
  TRA_Proj_Energy_Demand_Other <- TRA_Proj_Energy_Demand_Other[TRA_Proj_Energy_Demand_Other$Energy != "final heat",]
  
  TRA_Proj_Energy_Demand_Other_plug_in <- TRA_Proj_Energy_Demand_Other[TRA_Proj_Energy_Demand_Other$Energy == "oil",]
  TRA_Proj_Energy_Demand_Other_plug_in$Energy <- "plug-in hybrid"
  TRA_Proj_Energy_Demand_Other_plug_in[,10:length(TRA_Proj_Energy_Demand_Other_plug_in)] <- 0
  
  TRA_Proj_Energy_Demand_Other_self_recharge <- TRA_Proj_Energy_Demand_Other[TRA_Proj_Energy_Demand_Other$Energy == "oil",]
  TRA_Proj_Energy_Demand_Other_self_recharge$Energy <- "self-recharge hybrid"
  TRA_Proj_Energy_Demand_Other_self_recharge[,10:length(TRA_Proj_Energy_Demand_Other_self_recharge)] <- 0
  
  TRA_Proj_Energy_Demand_Other <- rbind(TRA_Proj_Energy_Demand_Other ,TRA_Proj_Energy_Demand_Other_plug_in, TRA_Proj_Energy_Demand_Other_self_recharge)
  
  TRA_Proj_Energy_Demand <- rbind(TRA_Proj_Energy_Demand_Road, TRA_Proj_Energy_Demand_Other)
  TRA_Proj_Energy_Demand <- calculate_total_sector_TRA(TRA_Proj_Energy_Demand)
  
  return(TRA_Proj_Energy_Demand)
}



Calibration_pkm_tkm_stock_TRA <- function(Hist_Macro, Hist_Assumption, Proj_Macro, Proj_Assumption, Hist_Energy_Demand)
{
  # browser()
  #1.Calibration et projection des pkm pour chaque mode de trnaports (with logistic function)
  
  #chargement des data necessaire a la calibration
  data_transport_calibration <- prepare_data_transport_calibration(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
  data_pkm_calibration <- transform_data_pkm(data_transport_calibration)
  
  message(paste0("Calibration and pkm projection started"))
  ## erreur ici hermence
  Proj_pkm <- transport_hist_pkm_calibration_and_projection(data_pkm_calibration, Hist_Assumption, Proj_Macro)$Proj_pkm
  message(paste0("Calibration and pkm projection finished"))
  
  #2. Calibration et projection des tkm 
  
  message(paste0("Calibration and tkm projection started"))
  
  Proj_tkm <- transport_hist_tkm_projection(Hist_Assumption, Proj_Macro, Proj_Assumption)
  
  message(paste0("Calibration and tkm projection finished"))
  
  #3. Calibration and projection of total car stock (with gompertz curve foe each country) (edit : le taux de saturation gamma est fixe en limte haute a 0.8 apres une premiere evaluaiotn d u modele ou gamma et laisse libre (contraint BG, PL et RO))
  
  #3.1 chargement historique necessaire
  data_veh_calibration <- transform_data_veh(data_transport_calibration)
  total_car_hist_vehicle_stock <- Extract_Specific_Energy(Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "car"), "total")
  Proj_GDP_per_capita <- Extract_Item(Proj_Macro, "GDP per capita")
  Population <- Extract_Item(Proj_Macro, "Population")
  Hist_annual_distance_car <- Extract_Usage( Extract_Item(Hist_Assumption, "Annual distance"), "car") 
  hist_passenger_per_vehicle <- Extract_Usage( Extract_Item(Hist_Assumption, "Passenger per vehicle"), "car")
  
  #3.2 Hypothese passenger per vehicle a definir
  Year_Proj <- min(which(is.nan(colSums(hist_passenger_per_vehicle[10:length(hist_passenger_per_vehicle)])))) + 9
  proj_passenger_per_vehicle <- hist_passenger_per_vehicle 
  proj_passenger_per_vehicle[,Year_Proj:length(proj_passenger_per_vehicle)] <- proj_passenger_per_vehicle[,(Year_Proj-1)] 

  #3.3 Stock and annual distance projection 
  result_car_stock_calibration_projection <- transport_car_stock_calibration_and_projection(data_veh_calibration, total_car_hist_vehicle_stock, Proj_GDP_per_capita, Population, Hist_annual_distance_car, proj_passenger_per_vehicle, Proj_pkm)
  proj_total_car_stock <- result_car_stock_calibration_projection$total_car_proj_vehicle_stock
  proj_car_annual_distance <- result_car_stock_calibration_projection$proj_annual_distance_car
  
  #4. Projection of total stock for other vehicles
  
  #4.1 bus
  hist_passenger_per_vehicle_bus <- Extract_Usage( Extract_Item(Hist_Assumption, "Passenger per vehicle"), "bus")
  proj_passenger_per_vehicle_bus <- Sort_By_Country(hist_passenger_per_vehicle_bus)
  proj_passenger_per_vehicle_bus[,Year_Proj:length(proj_passenger_per_vehicle_bus)] <- proj_passenger_per_vehicle_bus[,(Year_Proj-1)]
  
  hist_annual_distance_bus <- Extract_Usage( Extract_Item(Hist_Assumption, "Annual distance"), "bus")
  proj_annual_distance_bus <- Sort_By_Country(hist_annual_distance_bus) 
  proj_annual_distance_bus[,Year_Proj:length(proj_annual_distance_bus)] <- proj_annual_distance_bus[,(Year_Proj-1)]
  
  bus_Proj_pkm <- Extract_Usage(Proj_pkm, "bus")
  
  total_bus_hist_vehicle_stock <- Extract_Specific_Energy(Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "bus"), "total")
  proj_bus_total_stock <- Sort_By_Country(total_bus_hist_vehicle_stock) 
  proj_bus_total_stock[, Year_Proj:length(proj_bus_total_stock)] <- bus_Proj_pkm[, Year_Proj:length(bus_Proj_pkm)] / ( proj_annual_distance_bus[, Year_Proj:length(proj_annual_distance_bus)] * proj_passenger_per_vehicle_bus[, Year_Proj:length(proj_passenger_per_vehicle_bus)]) * 1000000
    
  #4.2 two wheels
  hist_annual_distance_2_wheels <- Extract_Usage( Extract_Item(Hist_Assumption, "Annual distance"), "2 wheels")
  proj_annual_distance_2_wheels <- Sort_By_Country(hist_annual_distance_2_wheels) 
  proj_annual_distance_2_wheels[,Year_Proj:length(proj_annual_distance_2_wheels)] <- proj_annual_distance_2_wheels[,(Year_Proj-1)]
  
  proj_passenger_per_vehicle_two_wheels <- hist_passenger_per_vehicle_bus
  proj_passenger_per_vehicle_two_wheels[,10:length(proj_passenger_per_vehicle_two_wheels)] <- 1
  
  two_wheels_Proj_pkm <- Extract_Usage(Proj_pkm, "2 wheels")
  
  total_2_wheels_hist_vehicle_stock <- Extract_Specific_Energy(Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "2 wheels"), "total")
  proj_2_wheels_total_stock <- Sort_By_Country(total_2_wheels_hist_vehicle_stock)
  proj_2_wheels_total_stock[, Year_Proj:length(proj_2_wheels_total_stock)] <- two_wheels_Proj_pkm[, Year_Proj:length(two_wheels_Proj_pkm)] / ( proj_annual_distance_2_wheels[, Year_Proj:length(proj_annual_distance_2_wheels)]) * 1000000000
  
  #4.3 light trucks
  hist_tons_per_vehicle_light_truck <- Extract_Usage( Extract_Item(Hist_Assumption, "Tons per vehicle"), "light truck")
  proj_tons_per_vehicle_light_truck <- hist_tons_per_vehicle_light_truck
  proj_tons_per_vehicle_light_truck[,Year_Proj:length(proj_tons_per_vehicle_light_truck)] <- proj_tons_per_vehicle_light_truck[,(Year_Proj-1)]
  
  hist_annual_distance_light_truck <- Extract_Usage( Extract_Item(Hist_Assumption, "Annual distance"), "light truck")
  proj_annual_distance_light_truck <- Sort_By_Country(hist_annual_distance_light_truck) 
  proj_annual_distance_light_truck[,Year_Proj:length(proj_annual_distance_light_truck)] <- proj_annual_distance_light_truck[,(Year_Proj-1)]
  
  light_truck_Proj_pkm <- Extract_Usage(Proj_tkm, "light truck")
  
  total_light_truck_hist_vehicle_stock <- Extract_Specific_Energy(Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "light truck"), "total")
  proj_light_truck_total_stock <- Sort_By_Country(total_light_truck_hist_vehicle_stock)
  proj_light_truck_total_stock[, Year_Proj:length(proj_light_truck_total_stock)] <- light_truck_Proj_pkm[, Year_Proj:length(light_truck_Proj_pkm)] / ( proj_annual_distance_light_truck[, Year_Proj:length(proj_annual_distance_light_truck)] * proj_tons_per_vehicle_light_truck[, Year_Proj:length(proj_tons_per_vehicle_light_truck)]) * 1000000
  
  #4.4 heavy trucks
  hist_tons_per_vehicle_heavy_truck <- Extract_Usage( Extract_Item(Hist_Assumption, "Tons per vehicle"), "heavy truck")
  proj_tons_per_vehicle_heavy_truck <- hist_tons_per_vehicle_heavy_truck
  proj_tons_per_vehicle_heavy_truck[,Year_Proj:length(proj_tons_per_vehicle_heavy_truck)] <- proj_tons_per_vehicle_heavy_truck[,(Year_Proj-1)]
  
  hist_annual_distance_heavy_truck <- Extract_Usage( Extract_Item(Hist_Assumption, "Annual distance"), "heavy truck")
  proj_annual_distance_heavy_truck <- Sort_By_Country(hist_annual_distance_heavy_truck) 
  proj_annual_distance_heavy_truck[,Year_Proj:length(proj_annual_distance_heavy_truck)] <- proj_annual_distance_heavy_truck[,(Year_Proj-1)]
  
  heavy_truck_Proj_pkm <- Extract_Usage(Proj_tkm, "heavy truck")
  
  total_heavy_truck_hist_vehicle_stock <- Extract_Specific_Energy(Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "heavy truck"), "total")
  proj_heavy_truck_total_stock <- Sort_By_Country(total_heavy_truck_hist_vehicle_stock)
  proj_heavy_truck_total_stock[, Year_Proj:length(proj_heavy_truck_total_stock)] <- heavy_truck_Proj_pkm[, Year_Proj:length(heavy_truck_Proj_pkm)] / ( proj_annual_distance_heavy_truck[, Year_Proj:length(proj_annual_distance_heavy_truck)] * proj_tons_per_vehicle_heavy_truck[, Year_Proj:length(proj_tons_per_vehicle_heavy_truck)]) * 1000000
  
  #5. Fleet model for projection of transport consumption
  
  #5.1 Car stock projection 
  hist_car_stock_per_fuel <- Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "car")
  hist_car_stock_per_fuel <- Sort_By_Country(hist_car_stock_per_fuel)
  hist_car_stock_per_fuel <- hist_car_stock_per_fuel[order(as.character(hist_car_stock_per_fuel$Energy)),]
  hist_car_stock_per_fuel <- hist_car_stock_per_fuel[hist_car_stock_per_fuel$Energy !="total",]
    
  car_destruction_rate <- Extract_Usage( Extract_Item(Proj_Assumption, "Destruction Rate"), "car")
  
  car_market_shares_new <- Extract_Usage( Extract_Item(Proj_Assumption, "Market share in new vehicle"), "car")
  car_market_shares_new <- Sort_By_Country(car_market_shares_new)
  car_market_shares_new <- car_market_shares_new[order(as.character(car_market_shares_new$Energy)),]
  car_market_shares_new <- car_market_shares_new[car_market_shares_new$Energy !="total",]
  
  hist_car_average_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption"), "car")
  hist_car_average_efficiency <- Sort_By_Country(hist_car_average_efficiency)
  hist_car_average_efficiency <- hist_car_average_efficiency[order(as.character(hist_car_average_efficiency$Energy)),]
  
  hist_new_car_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption of new vehicle"), "car")
  hist_new_car_efficiency <- Sort_By_Country(hist_new_car_efficiency)
  hist_new_car_efficiency <- hist_new_car_efficiency[order(as.character(hist_new_car_efficiency$Energy)),]

  proj_new_car_efficiency <- Extract_Usage( Extract_Item(Proj_Assumption, "Indice unitary consumption of new vehicle"), "car")
  proj_new_car_efficiency <- Sort_By_Country(proj_new_car_efficiency)
  proj_new_car_efficiency <- proj_new_car_efficiency[order(as.character(proj_new_car_efficiency$Energy)),]
  
  proj_results <- simulate_stock_model_projection(hist_car_stock_per_fuel, car_destruction_rate, car_market_shares_new, proj_total_car_stock, hist_car_average_efficiency, hist_new_car_efficiency, proj_new_car_efficiency)
  
  proj_car_fleet_stock <- proj_results$proj_fleet
  proj_car_average_efficiency <- proj_results$proj_average_efficiency
  proj_car_new_efficiency <- proj_results$proj_new_efficiency
  
  #5.2 Bus stock projection 
  
  hist_bus_stock_per_fuel <- Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "bus")
  hist_bus_stock_per_fuel <- Sort_By_Country(hist_bus_stock_per_fuel)
  hist_bus_stock_per_fuel <- hist_bus_stock_per_fuel[order(as.character(hist_bus_stock_per_fuel$Energy)),]
  hist_bus_stock_per_fuel <- hist_bus_stock_per_fuel[hist_bus_stock_per_fuel$Energy !="total",]
  
  bus_destruction_rate <- Extract_Usage( Extract_Item(Proj_Assumption, "Destruction Rate"), "bus")
  
  bus_market_shares_new <- Extract_Usage( Extract_Item(Proj_Assumption, "Market share in new vehicle"), "bus")
  bus_market_shares_new <- Sort_By_Country(bus_market_shares_new)
  bus_market_shares_new <- bus_market_shares_new[order(as.character(bus_market_shares_new$Energy)),]
  bus_market_shares_new <- bus_market_shares_new[bus_market_shares_new$Energy !="total",]
  
  hist_bus_average_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption"), "bus")
  hist_bus_average_efficiency <- Sort_By_Country(hist_bus_average_efficiency)
  hist_bus_average_efficiency <- hist_bus_average_efficiency[order(as.character(hist_bus_average_efficiency$Energy)),]
  
  hist_new_bus_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption of new vehicle"), "bus")
  hist_new_bus_efficiency <- Sort_By_Country(hist_new_bus_efficiency)
  hist_new_bus_efficiency <- hist_new_bus_efficiency[order(as.character(hist_new_bus_efficiency$Energy)),]
  
  proj_new_bus_efficiency <- Extract_Usage( Extract_Item(Proj_Assumption, "Indice unitary consumption of new vehicle"), "bus")
  proj_new_bus_efficiency <- Sort_By_Country(proj_new_bus_efficiency)
  proj_new_bus_efficiency <- proj_new_bus_efficiency[order(as.character(proj_new_bus_efficiency$Energy)),]
  
  proj_results <- simulate_stock_model_projection(hist_bus_stock_per_fuel, bus_destruction_rate, bus_market_shares_new, proj_bus_total_stock, hist_bus_average_efficiency, hist_new_bus_efficiency, proj_new_bus_efficiency)
  
  proj_bus_fleet_stock <- proj_results$proj_fleet
  proj_bus_average_efficiency <- proj_results$proj_average_efficiency
  proj_bus_new_efficiency <- proj_results$proj_new_efficiency
  
  #5.3 Two wheels stock projection 
  
  hist_two_wheels_stock_per_fuel <- Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "2 wheels")
  hist_two_wheels_stock_per_fuel <- Sort_By_Country(hist_two_wheels_stock_per_fuel)
  hist_two_wheels_stock_per_fuel <- hist_two_wheels_stock_per_fuel[order(as.character(hist_two_wheels_stock_per_fuel$Energy)),]
  hist_two_wheels_stock_per_fuel <- hist_two_wheels_stock_per_fuel[hist_two_wheels_stock_per_fuel$Energy !="total",]
  
  two_wheels_destruction_rate <- Extract_Usage( Extract_Item(Proj_Assumption, "Destruction Rate"), "2 wheels")
  
  two_wheels_market_shares_new <- Extract_Usage( Extract_Item(Proj_Assumption, "Market share in new vehicle"), "2 wheels")
  two_wheels_market_shares_new <- Sort_By_Country(two_wheels_market_shares_new)
  two_wheels_market_shares_new <- two_wheels_market_shares_new[order(as.character(two_wheels_market_shares_new$Energy)),]
  two_wheels_market_shares_new <- two_wheels_market_shares_new[two_wheels_market_shares_new$Energy !="total",]
  
  hist_two_wheels_average_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption"), "2 wheels")
  hist_two_wheels_average_efficiency <- Sort_By_Country(hist_two_wheels_average_efficiency)
  hist_two_wheels_average_efficiency <- hist_two_wheels_average_efficiency[order(as.character(hist_two_wheels_average_efficiency$Energy)),]
  
  hist_new_two_wheels_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption of new vehicle"), "2 wheels")
  hist_new_two_wheels_efficiency <- Sort_By_Country(hist_new_two_wheels_efficiency)
  hist_new_two_wheels_efficiency <- hist_new_two_wheels_efficiency[order(as.character(hist_new_two_wheels_efficiency$Energy)),]
  
  proj_new_two_wheels_efficiency <- Extract_Usage( Extract_Item(Proj_Assumption, "Indice unitary consumption of new vehicle"), "2 wheels")
  proj_new_two_wheels_efficiency <- Sort_By_Country(proj_new_two_wheels_efficiency)
  proj_new_two_wheels_efficiency <- proj_new_two_wheels_efficiency[order(as.character(proj_new_two_wheels_efficiency$Energy)),]
  
  proj_results <- simulate_stock_model_projection(hist_two_wheels_stock_per_fuel, two_wheels_destruction_rate, two_wheels_market_shares_new, proj_2_wheels_total_stock, hist_two_wheels_average_efficiency, hist_new_two_wheels_efficiency, proj_new_two_wheels_efficiency)
  
  proj_two_wheels_fleet_stock <- proj_results$proj_fleet
  proj_two_wheels_average_efficiency <- proj_results$proj_average_efficiency
  proj_two_wheels_new_efficiency <- proj_results$proj_new_efficiency
  
  #5.3 light truck stock projection 
  
  hist_light_truck_stock_per_fuel <- Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "light truck")
  hist_light_truck_stock_per_fuel <- Sort_By_Country(hist_light_truck_stock_per_fuel)
  hist_light_truck_stock_per_fuel <- hist_light_truck_stock_per_fuel[order(as.character(hist_light_truck_stock_per_fuel$Energy)),]
  hist_light_truck_stock_per_fuel <- hist_light_truck_stock_per_fuel[hist_light_truck_stock_per_fuel$Energy !="total",]
  
  light_truck_destruction_rate <- Extract_Usage( Extract_Item(Proj_Assumption, "Destruction Rate"), "light truck")
  
  light_truck_market_shares_new <- Extract_Usage( Extract_Item(Proj_Assumption, "Market share in new vehicle"), "light truck")
  light_truck_market_shares_new <- Sort_By_Country(light_truck_market_shares_new)
  light_truck_market_shares_new <- light_truck_market_shares_new[order(as.character(light_truck_market_shares_new$Energy)),]
  light_truck_market_shares_new <- light_truck_market_shares_new[light_truck_market_shares_new$Energy !="total",]
  
  hist_light_truck_average_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption"), "light truck")
  hist_light_truck_average_efficiency <- Sort_By_Country(hist_light_truck_average_efficiency)
  hist_light_truck_average_efficiency <- hist_light_truck_average_efficiency[order(as.character(hist_light_truck_average_efficiency$Energy)),]
  
  hist_new_light_truck_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption of new vehicle"), "light truck")
  hist_new_light_truck_efficiency <- Sort_By_Country(hist_new_light_truck_efficiency)
  hist_new_light_truck_efficiency <- hist_new_light_truck_efficiency[order(as.character(hist_new_light_truck_efficiency$Energy)),]
  
  proj_new_light_truck_efficiency <- Extract_Usage( Extract_Item(Proj_Assumption, "Indice unitary consumption of new vehicle"), "light truck")
  proj_new_light_truck_efficiency <- Sort_By_Country(proj_new_light_truck_efficiency)
  proj_new_light_truck_efficiency <- proj_new_light_truck_efficiency[order(as.character(proj_new_light_truck_efficiency$Energy)),]
  
  proj_results <- simulate_stock_model_projection(hist_light_truck_stock_per_fuel, light_truck_destruction_rate, light_truck_market_shares_new, proj_light_truck_total_stock, hist_light_truck_average_efficiency, hist_new_light_truck_efficiency, proj_new_light_truck_efficiency)

  proj_light_truck_fleet_stock <- proj_results$proj_fleet
  proj_light_truck_average_efficiency <- proj_results$proj_average_efficiency
  proj_light_truck_new_efficiency <- proj_results$proj_new_efficiency
  
  #5.5 heavy truck stock projection 
  
  hist_heavy_truck_stock_per_fuel <- Extract_Usage( Extract_Item(Hist_Assumption, "Vehicle Stock"), "heavy truck")
  hist_heavy_truck_stock_per_fuel <- Sort_By_Country(hist_heavy_truck_stock_per_fuel)
  hist_heavy_truck_stock_per_fuel <- hist_heavy_truck_stock_per_fuel[order(as.character(hist_heavy_truck_stock_per_fuel$Energy)),]
  hist_heavy_truck_stock_per_fuel <- hist_heavy_truck_stock_per_fuel[hist_heavy_truck_stock_per_fuel$Energy !="total",]
  
  heavy_truck_destruction_rate <- Extract_Usage( Extract_Item(Proj_Assumption, "Destruction Rate"), "heavy truck")
  
  heavy_truck_market_shares_new <- Extract_Usage( Extract_Item(Proj_Assumption, "Market share in new vehicle"), "heavy truck")
  heavy_truck_market_shares_new <- Sort_By_Country(heavy_truck_market_shares_new)
  heavy_truck_market_shares_new <- heavy_truck_market_shares_new[order(as.character(heavy_truck_market_shares_new$Energy)),]
  heavy_truck_market_shares_new <- heavy_truck_market_shares_new[heavy_truck_market_shares_new$Energy !="total",]
  
  hist_heavy_truck_average_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption"), "heavy truck")
  hist_heavy_truck_average_efficiency <- Sort_By_Country(hist_heavy_truck_average_efficiency)
  hist_heavy_truck_average_efficiency <- hist_heavy_truck_average_efficiency[order(as.character(hist_heavy_truck_average_efficiency$Energy)),]
  
  hist_new_heavy_truck_efficiency <- Extract_Usage( Extract_Item(Hist_Assumption, "Unitary consumption of new vehicle"), "heavy truck")
  hist_new_heavy_truck_efficiency <- Sort_By_Country(hist_new_heavy_truck_efficiency)
  hist_new_heavy_truck_efficiency <- hist_new_heavy_truck_efficiency[order(as.character(hist_new_heavy_truck_efficiency$Energy)),]
  
  proj_new_heavy_truck_efficiency <- Extract_Usage( Extract_Item(Proj_Assumption, "Indice unitary consumption of new vehicle"), "heavy truck")
  proj_new_heavy_truck_efficiency <- Sort_By_Country(proj_new_heavy_truck_efficiency)
  proj_new_heavy_truck_efficiency <- proj_new_heavy_truck_efficiency[order(as.character(proj_new_heavy_truck_efficiency$Energy)),]
  
  proj_results <- simulate_stock_model_projection(hist_heavy_truck_stock_per_fuel, heavy_truck_destruction_rate, heavy_truck_market_shares_new, proj_heavy_truck_total_stock, hist_heavy_truck_average_efficiency, hist_new_heavy_truck_efficiency, proj_new_heavy_truck_efficiency)

  proj_heavy_truck_fleet_stock <- proj_results$proj_fleet
  proj_heavy_truck_average_efficiency <- proj_results$proj_average_efficiency
  proj_heavy_truck_new_efficiency <- proj_results$proj_new_efficiency
  
  saveRDS(unique(rbind(Proj_Assumption,
                       Proj_tkm, 
                       proj_car_annual_distance,
                       proj_car_average_efficiency,
                       proj_car_fleet_stock,
                       proj_passenger_per_vehicle,
                       Proj_pkm,
                       proj_bus_average_efficiency,
                       proj_bus_fleet_stock,
                       proj_annual_distance_bus,
                       proj_two_wheels_average_efficiency,
                       proj_two_wheels_fleet_stock,
                       proj_annual_distance_2_wheels,
                       proj_heavy_truck_average_efficiency,
                       proj_heavy_truck_fleet_stock,
                       proj_annual_distance_heavy_truck,
                       proj_light_truck_average_efficiency,
                       proj_light_truck_fleet_stock,
                       proj_annual_distance_light_truck,
                       proj_heavy_truck_new_efficiency,
                       proj_light_truck_new_efficiency,
                       proj_two_wheels_new_efficiency,
                       proj_car_new_efficiency,
                       proj_bus_new_efficiency)),
          "../../Data/temp/Proj_Assumption.Rds")
}

#################################################################
# Function for loading et preparing data for calibration
#################################################################

prepare_data_transport_calibration <- function(Hist_Macro, Hist_Assumption, Hist_Energy_Demand)
{
  #1. Data input extraction 
  TRA_Hist_Energy_Demand <- Extract_Sector(Hist_Energy_Demand, "TRA")
  TRA_Hist_Energy_Demand <- Sort_By_Country(TRA_Hist_Energy_Demand)
  TRA_Hist_Energy_Demand <- TRA_Hist_Energy_Demand[order(TRA_Hist_Energy_Demand$Sub_Sector),]
  TRA_Hist_Energy_Demand <- TRA_Hist_Energy_Demand[order(TRA_Hist_Energy_Demand$Energy),]
  
  TRA_Hist_Assumption <- Extract_Sector(Hist_Assumption, "TRA")
  TRA_Hist_Assumption <- Sort_By_Country(TRA_Hist_Assumption)
  TRA_Hist_Assumption <- TRA_Hist_Assumption[order(TRA_Hist_Assumption$Usage),]
  TRA_Hist_Assumption <- TRA_Hist_Assumption[order(TRA_Hist_Assumption$Energy),]
  TRA_Hist_Assumption <- TRA_Hist_Assumption[order(TRA_Hist_Assumption$ID_Item),]
  
  TRA_Hist_Macro <- Extract_Item(Hist_Macro, c("GDP","Population", "GDP per capita"))
  TRA_Hist_Macro <- Sort_By_Country(TRA_Hist_Macro)
  TRA_Hist_Macro <- TRA_Hist_Macro[order(TRA_Hist_Macro$ID_Item),]
  
  TRA_hist_pkm <- Extract_Sub_Sector( Extract_Item(TRA_Hist_Assumption, "Passenger traffic"), "total")
  GDP <- Sort_By_Country(Extract_Item(Hist_Macro, "GDP"))
  GDP_per_capita <- Sort_By_Country(Extract_Item(Hist_Macro, "GDP per capita"))
  Population <- Sort_By_Country(Extract_Item(Hist_Macro, "Population"))
  
  TRA_hist_vehicle_stock <- Extract_Item(TRA_Hist_Assumption, "Vehicle Stock")
  TRA_hist_passenger_vehicle_stock <- Extract_Specific_Energy(Extract_Usage(TRA_hist_vehicle_stock, c("car")), "total")
  TRA_hist_passenger_vehicle_stock <- aggregate(TRA_hist_passenger_vehicle_stock[10:length(TRA_hist_passenger_vehicle_stock)], by = list(TRA_hist_passenger_vehicle_stock$Country), FUN = sum)
  TRA_hist_passenger_vehicle_stock_header <- TRA_hist_vehicle_stock[1:nrow(TRA_hist_passenger_vehicle_stock), 1:9]
  TRA_hist_passenger_vehicle_stock_header$Country <- c(Country = levels(as.factor(TRA_hist_passenger_vehicle_stock[,1])))
  names(TRA_hist_passenger_vehicle_stock_header)[3] <- "Country"
  TRA_hist_passenger_vehicle_stock_header$Energy <- "total"
  TRA_hist_passenger_vehicle_stock_header$Usage <- "total"
  TRA_hist_passenger_vehicle_stock <- TRA_hist_passenger_vehicle_stock[-1]
  TRA_hist_passenger_vehicle_stock <- cbind(TRA_hist_passenger_vehicle_stock_header, TRA_hist_passenger_vehicle_stock)
  
  passenger_vehicule_per_capita <- TRA_hist_passenger_vehicle_stock
  passenger_vehicule_per_capita$ID_Item <- "Vehicule per capita"
  passenger_vehicule_per_capita$Unit <- "veh/capita"
  passenger_vehicule_per_capita[,10:length(passenger_vehicule_per_capita)] <- TRA_hist_passenger_vehicle_stock[,10:length(TRA_hist_passenger_vehicle_stock)] / Population[,10:length(Population)]
  
  total_passenger_vehicule_stock <- cbind(TRA_hist_passenger_vehicle_stock[1,1:9], t(colSums(TRA_hist_passenger_vehicle_stock[,10:length(TRA_hist_passenger_vehicle_stock)])))
  total_passenger_vehicule_stock$Country <- "EU23"
  
  total_GDP <- cbind(GDP[1,1:9], t(colSums(GDP[,10:length(GDP)])))
  total_GDP$Country <- "EU23"
  
  total_Population <- cbind(Population[1,1:9], t(colSums(Population[,10:length(Population)])))
  total_Population$Country <- "EU23"
  
  
  pkm_per_cap <- TRA_hist_pkm
  pkm_per_cap$ID_Item <- "Passenger traffic per capita"
  pkm_per_cap$Unit <- "pkm/cap"
  pkm_per_cap[,10:length(pkm_per_cap)] <- TRA_hist_pkm[,10:length(TRA_hist_pkm)] / Population[,10:length(Population)] *1000000
  
  saveRDS(unique(rbind(Hist_Assumption ,pkm_per_cap)), "../../Data/temp/Hist_Assumption.Rds")
  
  total_GDP_per_capita <- total_GDP
  total_GDP_per_capita$ID_Item <- "GDP per capita"
  total_GDP_per_capita$Unit <- "k$/cap"
  total_GDP_per_capita[,10:length(total_GDP_per_capita)] <- total_GDP[,10:length(total_GDP)] / total_Population[,10:length(total_Population)] 
  
  total_passenger_vehicule_per_capita <- total_passenger_vehicule_stock
  total_passenger_vehicule_per_capita$ID_Item <- "Passenger vehicule per capita"
  total_passenger_vehicule_per_capita$Unit <- "veh/capita"
  total_passenger_vehicule_per_capita[,10:length(total_passenger_vehicule_per_capita)] <- total_passenger_vehicule_stock[,10:length(total_passenger_vehicule_stock)] / total_Population[,10:length(total_Population)]
  
  rownames(GDP) <- GDP$Country
  rownames(GDP_per_capita) <- GDP_per_capita$Country
  rownames(passenger_vehicule_per_capita) <- passenger_vehicule_per_capita$Country
  rownames(pkm_per_cap) <- pkm_per_cap$Country 
  
  # Country_area <- Sort_By_Country(Extract_Item(Hist_Macro, "Country Area"))
  # Population_density <- Population
  # Population_density$ID_Item <- "Population density"
  # Population_density$Unit <- "pers/km²"
  # Population_density[,10:length(Population_density)] <- Population[,10:length(Population)] / Country_area[,10:length(Country_area)] * 1000
  # Population_density[,10:length(Population_density)] <- Population_density[,10:length(Population_density)] / rowMeans(Population_density[,10:length(Population_density)], na.rm = T)
  # rownames(Population_density) <- Population_density$Country
  
  tkm <- Extract_Usage(Extract_Sub_Sector( Extract_Item(TRA_Hist_Assumption, "Goods traffic"), "Road"), "total")
  tkm_per_infra <- tkm
  # tkm_per_infra$ID_Item <- "Goods traffic per capita"
  # tkm_per_infra$Unit <- "tkm/cap"
  # tkm_per_infra[,10:length(tkm_per_infra)] <- tkm[,10:length(tkm)]  / tkm_ponderation[,2:length(tkm_ponderation)]
  
  
  final_data <- list("GDP_per_cap" = GDP_per_capita[,10:26], "veh_per_cap" = passenger_vehicule_per_capita[,10:26], "pkm_per_cap" = pkm_per_cap[10:26], "tkm" = tkm[,10:26]) 
  
  return(final_data)  
}


#################################################################
# Function for car stock projection 
#################################################################

sse_formula.gompertz <- function(veh_per_capita, GDP_per_capita, calibrated_parameters)
{
    mean_gamma <- calibrated_parameters[1]
    alpha <- calibrated_parameters[2]
    beta <- calibrated_parameters[3]
    
    y <- sum( ( veh_per_capita - formula.gompertz(GDP_per_capita, mean_gamma, alpha, beta) )^2 )
  
  return(y)
}

formula.gompertz <- function(GDP_per_capita, mean_gamma, alpha, beta)
{
  y <- GDP_per_capita
  
  y <- mean_gamma * exp(alpha*exp(beta * GDP_per_capita))

  return(y)
}

fit.gompertz <- function(veh_per_capita, GDP_per_capita, lower_bounds, upper_bounds){

  mean_gamma <- NULL
  alpha <- NULL
  beta <- NULL

  par <- direct(fn = sse_formula.gompertz, veh_per_capita = veh_per_capita, GDP_per_capita = GDP_per_capita, lower = lower_bounds  , upper = upper_bounds, control = list(maxeval = 50000))$par
  
  mean_gamma <- par[1]
  alpha <- par[2]
  beta <- par[3]

  final_data = list("mean_gamma" = mean_gamma, "alpha" = alpha, "beta" = beta)
  
  return(final_data)
}


transform_data_veh <- function(input)
{
  GDP_per_cap <- NULL
  veh_per_cap <- NULL
  
  for(i in 1:nrow(input$GDP_per_cap))
  {
    GDP_per_cap <- c(GDP_per_cap, input$GDP_per_cap[i,])
    veh_per_cap <- c(veh_per_cap, input$veh_per_cap[i,])
  }
  
  final_data = data.frame("Country" = rep(rownames(input$GDP_per_cap), each = 17), "GDP_per_cap" = unlist(GDP_per_cap), "veh_per_cap" = unlist(veh_per_cap))
 # final_data <- final_data[order(final_data$GDP_per_cap),]
  return(final_data)
}

transport_car_stock_calibration_and_projection <- function(data_veh_calibration, total_car_hist_vehicle_stock, Proj_GDP_per_capita, Population, Hist_annual_distance_car, proj_passenger_per_vehicle, Proj_pkm)
{
  ##2.1 On calcule alpha (not country specific) poure les pays ayant un faible niveau de GDP_per_cap
  lower_bounds <- c(0, -10, -1)
  upper_bounds <- c(0.8, 0, 0)
  alpha <- NULL
  
  list_poor_country <- c("RO", "HU", "PL", "BG")
  
  message(paste0("Calibration of alpha parameter started"))
  for(i in 1:length(list_poor_country))
  {
    data_veh_calibration_loop <-  data_veh_calibration[data_veh_calibration$Country == list_poor_country[i],]
    par <- fit.gompertz(data_veh_calibration_loop$veh_per_cap, data_veh_calibration_loop$GDP_per_cap, lower_bounds, upper_bounds)
    alpha <- c(alpha, par$alpha)
  }
  message(paste0("Calibration of alpha parameter finished"))
  
  alpha = mean(alpha)
  
  ##2.2 On fixe alpha et on calcule beta et gammma pour chaque pays
  
  lower_bounds <- c(0, alpha, -1)
  upper_bounds <- c(0.8, alpha, 0)
  gamma <- NULL
  beta <- NULL
  for(i in 1:nlevels(as.factor(data_veh_calibration$Country)))
  {
    data_veh_calibration_loop <-  data_veh_calibration[data_veh_calibration$Country == levels(as.factor(data_veh_calibration$Country))[i],]
    
    message(paste0("Calibration of gamma and beta parameters for ", levels(as.factor(data_veh_calibration$Country))[i], " started"))
    par <- fit.gompertz(data_veh_calibration_loop$veh_per_cap, data_veh_calibration_loop$GDP_per_cap, lower_bounds, upper_bounds)
    message(paste0("Calibration of gamma and beta parameters for ", levels(as.factor(data_veh_calibration$Country))[i], " finished"))
    
    gamma <- c(gamma, par$mean_gamma)
    beta <- c(beta, par$beta)
  }
  
  names(gamma) <- levels(as.factor(data_veh_calibration$Country))
  names(beta) <- levels(as.factor(data_veh_calibration$Country))
  
  ##2.3 recalibration si necessaire 
  
  min_beta <- beta["NO"]
  max_beta <- beta["BG"]
  
  list_country_weird <- names(c( beta[beta < max_beta], beta[beta > min_beta])) 
  
  lower_bounds <- c(0, alpha, max_beta)
  upper_bounds <- c(0.8, alpha, min_beta)
  
  for(i in 1:length(list_country_weird))
  {
    message(paste0("Recalibration of ", list_country_weird[i], " started"))
    par <- fit.gompertz(data_veh_calibration[data_veh_calibration$Country == list_country_weird[i],]$veh_per_cap, data_veh_calibration[data_veh_calibration$Country == list_country_weird[i],]$GDP_per_cap, lower_bounds, upper_bounds)
    message(paste0("Recalibration of ", list_country_weird[i], " finished"))
    
    gamma[list_country_weird[i]] <- par$mean_gamma
    beta[list_country_weird[i]] <- par$beta
  }

  ##2.5 projection of total car stock for each country 
  
  total_car_hist_vehicle_stock <- Sort_By_Country(total_car_hist_vehicle_stock)
  total_car_trajectories_vehicle_stock <- total_car_hist_vehicle_stock
  
  Proj_GDP_per_capita <- Sort_By_Country(Proj_GDP_per_capita)
  Population <- Sort_By_Country(Population)
  
  Year_Proj <- min(which(is.nan(colSums(total_car_trajectories_vehicle_stock[10:length(total_car_trajectories_vehicle_stock)])))) + 9
  for(i in 1:nrow(total_car_trajectories_vehicle_stock))
  {
    total_car_trajectories_vehicle_stock[i, (Year_Proj-1):length(total_car_trajectories_vehicle_stock)] <- formula.gompertz(Proj_GDP_per_capita[i, (Year_Proj-1):length(Proj_GDP_per_capita)], gamma[i], alpha, beta[i]) * Population[i, (Year_Proj-1):length(Population)]
  }
  
  total_car_proj_vehicle_stock <- total_car_hist_vehicle_stock
  for(i in Year_Proj:length(total_car_proj_vehicle_stock))
  {
    total_car_proj_vehicle_stock[, i] <- total_car_proj_vehicle_stock[, i-1] * total_car_trajectories_vehicle_stock[,i] / total_car_trajectories_vehicle_stock[,i-1]
  }
  
  car_proj_pkm <- Extract_Usage(Proj_pkm, "car")
  
  proj_annual_distance_car <- Sort_By_Country(Hist_annual_distance_car)
  proj_annual_distance_car[,Year_Proj:length(proj_annual_distance_car)] <- car_proj_pkm[,Year_Proj:length(car_proj_pkm)] / (proj_passenger_per_vehicle[,Year_Proj:length(proj_passenger_per_vehicle)] * total_car_proj_vehicle_stock[,Year_Proj:length(total_car_proj_vehicle_stock)]) * 1000000 
  
  final_data <- list( "total_car_proj_vehicle_stock" = total_car_proj_vehicle_stock, "proj_annual_distance_car" = proj_annual_distance_car)
 
  write.csv2(c("gamma" = gamma, "alpha" = alpha, "beta" = beta), file = "../../Data/temp/veh_per_cap_calibration_parameters.csv")
 
   return(final_data)
}

#################################################################
# Function for pkm projection 
#################################################################

transform_data_pkm <- function(input)
{
  GDP_per_cap <- NULL
  pkm_per_cap <- NULL
  
  for(i in 1:nrow(input$GDP_per_cap))
  {
    GDP_per_cap <- c(GDP_per_cap, input$GDP_per_cap[i,])
    pkm_per_cap <- c(pkm_per_cap, input$pkm_per_cap[i,])
  }
  
  final_data = data.frame("Country" = rep(rownames(input$GDP_per_cap), each = 17), "GDP_per_cap" = unlist(GDP_per_cap), "pkm_per_cap" = unlist(pkm_per_cap))
  final_data <- final_data[order(final_data$GDP_per_cap),]
  return(final_data)
}

formula_logistic <- function(GDP_per_cap, gamma, beta, alpha)
{
  y <- GDP_per_cap
  y <- gamma + beta / (1 + exp( alpha * GDP_per_cap))
  
  return(y)
}

sse_formula.logistic <- function(pkm_per_cap, GDP_per_cap, calibrated_parameters)
{
    gamma <- calibrated_parameters[1]
    beta <- calibrated_parameters[2]
    alpha <- calibrated_parameters[3]
    
  y <- sum( ( pkm_per_cap[1:length(pkm_per_cap)] - formula_logistic(GDP_per_cap, gamma, beta, alpha) )^2 )
  
  return(y)
}


fit.pkm <- function(pkm_per_capita, GDP_per_capita)
{
  lower_bounds<- c(-10000, 0, -0.1)
  upper_bounds<- c(0, 50000, 0)

  par <- direct(fn = sse_formula.logistic, pkm_per_cap = pkm_per_capita, GDP_per_cap = GDP_per_capita, lower = lower_bounds , upper = upper_bounds, control = list(maxeval = 10000))$par
    
  gamma <- par[1]
  beta <- par[2]
  alpha <- par[3]

  final_data = list("gamma" = gamma, "beta" = beta, "alpha" = alpha)
  
  return(final_data)
}  

transport_hist_pkm_calibration_and_projection <- function(data_pkm_calibration, Hist_Assumption, Proj_Macro)
{
  #on exclue BG et SI car statistiquement etrange
  data_pkm_calibration <- droplevels(subset(data_pkm_calibration, Country != "BG"))
  data_pkm_calibration <- droplevels(subset(data_pkm_calibration, Country != "SI"))
  
  #On calcule les parametre de la fonction logistique qui match le mieux au data
  results_pkm_calibration <- fit.pkm(data_pkm_calibration$pkm_per_cap, data_pkm_calibration$GDP_per_cap)
  
  #projection des pkm  pour chaque pays 
  
  Proj_GDP_per_cap <- Extract_Item(Proj_Macro,"GDP per capita")
  Proj_GDP_per_cap <- Sort_By_Country(Proj_GDP_per_cap)
  
  Hist_pkm_per_cap <- Extract_Item(Hist_Assumption, "Passenger traffic per capita")
  Proj_pkm_per_cap <- Hist_pkm_per_cap
  ## l'erruer vient d'ici, l'année de proj est inf, Hermence
  Year_proj <- min(which(is.nan(colSums(Proj_pkm_per_cap[10:length(Proj_pkm_per_cap)])))) + 9
  
  for(i in (Year_proj-1):length(Proj_GDP_per_cap))
  {
    Proj_pkm_per_cap[,i] <- formula_logistic(Proj_GDP_per_cap[,i], results_pkm_calibration$gamma, results_pkm_calibration$beta, results_pkm_calibration$alpha)
  }
  
  #on colle les trajectoires sur les points de depart
  
  pkm_per_cap <- Extract_Item(Hist_Assumption, "Passenger traffic per capita")
  
  for(i in Year_proj:length(pkm_per_cap))
  {
    pkm_per_cap[,i] <- (Proj_pkm_per_cap[,i]/Proj_pkm_per_cap[,i-1]) * pkm_per_cap[,i-1]
  }
  
  Proj_Population <- Sort_By_Country(Extract_Item(Proj_Macro,"Population"))
  
  total_pkm <- pkm_per_cap
  total_pkm$ID_Item <- "Passenger traffic"
  total_pkm$Unit <- "Gpkm"
  total_pkm[,10:length(total_pkm)] <- pkm_per_cap[,10:length(pkm_per_cap)] * Proj_Population[,10:length(Proj_Population)] * (1000 / 1000000000)
  
  #Allocation des pkm en fonction du type de transport (air, Marine or Road)
  Hist_pkm <- Extract_Usage(Extract_Item(Hist_Assumption, "Passenger traffic"), "total")
  Hist_pkm <- Sort_By_Country(Hist_pkm)
  Hist_pkm <- Hist_pkm[order(Hist_pkm$Sub_Sector),]
  Hist_share_pkm <- Hist_pkm
  Hist_share_pkm$ID_Item <- "Share in passenger traffic"
  Hist_share_pkm$Unit <- "%"
  
  for(i in 1:nrow(Hist_share_pkm))
  {
    Hist_share_pkm[i,10:length(Hist_share_pkm)] <- Hist_pkm[i,10:length(Hist_pkm)] / Extract_Sub_Sector(Hist_pkm, "total")[Extract_Sub_Sector(Hist_pkm, "total")$Country == Hist_pkm$Country[i] ,10:length( Extract_Sector(Hist_pkm, "total"))] 
  }
  
  Proj_share_pkm <- Hist_share_pkm
  Proj_share_pkm[,Year_proj:length(Proj_share_pkm)] <- Hist_share_pkm[,(Year_proj-1)]
  
  Proj_pkm <- Hist_pkm
  Proj_pkm[Proj_pkm$Sub_Sector == "total",] <- total_pkm
  
  for(i in 1:nrow(Proj_pkm))
  {
    Proj_pkm[i,Year_proj:length(Proj_pkm)] <- Proj_share_pkm[i, Year_proj:length(Proj_share_pkm)] * Extract_Sub_Sector(Proj_pkm, "total")[Extract_Sub_Sector(Proj_pkm, "total")$Country == Proj_pkm$Country[i] ,Year_proj:length(Proj_pkm)] 
  }
  
  #Puis Allocation des pkm pour le sous secteur Road (2wheels bus and cars)
  Hist_Road_pkm <- Extract_Sub_Sector(Extract_Item(Hist_Assumption, "Passenger traffic"), "Road")
  Hist_Road_pkm <- Sort_By_Country(Hist_Road_pkm)
  Hist_Road_pkm <- Hist_Road_pkm[order(Hist_Road_pkm$Usage),]
  Hist_share_Road_pkm <- Hist_Road_pkm
  Hist_share_Road_pkm$ID_Item <- "Share in passenger traffic"
  Hist_share_Road_pkm$Unit <- "%"
  
  for(i in 1:nrow(Hist_share_Road_pkm))
  {
    Hist_share_Road_pkm[i,10:length(Hist_share_Road_pkm)] <- Hist_Road_pkm[i,10:length(Hist_Road_pkm)] / Extract_Usage(Hist_Road_pkm, "total")[Extract_Usage(Hist_Road_pkm, "total")$Country == Hist_Road_pkm$Country[i] ,10:length(Hist_Road_pkm)] 
  }
  
  Proj_share_Road_pkm <- Hist_share_Road_pkm
  Proj_share_Road_pkm[,Year_proj:length(Proj_share_Road_pkm)] <- Hist_share_Road_pkm[,(Year_proj-1)]
  
  Proj_Road_pkm <- Hist_Road_pkm
  Proj_Road_pkm[Proj_Road_pkm$Usage == "total",] <- Proj_pkm[Proj_pkm$Sub_Sector == "Road" & Proj_pkm$Usage == "total",]
  
  for(i in 1:nrow(Proj_Road_pkm))
  {
    Proj_Road_pkm[i,Year_proj:length(Proj_Road_pkm)] <- Proj_share_Road_pkm[i, Year_proj:length(Proj_share_Road_pkm)] * Extract_Usage(Proj_Road_pkm, "total")[Extract_Usage(Proj_Road_pkm, "total")$Country == Proj_Road_pkm$Country[i] ,Year_proj:length(Proj_Road_pkm)] 
  }
  
  Proj_pkm <- rbind(Proj_pkm, Proj_Road_pkm[Proj_Road_pkm$Usage !="total",])
  
  Final_data <- list("Proj_pkm" = Proj_pkm, "gamma" = results_pkm_calibration$gamma, "beta" = results_pkm_calibration$beta, "alpha" = results_pkm_calibration$alpha)  
  
  write.csv2(c("gamma" = Final_data$gamma, "beta" = Final_data$beta, "alpha" = Final_data$alpha), file = "../../Data/temp/pkm_calibration_parameters.csv")
  
  return(Final_data)  
}

#################################################################
# Function for fleet model
#################################################################


simulate_stock_model_projection <- function(hist_fleet, destruction_rate, market_share_in_new, total_stock_projection, hist_car_average_efficiency, hist_new_car_efficiency, proj_new_car_efficiency, Proj_Assumption)
{
  #1. Stock projection
  Year_proj <- min(which(is.nan(colSums(hist_fleet[10:length(hist_fleet)])))) + 9
  
  destructed_vehicle <- total_stock_projection
  destructed_vehicle[,10] <- NaN
  
  for(i in 11:length(destructed_vehicle))
  {
    destructed_vehicle[,i] <- total_stock_projection[,i-1] * destruction_rate[,i]
  }
  
  destruction_rate_bis <- destruction_rate
  destruction_rate_bis <- destruction_rate_bis[ rep( seq_len( nrow( destruction_rate_bis ) ), nlevels( as.factor( market_share_in_new$Energy ) ) ), ]

  total_sales_vehicle <- total_stock_projection
  total_sales_vehicle[,10] <- NaN
  total_sales_vehicle$ID_Item <- "Sales of new vehicle"
  
  
  for(i in 11:length(total_sales_vehicle))
  {
    total_sales_vehicle[,i] <- total_stock_projection[,i] - total_stock_projection[i-1] + destructed_vehicle[,i]
  }
  
  total_sales_vehicle_bis <- total_sales_vehicle
  total_sales_vehicle_bis <- total_sales_vehicle_bis[ rep( seq_len( nrow( total_sales_vehicle_bis ) ), nlevels( as.factor( market_share_in_new$Energy ) ) ), ]
  
  
  #2. Average efficiency projection
  
  real_proj_new_car_efficiency <- hist_new_car_efficiency
  for(i in Year_proj:length(hist_new_car_efficiency))
  {
    real_proj_new_car_efficiency[,i] <- real_proj_new_car_efficiency[,(i-1)] * (proj_new_car_efficiency[,i] / proj_new_car_efficiency[,(i-1)]) 
  }
  
  
  proj_fleet <- hist_fleet
  k = 0
  before_destruction <- NULL
  destructed_vehicle_loop <- NULL
  real_destructed_vehicle <- market_share_in_new
  real_destructed_vehicle$ID_Item <- "Destructed vehicle"
  real_destructed_vehicle[,11:Year_proj] <- NaN
  
  # fisrt loop of stock projection
  for(j in 1:nrow(proj_fleet))
  {
    for(i in Year_proj:length(proj_fleet))
    {
      before_destruction <-  proj_fleet[j,i-1] + (total_sales_vehicle_bis[j,i] * market_share_in_new[j,i])
      
      if(round(sum(market_share_in_new[j,i:length(market_share_in_new)], na.rm = T),2) == 0 )
      {
        if( k == 0)
        {
          k = i-1
        }
        destructed_vehicle_loop <- destruction_rate_bis[j,i] * proj_fleet[j,k-5]
      }
      else
      {
        destructed_vehicle_loop <- destruction_rate_bis[j,i] * proj_fleet[j,i-1]
      }
      
      proj_fleet[j,i] <- max(before_destruction - destructed_vehicle_loop,0)
      real_destructed_vehicle[j,i] <- destructed_vehicle_loop

    }
    i = Year_proj
    k = 0
  }
  
  # browser()
  # second loop of stock projection : recalculation of sales (difference si stock en dessous atteint zero sur horizon de projection )
  
  check_total_stock_projection_value <- aggregate(x = proj_fleet[,10:length(proj_fleet)], by = proj_fleet[c("Country", "Usage")], FUN = sum)
  check_total_stock_projection_header  <- proj_fleet[1:nrow(check_total_stock_projection_value), 1:9]  
  check_total_stock_projection_header$Energy <- "total"
  check_total_stock_projection_header$Country <- check_total_stock_projection_value$Country
  check_total_stock_projection_header$Sub_Sector <- "Road"
  check_total_stock_projection_header$Usage <- check_total_stock_projection_value$Usage
  check_total_stock_projection <- unique(cbind(check_total_stock_projection_header, check_total_stock_projection_value[,-(1:2)]))
  
  check_total_stock_projection[,11:length(check_total_stock_projection)] <- total_stock_projection[,11:length(total_stock_projection)] - check_total_stock_projection[,11:length(check_total_stock_projection)]
  sales_to_add <- check_total_stock_projection
  sales_to_add <- sales_to_add[ rep( seq_len( nrow( sales_to_add ) ),  nlevels( as.factor( market_share_in_new$Energy ) ) ), ]
  
  real_sales_new_vehicle <- total_sales_vehicle_bis
  real_sales_new_vehicle$Energy <- market_share_in_new$Energy
  real_sales_new_vehicle[,11:Year_proj] <- NaN
  
  for(i in Year_proj:length(sales_to_add))
  {
    real_sales_new_vehicle[,i] <-  (total_sales_vehicle_bis[,i] + sales_to_add[,i]) * market_share_in_new[,i]
    sales_to_add[,i] <- sales_to_add[,i] * market_share_in_new[,i]
  }
  
  
  # browser()
  proj_average_efficiency <- hist_car_average_efficiency
  
  for(j in 1:nrow(proj_fleet))
  {
    for(i in Year_proj:length(proj_fleet))
    {
      proj_fleet[j,i] <- proj_fleet[j,i] + sales_to_add[j,i]
      real_destructed_vehicle[j,i] <- proj_fleet[j,i-1] - proj_fleet[j,i] + real_sales_new_vehicle[j,i]
      
      if(proj_fleet[j,i] < 0.001)
      {
        proj_average_efficiency[j,i] = proj_average_efficiency[j,i-1]
      }
      else  
      {
        proj_average_efficiency[j,i] <- ( proj_average_efficiency[j,i-1] * proj_fleet[j,i-1] - (rowMeans(proj_average_efficiency[j,(i-10):(i-1)])) * real_destructed_vehicle[j,i] + real_sales_new_vehicle[j,i] * real_proj_new_car_efficiency[j,i] )/ proj_fleet[j,i]
      }
     }    
  }
  
  
  Proj_Assumption <- readRDS("../../Data/temp/Proj_Assumption.RDS")
  saveRDS(rbind(Proj_Assumption, 
                real_destructed_vehicle,
                real_sales_new_vehicle),
          "../../Data/temp/Proj_Assumption.RDS")
  
  
  final_data <- list("proj_fleet" = proj_fleet, "proj_average_efficiency" = proj_average_efficiency, "proj_new_efficiency" = real_proj_new_car_efficiency)
  return(final_data)
}


#################################################################
# Function for tkm projection 
#################################################################




transform_data_tkm <- function(input)
{
  GDP_per_cap <- NULL
  tkm_per_infra <- NULL
  
  for(i in 1:nrow(input$GDP))
  {
    GDP_per_cap <- c(GDP_per_cap, input$GDP_per_cap[i,])
    tkm_per_infra <- c(tkm_per_infra, input$tkm_per_infra[i,])
  }
  
  final_data = data.frame("Country" = rep(rownames(input$GDP_per_cap), each = 17), "GDP_per_cap" = unlist(GDP_per_cap), "tkm_per_infra" = unlist(tkm_per_infra))
  final_data <- final_data[order(final_data$GDP_per_cap),]
  return(final_data)
}

transport_hist_tkm_projection <- function(Hist_Assumption, Proj_Macro, Proj_Assumption)
{
    GDP <- Sort_By_Country(Extract_Item(Proj_Macro, "GDP")) 
    Hist_total_tkm <- Sort_By_Country(Extract_Sub_Sector(Extract_Item(Hist_Assumption, "Goods traffic"), "total"))
    elasticity_tkm_GDP <- Sort_By_Country(Extract_Item(Proj_Assumption, "Elasticity tkm to GDP"))
    
    Proj_total_tkm <- Hist_total_tkm
    Year_Proj <- min(which(is.nan(colSums(Hist_total_tkm[10:length(Hist_total_tkm)])))) + 9
    for(i in Year_Proj:length(Proj_total_tkm)) 
    {
      Proj_total_tkm[,i] <- Proj_total_tkm[,i-1] * (1 + elasticity_tkm_GDP[,i] * (GDP[,i]/GDP[,i-1] - 1))
    }
    
    #Allocation des tkm en fonction du type de transport (air, Marine or Road)
    Hist_tkm <- Extract_Usage(Extract_Item(Hist_Assumption, "Goods traffic"), "total")
    Hist_tkm <- Sort_By_Country(Hist_tkm)
    Hist_tkm <- Hist_tkm[order(Hist_tkm$Sub_Sector),]
    Hist_share_tkm <- Hist_tkm
    Hist_share_tkm$ID_Item <- "Share in Goods traffic"
    Hist_share_tkm$Unit <- "%"
    
    for(i in 1:nrow(Hist_share_tkm))
    {
      Hist_share_tkm[i,10:length(Hist_share_tkm)] <- Hist_tkm[i,10:length(Hist_tkm)] / Extract_Sub_Sector(Hist_tkm, "total")[Extract_Sub_Sector(Hist_tkm, "total")$Country == Hist_tkm$Country[i] ,10:length( Extract_Sector(Hist_tkm, "total"))] 
    }
    
    Proj_share_tkm <- Hist_share_tkm
    Proj_share_tkm[,Year_Proj:length(Proj_share_tkm)] <- Hist_share_tkm[,(Year_Proj-1)]
    
    Proj_tkm <- Hist_tkm
    Proj_tkm[Proj_tkm$Sub_Sector == "total",] <- Proj_total_tkm
    
    for(i in 1:nrow(Proj_tkm))
    {
      Proj_tkm[i,Year_Proj:length(Proj_tkm)] <- Proj_share_tkm[i, Year_Proj:length(Proj_share_tkm)] * Extract_Sub_Sector(Proj_tkm, "total")[Extract_Sub_Sector(Proj_tkm, "total")$Country == Proj_tkm$Country[i] ,Year_Proj:length(Proj_tkm)] 
    }
    
    #Puis Allocation des tkm pour le sous secteur Road (2wheels bus and cars)
    Hist_Road_tkm <- Extract_Sub_Sector(Extract_Item(Hist_Assumption, "Goods traffic"), "Road")
    Hist_Road_tkm <- Sort_By_Country(Hist_Road_tkm)
    Hist_Road_tkm <- Hist_Road_tkm[order(Hist_Road_tkm$Usage),]
    Hist_share_Road_tkm <- Hist_Road_tkm
    Hist_share_Road_tkm$ID_Item <- "Share in Goods traffic"
    Hist_share_Road_tkm$Unit <- "%"
    
    for(i in 1:nrow(Hist_share_Road_tkm))
    {
      Hist_share_Road_tkm[i,10:length(Hist_share_Road_tkm)] <- Hist_Road_tkm[i,10:length(Hist_Road_tkm)] / Extract_Usage(Hist_Road_tkm, "total")[Extract_Usage(Hist_Road_tkm, "total")$Country == Hist_Road_tkm$Country[i] ,10:length(Hist_Road_tkm)] 
    }
    
    Proj_share_Road_tkm <- Hist_share_Road_tkm
    Proj_share_Road_tkm[,Year_Proj:length(Proj_share_Road_tkm)] <- Hist_share_Road_tkm[,(Year_Proj-1)]
    
    Proj_Road_tkm <- Hist_Road_tkm
    Proj_Road_tkm[Proj_Road_tkm$Usage == "total",] <- Proj_tkm[Proj_tkm$Sub_Sector == "Road" & Proj_tkm$Usage == "total",]
    
    for(i in 1:nrow(Proj_Road_tkm))
    {
      Proj_Road_tkm[i,Year_Proj:length(Proj_Road_tkm)] <- Proj_share_Road_tkm[i, Year_Proj:length(Proj_share_Road_tkm)] * Extract_Usage(Proj_Road_tkm, "total")[Extract_Usage(Proj_Road_tkm, "total")$Country == Proj_Road_tkm$Country[i] ,Year_Proj:length(Proj_Road_tkm)] 
    }
    
    Proj_tkm <- rbind(Proj_tkm, Proj_Road_tkm[Proj_Road_tkm$Usage !="total",])
    
    return(Proj_tkm)
}

###################################################### Aggregat #######################################################

calculate_total_sector_TRA <- function(input)
{
  input <- droplevels(subset(input, Energy != "total"))
  
  input_road <- Extract_Sub_Sector(input,"Road")
  
  TRA_Proj_Demand_Country_Usage_Road_total_value <- aggregate(x = input_road[,10:length(input_road)], by = input_road[c("Country", "Usage")], FUN = sum)
  TRA_Proj_Demand_Country_Usage_Road_total_header  <- input_road[1:nrow(TRA_Proj_Demand_Country_Usage_Road_total_value), 1:9]  
  TRA_Proj_Demand_Country_Usage_Road_total_header$Energy <- "total"
  TRA_Proj_Demand_Country_Usage_Road_total_header$Country <- TRA_Proj_Demand_Country_Usage_Road_total_value$Country
  TRA_Proj_Demand_Country_Usage_Road_total_header$Sub_Sector <- "Road"
  TRA_Proj_Demand_Country_Usage_Road_total_header$Usage <- TRA_Proj_Demand_Country_Usage_Road_total_value$Usage
  TRA_Proj_Demand_Country_Usage_Road_total <- unique(cbind(TRA_Proj_Demand_Country_Usage_Road_total_header, TRA_Proj_Demand_Country_Usage_Road_total_value[,-(1:2)]))
  
  TRA_Proj_Demand_Country_Energy_Road_total_value <- aggregate(x = input_road[,10:length(input_road)], by = input_road[c("Country", "Energy")], FUN = sum)
  TRA_Proj_Demand_Country_Energy_Road_total_header  <- input_road[1:nrow(TRA_Proj_Demand_Country_Energy_Road_total_value), 1:9]  
  TRA_Proj_Demand_Country_Energy_Road_total_header$Energy <- TRA_Proj_Demand_Country_Energy_Road_total_value$Energy
  TRA_Proj_Demand_Country_Energy_Road_total_header$Country <- TRA_Proj_Demand_Country_Energy_Road_total_value$Country
  TRA_Proj_Demand_Country_Energy_Road_total_header$Sub_Sector <- "Road"
  TRA_Proj_Demand_Country_Energy_Road_total_header$Usage <- "total"
  TRA_Proj_Demand_Country_Energy_Road_total <- unique(cbind(TRA_Proj_Demand_Country_Energy_Road_total_header, TRA_Proj_Demand_Country_Energy_Road_total_value[,-(1:2)]))
  
  
  TRA_Proj_Demand_Country_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country")], FUN = sum)
  TRA_Proj_Demand_Country_total_header  <- input[1:nrow(TRA_Proj_Demand_Country_total_value), 1:9]  
  TRA_Proj_Demand_Country_total_header$Energy <- "total"
  TRA_Proj_Demand_Country_total_header$Country <- TRA_Proj_Demand_Country_total_value$Country
  TRA_Proj_Demand_Country_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_Country_total_header$Usage <- "total"
  TRA_Proj_Demand_Country_total <- unique(cbind(TRA_Proj_Demand_Country_total_header, TRA_Proj_Demand_Country_total_value[,-1]))
  

  TRA_Proj_Demand_Country_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Energy")], FUN = sum)
  TRA_Proj_Demand_Country_Energy_total_header  <- input[1:nrow(TRA_Proj_Demand_Country_Energy_total_value), 1:9]  
  TRA_Proj_Demand_Country_Energy_total_header$Energy <- TRA_Proj_Demand_Country_Energy_total_value$Energy
  TRA_Proj_Demand_Country_Energy_total_header$Country <- TRA_Proj_Demand_Country_Energy_total_value$Country
  TRA_Proj_Demand_Country_Energy_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_Country_Energy_total_header$Usage <- "total"
  TRA_Proj_Demand_Country_Energy_total <- unique(cbind(TRA_Proj_Demand_Country_Energy_total_header, TRA_Proj_Demand_Country_Energy_total_value[,-(1:2)]))
  

  TRA_Proj_Demand_Country_Sub_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Country", "Sub_Sector")], FUN = sum)
  TRA_Proj_Demand_Country_Sub_Sector_total_header  <- input[1:nrow(TRA_Proj_Demand_Country_Sub_Sector_total_value), 1:9]  
  TRA_Proj_Demand_Country_Sub_Sector_total_header$Energy <- "total"
  TRA_Proj_Demand_Country_Sub_Sector_total_header$Country <- TRA_Proj_Demand_Country_Sub_Sector_total_value$Country
  TRA_Proj_Demand_Country_Sub_Sector_total_header$Sub_Sector <- TRA_Proj_Demand_Country_Sub_Sector_total_value$Sub_Sector
  TRA_Proj_Demand_Country_Sub_Sector_total_header$Usage <- "total"
  TRA_Proj_Demand_Country_Sub_Sector_total <- unique(cbind(TRA_Proj_Demand_Country_Sub_Sector_total_header, TRA_Proj_Demand_Country_Sub_Sector_total_value[,-(1:2)]))
  
  #####EU 23
  TRA_Proj_Demand_EU23_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy")], FUN = sum)
  TRA_Proj_Demand_EU23_Energy_total_header  <- input[1:nrow(TRA_Proj_Demand_EU23_Energy_total_value), 1:9]  
  TRA_Proj_Demand_EU23_Energy_total_header$Energy <- TRA_Proj_Demand_EU23_Energy_total_value$Energy
  TRA_Proj_Demand_EU23_Energy_total_header$Country <- "EU23"
  TRA_Proj_Demand_EU23_Energy_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_EU23_Energy_total_header$Usage <- "total"
  TRA_Proj_Demand_EU23_Energy_total <- unique(cbind(TRA_Proj_Demand_EU23_Energy_total_header, TRA_Proj_Demand_EU23_Energy_total_value[,-1]))
  
  
  TRA_Proj_Demand_EU23_Sub_Sector_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sub_Sector")], FUN = sum)
  TRA_Proj_Demand_EU23_Sub_Sector_total_header  <- input[1:nrow(TRA_Proj_Demand_EU23_Sub_Sector_total_value), 1:9]  
  TRA_Proj_Demand_EU23_Sub_Sector_total_header$Energy <- "total"
  TRA_Proj_Demand_EU23_Sub_Sector_total_header$Country <- "EU23"
  TRA_Proj_Demand_EU23_Sub_Sector_total_header$Sub_Sector <- TRA_Proj_Demand_EU23_Sub_Sector_total_value$Sub_Sector
  TRA_Proj_Demand_EU23_Sub_Sector_total_header$Usage <- "total"
  TRA_Proj_Demand_EU23_Sub_Sector_total <- unique(cbind(TRA_Proj_Demand_EU23_Sub_Sector_total_header, TRA_Proj_Demand_EU23_Sub_Sector_total_value[,-1]))
  
  #on calcule le total EU23 pour chaques energy
  TRA_Proj_Demand_EU23_Usage_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Usage")], FUN = sum)
  TRA_Proj_Demand_EU23_Usage_total_header  <- input[1:nrow(TRA_Proj_Demand_EU23_Usage_total_value), 1:9]  
  TRA_Proj_Demand_EU23_Usage_total_header$Energy <- "total"
  TRA_Proj_Demand_EU23_Usage_total_header$Country <- "EU23"
  TRA_Proj_Demand_EU23_Usage_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_EU23_Usage_total_header$Usage <- TRA_Proj_Demand_EU23_Usage_total_value$Usage
  TRA_Proj_Demand_EU23_Usage_total <- unique(cbind(TRA_Proj_Demand_EU23_Usage_total_header, TRA_Proj_Demand_EU23_Usage_total_value[,-1]))
  TRA_Proj_Demand_EU23_Usage_total <- TRA_Proj_Demand_EU23_Usage_total[TRA_Proj_Demand_EU23_Usage_total$Usage !="total",]
  

  TRA_Proj_Demand_EU23_Usage_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Usage")], FUN = sum)
  TRA_Proj_Demand_EU23_Usage_Energy_total_header  <- input[1:nrow(TRA_Proj_Demand_EU23_Usage_Energy_total_value), 1:9]  
  TRA_Proj_Demand_EU23_Usage_Energy_total_header$Energy <- TRA_Proj_Demand_EU23_Usage_Energy_total_value$Energy
  TRA_Proj_Demand_EU23_Usage_Energy_total_header$Country <- "EU23"
  TRA_Proj_Demand_EU23_Usage_Energy_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_EU23_Usage_Energy_total_header$Usage <- TRA_Proj_Demand_EU23_Usage_Energy_total_value$Usage
  TRA_Proj_Demand_EU23_Usage_Energy_total <- unique(cbind(TRA_Proj_Demand_EU23_Usage_Energy_total_header, TRA_Proj_Demand_EU23_Usage_Energy_total_value[,-(1:2)]))
  TRA_Proj_Demand_EU23_Usage_Energy_total <- TRA_Proj_Demand_EU23_Usage_Energy_total[TRA_Proj_Demand_EU23_Usage_Energy_total$Usage !="total",]
  
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Energy", "Sub_Sector")], FUN = sum)
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_header  <- input[1:nrow(TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_value), 1:9]  
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_header$Energy <- TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_value$Energy
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_header$Country <- "EU23"
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_header$Sub_Sector <- TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_value$Sub_Sector
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_header$Usage <- "total"
  TRA_Proj_Demand_EU23_Sub_Sector_Energy_total <- unique(cbind(TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_header, TRA_Proj_Demand_EU23_Sub_Sector_Energy_total_value[,-(1:2)]))
  
  TRA_Proj_Demand_EU23_total_value <- aggregate(x = input[,10:length(input)], by = input[c("Sector")], FUN = sum)
  TRA_Proj_Demand_EU23_total_header  <- input[1:nrow(TRA_Proj_Demand_EU23_total_value), 1:9]  
  TRA_Proj_Demand_EU23_total_header$Energy <- "total"
  TRA_Proj_Demand_EU23_total_header$Country <- "EU23"
  TRA_Proj_Demand_EU23_total_header$Sub_Sector <- "total"
  TRA_Proj_Demand_EU23_total_header$Usage <- "total"
  TRA_Proj_Demand_EU23_total_header$Sector <- TRA_Proj_Demand_EU23_total_value$Sector
  TRA_Proj_Demand_EU23_total <- unique(cbind(TRA_Proj_Demand_EU23_total_header, TRA_Proj_Demand_EU23_total_value[,-1]))
  
  
  input <- rbind(input, TRA_Proj_Demand_Country_total, TRA_Proj_Demand_Country_Energy_total, TRA_Proj_Demand_EU23_Energy_total, TRA_Proj_Demand_EU23_Sub_Sector_Energy_total, TRA_Proj_Demand_EU23_Usage_Energy_total, TRA_Proj_Demand_EU23_Sub_Sector_total,  TRA_Proj_Demand_EU23_Usage_total,  TRA_Proj_Demand_EU23_total, TRA_Proj_Demand_Country_Sub_Sector_total, TRA_Proj_Demand_Country_Usage_Road_total, TRA_Proj_Demand_Country_Energy_Road_total)
  
  return(input)
}   