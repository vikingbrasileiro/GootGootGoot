source("../Functions/Input/General_Inputs_Functions.R")

########################################################################################################################################################################
# Substitution model for Industry
# 2018-07-03 v1: Simulation of subtituion rate based on historical demand and snesibility to evolution of energy prices
########################################################################################################################################################################

require( reshape )
require( ggplot2 )
library( tictoc )
library(numDeriv)
library(nloptr)

Substitution_Calibration_Model <- function(calibrarion_date_start = 2000, calibration_date_end = 2017, simulation_date_end = 2050, Proj_Assumption, Hist_Energy_Demand)
{
  message("Read Input Data Start")
  start_time <- Sys.time()
  historical_data <- read_data(Proj_Assumption, Hist_Energy_Demand)
  end_time <- Sys.time()
  message(paste("Read Input Data finished in", as.character(round(end_time - start_time,2)), "s" ))
  
  industries = levels( as.factor(historical_data$Sub_Sector ))
  industries = industries[-3] #on enleve le secteur total industry car pas de activity rate 
  n_industries = nlevels( as.factor(historical_data$Sub_Sector) ) - 1 
  
  countries = levels(as.factor(historical_data$Country))
  n_countries = nlevels(as.factor(historical_data$Country))
  
  start_year_hist <- 2000
  No_year_to_calibrate <- calibration_date_end - calibrarion_date_start 
  calibration_end = (calibration_date_end-(start_year_hist))
  calibration_begin =  calibration_end - No_year_to_calibrate + 1
  calibration_range = c( calibration_begin, calibration_end )
  simulation_end = calibration_end + ( simulation_date_end - calibration_date_end ) + 1
  
  final_output <- NULL
  
  start_time_cal_sim <- Sys.time()
  for( j in 1:n_countries)
  #for( j in 6:6)
  {
    country = countries[ j ]
    historical_data_country_specific = Extract_Country(historical_data, country)
    
    start_country_time <- Sys.time()
    message(paste("Simulation and Calibration of", country, "just start"))
    
    for( i in 1:n_industries )
    {
      ind = industries[ i ]
      
      energy_demand <-  Extract_Sub_Sector( Extract_Item(historical_data_country_specific, "Energy Demand"), ind)
      Year_Proj_col = min(which(is.nan(colSums(energy_demand[,10:length(energy_demand)])))) + 9
      sum_sub_sector_demand <- sum(rowSums(energy_demand[,10:(Year_Proj_col-1)]))
                                   
      if(sum_sub_sector_demand > 1e-3)
      {
        calibrated_parameters = suppressWarnings(get_calibrated_parameters( historical_data_country_specific, ind, calibration_range, prepare_data_for_calibration, calibrate_system, simulate_system ))
        
        simulated_results = suppressWarnings(get_simulated_demand( historical_data_country_specific, ind, calibrated_parameters, simulation_end, prepare_data_for_calibration, simulate_system, get_melted_df ))
        simulated_demand = simulated_results$demand
        substitution_rate = simulated_results$substitution_rate
        
        simulated_demand = as.data.frame(simulated_demand)
        names(simulated_demand) = levels(as.factor(historical_data_country_specific$Energy))
        row.names(simulated_demand) = paste("X", seq(calibrarion_date_start, simulation_date_end), sep = "" )
        
        row.names(substitution_rate) = paste("X", seq(calibrarion_date_start, simulation_date_end), sep = "" )
      
        simulated_demand <- transform_to_input_format("demand", simulated_demand, country, ind, calibrarion_date_start, energy_demand)
        substitution_rate <- transform_to_input_format("substitution", substitution_rate, country, ind, calibrarion_date_start, energy_demand)
      }
      else
      {
        #substitution_rate
        simulated_demand <- energy_demand
        simulated_demand$ID_Item <- "Simulated Demand"
        simulated_demand[,Year_Proj_col:length(energy_demand)] <- 0
        #simulated_demand <- Extract_Years(simulated_demand, calibrarion_date_start, calibration_date_end)
        
        Header_sub <- data.frame(ID_Item = rep("Substitution Rate", 4), Scenario = rep("GB", 4), Country = rep(country, 4), Energy = levels(as.factor(energy_demand$Energy)),
                                 Sector = rep("IND", 4), Sub_Sector = rep(ind, 4), Usage = rep("na", 4), Source = rep("AMADEUS", 4), "Unit" = rep("%", 4) )
        value_sub <- energy_demand[,10:length(energy_demand)]
        value_sub[,1:length(value_sub)] <-0
        substitution_rate <- cbind(Header_sub, value_sub)
        #substitution_rate <- Extract_Years(substitution_rate, calibrarion_date_start, calibration_date_end)
      }
      ####################### Ajout ligne biomass heat and total a zero necessaire pour la projection (total etant juste la somme des energies)
      heat <- substitution_rate[1,]
      heat$Energy <- "final heat"
      heat[,10:length(heat)] <- 0
      
      biomass <- substitution_rate[1,]
      biomass$Energy <- "biomass and waste"
      biomass[,10:length(biomass)] <- 0
      
      hydrogen <- substitution_rate[1,]
      hydrogen$Energy <- "direct hydrogen"
      hydrogen[,10:length(hydrogen)] <- 0
      
      total <- substitution_rate[1,]
      total$Energy <- "total"
      total[,10:length(total)] <- 0
      ########################################################################################
      
      substitution_rate <- rbind(substitution_rate, heat, biomass, hydrogen, total) 
      substitution_rate[, 10:25] <- 0
      final_output <- rbind( final_output, simulated_demand, substitution_rate)
      
      print(ind)
    }
    end_country_time <- Sys.time()
    message(paste("Simulation and Calibration of", country, "finished in", as.character(round(end_country_time - start_country_time, 2)), "s" ))
  }
  
  final_output <- final_output[order(final_output$ID_Item),]
  final_output <- Sort_By_Country(final_output)
  
  end_time_cal_sim <- Sys.time()
  message(paste("Calibration and simulation of all countries and industries finished in", as.character(floor(end_time_cal_sim - start_time_cal_sim)), "min", "and", 
                as.character(round( ((end_time_cal_sim - start_time_cal_sim) - floor(end_time_cal_sim - start_time_cal_sim)) * 60, 0)) ,"s"))

  return(final_output)
  
}
###################################################################################################################################################################################???

# simulate the system
#  input:
#   horizon: vector of time periods
#	total_demand: vector of demand for energy
#   initial_stock: vector of initial stock of equipment
#   cost: matrix of cost of each equipment - constant part
#	premium: vector of premia of each equipment - calibrated part of the cost
#	lifetime: vector of lifetime of equipment
#	invest_time: time before investment is active
#	alpha: value of the logit function coefficient
#	a: value of the coefficient for amount of new equipment when demand is less than existing equipment
# output:
#   trajectory: data frame of share of each technology for each time period


simulate_system = function( horizon, total_demand, historical_demand, initial_stock_new, initial_stock_old, cost, activity_rate, eco_eff_rate, tech_eff_rate, premium, lifetime = 20, invest_time = 1, alpha, a = 1 )
{
  # function code
  vintage_new = matrix( 0, nrow = length( horizon ), ncol = length( initial_stock_new ) )
  vintage_old = matrix( 0, nrow = length( horizon ), ncol = length( initial_stock_new ) )
  demand = matrix( 0, nrow = length( horizon ), ncol = length( initial_stock_new ) )
  
  vintage_new[ horizon[ 1 ], ] = initial_stock_new
  vintage_old[ horizon[ 1 ], ] = initial_stock_old
  demand[ horizon[ 1 ], ] = initial_stock_new + initial_stock_old
  
  substitution_rate  = data.frame(coal = rep (0, length(horizon)), electricity = rep (0, length(horizon)), gas = rep (0, length(horizon)), oil = rep (0, length(horizon)))
  share = matrix( 0, nrow = length( horizon ), ncol = length( initial_stock_new ) )
  
  last_Hist_year_col = min( which( total_demand == 0) ) - 1
  
  for( p in horizon[ 2:length( horizon ) ] )
  {
    
    if(p - 1 == last_Hist_year_col)
    {
      #demand[1:(p-1),] <-  historical_demand[1:(p-1),]
      demand[ p - 1, ] <-  historical_demand[ p - 1, ]
      vintage_old_last_year <- vintage_old[ p - 1, ]
      vintage_new_last_year <- vintage_new[ p - 1, ]
      vintage_old[ p - 1, ] <- demand[ p - 1, ] * ( vintage_old_last_year / (vintage_old_last_year + vintage_new_last_year) )
      vintage_new[ p - 1, ] <- demand[ p - 1, ] * ( vintage_new_last_year / (vintage_old_last_year + vintage_new_last_year) )
    }

    if ( p > last_Hist_year_col) # for projection of the substitution effect
    {
      #projection de la demande totale pour le calcul des taux de subtitution en ne prenant pas en compte efficacite technique
      total_demand[ p ] = total_demand[ p - 1 ] * ( 1 + activity_rate[ p , 1 ] ) * ( 1 + eco_eff_rate[ p , 1 ] )
      
      scrapped_equipment = sum( vintage_old[ p - 1, ] / ( lifetime / 2 ) )
      existing_equipment = sum( vintage_new[ p - 1, ] + vintage_old[ p - 1, ] )
      
      if( ( total_demand[ p ] - existing_equipment ) >= 0 )
      {
        total_investment = ( ( total_demand[ p ] - existing_equipment ) / ( invest_time ) + scrapped_equipment )
      } else {
        total_investment = scrapped_equipment * exp( a * ( ( total_demand[ p ] - existing_equipment ) / ( invest_time * scrapped_equipment ) ) )
      }
      
      cost[ p - 1, ] = cost[ p - 1, ] - premium
      share[ p, ] = exp( - alpha * cost[ p - 1, ] / 100 ) / sum( exp( - alpha * cost[ p - 1, ] / 100 ) )
      
     # print(sum(share))
      
      if( any( is.nan( share[ p, ] ) ) )
      {
        share[ p, ][ is.nan( share[ p, ] ) ] = 1e10 # if argument of exp is too large, share is NaN, replace by 1e10
      }
      
      new_investment = share[ p, ] * total_investment
      
      vintage_new[ p, ] = vintage_new[ p - 1, ] + new_investment - vintage_new[ p - 1, ] / ( lifetime / 2 )
      vintage_old[ p, ] = vintage_old[ p - 1, ] + vintage_new[ p - 1, ] / ( lifetime / 2 ) - vintage_old[ p - 1, ] / ( lifetime / 2 )
      existing_equipment = sum( vintage_new[ p, ] + vintage_old[ p, ] )
      
      demand[ p, ] = total_demand[ p ] * ( vintage_new[ p, ] + vintage_old[ p, ] ) / existing_equipment
      if( any( is.nan( demand[ p, ] ) ) )
      {
        demand[ p, ] = 0
      }
      
      #On calcule le taux de substitution qui correspond au taux de croissance de la demande avant effet efficacite technique
      substitution_rate[p,] = (demand[ p, ] / total_demand[ p ]) / (demand[ p - 1, ] / total_demand[ p - 1 ]) - 1
      
      #On applique le taux d'efficacité technique par énergie pour avoir le bonne demand et le bon total pour the following projection year
      demand[ p, ] = demand[ p, ] * ( 1 + tech_eff_rate [ p, ] )
      
      #On calcule le bon total prenant en compte efficacite technique
      total_demand[ p ] = sum( demand[ p, ] , na.rm = T)
      
    }
    
    else # simulate the system for historical year based on the calibration parameters
    {
      scrapped_equipment = sum( vintage_old[ p - 1, ] / ( lifetime / 2 ) )
      existing_equipment = sum( vintage_new[ p - 1, ] + vintage_old[ p - 1, ] )
      
      if( ( total_demand[ p ] - existing_equipment ) >= 0 )
      {
        total_investment = ( ( total_demand[ p ] - existing_equipment ) / ( invest_time ) + scrapped_equipment )
      } else {
        total_investment = scrapped_equipment * exp( a * ( ( total_demand[ p ] - existing_equipment ) / ( invest_time * scrapped_equipment ) ) )
      }
      
      cost[ p - 1, ] = cost[ p - 1, ] - premium
      share[ p, ] = exp( - alpha * cost[ p - 1, ] / 100 ) / sum( exp( - alpha * cost[ p - 1, ] / 100) )
      
      if( any( is.nan( share[ p, ] ) ) )
      {
        share[ p, ][ is.nan( share[ p, ] ) ] = 1e10 # if argument of exp is too large, share is NaN, replace by 1e10
      }
      new_investment = share[ p, ] * total_investment
      
      vintage_new[ p, ] = vintage_new[ p - 1, ] + new_investment - vintage_new[ p - 1, ] / ( lifetime / 2 )
      vintage_old[ p, ] = vintage_old[ p - 1, ] + vintage_new[ p - 1, ] / ( lifetime / 2 ) - vintage_old[ p - 1, ] / ( lifetime / 2 )
      existing_equipment = sum( vintage_new[ p, ] + vintage_old[ p, ] )
      
      demand[ p, ] = total_demand[ p ] * ( vintage_new[ p, ] + vintage_old[ p, ] ) / existing_equipment
      if( any( is.nan( demand[ p, ] ) ) )
      {
        demand[ p, ] = 0
      }
      
      substitution_rate[ p, ] = share[ p, ] / share[ p - 1, ] - 1
    }
    
  }
  
  final_output = list( "demand" = demand, "substitution_rate" = substitution_rate)
  return( final_output )
}

# calibrate the parameters of the simulation
#  input:
#   horizon: vector of time periods
#	demand: matrix of demand for each energy
#   cost: matrix of cost of each equipment - constant part
#	lifetime: vector of lifetime of equipment
#	invest_time: time before investment is active
#	simulate_system: function for system simulation
# output:
#   initial_stock: vector of initial stock of equipment
#	premium: matrix of premia of each equipment - calibrated part of the cost
#	alpha: value of the logit function coefficient
#	a: value of the coefficient for amount of new equipment when demand is less than existing equipment

calibrate_system = function( horizon, demand, cost, lifetime, invest_time, simulate_system )
{
  sum_of_squares = function( calibrated_parameters, horizon, demand, cost, lifetime, invest_time, simulate_system )
  {
    total_demand = rowSums( demand, na.rm=T)
    initial_stock_new = calibrated_parameters[ 1:ncol( demand ) ]
    initial_stock_old = calibrated_parameters[ ( ncol( demand ) + 1 ):( 2 * ncol( demand ) ) ]
    premium = calibrated_parameters[ ( 2 * ncol( demand ) + 1 ):( 3 * ncol( demand ) ) ]
    alpha = calibrated_parameters[ ( 3 * ncol( demand ) + 1 ) ]
    a = calibrated_parameters[ ( 3 * ncol( demand ) + 2 ) ] 
    
    sse = sum( ( demand - simulate_system( horizon = horizon, total_demand = total_demand, historical_demand = demand, initial_stock_new = initial_stock_new, initial_stock_old = initial_stock_old, cost = cost, premium = premium, lifetime = lifetime, invest_time = invest_time, alpha = alpha, a = a )$demand )^2 )
          #+ 10000 * ( initial_stock_new^2 + initial_stock_old^2 + premium^2 + alpha^2 + a^2 )
    return( sse )
  }
  
  min_sse = +Inf

    initial_share = runif( 1, 0, 1 )
    initial_alpha = runif( 1, 0, 2 )
    initial_premium = runif( ncol( demand ), -1000, 1000 )
    initial_point = c( demand[ 1, ] * initial_share, demand[ 1, ] * ( 1 - initial_share ), initial_premium, initial_alpha, 1 )
    lower_bounds = c( rep( 0, 2 * ncol( demand ) ), rep( -20000, ncol( demand ) ), 0, 1 - 1e-2 )
    upper_bounds = c( rep( demand[ 1, ], 2 ), rep( 20000, ncol( demand ) ), 2, 1 + 1e-2 )
    #optim_results = grad(sum_of_squares, initial_point, horizon = horizon, demand = demand, cost = cost, lifetime = lifetime, invest_time = invest_time, simulate_system = simulate_system)
    # optim_results = optim(
    #   initial_point,
    #   sum_of_squares,
    #   horizon = horizon, demand = demand, cost = cost, lifetime = lifetime, invest_time = invest_time, simulate_system = simulate_system,
    #   lower = lower_bounds, upper = upper_bounds,
    #   method = "L-BFGS-B", control = list( trace = 0 )
    # )
    optim_results = direct(
      #initial_point,
      sum_of_squares,
      horizon = horizon, demand = demand, cost = cost, lifetime = lifetime, invest_time = invest_time, simulate_system = simulate_system,
      lower = lower_bounds, upper = upper_bounds
    )
    
    if( optim_results$value < min_sse )
    {
      calibrated_parameters = optim_results$par
      min_sse = optim_results$value
    }
  
  
  return( calibrated_parameters )
}

# read the data from csv files
#  output:
#   historical_data: data frame with consumption and prices for each year
read_data = function(Proj_Assumption, Hist_Energy_Demand)
{
  fuel_consumption = Extract_Specific_Energy( Extract_Sector( Hist_Energy_Demand, "IND" ), c( "coal", "electricity", "oil", "gas" ))
  fuel_consumption = Sort_By_Country(fuel_consumption)
  fuel_consumption = fuel_consumption[ order( fuel_consumption$Energy ),]
  #fuel_consumption[,10:length(fuel_consumption)][fuel_consumption[,10:length(fuel_consumption)] == 0 ] <- 1e-10 #permet d eviter le bug de l'optimisateur quand des valeurs sont strictement egale a zero

  fuel_assumption = Extract_Specific_Energy( Extract_Sector( Proj_Assumption, "IND" ), c( "coal", "electricity", "oil", "gas", "na"))
  
  
  
  ###########ici pour changer les scenario de prix#######################
  fuel_prices = Extract_Scenario( Extract_Item(fuel_assumption, "Energy Prices with CO2"), "RES GB")
  
  
  
  
  fuel_prices = Sort_By_Country(fuel_prices)
  fuel_prices = fuel_prices[ order( fuel_prices$Energy ), ]
  
  # fuel_CO2_prices_per_tCO2 = Extract_Item( Extract_Specific_Energy( Extract_Sector( Proj_Assumption, "IND" ), "CO2"), "Energy Prices")
  # fuel_CO2_prices_per_tCO2 = Sort_By_Country(fuel_CO2_prices_per_tCO2)
  
  # #on ajoute le prix du Co2 en multipliant prix per ton CO2 par emission de Co2 de chaque energies
  # fuel_prices[fuel_prices$Energy == "gas", 10:length(fuel_prices)] = (204.1 / 85.98454) * fuel_CO2_prices_per_tCO2[, 10:length(fuel_CO2_prices_per_tCO2)] + fuel_prices[fuel_prices$Energy == "gas", 10:length(fuel_prices)]
  # fuel_prices[fuel_prices$Energy == "oil", 10:length(fuel_prices)] = (282.9 / 85.98454) * fuel_CO2_prices_per_tCO2[, 10:length(fuel_CO2_prices_per_tCO2)] + fuel_prices[fuel_prices$Energy == "oil", 10:length(fuel_prices)]
  # fuel_prices[fuel_prices$Energy == "coal", 10:length(fuel_prices)] = (345.6 / 85.98454) * fuel_CO2_prices_per_tCO2[, 10:length(fuel_CO2_prices_per_tCO2)] + fuel_prices[fuel_prices$Energy == "coal", 10:length(fuel_prices)]
  
  fuel_prices = fuel_prices[ rep( seq_len( nrow( fuel_prices ) ), nlevels( as.factor(fuel_consumption$Sub_Sector) )), ]
  fuel_prices = Sort_By_Country(fuel_prices)
  fuel_prices = fuel_prices[ order( fuel_prices$Energy ), ]
  fuel_prices$Sub_Sector = fuel_consumption$Sub_Sector
  
  
  #fuel_prices[,10:length(fuel_prices)] <- 1.2 * fuel_prices[,10:length(fuel_prices)]
  

  fuel_eff_tech = Extract_Item(fuel_assumption, "Technical efficiency")
  fuel_eff_tech = Sort_By_Country(fuel_eff_tech)
  fuel_eff_tech = fuel_eff_tech[ order( fuel_eff_tech$Energy ), ]

  fuel_eff_eco = Extract_Item(fuel_assumption, "Economic efficiency")
  fuel_eff_eco = Sort_By_Country(fuel_eff_eco)
  fuel_eff_eco = fuel_eff_eco[ rep( seq_len( nrow( fuel_eff_eco ) ), nlevels(as.factor(fuel_consumption$Energy))), ]
  fuel_eff_eco$Energy = as.factor(rep(levels(as.factor(fuel_consumption$Energy)), each = nlevels(as.factor(fuel_eff_eco$Sub_Sector))*nlevels(as.factor(fuel_eff_eco$Country)) ))

  fuel_activity = Extract_Item(fuel_assumption, "Activity Rate")
  fuel_activity = Sort_By_Country(fuel_activity)
  fuel_activity = fuel_activity[ rep( seq_len( nrow( fuel_activity ) ), nlevels(as.factor(fuel_consumption$Energy)) ), ]
  fuel_activity["Energy"] = as.factor(rep(levels(as.factor(fuel_consumption$Energy)), each = nlevels(as.factor(fuel_activity$Sub_Sector))*nlevels(as.factor(fuel_activity$Country)) ))
   
  historical_data = rbind( fuel_consumption, fuel_prices, fuel_eff_tech, fuel_eff_eco, fuel_activity )
  
  return( historical_data)
}

# process the input data to get a data frame in database format
get_melted_df = function( historical_data, sub_sector, id_item )
{
  input_data.df = melt( historical_data[ historical_data$ID_Item == id_item & historical_data$Sub_Sector == sub_sector,  ], id = c( "ID_Item","Scenario","Country","Energy","Sector","Sub_Sector","Usage","Source","Unit" ) )
  names( input_data.df ) = c( "ID_Item","Scenario","Country","Energy","Sector","Sub_Sector","Usage","Source","Unit","Years","Historical" )
  input_data.df$Years = as.numeric( gsub( "X", "", input_data.df$Years ) )
  return( input_data.df )
}


# extract data from input data frame
# returns a list of matrices and vectors to be used for calibration or system simulation
prepare_data_for_calibration = function( historical_data, sub_sector, horizon_range, projection_choice = F )
{
  if (!projection_choice)
  {
    # input data
    horizon = 1:( horizon_range[ 2 ] - horizon_range[ 1 ]) # easier to deal with horizon starting at 1, but need to be careful to scale back
    begin_col = 9 + horizon_range[ 1 ] # first 9 columns of data contain labels, data starts at 10th column
    
    energy_part <- Extract_Item(historical_data, "Energy Demand") 
    
    last_hist_year_col = min(which(is.na(colSums(energy_part[,10:length(energy_part)]))))-1 + 9
    end_col = begin_col + min( length( horizon ), last_hist_year_col - begin_col )
    
    horizon = 1:(min( length( horizon )+1, last_hist_year_col- begin_col+1))
    
    # extract consumption in a data frame
    energy_col = which( names( historical_data ) == "Energy" )
    input_demand_df = historical_data[ historical_data$ID_Item == "Energy Demand" & historical_data$Sub_Sector == sub_sector, c( energy_col, begin_col:end_col ) ]
    # name rows of the data frame with product type so that the matrix created later keeps names
    row.names( input_demand_df ) = input_demand_df$Energy
    # drop product column before transforming into a matrix
    input_demand_df = input_demand_df[ , -1 ]
    input_demand = t( as.matrix( input_demand_df ) )
    
    cost = t( as.matrix( historical_data[ historical_data$ID_Item == "Energy Prices with CO2" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    eco_eff_rate = t( as.matrix( historical_data[ historical_data$ID_Item == "Economic efficiency" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    tech_eff_rate = t( as.matrix( historical_data[ historical_data$ID_Item == "Technical efficiency" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    activity_rate = t( as.matrix( historical_data[ historical_data$ID_Item == "Activity Rate" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    
    lifetime = rep( 20, nlevels( as.factor(historical_data$Energy) ) )
    invest_time = 1
    
    data_list = list( "horizon" = horizon, "input_demand" = input_demand, "cost" = cost, "activity_rate" = activity_rate, "eco_eff_rate" = eco_eff_rate, "tech_eff_rate" = tech_eff_rate, "lifetime" = lifetime, "invest_time" = invest_time )
  }
  else
  {
    # input data
    horizon = 1:( 1 + horizon_range[ 2 ] - horizon_range[ 1 ]) # easier to deal with horizon starting at 1, but need to be careful to scale back
    begin_col = 9 + horizon_range[ 1 ] # first 9 columns of data contain labels, data starts at 10th column
    end_col = begin_col + length( horizon) - 1
    
    # extract consumption in a data frame
    energy_col = which( names( historical_data ) == "Energy" )
    input_demand_df = historical_data[ historical_data$ID_Item == "Energy Demand" & historical_data$Sub_Sector == sub_sector, c( energy_col, begin_col:end_col ) ]
    # name rows of the data frame with product type so that the matrix created later keeps names
    row.names( input_demand_df ) = input_demand_df$Energy
    # drop product column before transforming into a matrix
    input_demand_df = input_demand_df[ , -1 ]
    input_demand = t( as.matrix( input_demand_df ) )
    
    cost = t( as.matrix( historical_data[ historical_data$ID_Item == "Energy Prices with CO2" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    eco_eff_rate = t( as.matrix( historical_data[ historical_data$ID_Item == "Economic efficiency" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    tech_eff_rate = t( as.matrix( historical_data[ historical_data$ID_Item == "Technical efficiency" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    activity_rate = t( as.matrix( historical_data[ historical_data$ID_Item == "Activity Rate" & historical_data$Sub_Sector == sub_sector, begin_col:end_col ] ) )
    
    lifetime = rep( 20, nlevels( as.factor(historical_data$Energy) ) )
    invest_time = 1
    
    data_list = list( "horizon" = horizon, "input_demand" = input_demand, "cost" = cost, "activity_rate" = activity_rate, "eco_eff_rate" = eco_eff_rate, "tech_eff_rate" = tech_eff_rate, "lifetime" = lifetime, "invest_time" = invest_time )
  }
  return( data_list )
}

get_calibrated_parameters = function( historical_data, sub_sector, horizon_range, prepare_data_for_calibration, calibrate_system, simulate_system )
{
  # input data
  calibration_data_list = prepare_data_for_calibration( historical_data, sub_sector, horizon_range )
  horizon = calibration_data_list$horizon
  input_demand = calibration_data_list$input_demand
  cost = calibration_data_list$cost
  lifetime = calibration_data_list$lifetime
  invest_time = calibration_data_list$invest_time
  
  # calibrate model
  calibrated_parameters = calibrate_system( horizon, input_demand, cost, lifetime, invest_time, simulate_system )
  
  # process calibrated parameters
  initial_stock_new = calibrated_parameters[ 1:ncol( input_demand ) ]
  initial_stock_old = calibrated_parameters[ ( ncol( input_demand ) + 1 ):( 2 * ncol( input_demand ) ) ]
  premium = calibrated_parameters[ ( 2 * ncol( input_demand ) + 1 ):( 3 * ncol( input_demand ) ) ]
  alpha = calibrated_parameters[ ( 3 * ncol( input_demand ) + 1 ) ]
  a = calibrated_parameters[ ( 3 * ncol( input_demand ) + 2 ) ] 
  
  param_list = list( "horizon" = horizon_range, "initial_stock_new" = initial_stock_new, "initial_stock_old" = initial_stock_old, "premium" = premium, "alpha" = alpha, "a" = a )
  return( param_list )
}

get_simulated_demand = function( historical_data, sub_sector, calibrated_parameters, simulation_end, prepare_data_for_calibration, simulate_system, get_melted_df )
{
  horizon_range = calibrated_parameters$horizon
  initial_stock_new = calibrated_parameters$initial_stock_new
  initial_stock_old = calibrated_parameters$initial_stock_old
  premium = calibrated_parameters$premium
  alpha = calibrated_parameters$alpha
  a = calibrated_parameters$a
  
  # prepare data for larger horizon
  simulation_data_list = prepare_data_for_calibration(
    historical_data, sub_sector, c( horizon_range[ 1 ], simulation_end), projection_choice = T
  )
  sim_horizon = simulation_data_list$horizon
  input_demand = simulation_data_list$input_demand
  cost = simulation_data_list$cost
  
  activity_rate = simulation_data_list$activity_rate
  eco_eff_rate = simulation_data_list$eco_eff_rate
  tech_eff_rate = simulation_data_list$tech_eff_rate
  
  lifetime = simulation_data_list$lifetime
  invest_time = simulation_data_list$invest_time
  total_demand = rowSums( input_demand, na.rm= T)
  
  #On repart sur un point initial baséée sur le bon total de demande historique 
  initial_stock_new = initial_stock_new * ( (total_demand[1] / sum(initial_stock_new + initial_stock_old) ))
  initial_stock_old = initial_stock_old * ( (total_demand[1] / sum(initial_stock_new + initial_stock_old) ))
  
  historical_demand <-  input_demand
  
  # simulate on larger horizon
  demand = simulate_system(
    sim_horizon, total_demand, historical_demand, initial_stock_new, initial_stock_old,
    cost, activity_rate, eco_eff_rate, tech_eff_rate,  premium, lifetime, invest_time, alpha, a
  )
  
  return( demand )
}

transform_to_input_format <- function(nom_input, input, country, sub_sector, calibration_start_date, input_hist)
{
  if(nom_input == "demand")
  {
    Header_demand <- data.frame("ID_Item" = rep("Simulated Demand", 4), "Scenario" = rep("GB", 4), "Country" = rep(country, 4), "Energy" = names(input),
                             "Sector" = rep("IND", 4), "Sub_Sector" = rep(sub_sector, 4), "Usage" = rep("na", 4), "Source" = rep("AMADEUS", 4), "Unit" = rep("TWh", 4) )
    
    simulated_demand_test <- as.data.frame(t(input))
    row.names(simulated_demand_test) <- seq(1,4)
    
    if(calibration_start_date == 2000)
    {
      final_ouptut <- cbind(Header_demand, simulated_demand_test)
    }
    else if(calibration_start_date == 2001)
    {
      real_demand <-data.frame(X2000 = input_hist[,10]) 
      final_ouptut <- cbind(Header_demand, real_demand, simulated_demand_test)
    }
    else
    {
      real_demand <- input_hist[,10:(calibration_start_date + 9 - 2000)] 
      final_ouptut <- cbind(Header_demand, real_demand, simulated_demand_test)
    }
  }
  else
  {
    Header_sub <- data.frame("ID_Item" = rep("Substitution Rate", 4), "Scenario" = rep("GB", 4), "Country" = rep(country, 4), "Energy" = names(input),
                             "Sector" = rep("IND", 4), "Sub_Sector" = rep(sub_sector, 4), "Usage" = rep("na", 4), "Source" = rep("AMADEUS", 4), "Unit" = rep("%", 4) )
    
    substitution_rate_test <- as.data.frame(t(input))
    row.names(substitution_rate_test) <- seq(1,4)
    
    if(calibration_start_date == 2000)
    {
      final_ouptut <- cbind(Header_sub, substitution_rate_test)
    }
    else if(calibration_start_date == 2001)
    {
      sub_hist<- data.frame(X2000 = c(0,0,0,0)) 
      final_ouptut <- cbind(Header_sub, sub_hist, substitution_rate_test)
    }
    else
    {
      sub_hist <- input_hist[,10:(calibration_start_date + 9 - 2000)]
      sub_hist[,1:length(sub_hist)] <- 0 
      final_ouptut <- cbind(Header_sub, sub_hist, substitution_rate_test)
    }
  }
  
  return(final_ouptut)
}
