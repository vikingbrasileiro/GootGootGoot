##########################################################################
library(R6)
library(stringr)
library(lubridate)
library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(mgcv)
library(timeDate)
library(maps)
library(sp)
library(truncnorm)
  
options( lubridate.week.start = 1 )
options(scipen = 999) # disabled scn notation
options( dplyr.summarise.inform = FALSE )
  
###########################################################################################
##### parent class for Building Stock ( general with no single/multi dwelling difference )
  
  ##### Functions to project building stock, project building consumption
  ##
  
  p_initialize_model <- function( country_mapping, country, data_raw, ls_inputs_data = NULL, ls_outputs_data = NULL, projection_year = 2020 )
  {
    self$country_mapping = country_mapping
    
    self$country = country
    self$data_raw = data_raw
    self$projection_year = 2020
    self$ls_inputs_data = ls_inputs_data
    self$ls_outputs_data = ls_outputs_data
    
    self$prepare_data()
  }
  
  p_prepare_data <- function() 
  {
    self$country_zone = self$country_mapping %>%
      filter( Country == self$country)
    self$country_zone = self$country_zone$Region
    
    self$data_raw = self$data_raw %>%
      filter( ID_Item == "Efficiency", Country == self$country_zone ) %>%
      mutate( Country = self$country ) %>%
      rbind( self$data_raw )
      
    self$data_raw = self$data_raw %>% 
      filter( Country == self$country ) %>%
      gather( "Year", "Value", 11:ncol(self$data_raw) ) %>% # columns to row
      mutate( ID_Item = tolower(ID_Item), 
              Usage = tolower(Usage),
              Sub_Sector = tolower(Sub_Sector),
              Equipment = tolower(Equipment), 
              Year = as.numeric(Year), 
              Value = as.numeric( stringr::str_replace_all(Value, pattern = ",", replacement = ".") ) )
    
    self$ls_inputs_data$building_stock = self$data_raw %>% 
      filter( ID_Item == "building stock" , Usage == "space heating" ) %>%
      select(  Sector, Equipment, Energy, Unit, Year, Value)
    
    self$ls_inputs_data$final_consumption = self$data_raw %>% 
      filter( ID_Item == "final energy demand" ) %>%
      select( Sector, Usage, Equipment, Energy, Unit, Year, Value)

    self$ls_inputs_data$thermal_consumption = self$data_raw %>% 
      filter( ID_Item == "thermal energy demand" ) %>%
      select( Sector, Usage, Equipment, Energy, Unit, Year, Value)
    
    self$ls_inputs_data$efficiency_old_equipment = self$ls_inputs_data$thermal_consumption %>% 
      left_join( self$ls_inputs_data$final_consumption, by = c("Sector", "Usage", "Equipment", "Energy", "Year", "Unit") ) %>% 
      rename( thermal_energy = Value.x, final_energy = Value.y ) %>%
      mutate( efficiency = thermal_energy / final_energy ) %>%
      select( Sector, Usage, Equipment, Year, efficiency ) %>%
      mutate( building_class = "old" )
    
    
    self$ls_inputs_data$useful_floor_area = self$data_raw %>% 
      filter( stringr::str_detect(ID_Item, "useful surface") ) %>% # filter "if contains primary energy"
      select(Sector, Unit, Year, Value) %>%
      rename(floor_area = Value )
    
    self$ls_inputs_data$insulation_old = self$ls_inputs_data$thermal_consumption %>%
      left_join( self$ls_inputs_data$useful_floor_area , by = c("Sector", "Year") ) %>%
      left_join( self$ls_inputs_data$building_stock , by = c("Sector", "Equipment", "Year", "Energy")) %>%
      filter( Usage == "space heating", Sector == "RES", Equipment != "total" ) %>%
      select( -c(Unit.x, Unit.y, Unit, Usage) ) %>%
      rename( thermal_energy = Value.x, stock = Value.y ) %>%
      mutate( insulation = thermal_energy / ( stock * floor_area ) * 10^9 ) %>%
      select( -c(thermal_energy, floor_area, stock) ) %>%
      mutate( building_class = "old" )
    
    self$ls_inputs_data$insulation_old = self$ls_inputs_data$insulation_old %>%
      mutate( building_class = "old_new_flux" ) %>%
      rbind( self$ls_inputs_data$insulation_old )
    
    rows = which( self$ls_inputs_data$insulation_old$Equipment == "hp air/air" )
    rows_to_change = which( self$ls_inputs_data$insulation_old$Equipment == "hp air/water" )
    self$ls_inputs_data$insulation_old[rows_to_change, ]$insulation = self$ls_inputs_data$insulation_old[rows, ]$insulation
    
    rows_to_change = which( self$ls_inputs_data$insulation_old$Equipment == "hp hybrid" )
    self$ls_inputs_data$insulation_old[rows_to_change, ]$insulation = self$ls_inputs_data$insulation_old[rows, ]$insulation
    
    self$ls_inputs_data$insulation_policy = self$data_raw %>% 
      filter( stringr::str_detect(ID_Item, "primary energy")) %>% # filter "if contains primary energy"
      select( Building_Class = ID_Item, Sector, Usage, Unit, Year, Value) %>%
      mutate( Building_Class = case_when( stringr::str_detect(Building_Class, "new") ~ "construction" ,
                                          stringr::str_detect(Building_Class, "renovated")  ~ "renovation") ) # case_when is similar to
    
    self$ls_inputs_data$evolution_rate = self$data_raw %>% 
      filter( stringr::str_detect(ID_Item, "rate") ) %>%
      mutate( Building_Class = stringr::str_replace(ID_Item, pattern = " rate", replacement = "") ) %>%
      select( Building_Class, Sector, Unit, Year, Value)
    
    self$ls_inputs_data$market_share = self$data_raw %>%
      filter( stringr::str_detect(ID_Item, "market share") ) %>%
      mutate( Energy = case_when(Equipment == "biomass boiler" ~ "biomass and waste",
                                 Equipment == "hp hybrid" ~ "electricity",
                                 TRUE ~ Energy)) %>%
      mutate( Building_Class = Sub_Sector) %>%
      select( Building_Class, Sector, Scenario, Usage, Equipment, Energy, Unit, Year, Value)
    
    self$ls_inputs_data$efficiency_new_equipment = self$data_raw %>%
      filter( ID_Item ==  "efficiency") %>%
      tidyr::expand_grid( tibble( building_class = c( "renovation", "construction" , "old_new_flux") ) ) %>% ungroup %>%
      select( building_class, Equipment, Year, Value) %>%
      arrange( building_class, Equipment, Year ) %>%
      rename( efficiency = Value)
    
    # browser()
  }
  
  p_project_stock_RES <- function()
  {
  
    evolution_rate = self$ls_inputs_data$evolution_rate %>% 
      filter( Year >= self$projection_year, Sector == "RES") %>%
      arrange( Sector, Building_Class, Year ) %>% 
      spread( Building_Class, Value )
    
    building_stock = self$ls_inputs_data$building_stock %>%
      group_by( Sector, Year ) %>% summarise( Value = sum(Value, na.rm = T)) %>%
      mutate( total = case_when( Year >= self$projection_year ~  NA_real_ , TRUE ~ Value ) ) %>%
      mutate( construction = NA_real_, destruction = NA_real_, renovation = NA_real_ , old = total ) %>%
      select( Sector, Year , total, construction, destruction, renovation , old ) %>%
      filter( Sector == "RES")
    
    for( year in (self$projection_year):2050 )
    {
      for( sector in unique(building_stock$Sector) )
      {
        bs_row = which( building_stock$Year == year & building_stock$Sector == sector )
        er_row = which( evolution_rate$Year == year & evolution_rate$Sector == sector )
        building_stock[ bs_row , ]$construction = evolution_rate[ er_row , ]$construction * building_stock[ bs_row - 1, ]$total
        building_stock[ bs_row , ]$destruction = evolution_rate[ er_row , ]$destruction * building_stock[ bs_row -1, ]$total
        building_stock[ bs_row , ]$total = building_stock[ bs_row - 1, ]$total + building_stock[ bs_row , ]$construction - building_stock[ bs_row , ]$destruction
        building_stock[ bs_row , ]$renovation = evolution_rate[ er_row , ]$renovation * building_stock[ bs_row - 1, ]$total 
        building_stock[ bs_row , ]$old = building_stock[bs_row - 1, ]$old - building_stock[bs_row, ]$destruction - building_stock[ bs_row, ]$renovation
      }
    }
    
    building_stock = building_stock %>% gather( key = "building_class", value = "building_stock", 3:length(building_stock))
    
    return(building_stock)
  }
  
  p_old_equipment_lifetime_distribution <- function()
  {
    last_historical_year = self$ls_inputs_data$building_stock %>%
      filter( Year == self$projection_year - 1 )
    
    # browser()
    all_equipment = NULL
    for( equipment in 1:nrow(last_historical_year) ) 
    {
      value_equipment = round( last_historical_year[ equipment, ]$Value, 0 )
      
      set.seed(0)
      quantile_vector = seq( 1, 20, 1 )
      distribution_equipment = dunif( x = quantile_vector, min = 0, max = 20 )
      
      tmp_equipment = tibble( Sector = rep( last_historical_year[ equipment, ]$Sector, length(distribution_equipment) ), 
                              building_class = rep( "old", length(distribution_equipment) ),
                              construction_year = rep( self$projection_year - 1, length(distribution_equipment) ),
                              Equipment = rep( last_historical_year[ equipment, ]$Equipment, length(distribution_equipment) ),
                              Energy = rep( last_historical_year[ equipment, ]$Energy, length(distribution_equipment) ),
                              installation_year = rep(  self$projection_year - 1, length(distribution_equipment) ),
                              lifetime = seq( 1, length(distribution_equipment) ) ,  
                              no_equipment = value_equipment * distribution_equipment,
                              Year = rep( self$projection_year - 1, length(distribution_equipment) ) )
      
      all_equipment = rbind( all_equipment, tmp_equipment )
    }
    
    return(all_equipment)
  }
  
  p_new_equipment_lifetime_distribution <- function( equipment_df , sigma = 5 )
  {

    building_types = unique(equipment_df$building_class)
    construction_years = unique(equipment_df$construction_year)
    
    # browser( condition = length( construction_years) > 1 )
    
    all_equipment = NULL
    for( building_type in building_types)
    {
      for( construction_year_loop in construction_years )
      {
        value_nb_df = equipment_df %>% filter( building_type == building_type, construction_year == construction_year_loop )
        equipments = unique(value_nb_df$Equipment)
        
        for( equipment in equipments )
        {
          # browser()
          value_nb_equipment = value_nb_df %>% filter( building_type == building_type, Equipment == equipment, construction_year == construction_year_loop ) 
          value_nb_equipment =value_nb_equipment$no_equipment
          
          lifetime_equipment = self$EQUIPMENT_MAPPING %>% filter( Equipment == equipment )
          lifetime_equipment = lifetime_equipment$Lifetime
          
          energy_equipment = self$EQUIPMENT_MAPPING %>% filter( Equipment == equipment )
          energy_equipment = energy_equipment$Energy
          
          quantile_vector = seq( lifetime_equipment - sigma, lifetime_equipment + sigma, 1)
          set.seed(0)
          tmp_hist = dnorm( quantile_vector, mean = lifetime_equipment, sd = 1)
          

          # time_tmp_hist1 = Sys.time()
          tmp_equipment = tibble( Scenario = rep( unique(equipment_df$Scenario), length(tmp_hist)),
                                  Sector = rep( unique(equipment_df$Sector), length(tmp_hist) ), 
                                  building_class = rep( building_type, length(tmp_hist) ),
                                  construction_year = rep( construction_year_loop, length(tmp_hist) ),
                                  Equipment = rep( equipment, length(tmp_hist) ),
                                  Energy = rep( energy_equipment, length(tmp_hist) ),
                                  installation_year = rep( unique(equipment_df$installation_year), length(tmp_hist) ),
                                  lifetime = quantile_vector ,  
                                  no_equipment = value_nb_equipment * tmp_hist,
                                  Year = rep( unique(equipment_df$Year), length(tmp_hist) )
          )
          
          all_equipment = rbind( all_equipment, tmp_equipment )
        }
      }
    }
    
    if( equipment_df$Year[1] == 2045 )
    {
      browser()
    }

    return( all_equipment )
  }
  
  p_new_equipment_lifetime_distribution_V2 <- function( equipment_df , sigma = 5 )
  {
    lifetime_df = self$EQUIPMENT_MAPPING %>% select( -Energy ) 
    equipments = unique(lifetime_df$Equipment)  

    quantile_df = NULL
    for (equipment in equipments) {
        lifetime_equipment = ( lifetime_df %>% filter( Equipment == equipment ) )$Lifetime
        quantile_vector = t( tibble( seq( lifetime_equipment - sigma, lifetime_equipment + sigma, 1) ) )
        rownames( quantile_vector ) = equipment
        quantile_df = rbind( quantile_df, quantile_vector )
    }
    
    quantile_df = as.tibble( t( quantile_df ) )
    colnames( quantile_df ) = equipments
    quantile_df = quantile_df %>% gather( "Equipment", "lifetime", 1:length(quantile_df) )

    set.seed(0)
    quantile_df = quantile_df %>%
      group_by ( Equipment ) %>%
      mutate( distri =  dnorm( lifetime, mean = mean(lifetime), sd = 1) )

    all_equipment = quantile_df %>%
      left_join( equipment_df, by = "Equipment" ) %>%
      mutate( no_equipment = no_equipment * distri ) %>%
      select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, lifetime, no_equipment, Year)

    # browser()
    return( all_equipment )
  }

  p_project_equipment_RES <- function()
  {
    hist_equipment_stock = self$ls_inputs_data$building_stock %>%
      mutate( building_class = "total" ) %>%
      select( Sector, building_class, Equipment, Year, Value )
    
    market_share = self$ls_inputs_data$market_share %>%
      filter( Year >= self$projection_year, Sector == "RES" ) %>%
      rename( building_class = Building_Class, market_share = Value ) %>% 
      mutate( building_class = case_when( building_class == "new" ~  "construction" , 
                                          building_class == "renovated" ~  "renovation" ,
                                          TRUE ~ building_class ) ) %>%
      select( Scenario, Sector, building_class, Equipment, Year , market_share ) 
    
    
    new_reno = market_share %>% 
      left_join( self$ls_outputs_data$building_stock ) %>%
      mutate( equipment_stock = market_share * building_stock )
    
    equipment_stock = self$ls_inputs_data$building_stock %>%
      select( Sector, Year, Equipment )
    equipment_stock = equipment_stock %>% 
      tidyr::expand_grid( unique(market_share$Scenario) ) %>%
      tidyr::expand_grid( self$ls_outputs_data$building_stock$building_class ) %>%
      unique() %>% filter( Sector == "RES" )
    
    names(equipment_stock) = c( "Sector", "Year", "Equipment", "Scenario", "building_class")
    
    equipment_stock = equipment_stock %>%
      left_join( hist_equipment_stock ) %>%
      left_join( new_reno ) %>%
      mutate( Value = rowSums( cbind(.$Value, .$equipment_stock), na.rm = T ) ) %>%
      select( Scenario, Sector, Equipment, Year, building_class, Value) %>%
      spread( building_class, Value ) %>%
      mutate( old = total, renovation_destructed = 0 ) %>%
      rename( renovation_constructed = renovation )
    
    equipment_levels = unique( equipment_stock$Equipment ) 
    scenarios = unique( equipment_stock$Scenario ) 
    
    
    evolution_rate = self$ls_inputs_data$evolution_rate %>% 
      filter( Year >= self$projection_year, Sector == "RES", Building_Class == "destruction" ) %>%
      arrange( Sector, Building_Class, Year ) %>% 
      spread( Building_Class, Value )
    
    
    # browser()
    for( scenario in scenarios )
    {
      for( year in (self$projection_year):2050 )
      {
        for( equipment in equipment_levels)
        {
          bs_row = which( equipment_stock$Year == year & equipment_stock$Scenario == scenario & equipment_stock$Equipment == equipment )
          er_row = which( evolution_rate$Year == year )
          year_row = which( equipment_stock$Year == year & equipment_stock$Scenario == scenario )
          equipment_stock[ bs_row , ]$destruction = evolution_rate[ er_row , ]$destruction * equipment_stock[ bs_row -1, ]$old
          equipment_stock[ bs_row , ]$renovation_destructed = equipment_stock[bs_row - 1, ]$old / sum( equipment_stock[year_row - 1, ]$old ) * sum( equipment_stock[ year_row , ]$renovation_constructed ) 
          equipment_stock[ bs_row , ]$old = equipment_stock[bs_row - 1, ]$old - equipment_stock[bs_row, ]$destruction - equipment_stock[ bs_row, ]$renovation_destructed
          equipment_stock[ bs_row , ]$total = equipment_stock[ bs_row -1, ]$total + equipment_stock[ bs_row , ]$construction + equipment_stock[ bs_row, ]$renovation_constructed - equipment_stock[ bs_row, ]$renovation_destructed - equipment_stock[ bs_row , ]$destruction 
        }
      }
    }
  
    
    ### eQUIPMENT END OF LIFE
    equipment_stock$old_brut = NA_real_
    
    for( equipment in equipment_levels )
    {
      value_old_equipment = unique( equipment_stock[ equipment_stock$Equipment == equipment &
                                                       equipment_stock$Year == 2019 ,]$old / 20 )
      
      eff_rows = which( equipment_stock$Equipment == equipment & equipment_stock$Year > 2019 & equipment_stock$Year < 2041 )
      equipment_stock[ eff_rows , ]$old_brut = value_old_equipment
    }
    
    equipment_stock = equipment_stock %>% 
      mutate( old_already_replaced = destruction / 20 + renovation_destructed / 20,
              old_staying_to_replace = case_when( Year == 2019 ~ old, 
                                                  TRUE ~ NA_real_ ) )
    
    for( equipment in equipment_levels )
    {
      for( year in 2019:2039 )
      {
        row_n = which( equipment_stock$Equipment == equipment & equipment_stock$Year == year )
        row_nn =  which( equipment_stock$Equipment == equipment & equipment_stock$Year == year + 1 )
        
        equipment_stock[ row_nn , ]$old_staying_to_replace = max( equipment_stock[ row_n , ]$old_staying_to_replace - equipment_stock[ row_nn , ]$old_brut , 0 )
      }
    }
    
    
    equipment_stock = equipment_stock %>% 
      mutate( old_replaced = case_when( old_staying_to_replace > 0 ~ old_brut - old_already_replaced,
                                        old_staying_to_replace == 0 ~ 0, 
                                        TRUE ~NA_real_
                                         ))
    
    equipment_stock$old_replaced_cum = NA_real_
    equipment_stock[equipment_stock$Year == 2020 ,]$old_replaced_cum = equipment_stock[equipment_stock$Year == 2020 ,]$old_replaced
    
    for( year in 2021:2040 )
    {
      row_n = which( equipment_stock$Year == year - 1 )
      row_nn =  which( equipment_stock$Year == year )
      equipment_stock[ row_nn , ]$old_replaced_cum = equipment_stock[ row_nn , ]$old_replaced + equipment_stock[ row_n , ]$old_replaced_cum 
    }
    
    equipment_stock = equipment_stock %>% 
      mutate( old_true = old - replace_na( old_replaced_cum, 0 ) ) %>%
      mutate( old_true = case_when( old_true < 0 ~ 0 ,
                               TRUE ~ old_true )) %>%
      mutate( old_true = case_when( Year > 2039 ~0, 
                                    TRUE ~ old_true ))
      
    
    market_shares_construction = market_share %>%
      filter( building_class == "construction" ) %>%
      select( -building_class) %>%
      rename( market_share_construction = market_share )
    
    market_shares_renovation = market_share %>%
      filter( building_class == "renovation" ) %>%
      select( -building_class) %>%
      rename( market_share_renovation = market_share )
    
    total_old_replaced_2020_2040 = equipment_stock %>%
      mutate( old_really_replaced = case_when( old_true > 0 ~ old_replaced ,
                                               TRUE ~ 0 ) ) %>%
      group_by( Scenario, Sector ,Year ) %>%
      summarise( total_old_replaced_2020_2040 = sum( old_really_replaced ) ) 
    
    
    
    mapping_year = tibble( Year = seq( 2020 , 2030 ), 
                           new_year = seq( 2040 , 2050 ) )
      
    
    total_per_year_old_true = equipment_stock %>%
      group_by( Scenario, Sector, Year ) %>%
      summarise( total_old_true = sum( old_true, na.rm = T ) ) 
    
    market_share_old_true = equipment_stock %>%
      select( Scenario, Sector, Equipment, Year, old_true ) %>%
      left_join( total_per_year_old_true ) %>%
      mutate( market_share_old = old_true / total_old_true ) %>%
      select( -c( old_true, total_old_true ) )
  
    
    # browser() 
    equipment_stock = equipment_stock %>%
      left_join( market_shares_renovation ) %>%
      left_join( market_shares_construction ) %>%
      left_join( total_old_replaced_2020_2040 ) %>%
      left_join( market_share_old_true ) %>%
      mutate( market_shares_2020_2040 = market_share_construction + market_share_old ) %>%
      mutate( old_new_flux_2020_2040 = total_old_replaced_2020_2040 * 0.5 * market_shares_2020_2040 )
    
    # test = equipment_stock %>%
    #   group_by( Scenario, Sector, Year )%>%
    #   summarise( old = sum( old , na.rm = T ), 
    #              old_true = sum( old_true, na.rm = T ) , 
    #              old_new = sum( old_new_flux_2020_2040, na.rm = T ) ) %>%
    #   ungroup() %>%
    #   group_by( Scenario, Sector )%>%
    #   summarise( old_new_cum = cumsum( old_new ), old = old, old_true = old_true, Year = Year) %>%
    #   mutate( old_check = old_new_cum + old_true )
    
    
    # browser()
    
    total_old_replaced_2040_2050 = total_old_replaced_2020_2040 %>%
      left_join( mapping_year ) %>%
      select( -Year ) %>%
      rename( Year = new_year, total_old_replaced_2040_2050 = total_old_replaced_2020_2040 )
  
    equipment_stock = equipment_stock %>%
      left_join( total_old_replaced_2040_2050 ) %>%
      mutate( old_new_flux_2040_2050 = total_old_replaced_2040_2050 * market_share_construction,
              old_new_flux = replace_na( old_new_flux_2020_2040, 0 ) + replace_na( old_new_flux_2040_2050, 0 ) ) 
    
    construction_renovation_2040_2050 = equipment_stock %>%
      select( Scenario, Sector, Equipment, Year , construction, renovation_constructed ) %>%
      group_by( Scenario, Sector ,Year ) %>%
      summarise( total_construction_replaced_2040_2050 = sum( construction ), total_renovation_replaced_2040_2050 = sum( renovation_constructed ) ) %>%
      left_join( mapping_year ) %>%
      select( -Year ) %>%
      rename( Year = new_year )
    
    equipment_stock = equipment_stock %>%
      left_join( construction_renovation_2040_2050 ) %>%
      mutate( construction_new = total_construction_replaced_2040_2050 * market_share_construction, 
              renovation_new = total_renovation_replaced_2040_2050 * market_share_renovation, 
              construction = replace_na( construction_new , 0 ) + construction,
              renovation = replace_na( renovation_new , 0 ) + renovation_constructed )
    
    equipment_stock = equipment_stock %>%
    select( -old ) %>%
    rename( old = old_true) %>%  
    select( Scenario, Sector, Equipment, Year, construction, destruction, old, old_new_flux, renovation_constructed, renovation_destructed )
      
    equipment_stock = equipment_stock %>% gather( key = "building_class", value = "equipment_stock", 5:length(equipment_stock))
    
    return(equipment_stock)
  }
  
  p_project_equipment_RES_v2 <- function()
  {
   
    print("Stock projection initialization") 
    
    market_share = self$ls_inputs_data$market_share %>%
      filter( Year >= self$projection_year, Sector == "RES" ) %>%
      rename( building_class = Building_Class, market_share = Value ) %>% 
      mutate( building_class = case_when( building_class == "new" ~  "construction" , 
                                          building_class == "renovated" ~  "renovation" ,
                                          TRUE ~ building_class ) ) %>%
      select( Scenario, Sector, building_class, Equipment, Energy, Year , market_share ) 
    
    evolution_rate = self$ls_inputs_data$evolution_rate %>% 
      filter( Year >= self$projection_year, Sector == "RES" )

    old_bs_starting_point = self$old_equipment_lifetime_distribution() %>% 
      filter( Sector == "RES" )
    
    equipment_levels = unique( market_share$Equipment ) 
    scenarios = unique( market_share$Scenario )
  
    STOCK_EQUIPMENT <- NULL

    # bstock_total = old_bs_starting_point
    print("Start of stock projection loop") 
    
    for( scenario in scenarios )
    {
      bs_old = old_bs_starting_point
      bs_construction = NULL
      bs_renovation = NULL
      bstock_renovation_tmp = NULL
      bstock_construction_tmp = NULL
      bstock_old_tmp = NULL
      bstock_construction = NULL
      bstock_renovation = NULL 
      
      bstock_old = old_bs_starting_point %>%
        group_by(  Sector, building_class, construction_year, Equipment, Energy, installation_year,  Year ) %>%
        summarise( no_equipment = sum( no_equipment, na.rm = T ) ) %>%
        select( Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year )
      
      bstock_total = bstock_old
      
      for( year in (self$projection_year):2050 )
      {
        print( paste( scenario , ":", year ) )

        # time1 = Sys.time()

        evolution_rate_tmp = evolution_rate %>% filter( Year == year )
        market_share_tmp = market_share %>% filter( Year == year, Scenario == scenario )

        
          # 1. Calcul des flux construction renovation positive
          
          construction_tmp = ( evolution_rate_tmp %>% filter( Building_Class == "construction" ))$Value 
          renovation_tmp = ( evolution_rate_tmp %>% filter( Building_Class == "renovation" ))$Value 
          destruction_tmp = ( evolution_rate_tmp %>% filter( Building_Class == "destruction" ))$Value 
          
          market_share_construction_tmp = market_share_tmp %>% filter( building_class == "construction")
          market_share_renovation_tmp = market_share_tmp %>% filter( building_class == "renovation")
          
          total_bs_year_before = sum( (bstock_total %>% filter( Year ==  year - 1  ) )$no_equipment ) 
          total_bs_construction_tmp  =  construction_tmp * total_bs_year_before
          total_bs_renovation_tmp  =  renovation_tmp * total_bs_year_before
          total_bs_destruction_tmp  =  destruction_tmp * total_bs_year_before
          
          # bs_construction_tmp = old_bs_starting_point %>% mutate( installation_year = year, building_class = "construction", no_equipment = 0 )
          # bs_renovation_tmp = old_bs_starting_point %>% mutate( installation_year = year, building_class = "renovation", no_equipment = 0 )
          # 

          # share in old building
          
          
          share_bs_old <- bstock_old %>%
            filter( Year == year-1 ) %>%
            group_by( Sector, building_class, Equipment, Energy ) %>%
            summarise( no_equipment = sum( no_equipment , na.rm = T ) ) %>%
            ungroup() %>%
            mutate( market_share = no_equipment / sum( no_equipment, na.rm = T ) )
        
          
          #### Flux calculation
          {
          
          # browser()
        
          flux_construction_tmp <- market_share_construction_tmp %>%
            mutate( no_equipment = market_share * total_bs_construction_tmp ) %>%
            rename( installation_year = Year ) %>%
            mutate( construction_year = year, Year = year ) %>%  
            select( -market_share ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year )
          
          flux_renovation_positif_tmp <- market_share_renovation_tmp %>%
            mutate( no_equipment = market_share * total_bs_renovation_tmp ) %>%
            rename( installation_year = Year ) %>%
            mutate( construction_year = year, Year = year ) %>%  
            select( -market_share ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year )
        
          # logement renove a enlever dans le stock ancien ; flux de destruction de l'ancien lie a la renovation
          flux_renovation_negatif_tmp <- share_bs_old %>%
            mutate( no_equipment = market_share * total_bs_renovation_tmp ) %>%
            mutate( Scenario = scenario,
                    construction_year = NA_real_ , 
                    Year = year, 
                    installation_year = NA_real_ ) %>%  
            select( -market_share ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year )
          
          # flux de destruction de l'ancien dans l'ancien
          flux_destruction_tmp <- share_bs_old %>%
            mutate( no_equipment = market_share * total_bs_destruction_tmp ) %>%
            mutate( Scenario = scenario,
                    construction_year = NA_real_ , 
                    Year = year, 
                    installation_year = NA_real_ ) %>%  
            select( -market_share ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year )
          

          
          # time2 = Sys.time()
          # print( time2 - time1 )
          # associate a lifetime to all equipments in renovated and new (flux)
          ## new

          # time3 = Sys.time()
          flux_construction_tmp <- self$new_equipment_lifetime_distribution( flux_construction_tmp ) %>%
            mutate( Year = year)

          # time4 = Sys.time()
          # print( time4 - time3 )
          ## renovation
          flux_renovation_positif_tmp <- self$new_equipment_lifetime_distribution( flux_renovation_positif_tmp ) %>%
            mutate( Year = year)
          }
          
          #### Demolished and renovated buildings to substract to old stock building
          {

          
          
            
          bs_old_total <- bs_old %>% # intermediate results necessary to calculate the share of old building to be replaced BY LIFETIME
            group_by( Sector, building_class, Equipment, Energy ) %>%
            summarise( no_equipment_total = sum(no_equipment, na.rm = TRUE) )
          
          ###################### Correction bug pas assez de destruction ##########################
          value_old_remaining = sum( (bstock_old %>% filter( Year == year-1 ))$no_equipment )
          
          if( is.na( sum( flux_renovation_negatif_tmp$no_equipment ) ) ) ### Gestion quand sotck de batiment ancien est nul donc share_bs_old = NA
          {
            # browser()
            to_be_destroyed = total_bs_renovation_tmp + total_bs_destruction_tmp
            # browser()
          }
          else 
          {
            to_be_destroyed = sum( flux_renovation_negatif_tmp$no_equipment ) + sum( flux_destruction_tmp$no_equipment )
          }
          
         
          # browser()

          if( to_be_destroyed > value_old_remaining ) # gestion des annees ou le stock d'ancien est nul
          {
            
            bs_old <- bs_old %>%
              mutate( Year = year ) %>%
              mutate( no_equipment = case_when( to_be_destroyed > value_old_remaining ~ 0,
                                                TRUE ~ no_equipment ) )
          }
          else { # Gestion des annees ou il reste du stock d'ancien
            bs_old <- bs_old %>%
            filter( Year == year -1 ) %>%
            mutate(Scenario = scenario) %>%
            left_join( bs_old_total , by = c("Sector", "building_class", "Equipment", "Energy") ) %>%
            left_join( flux_destruction_tmp, by = c("Sector", "building_class", "Equipment", "Energy") ) %>%
            left_join( flux_renovation_negatif_tmp, by = c("Sector", "building_class", "Equipment", "Energy") ) %>%
            select( -c( construction_year, 
                        construction_year.y,
                        installation_year,
                        installation_year.y,
                        Year,
                        Year.x,
                        Scenario.x,
                        Scenario.y )) %>%
            rename( flux_renovation = no_equipment,
                    flux_destruction = no_equipment.y,
                    no_equipment = no_equipment.x,
                    Year = Year.y,
                    construction_year = construction_year.x,
                    installation_year = installation_year.x,
                    ) %>%
            mutate( market_share = no_equipment / no_equipment_total, #market share pour un equipement pour chaque lifetime
                    flux_destruction = flux_destruction * market_share, 
                    flux_renovation = flux_renovation * market_share,
                    no_equipment = no_equipment - flux_destruction - flux_renovation, # Update du stock d'ancien
                    no_equipment = case_when( is.na( no_equipment ) ~ 0, TRUE ~ no_equipment )
                    )  %>%  
            select( -c( market_share, flux_destruction, flux_renovation, no_equipment_total ) ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, lifetime, no_equipment, Year )
          }
         
          # Calculer equipement en fin de vie dans l'ancien 
          # Appliquer le Alpha pour voir quel equipement reste dans le stock
          
          
          bs_old <- bs_old %>%  # enlever une annee de lifetime a tous les equipements 
            mutate( lifetime = lifetime - 1 )
          
          flux_obsol_total = bs_old %>% # deduire le flux d'equipement en fin de vie
            filter( lifetime == 0 )
          flux_obsol_total = sum( flux_obsol_total$no_equipment, na.rm = TRUE )
          
          bs_obsol <- bs_old %>% # equipements qui sortent ; ceux qui ont plus de lifetime
            filter( lifetime == 0 )
          
          # print( paste( "Flux_old_EOF_negative:", sum( bs_obsol$no_equipment ) ) )
          
          
          bs_old <- bs_old %>% # enlever ces equipements qui sortent du stock old
            filter( lifetime != 0 )
        
          # appliquer alpha (part des equipements dans l'ancien qui reste )
          
          # browser()  
          
          flux_obsol_alpha <- bs_obsol %>%
            left_join( self$EQUIPMENT_ALPHA, by = "Equipment") %>%
            mutate( no_equipment = alpha * no_equipment ) %>%
            select( -c( lifetime, alpha ) ) %>%
            mutate( installation_year = year )
          
          noEquipmentNotSubstitute = sum( flux_obsol_alpha$no_equipment ) # Number of equipment that will not changed of heating technologies when reaching its lifetime
          flux_obsol_minus_alpha <- market_share_construction_tmp %>%
            mutate( no_equipment = market_share * ( flux_obsol_total - noEquipmentNotSubstitute ) ) %>%
            mutate( building_class = "old",
                    installation_year = year, 
                    construction_year = self$projection_year -1 ) %>% # Ancien donc meme annee de construction tout le long
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
          
          flux_obsol <- flux_obsol_alpha %>%
            rbind( flux_obsol_minus_alpha ) %>%
            group_by(Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, Year) %>%
            summarise(no_equipment = sum(no_equipment, na.rm = TRUE) ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
            # 
          
          
          # appliquer function lifetime apres installation

          if( to_be_destroyed > value_old_remaining )
          {
            flux_obsol <- NULL
          }
          else{
            flux_obsol <- self$new_equipment_lifetime_distribution( flux_obsol )
          }
          
          
          # print( paste( "Flux_old_EOF_positive:", sum( flux_obsol$no_equipment ) ) )
          
          # rajouter le flux d'equipement qui remplacent les equipements anciens en fin de vie dans le stock ancien
          bs_old <- rbind(bs_old, flux_obsol)
          
          }
          
          
        
          # 5. Calculer equipement en fin de vie dans construction et renovation
          {
    
            
            if(!is.null(bs_construction)){
            bs_construction <- bs_construction %>% # take out lifetime
              mutate(lifetime = lifetime-1)
            }
        
            if(!is.null(bs_renovation)){
              bs_renovation <- bs_renovation %>%  # take out lifetime
                mutate(lifetime = lifetime-1)
            }
        
            ################################## Construction reaching lifetime ###################################################
          
          to_be_destroyed_in_new_and_reno = to_be_destroyed - value_old_remaining # in new and renovated

          if( to_be_destroyed_in_new_and_reno > 0 ) # Gestion des annees ou le sotck d'ancien est nul, on doit donc détuire du rénové et du neuf
          {
            # print( paste( "Stock contruction avant destruction:", sum( bs_construction$no_equipment ) ) )
            # print( paste( "Stock renovation avant destruction:", sum( bs_renovation$no_equipment ) ) )
            # print( paste( "Logement à détruire dans le pas ancien:", to_be_destroyed_in_new_and_reno ) )

            # browser()  
            new_total = sum( bs_construction$no_equipment )
            reno_total = sum( bs_renovation$no_equipment )

            to_be_destroyed_in_new = to_be_destroyed_in_new_and_reno * new_total / ( new_total + reno_total )
            to_be_destroyed_in_reno = to_be_destroyed_in_new_and_reno - to_be_destroyed_in_new

            bs_construction <- bs_construction %>% ungroup() %>%
              mutate( ms_new = case_when( no_equipment == 0 ~ 0,
                                          TRUE ~ no_equipment/sum( no_equipment, na.rm = TRUE ) ) ) %>%# share of destruction for each type of equipment uniformaly spread out
              mutate( no_equipment = no_equipment - ms_new * to_be_destroyed_in_new ) %>% # destruction du bon nombre dequipment
              select( -c( ms_new ) )

            bs_renovation <- bs_renovation %>% ungroup() %>%
              mutate( ms_reno = case_when( no_equipment == 0 ~ 0,
                                           TRUE ~ no_equipment/sum( no_equipment, na.rm = TRUE ) ) ) %>%# share of destruction for each type of equipment uniformaly spread out
              mutate( no_equipment = no_equipment - ms_reno * to_be_destroyed_in_reno ) %>%
              select( -c( ms_reno ) )

            # print( paste( "Stock contruction après destruction:", sum( bs_construction$no_equipment ) ) )
            # print( paste( "Stock renovation après destruction:", sum( bs_renovation$no_equipment ) ) )
          }

          bs_construction <-rbind(bs_construction, flux_construction_tmp) %>% mutate( Year = year )
          bs_renovation <- rbind(bs_renovation, flux_renovation_positif_tmp) %>% mutate( Year = year )
          
     
          flux_obsol_total = bs_construction %>% # deduire le flux d'equipement en fin de vie
            filter( lifetime == 0 ) %>%
            group_by( construction_year ) %>%
            summarise( no_equipment_total = sum( no_equipment , na.rm = T ) ) 
          
          bs_obsol <- bs_construction %>% # equipement qui sortent ; ceux qui ont plus de lifetime
            filter( lifetime == 0 )
          
          # print( paste( "flux obsol before alpha:", sum( bs_obsol$no_equipment ) ) )

          # print( paste( "bs_construction before negative flux:", sum( bs_construction$no_equipment ) ) )

          bs_construction <- bs_construction %>% # enlever ces equipements qui sortent du stock construction
            filter( lifetime != 0 )
          
          # print( paste( "bs_construction after negative flux:", sum( bs_construction$no_equipment ) ) )
          # appliquer alpha ; inertie lors du changement d'un equipement
          
          if( nrow(bs_obsol) > 0 ) #On applique alpha uniqement quand bs_obsol n'est pas vide
          {
            flux_obsol_alpha <- bs_obsol %>%
              left_join( self$EQUIPMENT_ALPHA, by = "Equipment" ) %>%
              mutate( no_equipment = alpha * no_equipment ) %>%
              mutate( installation_year = year ) %>%
              select( -c( lifetime, alpha ) )
            
            # print( paste( "bs_renovation after positive alpha flux:", sum( flux_obsol_alpha$no_equipment ) ) )

            # Calculatino of equipment that will be subsitute with other equipment that the one reaching its end of life
            flux_obsol_total = flux_obsol_alpha %>%
              group_by( construction_year ) %>%
              summarise( no_equipment = sum ( no_equipment ) ) %>%
              left_join( flux_obsol_total, by = "construction_year" ) %>%
              mutate( no_equipment_total = no_equipment_total - no_equipment ) %>%
              select( -no_equipment )

             # Number of equipment that will not changed of heating technologies when reaching its lifetime
            flux_obsol_minus_alpha <- market_share_construction_tmp %>%
              merge( flux_obsol_total %>% select( construction_year ) ) %>%
              left_join( flux_obsol_total, by = "construction_year" ) %>%
              mutate( no_equipment = market_share * no_equipment_total ) %>%
              mutate( building_class = "construction",
                      installation_year = year ) %>%
              select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
            
            # print( paste( "bs_renovation after positive minus alpha flux:", sum( flux_obsol_minus_alpha$no_equipment ) ) )
            
            flux_obsol <- flux_obsol_alpha %>%
              rbind( flux_obsol_minus_alpha ) %>%
              group_by( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, Year) %>%
              summarise( no_equipment = sum(no_equipment, na.rm = TRUE) ) %>%
              select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
              
            # appliquer function lifetime apres installation
            # print( paste( "flux before lifetime:", sum( flux_obsol$no_equipment ) ) ) 
            # time5 =Sys.time()

            flux_obsol <- self$new_equipment_lifetime_distribution( flux_obsol )
            # print( paste( "flux after lifetime:", sum( flux_obsol$no_equipment ) ) )
            # time6 =Sys.time()
            # print( paste( "all lifetime function:", time6 - time5 ) )

          }
          else
          {
            flux_obsol <- NULL
          }
          
          # print( paste( "Flux_construction_EOF_positive:", sum( flux_obsol$no_equipment ) ) )
          
          # rajouter le flux d'equipement qui remplacent les equipements anciens en fin de vie dans le stock neuf et renove
          bs_construction <- rbind(bs_construction, flux_obsol)

      ################################## Renovation reaching lifetime ###################################################
          
          flux_obsol_total = bs_renovation %>% # deduire le flux d'equipement en fin de vie
            filter( lifetime == 0 ) %>%
            group_by( construction_year ) %>%
            summarise( no_equipment_total = sum( no_equipment , na.rm = T ) ) 
          
          
          bs_obsol <- bs_renovation %>% # equipement qui sortent ; ceux qui ont plus de lifetime
            filter( lifetime == 0 )
          
          # print( paste( "Flux_renovation_EOF_negative:", sum( bs_obsol$no_equipment ) ) )
          # 
          # print( paste( "bs_renovation before negative flux:", sum( bs_renovation$no_equipment ) ) )
          # 
          bs_renovation <- bs_renovation %>% # enlever ces equipements qui sortent du stock renovation
            filter( lifetime != 0 )
          
          # print( paste( "bs_renovation after negative flux:", sum( bs_renovation$no_equipment ) ) )
          # 
          # appliquer alpha ; inertie lors du changement d'un equipement
          
          if( nrow(bs_obsol) > 0 ) #On applique alpha uniqement quand bs_obsol n'est pas vide
          {
            
            # browser()
            
            flux_obsol_alpha <- bs_obsol %>%
              left_join( self$EQUIPMENT_ALPHA, by = "Equipment" ) %>%
              mutate( no_equipment = alpha * no_equipment ) %>%
              mutate( installation_year = year ) %>%
              select(- c( lifetime, alpha ) )
            
            # print( paste( "bs_renovation after positive alpha flux:", sum( flux_obsol_alpha$no_equipment ) ) )

            flux_obsol_total = flux_obsol_alpha %>%
              group_by( construction_year ) %>%
              summarise( no_equipment = sum ( no_equipment ) ) %>%
              left_join( flux_obsol_total, by = "construction_year" ) %>%
              mutate( no_equipment_total = no_equipment_total - no_equipment ) %>%
              select( -no_equipment )

            flux_obsol_minus_alpha <- market_share_construction_tmp %>%
              merge( flux_obsol_total %>% select( construction_year ) ) %>%
              left_join( flux_obsol_total, by = "construction_year" ) %>%
              mutate( no_equipment = market_share * no_equipment_total ) %>%
              mutate( building_class = "renovation",
                      installation_year = year ) %>%
              select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)

            # print( paste( "bs_renovation after positive minus alpha flux:", sum( flux_obsol_minus_alpha$no_equipment  , na.rm = T) ) )
            
            flux_obsol <- flux_obsol_alpha %>%
              rbind( flux_obsol_minus_alpha ) %>%
              group_by( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, Year) %>%
              summarise( no_equipment = sum(no_equipment, na.rm = TRUE) ) %>%
              select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
            
            
            flux_obsol <- self$new_equipment_lifetime_distribution( flux_obsol )
          }
          else
          {
            flux_obsol <- NULL
          }

          # print( paste( "Flux_renovation_EOF_positive before lifetime:", sum( flux_obsol$no_equipment ) ) )
          
          #Appliquer function lifetime apres installation
          
          
          # print( paste( "Flux_renovation_EOF_positive after lifetime:", sum( flux_obsol$no_equipment ) ) )
          
          # rajouter le flux d'equipement qui remplacent les equipements anciens en fin de vie dans le stock neuf et renove
          bs_renovation <- rbind(bs_renovation, flux_obsol)
          }

          ######################## Dataframe avec tous les Flux dans un stock
        
          ############ Old ##########
          bstock_old_tmp = bs_old %>%
            group_by( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, Year ) %>%
            summarise( no_equipment = sum( no_equipment, na.rm = TRUE ) ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year )
          
          bstock_old = bstock_old %>%
            mutate( Scenario = scenario ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year ) %>%
            rbind( bstock_old_tmp )
          
          ############ Construction ##########
          bstock_construction_tmp <- bs_construction %>%
            group_by( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, Year ) %>%
            summarise( no_equipment = sum( no_equipment, na.rm = TRUE ) ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
          
          bstock_construction = rbind( bstock_construction, bstock_construction_tmp )
          
          ############ Renovation ##########
          bstock_renovation_tmp <- bs_renovation %>%
            group_by( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, Year ) %>%
            summarise( no_equipment = sum( no_equipment, na.rm = TRUE ) ) %>%
            select( Scenario, Sector, building_class, construction_year, Equipment, Energy, installation_year, no_equipment, Year)
          
          bstock_renovation = rbind( bstock_renovation, bstock_renovation_tmp )
          
          bstock_total <- rbind(
            bstock_old,
            bstock_construction,
            bstock_renovation
          )
          
          # time7 =Sys.time()
          # print( time7 - time1 )
      }
      STOCK_EQUIPMENT = rbind( STOCK_EQUIPMENT, bstock_total )
    }
    
    return(STOCK_EQUIPMENT)
  
  }
    
  p_project_efficiency_main_RES <- function()
  {
    #####################  Data preparation ##########################
    
    # browser()
    
    # 1. Insualtion of construction and renovaton
    insulation_new = self$ls_inputs_data$insulation_policy %>%
      filter( Sector == "RES", 
              Year > 2019 ) %>%
      rename( building_class = Building_Class, 
              construction_year = Year, 
              insulation_new  = Value )  %>%
      select( Sector, building_class, construction_year, insulation_new )
    
    
    insulation_old = self$ls_inputs_data$insulation_old %>%
      mutate( building_class = "old", 
              Sector = "RES" )  %>%
      rename( construction_year = Year,
              insulation_old = insulation ) %>%
      select( Sector, Equipment, building_class, construction_year, insulation_old ) %>%
      unique( )
    
    # useful floor area
    floor_area = self$ls_inputs_data$useful_floor_area %>%
      filter( Sector == "RES" ) %>%
      select( Year, floor_area ) %>%
      rename( construction_year = Year )
    
    
    # efficiency of old equipment
    efficiency_old_equipment = self$ls_inputs_data$efficiency_old_equipment  %>%
      filter( Usage == "space heating",
              Equipment != "total",
              Sector == "RES",
              Year < 2020) %>%
      mutate( efficiency = case_when( is.na(efficiency) ~ 0,
                                      TRUE ~ efficiency )) %>%
      rename( installation_year = Year ) %>%
      select( Sector, Equipment, installation_year, efficiency )
    
    efficiency_new_equipment = self$ls_inputs_data$efficiency_new_equipment %>%
      mutate( Sector = "RES" ) %>%
      filter( building_class == "construction",
              Year > 2019 ) %>%
      rename( installation_year = Year ) %>%
      select( Sector, Equipment, installation_year, efficiency )
    
    efficiency_df = efficiency_old_equipment %>%
      rbind( efficiency_new_equipment ) %>%
      arrange( Equipment, installation_year )
    
    ############## Need to extend insulation and efficiency vlaue for old building ###########################
    
    equipments = unique( insulation_old$Equipment )
    scenarios = "BT" ### Need to be changed when the excel input files will be updated
    
    for( scenario in scenarios )
    {
      for( equipment in equipments)
      {
        value_efficiency = unique( efficiency_df[ efficiency_df$Equipment == equipment &
                                                    efficiency_df$installation_year == "2015",]$efficiency )
        
        eff_rows = which( efficiency_df$Equipment == equipment & 
                            efficiency_df$installation_year > 2015 &
                            efficiency_df$installation_year < 2020 )
        
        efficiency_df[ eff_rows, ]$efficiency = value_efficiency
        
        
        value_insulation = unique( insulation_old[ insulation_old$Equipment == equipment &
                                                     insulation_old$construction_year == "2015",]$insulation_old )
        
        insulation_rows = which( insulation_old$Equipment == equipment & 
                                   insulation_old$construction_year > 2015 )
        
        insulation_old[ insulation_rows, ]$insulation_old = value_insulation
      }
    }

    equipment_stock = self$ls_outputs_data$equipment_stock
    equipment_stock = equipment_stock %>%
      left_join( efficiency_df , by =  c( "Sector", "Equipment", "installation_year" ) ) %>%
      left_join( insulation_new , by = c( "Sector", "building_class", "construction_year" ) ) %>%
      left_join( insulation_old , by = c( "Sector", "Equipment", "building_class", "construction_year") ) %>%
      left_join( floor_area, by = "construction_year") %>%
      left_join( self$CEP_MAPPING, by = "Energy" )

    # convert insulation for new building from primary energy to thermal one
    equipment_stock = equipment_stock %>%
      mutate( insulation_new = insulation_new /  CEP * efficiency ) %>%
      select( -CEP )

    return(equipment_stock)
  }
  
  p_project_efficiency_backup_RES <- function()
  {
    #####################  Data preparation ##########################
    
    # 1. Insualtion of construction and renovaton
    insulation_new = self$ls_inputs_data$insulation_policy %>%
      filter( Sector == "RES", 
              Year > 2019 ) %>%
      rename( building_class = Building_Class, 
              construction_year = Year, 
              insulation_new  = Value )  %>%
      select( Sector, building_class, construction_year, insulation_new )
    
    
    insulation_old = self$ls_inputs_data$insulation_old %>%
      mutate( building_class = "old", 
              Sector = "RES" )  %>%
      rename( construction_year = Year,
              insulation_old = insulation ) %>%
      select( Sector, Equipment, building_class, construction_year, insulation_old ) %>%
      unique( )
    
    # useful floor area
    floor_area = self$ls_inputs_data$useful_floor_area %>%
      filter( Sector == "RES" ) %>%
      select( Year, floor_area ) %>%
      rename( construction_year =  Year )
    
    
    # efficiency of old equipment
    efficiency_old_equipment = self$ls_inputs_data$efficiency_old_equipment  %>%
      filter( Usage == "space heating",
              Equipment != "total",
              Sector == "RES",
              Year < 2020) %>%
      mutate( efficiency = case_when( is.na(efficiency) ~ 0,
                                      TRUE ~ efficiency )) %>%
      rename( installation_year = Year ) %>%
      select( Sector, Equipment, installation_year, efficiency )
    
    efficiency_new_equipment = self$ls_inputs_data$efficiency_new_equipment %>%
      mutate( Sector = "RES" ) %>%
      filter( building_class == "construction",
              Year > 2019 ) %>%
      rename( installation_year = Year ) %>%
      select( Sector, Equipment, installation_year, efficiency )
    
    efficiency_df = efficiency_old_equipment %>%
      rbind( efficiency_new_equipment ) %>%
      arrange( Equipment, installation_year )
    
    ############## Need to extend insulation and efficiency vlaue for old building ###########################
    
    equipments = unique( insulation_old$Equipment )
    scenarios = "BT" ### Need to be changed when the excel input files will be updated
    
    for( scenario in scenarios )
    {
      for( equipment in equipments)
      {
        value_efficiency = unique( efficiency_df[ efficiency_df$Equipment == equipment &
                                                    efficiency_df$installation_year == "2015",]$efficiency )
        
        eff_rows = which( efficiency_df$Equipment == equipment & 
                            efficiency_df$installation_year > 2015 &
                            efficiency_df$installation_year < 2020 )
        
        efficiency_df[ eff_rows, ]$efficiency = value_efficiency
        
        
        value_insulation = unique( insulation_old[ insulation_old$Equipment == equipment &
                                                     insulation_old$construction_year == "2015",]$insulation_old )
        
        insulation_rows = which( insulation_old$Equipment == equipment & 
                                   insulation_old$construction_year > 2015 )
        
        insulation_old[ insulation_rows, ]$insulation_old = value_insulation
      }
    }
    
    equipment_stock = self$ls_outputs_data$equipment_stock %>%
      left_join( self$BACKUP_MAPPING, by = "Equipment" ) %>%
      rename( Equipment_main = Equipment,
              Equipment = Backup ) %>% ungroup() %>%
      select( -c( Backup_share, Energy ) ) %>%
      left_join( self$EQUIPMENT_MAPPING, by = "Equipment" ) %>%
      select( -Lifetime )
    
    
    equipment_stock = equipment_stock %>%
      left_join( efficiency_df , by =  c( "Sector", "Equipment", "installation_year" ) ) %>%
      left_join( insulation_new , by = c( "Sector", "building_class", "construction_year" ) ) %>%
      left_join( insulation_old , by = c( "Sector", "Equipment", "building_class", "construction_year") ) %>%
      left_join( floor_area, by = "construction_year") %>%
      rename( Backup = Equipment,
              Equipment = Equipment_main ) %>%  ungroup() %>%
      mutate( efficiency = replace_na( efficiency, 0 ) ) %>%
      left_join( self$CEP_MAPPING, by = "Energy" )

    # convert insulation for new building from primary energy to thermal one
    equipment_stock = equipment_stock %>%
      mutate( insulation_new = insulation_new /  CEP * efficiency ) %>%
      select( -CEP )
    
    return(equipment_stock)
  }
  
  p_project_consumption_main_RES <- function()
  {
    consumption_stock = self$ls_outputs_data$efficiency_main_stock %>%
      left_join( self$BACKUP_MAPPING, by = "Equipment" ) %>%
      select( -Backup ) %>%
      mutate( main_share = 1 - Backup_share ) %>%
      select( -Backup_share ) 
  
    consumption_stock = consumption_stock %>%
      mutate( consumption_thermal = ( replace_na( insulation_old, 0 ) + replace_na( insulation_new, 0 ) ) * no_equipment * floor_area / 10^9, ## en TWh thermal
              consumption_final_main = case_when( efficiency == 0 ~ 0,
                                                  TRUE ~ main_share * consumption_thermal / efficiency ) ) %>% ### Imporatnt de bien multiplier par la part de chaleur l'equipement principale 
      mutate( consumption_thermal = main_share * consumption_thermal ) %>% # on recalcule le bon volume assure par le main equipmenet                                        
      select( -c( no_equipment, insulation_new, insulation_old, main_share, efficiency, floor_area ) )
    
    return(consumption_stock)
  }  
  
  p_project_consumption_backup_RES <- function()
  {
    consumption_stock = self$ls_outputs_data$efficiency_backup_stock %>%
      left_join( self$BACKUP_MAPPING, by = c( "Equipment", "Backup" ) ) %>%
      rename( backup_share = Backup_share ) 
    
    consumption_stock = consumption_stock %>%
      mutate( consumption_thermal = ( replace_na( insulation_old, 0 ) + replace_na( insulation_new, 0 ) ) * no_equipment * floor_area / 10^9, ## en TWh thermal
              consumption_final_backup = case_when( efficiency == 0 ~ 0,
                                                  TRUE ~ backup_share * consumption_thermal / efficiency ) ) %>% ### Imporatnt de bien multiplier par la part de chaleur l'equipement backup 
      mutate( consumption_thermal = backup_share * consumption_thermal ) %>% # on recalcule le bon volume assure par le main equipmenet
      select( -c( no_equipment, insulation_new, insulation_old, backup_share, efficiency, floor_area ) ) 
    
    return(consumption_stock)
  }  
  
  p_project_KPI <- function()
  {
    ########## Will calculate all the KPI based on raw stock and consumption results
    stock_df = self$ls_outputs_data$equipment_stock
    efficiency_main_df = self$ls_outputs_data$efficiency_main_stock
    consumption_main_df = self$ls_outputs_data$consumption_main_stock
    consumption_backup_df = self$ls_outputs_data$consumption_backup_stock
    
    # browser()
    #1. final consumption per energy
    final_consumption_per_energy_tmp = consumption_backup_df %>% 
      group_by( Scenario, Sector, Energy, Year ) %>%
      summarise( final_consumption = sum( consumption_final_backup ) )

    final_consumption_per_energy = consumption_main_df %>% 
      group_by( Scenario, Sector, Energy, Year ) %>%
      summarise( final_consumption = sum( consumption_final_main ) ) %>% ungroup() %>%
      rbind( final_consumption_per_energy_tmp ) %>%
      group_by( Scenario, Sector, Energy, Year ) %>%
      summarise( final_consumption = sum( final_consumption ) ) %>%
      filter( !(is.na(Energy)) )

    #2. final consumption per energy and per building class
    final_consumption_per_equipment_tmp = consumption_backup_df %>% 
      filter( !is.na( Backup ) ) %>%
      mutate( Equipment = paste( Equipment, "backup" ) ) %>%
      group_by( Scenario, Sector, Equipment, Year ) %>%
      summarise( final_consumption = sum( consumption_final_backup ) )

    final_consumption_per_equipment = consumption_main_df %>% 
      group_by( Scenario, Sector, Equipment, Year ) %>%
      summarise( final_consumption = sum( consumption_final_main ) ) %>%
      rbind( final_consumption_per_equipment_tmp ) %>%
      arrange( Scenario, Sector, Year, Equipment )

    #3. Stock per buiding class
    stock_per_bc = stock_df %>% 
      group_by( Scenario, Sector, building_class, Year ) %>%
      summarise( no_equipment = sum( no_equipment ) )

    #4. Stock per heating equipment 
    stock_per_equipment = stock_df %>% 
      group_by( Scenario, Sector, Equipment, Year ) %>%
      summarise( no_equipment = sum( no_equipment ) )

    #5. Thermal consumption per building class
    thermal_consumption_per_bc_tmp = consumption_backup_df %>% 
      group_by( Scenario, Sector, building_class, Year ) %>%
      summarise( thermal_consumption = sum( consumption_thermal ) )

    thermal_consumption_per_bc = consumption_main_df %>% 
      group_by( Scenario, Sector, building_class, Year ) %>%
      summarise( thermal_consumption = sum( consumption_thermal ) ) %>%
      rbind( thermal_consumption_per_bc_tmp ) %>%
      group_by( Scenario, Sector, building_class, Year ) %>%
      summarise( thermal_consumption = sum( thermal_consumption ) ) %>%
      arrange( Scenario, Sector, Year, building_class )

    #6. Thermal consumption per energy
    thermal_consumption_per_energy_tmp = consumption_backup_df %>% 
      group_by( Scenario, Sector, Energy, Year ) %>%
      summarise( thermal_consumption = sum( consumption_thermal ) )

    thermal_consumption_per_energy = consumption_main_df %>% 
      group_by( Scenario, Sector, Equipment, Year ) %>%
      summarise( thermal_consumption = sum( consumption_thermal ) ) %>%
      rbind( thermal_consumption_per_energy_tmp ) %>%
      group_by( Scenario, Sector, Energy, Year ) %>%
      summarise( thermal_consumption = sum( thermal_consumption ) ) %>%
      arrange( Scenario, Sector, Year, Energy )

    #7. Thermal consumption per building equipment
    thermal_consumption_per_equipment_tmp = consumption_backup_df %>% 
      filter( !is.na( Backup ) ) %>%
      mutate( Equipment = paste( Equipment, "backup" ) ) %>%
      group_by( Scenario, Sector, Equipment, Year ) %>%
      summarise( thermal_consumption = sum( consumption_thermal ) )

    thermal_consumption_per_equipment = consumption_main_df %>% 
      group_by( Scenario, Sector, Equipment, Year ) %>%
      summarise( thermal_consumption = sum( consumption_thermal ) ) %>%
      rbind( thermal_consumption_per_equipment_tmp ) %>%
      arrange( Scenario, Sector, Year, Equipment )
    
    ## Needed to calculate insulation
    # floor_area = self$ls_inputs_data$useful_floor_area %>%
    #   filter( Sector == "RES" ) %>%
    #   select( Year, floor_area ) %>%
    #   rename( contruction_year  = Year )
    # browser()
    # average thermal insulation per building class
    total_floor_area_per_bc = efficiency_main_df %>%
      mutate( total_floor_area = floor_area * no_equipment ) %>%
      group_by( Scenario, building_class, Year ) %>%
      summarise( total_floor_area = sum( total_floor_area) ) %>% ungroup()

    insulation_per_bc =  thermal_consumption_per_bc %>%
      left_join( total_floor_area_per_bc, by = c("Scenario", "building_class", "Year") ) %>%
      mutate( average_insulation = thermal_consumption / total_floor_area * 10^9 ) 

    ls_KPI = list( 
      "stock_per_bc" = stock_per_bc,
      "stock_per_equipment" = stock_per_equipment,
      "final_consumption_per_energy" = final_consumption_per_energy,
      "final_consumption_per_equipment" = final_consumption_per_equipment,
      "thermal_consumption_per_bc" = thermal_consumption_per_bc,
      "thermal_consumption_per_energy" = thermal_consumption_per_energy,
      "thermal_consumption_per_equipment" = thermal_consumption_per_equipment,
      "total_floor_area_per_bc" = total_floor_area_per_bc,
      "insulation_per_bc" = insulation_per_bc
    )

      
    return( ls_KPI )
  }
  
  p_export_to_AMADEUS <- function()
  {
    thermal_consumption_per_equipment = self$ls_outputs_data$KPI$thermal_consumption_per_equipment
    final_consumption_per_equipment = self$ls_outputs_data$KPI$final_consumption_per_equipment
    stock_per_equipment = self$ls_outputs_data$KPI$stock_per_equipment

    backup_energy = tibble( Equipment = c( "hp air/air backup", "hp air/water backup", "hp hybrid backup"),
                            Energy_backup =  c( "electricity", "electricity", "gas") )

    data_df = NULL

    thermal_consumption_per_equipment = thermal_consumption_per_equipment %>%
      mutate( ID_Item = "Heating Demand",
              Usage = "space heating",
              Sub_Sector = "total",
              Source = "AMADEUS",
              Unit = "TWh thermal",
              Country = self$country ) %>%
      left_join( self$EQUIPMENT_MAPPING, by = "Equipment" ) %>%
      left_join( backup_energy, by = "Equipment" ) %>%
      mutate( Energy = paste0( replace_na(  Energy, ""), replace_na( Energy_backup, "") ) ) %>% # adding energy of backup (not present in equipment mapping )
      select( ID_Item, Scenario, Country, Sector, Sub_Sector, Usage, Equipment, Energy, Source, Unit, Year, thermal_consumption )

    final_consumption_per_equipment = final_consumption_per_equipment %>%
      mutate( ID_Item = "Final Energy Demand",
              Usage = "space heating",
              Sub_Sector = "total",
              Source = "AMADEUS",
              Unit = "TWh",
              Country = self$country ) %>%
      left_join( self$EQUIPMENT_MAPPING, by = "Equipment" ) %>%
      left_join( backup_energy, by = "Equipment" ) %>%
      mutate( Energy = paste0( replace_na(  Energy, ""), replace_na( Energy_backup, "") ) ) %>% # adding energy of backup (not present in equipment mapping )
      select( ID_Item, Scenario, Country, Sector, Sub_Sector, Usage, Equipment, Energy, Source, Unit, Year, final_consumption ) 
    
    average_efficiency = final_consumption_per_equipment %>%
      left_join( thermal_consumption_per_equipment, by = c( "Scenario", "Country", "Sector", "Sub_Sector", "Usage", "Equipment", "Energy", "Source", "Year" )  ) %>%
      mutate( average_efficiency = replace_na( thermal_consumption / final_consumption, 0 ) , 
              ID_Item = "Average efficiency",
              Usage = "space heating",
              Sub_Sector = "total",
              Source = "AMADEUS",
              Unit = "%") %>%
      select( ID_Item, Scenario, Country, Sector, Sub_Sector, Usage, Equipment, Energy, Source, Unit, Year, average_efficiency )    

    stock_per_equipment = stock_per_equipment %>%
      mutate( ID_Item = "Number of buildings",
              Usage = "space heating",
              Sub_Sector = "total",
              Source = "AMADEUS",
              Unit = "buildings",
              Country = self$country ) %>%
      left_join( self$EQUIPMENT_MAPPING, by = "Equipment" ) %>%
      select( ID_Item, Scenario, Country, Sector, Sub_Sector, Usage, Equipment, Energy, Source, Unit, Year, no_equipment )

    thermal_consumption_per_equipment = thermal_consumption_per_equipment %>% spread( Year, thermal_consumption )
    final_consumption_per_equipment = final_consumption_per_equipment %>% spread( Year, final_consumption )
    average_efficiency = average_efficiency %>% spread( Year, average_efficiency )
    stock_per_equipment = stock_per_equipment %>% spread( Year, no_equipment )

    data_df = rbind( stock_per_equipment, thermal_consumption_per_equipment, final_consumption_per_equipment, average_efficiency )
    return( data_df )    
  }

  p_compute_stock_evolution <- function()
  {
    #1. compute evolution of stock for building class in Residential
    # time1 = Sys.time()
    print('1: Building Class')
    self$ls_outputs_data$building_stock = self$project_stock_RES()
    
    print('2: Equipment')
    self$ls_outputs_data$equipment_stock = self$project_equipment_RES()
    
    print('3: Insulation and Efficiency Main')
    self$ls_outputs_data$efficiency_main_stock = self$project_efficiency_main_RES()

    print('4: Consumption Main Equipment')
    self$ls_outputs_data$consumption_main_stock = self$project_consumption_main_RES()
  
    print('5: Efficiency Backup')
    self$ls_outputs_data$efficiency_backup_stock = self$project_efficiency_backup_RES()

    print('6: Consumption Backup Equipment')
    self$ls_outputs_data$consumption_backup_stock = self$project_consumption_backup_RES()

    print('7: KPI calculation')
    self$ls_outputs_data$KPI = self$project_KPI()

    print('8: Export to AMADEUS')
    self$ls_outputs_data$export_AMADEUS = self$export_to_AMADEUS()

    # time2 = Sys.time()
    # print( paste("total time:", time2- time1 ))
  }

  
  list_fields_BDSM1_class = list( ## Fields
    
    # Fields  
    country_mapping = NULL, 
    EQUIPMENT_MAPPING = tibble( Equipment = c("coal boiler", "oil boiler", "gas boiler", "biomass boiler", "district heating", "hp air/air", "hp air/water", "hp hybrid", "direct heater"),
                                Energy = c("coal", "oil", "gas", "biomass and waste", "final heat", "electricity", "electricity", "electricity", "electricity" ),
                                Lifetime = c( 20, 20, 20, 20, 20, 17, 17, 17, 15 ) ),
    
    BACKUP_MAPPING = tibble( Equipment = c("coal boiler", "oil boiler", "gas boiler", "biomass boiler", "district heating", "hp air/air", "hp air/water", "hp hybrid", "direct heater"),
                             Backup = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, "direct heater", "direct heater", "gas boiler", NA_real_ ),
                             Backup_share = c( 0, 0, 0, 0, 0, 0.35, 0.1, 0.1, 0 ) ),
    
    CEP_MAPPING = tibble( Energy = c( "coal", "oil", "gas", "biomass and waste", "final heat", "electricity"),
                          CEP = c( 1, 1, 1, 1, 1, 2.5 ) ),
    
    # part des equipements en fin de vie gardant le meme equipement
    EQUIPMENT_ALPHA = tibble( Equipment = c("coal boiler", "oil boiler", "gas boiler", "biomass boiler", "district heating", "hp air/air", "hp air/water", "hp hybrid", "direct heater"),
                              alpha = c( 0, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ) ),
    
    country = NULL,
    country_zone = NULL, 
    data_raw = NULL,
    projection_year = NULL, 
    
    ls_inputs_data = list( building_stock = NULL,
                    evolution_rate = NULL ,
                    average_floor_area = NULL ,
                    final_consumption = NULL,
                    thermal_consumption = NULL,
                    insulation_old= NULL,
                    insulation_policy = NULL,
                    efficiency_old_equipment = NULL, 
                    efficiency_new_equipment = NULL,
                    equipment_backup_share = NULL,
                    market_share = NULL,
                    useful_floor_area =NULL
                    ),
    
    ls_outputs_data = list( building_stock = NULL,
                            equipment_stock = NULL,
                            efficiency_main_stock = NULL,
                            efficiency_backup_stock = NULL,
                            consumption_main_stock = NULL,
                            consumption_backup_stock = NULL,
                            KPI = NULL,
                            export_AMADEUS = NULL
    ),
    
    # Methods
    initialize = p_initialize_model,
    
    prepare_data = p_prepare_data,
    project_stock_RES = p_project_stock_RES,
    project_equipment_RES = p_project_equipment_RES_v2,
    project_efficiency_main_RES = p_project_efficiency_main_RES,
    project_efficiency_backup_RES = p_project_efficiency_backup_RES,
    project_consumption_main_RES = p_project_consumption_main_RES,
    project_consumption_backup_RES = p_project_consumption_backup_RES,
    compute_stock_evolution = p_compute_stock_evolution,
    project_KPI = p_project_KPI,
    export_to_AMADEUS = p_export_to_AMADEUS,
    old_equipment_lifetime_distribution = p_old_equipment_lifetime_distribution,
    new_equipment_lifetime_distribution = p_new_equipment_lifetime_distribution_V2
  )
  
  
  BDSM_model <- R6Class( classname = "BDSM1", 
                         public = list_fields_BDSM1_class )
  
  
  
  
  ####################################################
  
  data = readr::read_delim( "./Building Stock Model/Data/Inputs/test_data_7.csv" , delim = ";")
  country_mapping = readr::read_delim( "./Building Stock Model/Data/Inputs/CountryRegionMapping.csv" , delim = ",")
  # country_list = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK","CH","NO")
  # country_list = c("AT","BE","CH","DE","ES","FR","IT","NL","PT") #???,"BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK","CH","NO")
  country_list = c( "IT" )
  ls_BDSM = list()

  for( country_name in country_list )
  {
    ls_BDSM[[ country_name ]] = BDSM_model$new( country_mapping = country_mapping, country = country_name, data_raw = data )
    ls_BDSM[[ country_name ]]$compute_stock_evolution()
  }

saveRDS( ls_BDSM, "./Building Stock Model/Data/Outputs/CWE_projection.RDS")