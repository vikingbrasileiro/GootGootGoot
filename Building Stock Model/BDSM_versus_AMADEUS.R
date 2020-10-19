
#######################################################
###                                                 ###
###                                                 ###
### comparison between Amadeus results and our BDSM ###
###                                                 ###
###                                                 ###
#######################################################

## data processing from Amadeus to a format comparable to BDSM
library( gridExtra )
library( tidyverse )


bindingBdsmOutput <- function( listBdsm )
{
    countryName = names( listBdsm )
    dataBdsm = NULL
    for( country in countryName )
    {
      dataBdsm = rbind( dataBdsm, listBdsm[[ country ]]$ls_outputs_data$export_AMADEUS )
    } 

    return( dataBdsm )
}

formattingBdsmFinalEnergyOutput <- function( dfBdsmData )
{
  # browser()
  finalEnergyBdsm <- dfBdsmData %>%
    filter( ID_Item == "Final Energy Demand" ) %>%
    gather( "Year", "Values", which( colnames( dfBdsmData ) == "2019" ):length( dfBdsmData ) )%>%
    group_by( Scenario, Country, Sector, Sub_Sector, Energy, Year ) %>%
    summarise( Values = sum(Values, na.rm = T) )

  # adding total demand (total of energy )
  finalEnergyBdsm <- finalEnergyBdsm %>%
    group_by( Scenario, Country, Sector, Sub_Sector, Year ) %>%
    summarise( Values = sum(Values, na.rm = T) ) %>%
    mutate( Energy = "total") %>%
    rbind( finalEnergyBdsm )

  # Changing Sub_sector col to be able to compare with AMADEUS results
  finalEnergyBdsm <- finalEnergyBdsm %>%
    mutate( Sub_Sector = case_when( Sub_Sector == "total" ~ "BDSM",
                                    TRUE ~ Sub_Sector) ) %>%
    select( Scenario, Country, Energy, Sector, Sub_Sector, Year, Values ) %>%
    arrange( Scenario, Country, Year )

    return( finalEnergyBdsm )
}

formattingBdsmHeatingDemandOutput <- function( dfBdsmData )
{
  # browser()
  heatingDemandBdsm <- dfBdsmData %>%
    filter( ID_Item == "Heating Demand", ) %>%
    gather( "Year", "Values", which( colnames( dfBdsmData ) == "2019" ):length( dfBdsmData ) )%>%
    group_by( Scenario, Country, Sector, Sub_Sector, Year ) %>%
    summarise( Values = sum(Values, na.rm = T) ) %>%
    mutate( Energy = "total") %>%
    filter( Year > 2019 )

  # Changing Sub_sector col to be able to compare with AMADEUS results
  heatingDemandBdsm <- heatingDemandBdsm %>%
    mutate( Sub_Sector = case_when( Sub_Sector == "total" ~ "BDSM",
                                    TRUE ~ Sub_Sector) ) %>%
    select( Scenario, Country, Sector, Sub_Sector, Year, Values ) %>%
    arrange( Scenario, Country, Year )

    return( heatingDemandBdsm )
}

formattingAmadeusFinalEnergyOutput <- function( dfAmadeusOutput, countryList )
{
  AmadeusData <- dfAmadeusOutput %>%
  filter(Sector == "RES") %>%
  filter(Sub_Sector == "total") %>%
  filter(Usage == "space heating") %>%
  mutate(Country = case_when(Country == "GB" ~ "UK",
                             TRUE ~ Country)) %>%
  mutate(Scenario = case_when(Scenario == "GB 2020" ~ "BT",
                              Scenario == "CS1" ~ "ME",
                              Scenario == "CS2" ~ "SD")) %>%
  filter(Country %in% countryList) %>%
  select(-c(which(colnames(.) == "2000"):which(colnames(.) == "2018"))) %>%
  select( -c(ID_Item,Usage,Source,Unit) ) %>%
  arrange(Scenario, Country) # smt wrong with France 

  AmadeusData <- AmadeusData %>%
  gather( "Year", "Values", which(colnames(AmadeusData)=="2019"):ncol(AmadeusData) ) %>%
  mutate(Sub_Sector = case_when(Sub_Sector == "total" ~ "Amadeus",
                   TRUE ~ Sub_Sector))

  return( AmadeusData)         

}

formattingAmadeusHeatingDemandOutput <- function( dfAmadeusOutput, countryList )
{
  AmadeusData <- dfAmadeusOutput %>%
  filter(Sector == "RES") %>%
  filter(Sub_Sector == "na") %>%
  filter(Usage == "space heating") %>%
  mutate( Country = case_when(Country == "GB" ~ "UK",
                             TRUE ~ Country)) %>%
  mutate( Scenario = case_when(Scenario == "GB 2020" ~ "BT",
                              Scenario == "CS1" ~ "ME",
                              Scenario == "CS2" ~ "SD")) %>%
  filter( Country %in% countryList ) %>%
  # select(-c(which(colnames(.) == "2000"):which(colnames(.) == "2018"))) %>%
  select( -c( ID_Item, Usage, Source, Unit, Energy ) ) %>%
  arrange(Scenario, Country) # smt wrong with France 

  AmadeusData <- AmadeusData %>%
  gather( "Year", "Values", which(colnames(AmadeusData)=="2020"):ncol(AmadeusData) ) %>%
  mutate(Sub_Sector = case_when(Sub_Sector == "na" ~ "Amadeus",
                   TRUE ~ Sub_Sector))

  return( AmadeusData)         

}

exportFinalEnergyComparisonPlotToPdf <- function( dfAmadeus, dfBdsm, filenamePdf )
{
  # browser()
  dfForGraphe <- rbind( dfAmadeus, dfBdsm ) %>%
    spread( Sub_Sector, Values )

  scenarios = unique( dfForGraphe$Scenario )
  country_list = unique( dfForGraphe$Country )
  energies = unique( dfForGraphe$Energy )

  plot_list <- list()
  p = NULL
  # for all scenario, country and energy procducing ggplot charts 
  for( scenario in scenarios ) 
  {
    for( country_name in country_list ) 
    {
      for( energy in energies ) 
      {
        dfForGrapheTmp <- dfForGraphe %>% 
          filter( Scenario == scenario, Country == country_name, Energy == energy ) %>%
          select( Year, Amadeus, BDSM ) %>%
          gather( key = "variable", value = "value", -Year ) %>% ungroup() %>%
          mutate( Year = as.numeric( as.character( Year ) ) )

        p = ggplot( dfForGrapheTmp, aes(x = Year, y = value, color = variable ) ) +
          geom_line( size = 1.1 ) +
          scale_color_manual( values = c("#00AFBB", "#E7B800") ) +
          expand_limits( y = 0 ) +
          theme_minimal() +
          labs( title = paste0( scenario,": ", country_name,", ",energy ) , y = "Final Energy Demand in TWh" ) +
          theme( axis.text = element_text(size = 5 ),
                 axis.title = element_text(size = 6, face = "bold" ),
                 plot.title = element_text(size = 8, face = "bold", hjust = 0.5 ),
                 legend.position = c( 0.95,0.85 ),
                 legend.title = element_text(size = 6),
                 legend.text = element_text(size = 5) )

        plot_list[[scenario]][[country_name]][[energy]] <- p
      }
    }
  }

  pdf( filenamePdf, onefile = TRUE  )

    for( i in 1:length( plot_list[[1]] ) )
    {
      do.call( "grid.arrange", plot_list[[1]][[i]] )
    }
  dev.off()
}

exportHeatingDemandComparisonPlotToPdf <- function( dfAmadeus, dfBdsm, filenamePdf )
{
  # browser()
  dfForGraphe <- rbind( dfAmadeus, dfBdsm ) %>%
    spread( Sub_Sector, Values )

  scenarios = unique( dfForGraphe$Scenario )
  country_list = unique( dfForGraphe$Country )

  plot_list <- list()
  p = NULL
  # for all scenario, country and energy procducing ggplot charts 
  for( scenario in scenarios ) 
  {
    for( country_name in country_list ) 
    {
        # browser()
      dfForGrapheTmp <- dfForGraphe %>% 
        filter( Scenario == scenario, Country == country_name ) %>%
        select( Year, Amadeus, BDSM ) %>%
        gather( key = "variable", value = "value", -Year ) %>% ungroup() %>%
        mutate( Year = as.numeric( as.character( Year ) ) )

      p = ggplot( dfForGrapheTmp, aes(x = Year, y = value, color = variable ) ) +
        geom_line( size = 1.1 ) +
        scale_color_manual( values = c("#00AFBB", "#E7B800") ) +
        expand_limits( y = 0 ) +
        theme_minimal() +
        labs( title = paste0( scenario,": ", country_name ) , y = "Heating Demand in TWh" ) +
        theme( axis.text = element_text(size = 5 ),
                axis.title = element_text(size = 6, face = "bold" ),
                plot.title = element_text(size = 8, face = "bold", hjust = 0.5 ),
                legend.position = c( 0.95,0.85 ),
                legend.title = element_text(size = 6),
                legend.text = element_text(size = 5) )

      plot_list[[scenario]][[country_name]] <- p
    }
  }

  pdf( filenamePdf, onefile = TRUE  )

    for( i in 1:length( plot_list ) )
    {
      do.call( "grid.arrange", plot_list[[i]] )
    }
  dev.off()
}


# charge Amadeus Data to compare with
listBdsm = readRDS( "./Building Stock Model/Data/Outputs/CWE_projection.RDS" )
bdsmRawData = bindingBdsmOutput( listBdsm )
countryList = unique( bdsmRawData$Country )

bdsmFinalEnergyData = formattingBdsmFinalEnergyOutput( bdsmRawData )
bdsmHeatingDemandData = formattingBdsmHeatingDemandOutput( bdsmRawData )

amadeusFinalEnergyRawData = readr::read_delim( "./Building Stock Model/Data/Inputs/Amadeus_ALL.csv" , delim = ",")
amadeusHeatingDemandRawData = readr::read_delim( "./Building Stock Model/Data/Inputs/Amadeus_heating_Demand.csv" , delim = ",")
amadeusFinalEnergyData = formattingAmadeusFinalEnergyOutput( amadeusFinalEnergyRawData, countryList )
amadeusHeatingDemandData = formattingAmadeusHeatingDemandOutput( amadeusHeatingDemandRawData, countryList )

exportFinalEnergyComparisonPlotToPdf( filenamePdf = "./Building Stock Model/Data/Outputs/FinalEnergyDemand_16102020.pdf", amadeusFinalEnergyData, bdsmFinalEnergyData )
exportHeatingDemandComparisonPlotToPdf( filenamePdf = "./Building Stock Model/Data/Outputs/HeatingDemandTest.pdf", amadeusHeatingDemandData, bdsmHeatingDemandData )

# bdsmFinalEnergyData %>% 
#   filter( Scenario == "BT", Energy != "total", Country != "FR" ) %>% 
#   group_by( Energy, Year) %>% 
#   summarise( Values = sum(Values)) %>%
#   ggplot( aes( as.numeric(Year), Values, fill = Energy ) ) + geom_area()

# amadeusFinalEnergyData %>% 
#   filter( Scenario == "BT", !( Energy %in% c("total", "direct hydrogen") ), Country != "FR" ) %>% 
#   group_by( Energy, Year) %>% 
#   summarise( Values = sum(Values)) %>%
#   ggplot( aes( as.numeric(Year), Values, fill = Energy ) ) + geom_area()