# source("../Functions/Input/General_Inputs_Functions.R")

source("../Functions/Input/Country Mapping.R")

library(stringr)
library("zoo")
library(maptools)
library(viridis)
library(stats)
library(dplyr)
library(magrittr)
library(sp)
library(rgdal)
library(rgeos)
library(lubridate)
library(reshape)
library(tidyr)

options(java.parameters = "- Xmx1024m")
############################################################################################################################################################################
# Benchmark Data Extracttion and formatting for AMADEUS Interface 
############################################################################################################################################################################
main_IHS_Data <- function(chemin_fichier)
{
  #1. Data input extraction 
  column_type = c(rep("character", times = 3),rep("numeric", times = 41))
  
  country_list <- levels(as.factor(Hist_Energy_Demand$Country))
  
  IHS_Country <- Hist_Energy_Demand[1,1:50]
  
  for(j in 1:2)
  {
    IHS_File <-read.xlsx2(chemin_fichier, sheetName = country_list[j], colClasses = column_type)
    
    #.On se debarasse des lignes et colonnes inutiles
    IHS_File <- droplevels(subset(IHS_File, Unit == "ktoe"))
    IHS_File <- IHS_File[,-1]
    mani
    #On nettoie les etiquettes 
    IHS_File[,1] <- str_trim(IHS_File[,1])
    IHS_File[which(str_detect(IHS_File[,1], "Electricity")),1] <- "electricity"
    IHS_File[which(str_detect(IHS_File[,1], fixed("Renewables", ignore_case=TRUE))),1] <- "electricity"
    
    IHS_File[which(str_detect(IHS_File[,1], "waste")),1] <- "biomass and waste"
    IHS_File[which(str_detect(IHS_File[,1], fixed("Natural", ignore_case=FALSE))),1] <- "gas"
    IHS_File[which(str_detect(IHS_File[,1], "Coal")),1] <- "coal"
    IHS_File[which(str_detect(IHS_File[,1], "Oil")),1] <- "oil"
    IHS_File[which(str_detect(IHS_File[,1], "Heat")),1] <- "final heat"
    IHS_File <- IHS_File[-2]
    
    sector_boundaries <- which(str_detect(IHS_File[,1], "demand") | str_detect(IHS_File[,1],"sector"))
    sector_boundaries[length(sector_boundaries)+1] <- length(rownames(IHS_File))
    
    IHS_Sector <- list()
    for(i in 1:(length(sector_boundaries)-1))
    {
      if(i!=(length(sector_boundaries)-1))
      {
        IHS_Sector[[i]] <- IHS_File[sector_boundaries[i]:(sector_boundaries[i+1]-1),]
      }
      else
      {
        IHS_Sector[[i]] <- IHS_File[sector_boundaries[i]:sector_boundaries[i+1],]
      }
    }
    
    IHS_Sector <- IHS_Sector[-2]
    names(IHS_Sector) <- c("Primary", "Final", "RES", "TER", "IND", "TRA", "Other")
    
    
    for(i in 1:length(IHS_Sector))
    {
      IHS_Sector[[i]] <- aggregate(x = IHS_Sector[[i]][,2:length(IHS_Sector[[i]])], by = list(IHS_Sector[[i]][,1]), FUN = sum)
      IHS_Sector[[i]][which(str_detect(IHS_Sector[[i]][,1], "demand") | str_detect(IHS_Sector[[i]][,1],"sector")),1] <- "total" 
      IHS_Sector_header <- data.frame(ID_Item = rep("Energy Demand", nrow(IHS_Sector[[i]])), Scenario = rep("IHS Rivalry 2017", nrow(IHS_Sector[[i]])),
                                      Country = rep(country_list[j], nrow(IHS_Sector[[i]])), Energy = IHS_Sector[[i]][,1], Sector = rep(names(IHS_Sector[i]), nrow(IHS_Sector[[i]]))
                                      , Sub_Sector = rep("total", nrow(IHS_Sector[[i]])), Usage = rep("total", nrow(IHS_Sector[[i]])), Source = rep("IHS", nrow(IHS_Sector[[i]]))
                                      , Unit = rep("ktoe", nrow(IHS_Sector[[i]])))
      IHS_Sector[[i]] <- cbind(IHS_Sector_header, IHS_Sector[[i]][-1])
    }
    
    IHS_Final <- IHS_Sector[[1]]
    
    for(i in 2:length(IHS_Sector))
    {
      IHS_Final <- rbind(IHS_Final, IHS_Sector[[i]])
    }
    
  IHS_Country <- rbind(IHS_Country, IHS_Final)  
  }
  
  IHS_Country <- droplevels(subset(IHS_Country, Scenario!="GB"))
  
  return(IHS_Country)
}


Benchmark_chart_starting_point <- function(dataframe)
{
  Projection_year_col <- min(which(is.nan(colSums(Hist_Energy_Demand[10:length(Hist_Energy_Demand)])))) + 9 - 1
  
  #on remplace l'historique des benchmarks par celui du scenrio amadeus
  new_dataframe <- droplevels(dataframe)
  new_dataframe[,1:9]$Scenario <- as.character(new_dataframe[,1:9]$Scenario)
  
  new_dataframe[,10:Projection_year_col] <- dataframe[dataframe$Scenario == "GB" ,10:Projection_year_col]
  
  #On rajoute une serie historique
  new_dataframe <- rbind(new_dataframe, new_dataframe[1,])
  new_dataframe$Scenario[nrow(new_dataframe)] <- "Historical"
  new_dataframe[nrow(new_dataframe), (Projection_year_col+1):length(new_dataframe)] <- NaN
  
  # On applique le taux de croissance des benchamrks au point de depart du scenario amadeus
  growth_rate <- dataframe
  for(i in 1:(nrow(new_dataframe)-1))
  {
    data_location <- which(!is.na(dataframe[i,]))
    data_location <- data_location[data_location > 9]
    k = 2
    while(k <= length(data_location))
    {
      growth_rate[i,data_location[k]] <-  dataframe[i,data_location[k]]/dataframe[i,data_location[k-1]] - 1
      k = k + 1 
    }
  }
  
  #starting point location
  growth_rate[,10:Projection_year_col] <- NaN
  no_starting_point <- 0
  k = Projection_year_col
  matrice_test <- growth_rate[,Projection_year_col]
  matrice_test[is.na(matrice_test)] <- 0
  
  while(no_starting_point < nrow(growth_rate))
  {
    active_col <-  dataframe[, k]
    active_col[which(is.na(active_col))] <- 0
    active_col[active_col!=0] <- 1
    active_col[matrice_test == 1] <- 0
    
    for(j in 1:length(active_col))
    {
      if(active_col[j] == 1 && matrice_test[j] == 0)
      {
        matrice_test[j] <- 1
      }
    }
    
    growth_rate[, k] <- active_col
    growth_rate[growth_rate[, k] == 0, k] <- NaN
    
    no_starting_point <- no_starting_point + sum(active_col)
    k = k- 1 
  }
  
  for(i in 1:(nrow(new_dataframe)-1))
  {
    data_location <- which(!is.na(growth_rate[i,]))
    data_location <- data_location[data_location > 9]
    k = 2
    while(k <= length(data_location))
    {
      new_dataframe[i,data_location[k]] <-  new_dataframe[i,data_location[k-1]] * (1 + growth_rate[i,data_location[k]])
      k = k + 1 
    }
  }
  
  return(new_dataframe)
}


##############################################################################
#Import of IHS world data base 
##############################################################################

main_IHS_world_data <- function()
{
  #1. Data input extraction 
  column_type = c(rep("character", times = 2),rep("numeric", times = 61))
  col_index <- seq(1,63)
  sheet_name <- "Regional Energy (toe)"
  
  dir_chemin <- tk_choose.files(default = "./", multi = FALSE )
  
  
  country_list <- levels(as.factor(Hist_Energy_Demand$Country))
  
  IHS_File_Region <-read.xlsx2(dir_chemin, sheetName = sheet_name, colClasses = column_type, colIndex = col_index)
  
  IHS_File_Region <- IHS_File_Region[,-1]
  IHS_File_Region <- IHS_File_Region[which((str_detect(IHS_File_Region[,1], ""))),]
  IHS_File_Region <- IHS_File_Region[-((nrow(IHS_File_Region)-12):nrow(IHS_File_Region)),]
  
  region_lines <- which(str_detect(IHS_File_Region[,1], "Total primary energy consumption by fuel1")) - 1
  region_name <- IHS_File_Region[region_lines, 1]
  region_vector <- IHS_File_Region[, 1]
  k <- 1
  region_lines_concerned <- 1
  
  for(i in region_lines[1]:length(region_vector))
  {
    test <- min(as.numeric(region_lines[k+1]), i) == as.numeric(region_lines[k+1])
    if(test)
    {
      region_lines_concerned <- k+1
      k <- min(k+1, (length(region_lines)-1))
    }
    
    region_vector[i] <- region_name[region_lines_concerned]
  }
  
  region_vector <- as.character(region_vector)
  
  sector_list <- c("Total primary energy consumption by fuel1" ,"Residential", "Agricultural", "Commercial", "Industry", "Feedstocks", "Transport", "Primary energy consumption in electricity")
  sector_lines <- which(IHS_File_Region[,1] %in% sector_list)
  
  sector_vector <- IHS_File_Region[, 1]
  k <- 2
  sector_lines_concerned <- 1
  
  for(i in sector_lines[1]:region_lines[2])
  {
    test <- min(as.numeric(sector_lines[k]), i) == as.numeric(sector_lines[k])
    
    if(test)
    {
      sector_lines_concerned <- k
      k <- k+1
    }
    
    sector_vector[i] <- sector_list[sector_lines_concerned]
  }
  
  sector_vector <- as.character(sector_vector)
  sector_vector_pattern <- sector_vector[sector_lines[1]:region_lines[2]]
  sector_vector_pattern <- rep(sector_vector_pattern, length(region_name)-1)
  sector_vector <- c(sector_vector[1:region_lines[2]], sector_vector_pattern)
  sector_vector <- sector_vector[-length(sector_vector) ]
  IHS_File_Region <- cbind(region_vector,sector_vector, IHS_File_Region)
  
  IHS_File_Region <- IHS_File_Region[which(!is.na(IHS_File_Region[,4])),]
  names(IHS_File_Region) <- c("Country", "Sector", "Energy", paste0("X", IHS_File_Region[1, 4:length(IHS_File_Region)]))
  IHS_File_Region <- IHS_File_Region[-1,]
  IHS_File_Region <- IHS_File_Region[IHS_File_Region[,2] != "Primary energy consumption in electricity",]
  
  IHS_File_Region <- droplevels(IHS_File_Region)
  IHS_File_Region[,1] <- as.character(IHS_File_Region[,1])
  IHS_File_Region[,2] <- as.character(IHS_File_Region[,2])
  IHS_File_Region[,3] <- as.character(IHS_File_Region[,3])
  
  IHS_File_Region[which(IHS_File_Region[,3] %in% sector_list), 3] <- "total"
  
  IHS_File_Region[which(str_detect(IHS_File_Region[,2], "primary")),2] <- "Primary"
  
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Coal")),3] <- "coal"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Oil")),3] <- "oil"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "gas")),3] <- "gas"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Others")),3] <- "others"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Nuclear")),3] <- "nuclear"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Hydro")),3] <- "hydro"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Renewable")),3] <- "renewable"
  IHS_File_Region[which(str_detect(IHS_File_Region[,3], "Electricity")),3] <- "electricity"
  
  IHS_File_Region <- cbind(data.frame("ID_Item" = rep("Energy Demand", nrow(IHS_File_Region))),
                           data.frame("Scenario" = rep("IHS World 2018", nrow(IHS_File_Region))),
                           data.frame("Country_Name" = IHS_File_Region[,1]),
                           data.frame("Energy" = IHS_File_Region[,3]),
                           data.frame("Sector" = IHS_File_Region[,2]),
                           data.frame("Sub_Sector" = rep("total", nrow(IHS_File_Region))),
                           data.frame("Usage" = rep("total", nrow(IHS_File_Region))),
                           data.frame("Source" = rep("IHS", nrow(IHS_File_Region))),
                           data.frame("Unit" = rep("TWh", nrow(IHS_File_Region))),
                           IHS_File_Region[,4:length(IHS_File_Region)])
  
  
  ###### Major country enrgy demand extraction
  
  sheet_name <- "Major Country Energy (toe)"
  IHS_File_Major_Country <-read.xlsx2(dir_chemin, sheetName = sheet_name, colClasses = column_type, colIndex = col_index)
  
  IHS_File_Major_Country <- IHS_File_Major_Country[,-1]
  IHS_File_Major_Country <- IHS_File_Major_Country[which((str_detect(IHS_File_Major_Country[,1], ""))),]
  IHS_File_Major_Country <- IHS_File_Major_Country[-((nrow(IHS_File_Major_Country)-12):nrow(IHS_File_Major_Country)),]
  IHS_File_Major_Country <- IHS_File_Major_Country[!(IHS_File_Major_Country[,1] %in% region_name),]
  
  
  country_lines <- which(str_detect(IHS_File_Major_Country[,1], "Total primary energy consumption by fuel1")) - 1
  country_name <- IHS_File_Major_Country[country_lines, 1]
  country_vector <- IHS_File_Major_Country[, 1]
  k <- 1
  country_lines_concerned <- 1
  
  for(i in country_lines[1]:length(country_vector))
  {
    test <- min(as.numeric(country_lines[k+1]), i) == as.numeric(country_lines[k+1])
    if(test)
    {
      country_lines_concerned <- k+1
      k <- min(k+1, (length(country_lines)-1))
    }
    
    country_vector[i] <- country_name[country_lines_concerned]
  }
  
  country_vector <- as.character(country_vector)
  
  sector_list <- c("Total primary energy consumption by fuel1" ,"Residential", "Agricultural", "Commercial", "Industry", "Feedstocks", "Transport", "Primary energy consumption in electricity")
  sector_lines <- which(IHS_File_Major_Country[,1] %in% sector_list)
  
  sector_vector <- IHS_File_Major_Country[, 1]
  k <- 2
  sector_lines_concerned <- 1
  
  for(i in sector_lines[1]:country_lines[2])
  {
    test <- min(as.numeric(sector_lines[k]), i) == as.numeric(sector_lines[k])
    
    if(test)
    {
      sector_lines_concerned <- k
      k <- k+1
    }
    
    sector_vector[i] <- sector_list[sector_lines_concerned]
  }
  
  sector_vector <- as.character(sector_vector)
  sector_vector_pattern <- sector_vector[sector_lines[1]:country_lines[2]]
  sector_vector_pattern <- rep(sector_vector_pattern, length(country_name)-1)
  sector_vector <- c(sector_vector[1:country_lines[2]], sector_vector_pattern)
  sector_vector <- sector_vector[-length(sector_vector) ]
  IHS_File_Major_Country <- cbind(country_vector,sector_vector, IHS_File_Major_Country)
  
  IHS_File_Major_Country <- IHS_File_Major_Country[which(!is.na(IHS_File_Major_Country[,4])),]
  names(IHS_File_Major_Country) <- c("Country", "Sector", "Energy", paste0("X", IHS_File_Major_Country[1, 4:length(IHS_File_Major_Country)]))
  IHS_File_Major_Country <- IHS_File_Major_Country[-1,]
  IHS_File_Major_Country <- IHS_File_Major_Country[IHS_File_Major_Country[,2] != "Primary energy consumption in electricity",]
  
  IHS_File_Major_Country <- droplevels(IHS_File_Major_Country)
  IHS_File_Major_Country[,1] <- as.character(IHS_File_Major_Country[,1])
  IHS_File_Major_Country[,2] <- as.character(IHS_File_Major_Country[,2])
  IHS_File_Major_Country[,3] <- as.character(IHS_File_Major_Country[,3])
  
  IHS_File_Major_Country[which(IHS_File_Major_Country[,3] %in% sector_list), 3] <- "total"
  
  
  ## On met les bonnes etiquettes pour les secteurs, energy
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,2], "primary")),2] <- "Primary"
  
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Coal")),3] <- "coal"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Oil")),3] <- "oil"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "gas")),3] <- "gas"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Others")),3] <- "others"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Nuclear")),3] <- "nuclear"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Hydro")),3] <- "hydro"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Renewable")),3] <- "renewable"
  IHS_File_Major_Country[which(str_detect(IHS_File_Major_Country[,3], "Electricity")),3] <- "electricity"
  
  IHS_File_Major_Country <- cbind(data.frame("ID_Item" = rep("Energy Demand", nrow(IHS_File_Major_Country))),
                                  data.frame("Scenario" = rep("IHS World 2018", nrow(IHS_File_Major_Country))),
                                  data.frame("Country_Name" = IHS_File_Major_Country[,1]),
                                  data.frame("Energy" = IHS_File_Major_Country[,3]),
                                  data.frame("Sector" = IHS_File_Major_Country[,2]),
                                  data.frame("Sub_Sector" = rep("total", nrow(IHS_File_Major_Country))),
                                  data.frame("Usage" = rep("total", nrow(IHS_File_Major_Country))),
                                  data.frame("Source" = rep("IHS", nrow(IHS_File_Major_Country))),
                                  data.frame("Unit" = rep("TWh", nrow(IHS_File_Major_Country))),
                                  IHS_File_Major_Country[,4:length(IHS_File_Major_Country)])
  
  IHS_File <- rbind(IHS_File_Region, IHS_File_Major_Country)
  
  ## On retire  les annees 1990 a 1999
  IHS_File <- IHS_File[,-(10:19)]
  
  IHS_File$Sector <- as.character(IHS_File$Sector)
  
  ## On met les bonnes etiquettes pour les secteurs
  IHS_File[which(str_detect(IHS_File[,5], "Residential")),5] <- "RES"
  IHS_File[which(str_detect(IHS_File[,5], "Commercial")),5] <- "TER"
  IHS_File[which(str_detect(IHS_File[,5], "Industry")),5] <- "IND"
  IHS_File[which(str_detect(IHS_File[,5], "Transport")),5] <- "TRA"
  IHS_File[which(str_detect(IHS_File[,5], "Feedstocks")),5] <- "NEU"
  IHS_File[which(str_detect(IHS_File[,5], "Agricultural")),5] <- "AGR"
  
  ## on convertie Mtoe to TWh
  IHS_File[,10:length(IHS_File)] <- IHS_File[,10:length(IHS_File)] * 11.628
  
  ## Nomenclature commune amadeus pour etiquette
  IHS_File <- replace_country_name_country_code(IHS_File)
  
  #on remplace les valeurs negatives par 0
  for(i in 10:length(IHS_File))
  {
    IHS_File[IHS_File[,i] < 0, i] <- 0
  }
  
  
  return(IHS_File)
}


##############################################################################
#Import of Enerdata historical database for every country 
##############################################################################

main_Enerdata_world_data <- function()
{
  #1. Data input extraction 
  column_type <- c(rep("character", times = 3),rep("numeric", times = 18))
  col_index <- seq(1,21)
  dir_chemin <- tk_choose.files(default = "./", multi = FALSE )
  
  
  # country_list <- levels(Hist_Energy_Demand$Country)
  
  Enerdata_File <-read.xlsx2(dir_chemin, sheetIndex = 1, colClasses = column_type, colIndex = col_index, header = F)
  names(Enerdata_File) <- c("ID_Enerdata", "Country", "Unit", paste0("X", Enerdata_File[2, 4:length(Enerdata_File)]))
  Enerdata_File <- Enerdata_File[which((str_detect(Enerdata_File[,1], ""))),]
  Enerdata_File <- Enerdata_File[-((nrow(Enerdata_File)-2):nrow(Enerdata_File)),]
  
  ID_enerdata_lines <- which(!(str_detect(Enerdata_File[,2], "")))
  ID_enerdata_names <- Enerdata_File[ID_enerdata_lines,1]
  k <- 1
  ID_enerdata_concerned <- 1
  
  for(i in ID_enerdata_lines[1]:nrow(Enerdata_File))
  {
    test <- min(as.numeric(ID_enerdata_lines[k+1]), i) == as.numeric(ID_enerdata_lines[k+1])
    if(test)
    {
      ID_enerdata_concerned <- k+1
      k <- min(k+1, (length(ID_enerdata_lines)-1))
    }
    
    Enerdata_File[i,1] <- ID_enerdata_names[ID_enerdata_concerned]
  }
  
  Enerdata_File <- Enerdata_File[which((str_detect(Enerdata_File[,2], ""))),]
  Enerdata_File["Unit"] <- "TWh"
  Enerdata_File[,1] <- str_match(Enerdata_File[,1], "[(](.*?)[)]")[,2]
  
  dir_chemin <- "../../Data/Inputs/Nomenclature Data/Nomenclature_Enerdata.xlsx"
  nomenclature_enerdata <- read.xlsx2(dir_chemin, sheetIndex = 1, colClasses = rep("character", 4), header = T)
  
  Enerdata_File <- merge(nomenclature_enerdata, Enerdata_File)
  Enerdata_File <- Enerdata_File[,-(1:2)]
  Enerdata_File <- cbind(data.frame("ID_Item" = rep("Energy Demand", nrow(Enerdata_File))),
                         data.frame("Scenario" = rep("Enerdata historical data", nrow(Enerdata_File))),
                         Enerdata_File["Country"],
                         Enerdata_File["Energy"],
                         Enerdata_File["Sector"],
                         data.frame("Sub_Sector" = rep("total", nrow(Enerdata_File))),
                         data.frame("Usage" = rep("total", nrow(Enerdata_File))),
                         data.frame("Source" = rep("Enerdata", nrow(Enerdata_File))),
                         Enerdata_File["Unit"],
                         Enerdata_File[,5:length(Enerdata_File)]
  )
  
  last_historal_year <- as.character(names(Enerdata_File)[length(Enerdata_File)])
  
  Enerdata_File_last_year <- Enerdata_File[,length(Enerdata_File)]
  Enerdata_File_other_year <- Enerdata_File[,10:(length(Enerdata_File)-1)]
  Enerdata_File_other_year[is.na(Enerdata_File_other_year)] <- 0
  Enerdata_File <- cbind(Enerdata_File[,1:9], Enerdata_File_other_year, Enerdata_File_last_year) 
  names(Enerdata_File)[length(Enerdata_File)] <- last_historal_year
  
  last_historal_year <- as.numeric(str_replace(last_historal_year, "X", ""))
  name_lacking_column <- paste0("X", as.character(seq((last_historal_year+1),2050)))
  nb_lacking_column <- length(name_lacking_column) 
  
  lacking_column <- as.data.frame(matrix(data = NaN, nrow = nrow(Enerdata_File), ncol = nb_lacking_column))
  names(lacking_column) <- name_lacking_column
  
  Enerdata_File <- cbind(Enerdata_File, lacking_column)
  
  
  return(Enerdata_File)
}

Enerdata_world_Primary <- function()
{
  
}
