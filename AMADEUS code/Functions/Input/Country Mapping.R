##################################################################### World country mapping function ################################################

source("../Functions/Input/General_Inputs_Functions.R")

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

replace_country_name_country_code <- function(df) 
{
  dir_chemin <- "../../Data/Inputs/Nomenclature Data/Country Classification.xlsx"
  
  country_classification <- read.xlsx2(dir_chemin, sheetIndex = 1)
  
  
  df <- merge(country_classification, df)

  df <- df[,-1]
  df <- df[,-2]
  
  df <- cbind(df[,2:3], df[,1], df[,4:length(df)])
  names(df)[3] <- "Country"
  return(df)
}

creation_spatial_region <- function()
{
  dir_chemin <- "../../Data/Inputs/Nomenclature Data/Country Mapping.xlsx" 
  
  country_mapping <- read.xlsx2(dir_chemin, sheetIndex = 1)
  data("wrld_simpl")
  
  afr_zone <- country_mapping[country_mapping$Region == "AFR",2]
  afr_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(afr_zone[1]),]
  
  for(i in 2:length(afr_zone))
  {
    afr_spatial <- rbind(afr_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(afr_zone[i]),])
  }
  
  
  eur_zone <- country_mapping[country_mapping$Region == "EUR",2]
  eur_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(eur_zone[1]),]
  
  for(i in 2:length(eur_zone))
  {
    eur_spatial <- rbind(eur_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(eur_zone[i]),])
  }
  
  
  cis_zone <- country_mapping[country_mapping$Region == "CIS",2]
  cis_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(cis_zone[1]),]
  
  for(i in 2:length(cis_zone))
  {
    cis_spatial <- rbind(cis_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(cis_zone[i]),])
  }
  
  
  mie_zone <- country_mapping[country_mapping$Region == "MIE",2]
  mie_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(mie_zone[1]),]
  
  for(i in 2:length(mie_zone))
  {
    mie_spatial <- rbind(mie_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(mie_zone[i]),])
  }
  
  
  lam_zone <- country_mapping[country_mapping$Region == "LAM",2]
  lam_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(lam_zone[1]),]
  
  for(i in 2:length(lam_zone))
  {
    lam_spatial <- rbind(lam_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(lam_zone[i]),])
  }
  
  nam_zone <- country_mapping[country_mapping$Region == "NAM",2]
  nam_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(nam_zone[1]),]
  
  for(i in 2:length(nam_zone))
  {
    nam_spatial <- rbind(nam_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(nam_zone[i]),])
  }
  
  asp_zone <- country_mapping[country_mapping$Region == "ASP",2]
  asp_spatial<-  wrld_simpl[wrld_simpl$ISO2 == as.character(asp_zone[1]),]
  
  for(i in 2:length(asp_zone))
  {
    asp_spatial <- rbind(asp_spatial, wrld_simpl[wrld_simpl$ISO2 == as.character(asp_zone[i]),])
  }
  
  afr_spatial$REGION <- "AFR"
  eur_spatial$REGION <- "EUR"
  mie_spatial$REGION <- "MIE"
  cis_spatial$REGION <- "CIS"
  asp_spatial$REGION <- "ASP"
  lam_spatial$REGION <- "LAM"
  nam_spatial$REGION <- "NAM"
  
  
  spatial_zone <- rbind(afr_spatial, cis_spatial, eur_spatial, asp_spatial, mie_spatial, lam_spatial, nam_spatial)
  return(spatial_zone)
}