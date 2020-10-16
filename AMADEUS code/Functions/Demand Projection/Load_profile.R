

hourly_load_projection <- function()
{
  column_type = c(rep("character", times = 9),rep("numeric", times = 8760))
  unitary_load_profile <- read.xlsx2("../../Data/Inputs/Projection Data/Unitary load profile.xlsx", sheetIndex = 1 , colClasses = column_type)
  Proj_Energy_Demand <- readRDS("../../Data/temp/Proj_Energy_Demand.RDS")
  
  number_usages <- nrow(unitary_load_profile)
  country_list <- levels(as.factor(Proj_Energy_Demand$Country))
  number_country <- nlevels(as.factor(Proj_Energy_Demand$Country))
  
  hourly_load_profile <- NULL
  hourly_load_profile_temp <- NULL
  sector_temp <- NULL
  usage_temp <- NULL
  yearly_projection <- NULL
  country_yearly_projection <- NULL
  unitary_load_profile_temp <- NULL
  country_temp <- NULL
  name_item_list <- NULL
  Proj_Energy_Demand <- readRDS("../../Data/temp/Proj_Energy_Demand.rds")
  yearly_projection_temp <- NULL 
  name_usages <- NULL
  
  
  for(k in 1:number_country)
  {
    country_temp <- country_list[k]
  
    for(j in 10:length(Proj_Energy_Demand))
    {
      for(i in 1:number_usages)
      {
        sector_temp <- as.character(droplevels(unitary_load_profile$Sector[i]))
        usage_temp <-  as.character(droplevels(unitary_load_profile$Usage[i]))
        
        
        yearly_projection_temp <- Proj_Energy_Demand[Proj_Energy_Demand$ID_Item == "Energy Demand"
                                              & Proj_Energy_Demand$Country == country_temp      
                                              & Proj_Energy_Demand$Sector == sector_temp
                                              & Proj_Energy_Demand$Sub_Sector == "total"
                                              & Proj_Energy_Demand$Usage == usage_temp
                                              & Proj_Energy_Demand$Energy == "electricity",j]
        
        yearly_projection <- rbind(yearly_projection, yearly_projection_temp) 
        name_usages <- c(name_usages, paste0(sector_temp, "_", usage_temp))
      }
      
      hourly_load_profile <- t(yearly_projection * unitary_load_profile[,10:length(unitary_load_profile)])
      
      hourly_load_profile <- as.data.frame(hourly_load_profile)
      names(hourly_load_profile) <- name_usages

      name_item_list <- paste0(country_temp, "_", j+1990)
      saveRDS(hourly_load_profile, file = paste0("../../Data/temp/load_profile/", name_item_list, ".rds"))
     
       i <- 1
       name_usages <- NULL
       yearly_projection <- NULL
    }
    j <- 10
 }
  
}

peak_evolution <- function()
{
  column_type = c(rep("character", times = 9),rep("numeric", times = 8760))
  unitary_load_profile <- read.xlsx2("../../Data/Inputs/Projection Data/Unitary load profile.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_usage <- paste0(unitary_load_profile$Sector, "_", unitary_load_profile$Usage)
  unitary_load_profile$Usage <- list_usage
  
  template_dataframe_year <- readRDS("../../Data/temp/Hist_Macro.RDS")
  list_country <- levels(as.factor(template_dataframe_year$Country))
  number_country <- nlevels(as.factor(template_dataframe_year$Country))
  template_dataframe_year <- template_dataframe_year[1:length(list_usage), 10:length(template_dataframe_year)]
  
  peak_evolution <- cbind(unitary_load_profile$Usage, template_dataframe_year)
  names(peak_evolution)[1] <- "Usage"
  peak_evolution <- rbind(peak_evolution, peak_evolution[1,]) 
  peak_evolution$Usage <- as.character(peak_evolution$Usage)
  peak_evolution$Usage[nrow(peak_evolution)] <- "total"
  peak_evolution$Usage <- as.factor(peak_evolution$Usage)
  peak_evolution[, 2:length(peak_evolution)] <- NA 
  
  list_load_profiles <- dir("../../Data/temp/load_profile")
  peak_evolution_temp <- NULL
  
  for(i in 1:number_country)
  {
    country_temp <- list_country[i]
    list_load_profiles_temp <- list_load_profiles[str_detect(list_load_profiles, country_temp)]
    
    for(j in 1:length(list_load_profiles_temp))
    {
      peak_evolution_temp <- readRDS(paste0("../../Data/temp/load_profile/", list_load_profiles_temp[j]))
      
      max_per_usage <- apply(peak_evolution_temp, 2, max) 
      max_per_usage <- max_per_usage * 1000000 
      
      max_total <- max(rowSums(peak_evolution_temp)) * 1000000
      
      peak_evolution[, j+1] <- c(max_per_usage, max_total)
      
    }
    
    saveRDS(peak_evolution, file = paste0("../../Data/temp/peak_evolution/", country_temp,"_peak_evolution.rds")) 
  }

}

offpeak_evolution <- function()
{
  column_type = c(rep("character", times = 9),rep("numeric", times = 8760))
  unitary_load_profile <- read.xlsx2("../../Data/Inputs/Projection Data/Unitary load profile.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_usage <- paste0(unitary_load_profile$Sector, "_", unitary_load_profile$Usage)
  unitary_load_profile$Usage <- list_usage
  
  template_dataframe_year <- readRDS("../../Data/temp/Hist_Macro.RDS")
  list_country <- levels(as.factor(template_dataframe_year$Country))
  number_country <- nlevels(as.factor(template_dataframe_year$Country))
  template_dataframe_year <- template_dataframe_year[1:length(list_usage), 10:length(template_dataframe_year)]
  
  offpeak_evolution <- cbind(unitary_load_profile$Usage, template_dataframe_year)
  names(offpeak_evolution)[1] <- "Usage"
  offpeak_evolution <- rbind(offpeak_evolution, offpeak_evolution[1,]) 
  offpeak_evolution$Usage <- as.character(offpeak_evolution$Usage)
  offpeak_evolution$Usage[nrow(offpeak_evolution)] <- "total"
  offpeak_evolution$Usage <- as.factor(offpeak_evolution$Usage)
  offpeak_evolution[, 2:length(offpeak_evolution)] <- NA 
  
  list_load_profiles <- dir("../../Data/temp/load_profile")
  offpeak_evolution_temp <- NULL
  
  for(i in 1:number_country)
  {
    country_temp <- list_country[i]
    list_load_profiles_temp <- list_load_profiles[str_detect(list_load_profiles, country_temp)]
    
    for(j in 1:length(list_load_profiles_temp))
    {
      offpeak_evolution_temp <- readRDS(paste0("../../Data/temp/load_profile/", list_load_profiles_temp[j]))
      
      min_per_usage <- apply(offpeak_evolution_temp, 2, min) 
      min_per_usage <- min_per_usage * 1000000 
      
      min_total <- min(rowSums(offpeak_evolution_temp)) * 1000000
      
      offpeak_evolution[, j+1] <- c(min_per_usage, min_total)
      
    }
    
    saveRDS(offpeak_evolution, file = paste0("../../Data/temp/peak_evolution/", country_temp,"_offpeak_evolution.rds")) 
  }
  
}

peak_composition <- function()
{
  column_type = c(rep("character", times = 9),rep("numeric", times = 8760))
  unitary_load_profile <- read.xlsx2("../../Data/Inputs/Projection Data/Unitary load profile.xlsx", sheetIndex = 1 , colClasses = column_type)
  list_usage <- paste0(unitary_load_profile$Sector, "_", unitary_load_profile$Usage)
  unitary_load_profile$Usage <- list_usage
  
  template_dataframe_year <- readRDS("../../Data/temp/Hist_Macro.RDS")
  list_country <- levels(as.factor(template_dataframe_year$Country))
  number_country <- nlevels(as.factor(template_dataframe_year$Country))
  template_dataframe_year <- template_dataframe_year[1:length(list_usage), 10:length(template_dataframe_year)]
  
  peak_composition <- cbind(unitary_load_profile$Usage, template_dataframe_year)
  names(peak_composition)[1] <- "Usage"
  # peak_composition <- rbind(peak_composition, peak_composition[1,]) 
  # peak_composition$Usage <- as.character(peak_composition$Usage)
  # peak_composition$Usage[nrow(peak_composition)] <- "total"
  # peak_composition$Usage <- as.factor(peak_composition$Usage)
  peak_composition[, 2:length(peak_composition)] <- NA 
  
  list_load_profiles <- dir("../../Data/temp/load_profile")
  peak_composition_temp <- NULL
  
  for(i in 1:number_country)
  {
    country_temp <- list_country[i]
    list_load_profiles_temp <- list_load_profiles[str_detect(list_load_profiles, country_temp)]
    
    for(j in 1:length(list_load_profiles_temp))
    {

      peak_composition_temp <- readRDS(paste0("../../Data/temp/load_profile/", list_load_profiles_temp[j]))
      
      position_peak <- which(rowSums(peak_composition_temp) == max(rowSums(peak_composition_temp)))
      position_peak <- as.numeric(position_peak)[1]
      peak_values <- t(as.matrix(peak_composition_temp[position_peak ,] * 1000000))
      
      
      peak_composition[, j+1] <- as.numeric(peak_values)
      
    }
    
    saveRDS(peak_composition, file = paste0("../../Data/temp/peak_composition/", country_temp,"_peak_composition.rds")) 
  }
  
}


# average_load_profile <- function(hourly_load_profile)
# {
#   norm_day_profile <- read.xlsx2("C:\\Users\\NF5412\\Desktop\\Load_profile_nomenclature.xlsx", sheetIndex = 1)
#   norm_day_profile <- norm_day_profile[ rep( seq_len( nrow( norm_day_profile ) ), 51 ), ]
#   
#   
#   average_hourly_load_profile <- cbind(hourly_load_profile, norm_day_profile)
#   
#   
#   average_hourly_load_profile <- average_hourly_load_profile[, c("Usage", "Hour", "Year", "load_profile")]
#   average_hourly_load_profile$load_profile <- as.numeric(average_hourly_load_profile$load_profile)
#   average_hourly_load_profile <- suppressWarnings(
#                                            aggregate(average_hourly_load_profile,
#                                            by = list(average_hourly_load_profile$Year, average_hourly_load_profile$Hour, average_hourly_load_profile$Usage), 
#                                            FUN = function(x) mean(as.numeric(as.character(x)))
#                                            )
#   )
#   
#   average_hourly_load_profile <- cbind(average_hourly_load_profile[,1:3], average_hourly_load_profile[,"load_profile"]) 
#   names(average_hourly_load_profile) <- c("Year", "Hour", "Usage", "load_profile")  
#   average_hourly_load_profile$Hour <- as.numeric(average_hourly_load_profile$Hour) 
#   average_hourly_load_profile <- average_hourly_load_profile[order(average_hourly_load_profile$Hour),]
#   average_hourly_load_profile <- average_hourly_load_profile[order(average_hourly_load_profile$Year),]
#   
#   return(average_hourly_load_profile)
# }
