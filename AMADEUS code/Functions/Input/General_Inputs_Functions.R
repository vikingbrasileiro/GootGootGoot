# setwd("C:\\Users\\NF5412\\Desktop\\Local\\Missions\\AMADEUS\\AMADEUS code\\Interface\\")


############################################################### Fonctions concernant import de donnees #############################################################

### Permet de lire un fichier excel dans le dossier inputs #####################################################
Read_Excel_Inputs <- function(base, sector, type_folder, type_file) 
{
  if (type_folder=='Hist')
  {
    if (type_file=="Macro")
    {
      chemin <- paste(base,type_folder,'orical Data/',type_folder,'_',type_file,'.xlsx',sep="")
    }
    else
    {
      chemin <- paste(base,type_folder,'orical Data/',sector,'/',sector,'_',type_folder,'_',type_file,'.xlsx',sep="")
    }
  }
  else
  {
    if (type_file=="Macro")
    {
      chemin <- paste(base,type_folder,'ection Data/',type_folder,'_',type_file,'.xlsx',sep="")
    }
    else if (type_file=="Target")
    {
      chemin <- paste(base,type_folder,'ection Data/',sector,'/',sector,'_',type_folder,'_Assumptions_',type_file,'.xlsx',sep="")
    }
    else
    {
      chemin <- paste(base,type_folder,'ection Data/',sector,'/',sector,'_',type_folder,'_',type_file,'.xlsx',sep="")
    }
  }
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Inputs<-read.xlsx2(chemin, sheetIndex = 1 , colClasses = column_type )
}

### Créer un dataframe pour tous les secteurs en choisissant le type de données : Hist ou Proj et Energy Demand ou Assumptions
Create_Input_Dataframe <- function(choice_type_folder,choice_type_file) 
{
  Input_Dataframe<- NULL
  
  if (choice_type_file=='Macro')
  {
    Input_Dataframe<-rbind(Input_Dataframe,Read_Excel_Inputs(base,list_sectors[1],list_type_folder[choice_type_folder],list_type_file[choice_type_file]))
    return(Input_Dataframe)
  }
  else if (choice_type_file=="Target")
  {
    for (i in 1:length(list_sectors))
    {
      Input_Dataframe<-rbind(Input_Dataframe,Read_Excel_Inputs(base,list_sectors[i],list_type_folder[choice_type_folder],list_type_file[choice_type_file]))
    }
    return(Input_Dataframe)
  }
  else
  {
    for (i in 1:length(list_sectors))
    {
      Input_Dataframe<-rbind(Input_Dataframe,Read_Excel_Inputs(base,list_sectors[i],list_type_folder[choice_type_folder],list_type_file[choice_type_file]))
    }
    return(Input_Dataframe)
  }
}

############################################################### Fonctions concernant extraction de donnees #############################################################

################################################################################################
Extract_Sub_Sector <- function(Input, Choice_Sub_Sector)
{
  Input<-droplevels(subset(Input,Sub_Sector %in% as.character(Choice_Sub_Sector)))
}

################################################################################################

Extract_Item <- function(Input, Choice_Item)
{
  Input<-droplevels(subset(Input,ID_Item %in% as.character(Choice_Item)))
}

################################################################################################

Extract_Country <- function(Input, Choice_Country)
{
  Input<-droplevels(subset(Input,Country %in% as.character(Choice_Country)))
}

################################################################################################

Sort_By_Country <- function(Input)
{
  Input<-Input[order(Input[,"Country"],decreasing = F),]
}

################################################################################################

################################################################################################

Extract_Years <- function(Input, Choice_Starting_Year, Choice_End_Year)
{
  Input<- cbind(Input[,1:9],Input[,(Choice_Starting_Year-(2000-10)):(Choice_End_Year-(2000-10))])
}

################################################################################################

Extract_Sector <- function(Input, Choice_Sector)
{
  Input<-droplevels(subset(Input,Sector %in% as.character(Choice_Sector)))
}

################################################################################################

Extract_Usage <- function(Input, Choice_Usage)
{
  Input<-droplevels(subset(Input,Usage %in% as.character(Choice_Usage)))
}

################################################################################################

Extract_Specific_Energy <- function(Input, Choice_Energy)
{
  Input<-droplevels(subset(Input,Energy %in% as.character(Choice_Energy)))
}

################################################################################################

Extract_Scenario <- function(Input, Choice_Scenario)
{
  Input<-droplevels(subset(Input,Scenario %in% as.character(Choice_Scenario)))
}

################################################################################################

Extract_Specific_Data <- function(Input, Choice_Item, Choice_Scenario, Choice_Country, Choice_Energy, Choice_Start_Year, Choice_End_Year, Choice_Sector, Choice_Sub_Sector, Choice_Usage)
{
  Extract_Item( Extract_Scenario( Extract_Country( Extract_Specific_Energy( Extract_Years( Extract_Sector( Extract_Sub_Sector( Extract_Usage(
    Input
    ,Choice_Usage ),Choice_Sub_Sector ),Choice_Sector ),Choice_Start_Year,Choice_End_Year ),Choice_Energy ),Choice_Country ),Choice_Scenario ),Choice_Item)
}

################################################################################################

############################################################### Fonctions de traitement des donnees #############################################################

#Permet interpolation des hypotheses necessaires a la projection de energy demand ###########################################
Interpolate_Missing_Values <- function(Input)
{
  Header <- Input[,1:9]
  
  Raw_Value <- Input[,10:length(Input)]
  Raw_Value <- t(Raw_Value)
  
  Interpol_Value <- na.approx(Raw_Value)
  
  for(i in names(Interpol_Value))
  {
    Interpol_Value$names(Interpol_Value[i]) <- factor(Interpol_Value$names(Interpol_Value[i]))
  }
  
  Interpol_Value <- t(Interpol_Value)
  colnames(Interpol_Value) <- paste("X",2000:2050, sep = "")
  
  Final_Table <- cbind(Header, Interpol_Value)
}

#Permet de transmormer la table en flat table avec reshape (necessaire pour the use of ggplot) #############################
Transform_to_Flat_Table <-function(Input)
{ 
  
  Flat_Table <- reshape(Input, direction = "long", varying = colnames(Input)[10:length(Input)], timevar = "Year", sep = "")
  Flat_Table <- Flat_Table[,-12] #on se debarasse de la derniere colonne issue de reshape inutile pour nous 
  
  #on extrait les dernieres colonnes pour transformation en type numeric, necessaire pour bon fonctionnement de ggplot 
  Year <- as.numeric(Flat_Table$Year) 
  Value <- as.numeric(as.character(Flat_Table$X))
  
  #on redefinit la falt table avec les bons types et en utilisant cbind, qui permet d'avoir un dataframe utilisable par ggplot  
  Flat_Table <- cbind(Flat_Table[,1:9], Year, Value)
  
}


# Permet de creer les listes de selction pour the interface en fonction du type de donnees choisies ########################
Create_list_input <- function(Input_File, Choice_list)
{
  list_input_final <- levels(as.factor(Input_File[,Choice_list]))
  list_input_final <- list_input_final[order(list_input_final, decreasing = FALSE)]
  list_input_final <- list_input_final[list_input_final!=""]
  
  return(list_input_final)
}

# Permet de charger le fichier selectionne par the user in the interface
Choice_Input_File <- function(type_file, 
                              Hist_Macro, 
                              Proj_Macro, 
                              Hist_Assumption,
                              Proj_Target,
                              Proj_Assumption,
                              Hist_Energy_Demand)
{
  if(type_file =="Hist_Energy"){Input_File <- Hist_Energy_Demand}
  else if(type_file =="Hist_Macro"){Input_File <- Hist_Macro}
  else if(type_file =="Proj_Macro"){Input_File <- Proj_Macro}
  else if(type_file =="Proj_Target"){Input_File <- Proj_Target}
  else if(type_file =="Hist_Assumption"){Input_File <- Hist_Assumption}
  else if(type_file =="Proj_Assumption"){Input_File <- Proj_Assumption}
}

# Permet the analysis of incoherent starting points
Hist_Last_Year_Analysis <- function(Input, No_Year_Average)
{
  List_Year_Data_Presence <- sapply(Input[,20:length(Input)], function(x){is.nan(x)})
  List_Year_Data_Presence <- apply(List_Year_Data_Presence,1, function(x){2000+(min(which(x))-2)})
  
  #Definition of year interval
  Start_Year_Average <- List_Year_Data_Presence - No_Year_Average
  End_Year_Average <- List_Year_Data_Presence
  
  #Transformation in a dataframe
  Year_df <- data.frame(Start_Year = Start_Year_Average, End_Year = End_Year_Average)
  
  #Extraction des X dernieres annees avant le starting point pour chaque ligne  
  df_average <- NULL
  for (i in 1:length(rownames(Input)))
  {
    df_average<- rbind(df_average, Extract_Years_Analysis(Input[i,], Year_df[i,"Start_Year"], Year_df[i,"End_Year"]))
  }
  
  #On calcule la valeur extrapole en fonction des dernieres annes avant le starting point (moyenne des X dernieres annees) pour chaque ligne 
  Last_Year_Value_Extrap <- apply(df_average[(length(df_average)-No_Year_Average):(length(df_average)-1)], 1, mean)
  
  #Calcul of the standard deviation pour chaque ligne
  Standard_Dev_Value <- apply(df_average[(length(df_average)-No_Year_Average):(length(df_average)-1)], 1, sd)
  
  # On en deduit un intervalle de confiance pour la valeur du starting point pour chaque ligne
  min_interval_Value <- Last_Year_Value_Extrap - 4*Standard_Dev_Value
  max_interval_Value <- Last_Year_Value_Extrap + 4*Standard_Dev_Value
  
  # on recupere la valeur du starting point pour chaque ligne 
  Last_Year_Value <- df_average[,length(df_average)]
  
  #creation de la table finale avec Starting_Point_Value, Startuing_Point_value_extrap, intervalle de confiance    
  Final_df <- cbind(Input[,1:9], Last_Year_Value, Last_Year_Value_Extrap, min_interval_Value, max_interval_Value)
  
  #On filtre les lignes ou le starting point is not in the confidence interval 
  Final_df <- Final_df[min_interval_Value>Last_Year_Value | max_interval_Value<Last_Year_Value,]
}

# Permet de extract les annees avant le starting point of a given line (can be different depending of the line)
Extract_Years_Analysis <- function(Input, Choice_Starting_Year, Choice_End_Year)
{
  Input<- cbind(Input[,1:9],Input[,(Choice_Starting_Year-(2000-10)):(Choice_End_Year-(2000-10))])
  
  for(i in 1:(Choice_End_Year+1-Choice_Starting_Year))
  {
    names(Input)[9+i] <- as.character(i)
  }
  
  return(Input)
}

# Permet de passer la table energy demand en TWh NVC
ktoe_to_TWh <- function(Energy_Input)
{
  #Puis on met le tout en TWh
  Energy_Input[,10:length(Energy_Input)] <- Energy_Input[,10:length(Energy_Input)]*11.628/1000
  Energy_Input[,"Unit"] <- "TWh"
  return(Energy_Input)
}  

#Permet de resizer une table ou une variable ne depend pas de the energy en une table ou la memem ligne est repete pour chaque energy (permet le calcul direct en multipliant les tables)
Reshape_Good_Size <- function(Input_table_by_energy, Input_Table_to_reshape) 
{
  Good_size_Table<-NULL
  
  for (i in 1:length(rownames(Input_table_by_energy)))
  {
    Good_size_Table<-rbind(Good_size_Table,Extract_Country(Input_Table_to_reshape,Input_table_by_energy[i,"Country"]))
  }
  return(Good_size_Table)
}  

derivate_dataframe <- function(input_df)
{
  derivate_df <- input_df
  index_i = 11:length(input_df)
  derivate_df[,index_i] <- input_df[, index_i] / input_df[,index_i-1] - 1
  
  derivate_df[,10] <- NaN
  derivate_df[is.na(derivate_df)] <- 0
  
  derivate_df[derivate_df$ID_Item == "Share of VA IND" & is.nan(derivate_df$X2050), 11:length(derivate_df)] <- 0
  
  return(derivate_df)
}

delta_dataframe <- function(input_df)
{
  delta_df <- input_df
  index_i = 11:length(input_df)
  delta_df[,index_i] <- input_df[, index_i] - input_df[,index_i-1]
  
  delta_df[,10] <- NaN
  
  # derivate_df[derivate_df$ID_Item == "Share of VA IND" & is.nan(derivate_df$X2050), 11:length(derivate_df)] <- 0
  
  return(delta_df)
}

apply_delta_for_shares <- function(input_hist_df, input_proj_df) # A finir
{
  input_hist_df <- Extract_Item( input_hist_df, c("Share of VA IND", "share_VA_sector", "tax") )
  input_proj_df <- Extract_Item( input_proj_df, c("Share of VA IND", "share_VA_sector", "tax") )
  
  input_hist_df <- input_hist_df[order(input_hist_df$Sector),]
  input_hist_df <- input_hist_df[order(input_hist_df$Sub_Sector),]
  input_hist_df <- Sort_By_Country(input_hist_df)
  input_hist_df <- input_hist_df[order(input_hist_df$ID_Item),]
  
  input_proj_df <- input_proj_df[order(input_proj_df$Sector),]
  input_proj_df <- input_proj_df[order(input_proj_df$Sub_Sector),]
  input_proj_df <- Sort_By_Country(input_proj_df)
  input_proj_df <- input_proj_df[order(input_proj_df$ID_Item),]
  
  
  delta <- delta_dataframe( input_proj_df )
  year_proj <- as.data.frame( sapply( input_hist_df,  function(x) {!is.nan(x)} ) )
  
  final_df <- input_hist_df
  for(i in 10:length(input_hist_df))
  {
    for(j in 1:length(rownames(input_hist_df)))
    {
      if(year_proj[j , i]) final_df[j , i] <- input_hist_df[j , i]
      else final_df[j , i] <- final_df[j , i - 1] + delta[j , i] 
    }
  }
  
  return(final_df)
} 


apply_growth_rate <- function(input_hist_df, input_proj_df) # A finir
{
  input_hist_df <- Extract_Item( input_hist_df, c("Market shares", "av_floor_dw", "Household_size", "GDP", "Population", "VA intensity of prod") )
  input_proj_df <- Extract_Item( input_proj_df, c("Market shares", "av_floor_dw", "Household_size", "GDP", "Population", "VA intensity of prod") )
  
  input_hist_df <- input_hist_df[order(input_hist_df$Sector),]
  input_hist_df <- input_hist_df[order(input_hist_df$Sub_Sector),]
  input_hist_df <- Sort_By_Country(input_hist_df)
  input_hist_df <- input_hist_df[order(input_hist_df$ID_Item),]
  
  input_proj_df <- input_proj_df[order(input_proj_df$Sector),]
  input_proj_df <- input_proj_df[order(input_proj_df$Sub_Sector),]
  input_proj_df <- Sort_By_Country(input_proj_df)
  input_proj_df <- input_proj_df[order(input_proj_df$ID_Item),]
  
  
  growth_rate <- derivate_dataframe( input_proj_df )
  year_proj <- as.data.frame( sapply( input_hist_df,  function(x) {!is.nan(x)} ) )
  
  final_df <- input_hist_df
  for(i in 10:length(input_hist_df))
  {
    for(j in 1:length(rownames(input_hist_df)))
    {
      if(year_proj[j , i]) final_df[j , i] <- input_hist_df[j , i]
      else final_df[j , i] <- final_df[j , i - 1] * (1+ growth_rate[j , i]) 
    }
  }
  
  return(final_df)
} 

apply_growth_rate_production <- function(input_hist_df, input_proj_df) # A finir
{
  input_hist_df <- input_hist_df[order(input_hist_df$Sector),]
  input_hist_df <- input_hist_df[order(input_hist_df$Sub_Sector),]
  input_hist_df <- Sort_By_Country(input_hist_df)
  input_hist_df <- input_hist_df[order(input_hist_df$ID_Item),]
  
  input_proj_df <- input_proj_df[order(input_proj_df$Sector),]
  input_proj_df <- input_proj_df[order(input_proj_df$Sub_Sector),]
  input_proj_df <- Sort_By_Country(input_proj_df)
  input_proj_df <- input_proj_df[order(input_proj_df$ID_Item),]
  
  
  growth_rate <- derivate_dataframe( input_proj_df )
  year_proj <- as.data.frame( sapply( input_hist_df,  function(x) {!is.nan(x)} ) )
  
  final_df <- input_hist_df
  for(i in 10:length(input_hist_df))
  {
    for(j in 1:length(rownames(input_hist_df)))
    {
      if(year_proj[j , i]) final_df[j , i] <- input_hist_df[j , i]
      else final_df[j , i] <- final_df[j , i - 1] * (1+ growth_rate[j , i]) 
    }
  }
  
  return(final_df)
} 

apply_growth_rate_market_shares <- function(input_hist_df, input_proj_df) 
{
  
  input_hist_df <- Sort_By_Country(input_hist_df)
  input_hist_df <- input_hist_df[order(as.factor(input_hist_df$Usage)),]
  input_hist_df <- input_hist_df[order(as.factor(input_hist_df$Energy)),]
  
  input_proj_df <- Sort_By_Country(input_proj_df)
  input_proj_df <- input_proj_df[order(as.factor(input_proj_df$Usage)),]
  input_proj_df <- input_proj_df[order(as.factor(input_proj_df$Energy)),]
  
  
  #delta <- delta_dataframe( input_proj_df )
  #delta[is.na(delta)] <- 0 
  year_proj <- min(which(is.nan(colSums(input_hist_df[10:length(input_hist_df)])))) + 9
  
  final_df <- input_hist_df
  final_df[,year_proj:length(final_df)] <- input_proj_df[,year_proj:length(input_proj_df)]
  final_df <- Interpolate_Missing_Values(final_df)
  
  # for(i in 10:length(input_hist_df))
  # {
  #   for(j in 1:length(rownames(input_hist_df)))
  #   {
  #     if(year_proj[j , i]) final_df[j , i] <- input_hist_df[j , i]
  #     else final_df[j , i] <- final_df[j , i - 1] + delta[j , i] 
  #   }
  # }
  
  return(final_df)
}

apply_growth_rate_equipment_rate <- function(input_hist_df, input_proj_df) # A finir
{
  
  input_hist_df <- Sort_By_Country(input_hist_df)

  input_proj_df <- Sort_By_Country(input_proj_df)
  
  growth_rate <- derivate_dataframe( input_proj_df )
  year_proj <- as.data.frame( sapply( input_hist_df,  function(x) {!is.nan(x)} ) )
  
  final_df <- input_hist_df
  for(i in 10:length(input_hist_df))
  {
    for(j in 1:length(rownames(input_hist_df)))
    {
      if(year_proj[j , i]) final_df[j , i] <- input_hist_df[j , i]
      else final_df[j , i] <- final_df[j , i - 1] * (1+ growth_rate[j , i]) 
    }
  }
  
  return(final_df)
}

calculate_macro_data <- function(input_macro)
{
  #calcul nombre de menages 
  average_household_size <- Sort_By_Country(Extract_Item(input_macro, "Household_size"))
  population <- Sort_By_Country(Extract_Item(input_macro, "Population"))
  
  Number_household <- average_household_size
  Number_household$ID_Item <- "No_Household"
  Number_household$Unit <- "Thousand"
  Number_household[,10:length(Number_household)] <- population[,10:length(population)] / average_household_size[,10:length(average_household_size)]
  
  #calcul surface totale residential 
  average_dwelling_area <- Extract_Item(input_macro,"av_floor_dw")
  average_dwelling_area <- Sort_By_Country(average_dwelling_area)
  
  Total_Surface_RES <- average_dwelling_area
  Total_Surface_RES$ID_Item <- "Floor Area of residential"
  Total_Surface_RES$Unit <- "Mm²"
  Total_Surface_RES[,10:length(Total_Surface_RES)] <- Number_household[,10:length(Number_household)]*average_dwelling_area[,10:length(average_dwelling_area)]/1000 #on divise par 1000 pour mettre le resultat en Mm², le nombre de household etant deja en millers
  Total_Surface_RES <- Sort_By_Country(Total_Surface_RES)
  
  #calcul GDP per capita
  GDP <- Sort_By_Country(Extract_Item(input_macro, "GDP"))
  
  GDP_per_capita <- GDP
  GDP_per_capita$ID_Item <- "GDP per capita"
  GDP_per_capita$Unit <- "k$/cap"
  GDP_per_capita[,10:length(GDP_per_capita)] <- GDP[,10:length(GDP)]/population[,10:length(population)] 
  GDP_per_capita <- Sort_By_Country(GDP_per_capita)
  
  # Calcul de la valeur ajoutee par pays = GDP*(1-tax)
  tax_rate <- Sort_By_Country(Extract_Item(input_macro, "tax"))
  Value_Added <-  GDP
  Value_Added$ID_Item <-"Value Added"
  Value_Added[,10:length(Value_Added)] <- GDP[,10:length(GDP)]*(1-tax_rate[,10:length(tax_rate)]) 
  Value_Added <- Sort_By_Country(Value_Added)
  
  # Calcul de la valeur ajoutee par secteur
  share_VA_sector <- Sort_By_Country(Extract_Item(input_macro, "share_VA_sector")) 
  Value_Added_Sector <- Value_Added[ rep( seq_len( nrow( Value_Added ) ), nlevels( as.factor(share_VA_sector$Sector) ) ), ]
  Value_Added_Sector <- Sort_By_Country(Value_Added_Sector)
  Value_Added_Sector$Sector <- share_VA_sector$Sector
  Value_Added_Sector[,10:length(Value_Added_Sector)] <- share_VA_sector[,10:length(share_VA_sector)]*Value_Added_Sector[,10:length(Value_Added_Sector)]
  
  #Calcul Value Added par branche industrielle 
  Value_Added_IND <- Sort_By_Country(Extract_Sector(Value_Added_Sector, "IND"))
  share_VA_IND_branch <- Sort_By_Country(Extract_Item(input_macro, "Share of VA IND"))
  
  Value_Added_IND_branch <- Value_Added_IND[ rep( seq_len( nrow( Value_Added_IND ) ), nlevels( as.factor(share_VA_IND_branch$Sub_Sector) ) ), ]
  Value_Added_IND_branch <- Sort_By_Country(Value_Added_IND_branch)
  Value_Added_IND_branch$Sub_Sector <- share_VA_IND_branch$Sub_Sector
  Value_Added_IND_branch[,10:length(Value_Added_IND_branch)] <- share_VA_IND_branch[,10:length(share_VA_IND_branch)]*Value_Added_IND_branch[,10:length(Value_Added_IND_branch)]
  
  input_macro <- unique(rbind(input_macro, Number_household, Total_Surface_RES, GDP_per_capita, Value_Added, Value_Added_Sector, Value_Added_IND_branch))
  
  return(input_macro)
}

calculate_proj_macro_data <- function(input_proj_macro, Hist_Macro, Proj_Assumption)
{
  #calcul nombre de menages
  average_household_size <- Sort_By_Country(Extract_Item(input_proj_macro, "Household_size"))
  population <- Sort_By_Country(Extract_Item(input_proj_macro, "Population"))

  Number_household <- average_household_size
  Number_household$ID_Item <- "No_Household"
  Number_household$Unit <- "Thousand"
  Number_household[,10:length(Number_household)] <- population[,10:length(population)] / average_household_size[,10:length(average_household_size)]

  #calcul surface totale residential
  average_dwelling_area <- Extract_Item(input_proj_macro,"av_floor_dw")
  average_dwelling_area <- Sort_By_Country(average_dwelling_area)

  Total_Surface_RES <- average_dwelling_area
  Total_Surface_RES$ID_Item <- "Floor Area of residential"
  Total_Surface_RES$Unit <- "Mm²"
  Total_Surface_RES[,10:length(Total_Surface_RES)] <- Number_household[,10:length(Number_household)]*average_dwelling_area[,10:length(average_dwelling_area)]/1000 #on divise par 1000 pour mettre le resultat en Mm², le nombre de household etant deja en millers
  Total_Surface_RES <- Sort_By_Country(Total_Surface_RES)
  
  #calcul GDP per capita
  GDP <- Sort_By_Country(Extract_Item(input_proj_macro, "GDP"))
  
  GDP_per_capita <- GDP
  GDP_per_capita$ID_Item <- "GDP per capita"
  GDP_per_capita$Unit <- "k$/cap"
  GDP_per_capita[,10:length(GDP_per_capita)] <- GDP[,10:length(GDP)]/population[,10:length(population)] 
  GDP_per_capita <- Sort_By_Country(GDP_per_capita)
  
  # Calcul de la valeur ajoutee par pays = GDP*(1-tax)
  tax_rate <- Sort_By_Country(Extract_Item(input_proj_macro, "tax"))
  Value_Added <-  GDP
  Value_Added$ID_Item <-"Value Added"
  Value_Added[,10:length(Value_Added)] <- GDP[,10:length(GDP)]*(1-tax_rate[,10:length(tax_rate)]) 
  Value_Added <- Sort_By_Country(Value_Added)
  
  # Calcul de la valeur ajoutee par secteur
  share_VA_sector <- Sort_By_Country(Extract_Item(input_proj_macro, "share_VA_sector")) 
  Value_Added_Sector <- Value_Added[ rep( seq_len( nrow( Value_Added ) ), nlevels( as.factor(share_VA_sector$Sector))  ), ]
  Value_Added_Sector <- Sort_By_Country(Value_Added_Sector)
  Value_Added_Sector$Sector <- share_VA_sector$Sector
  Value_Added_Sector[,10:length(Value_Added_Sector)] <- share_VA_sector[,10:length(share_VA_sector)]*Value_Added_Sector[,10:length(Value_Added_Sector)]
  
  
  #calcul surface totale residential (à patir de surface /VA_TER)
  TER_Area_per_VA_indice <- Sort_By_Country( Extract_Item( Extract_Sector(Proj_Assumption, "TER"), "floor_area_per_VA" ) )
  TER_Area_per_VA_indice <- derivate_dataframe(TER_Area_per_VA_indice)

  TER_Proj_VA <- Sort_By_Country( Extract_Sector(Value_Added_Sector, "TER") )
  TER_Hist_Total_Surface <- Sort_By_Country( Extract_Item(Hist_Macro, "Floor area of services") )
  TER_Hist_Area_per_VA <- TER_Hist_Total_Surface
  TER_Hist_Area_per_VA[,10:length(TER_Hist_Area_per_VA)] <- TER_Hist_Total_Surface[,10:length(TER_Hist_Total_Surface)] / TER_Proj_VA[,10:length(TER_Proj_VA)]

  TER_Proj_Total_Surface <- TER_Hist_Total_Surface
  
  Year_Proj_col = min(which(is.nan(colSums(TER_Hist_Total_Surface[10:length(TER_Hist_Total_Surface)])))) + 9
  
  for(i in Year_Proj_col:length(TER_Hist_Total_Surface))
  {
    TER_Hist_Area_per_VA[,i] <- TER_Hist_Area_per_VA[,i-1] * ( 1 + TER_Area_per_VA_indice[,i])
    TER_Proj_Total_Surface[,i] <- TER_Hist_Area_per_VA[,i] * TER_Proj_VA[,i]
  }
  
  #Calcul Value Added par branche industrielle 
  Value_Added_IND <- Sort_By_Country(Extract_Sector(Value_Added_Sector, "IND"))
  share_VA_IND_branch <- Sort_By_Country(Extract_Item(input_proj_macro, "Share of VA IND"))
  
  Value_Added_IND_branch <- Value_Added_IND[ rep( seq_len( nrow( Value_Added_IND ) ), nlevels(as.factor(share_VA_IND_branch$Sub_Sector)) ), ]
  Value_Added_IND_branch <- Sort_By_Country(Value_Added_IND_branch)
  Value_Added_IND_branch$Sub_Sector <- share_VA_IND_branch$Sub_Sector
  Value_Added_IND_branch[,10:length(Value_Added_IND_branch)] <- share_VA_IND_branch[,10:length(share_VA_IND_branch)]*Value_Added_IND_branch[,10:length(Value_Added_IND_branch)]
  Value_Added_IND_branch <- Sort_By_Country(Value_Added_IND_branch[order(Value_Added_IND_branch$Sub_Sector),])
  
  Value_Added_IND_branch <- droplevels(subset(Value_Added_IND_branch, Sub_Sector != "rest") )
  
  #Calcul Productivity par branche industrielle
  Production <- Extract_Item(Hist_Macro, "Indice_Prod")
  Production <- Sort_By_Country(Production[order(Production$Sub_Sector),])
  
  #On enlève histo production dans fichier origine 
  #input_proj_macro <- droplevels( subset(input_proj_macro, ID_Item != "Indice_Prod") )

  intensity_VA_prod <- Extract_Item(input_proj_macro, "VA intensity of prod")
  intensity_VA_prod <- Sort_By_Country(intensity_VA_prod[order(intensity_VA_prod$Sub_Sector),])
  intensity_VA_prod <- droplevels(subset(intensity_VA_prod, Sub_Sector != "Industry - energy") )

  Production_Proj <- Production
  Production_Proj[,10:length(Production_Proj)] <- Value_Added_IND_branch[,10:length(Value_Added_IND_branch)] / intensity_VA_prod[,10:length(intensity_VA_prod)]
  Production_Proj <- apply_growth_rate_production(Production, Production_Proj)
  Production_Proj[is.na(Production_Proj$X2050) , 11:length(Production_Proj)] <- 100
  
  input_proj_macro <- unique(rbind(input_proj_macro, Number_household, Total_Surface_RES, GDP_per_capita, Value_Added, Value_Added_Sector, Value_Added_IND_branch, Production_Proj, TER_Proj_Total_Surface))
  
  return(input_proj_macro)
}
  
calculate_proj_assumption_data <- function(input_proj_assumption, input_proj_macro)
{
  Production <- Extract_Item(input_proj_macro, "Indice_Prod")
  activity_rate_IND <- derivate_dataframe(Production)
  activity_rate_IND$ID_Item <- "Activity Rate"
  activity_rate_IND$Unit <- "%"
  
  input_proj_assumption <- rbind(input_proj_assumption, activity_rate_IND) #, activity_rate_RES_Lighting)
  
  return(input_proj_assumption)
}

transform_to_effect_table <- function(input, year_proj_col)
{
  y_dataset <- input[,1:10]
  names(y_dataset)[10] = "Value"
  
  y_dataset[y_dataset$ID_Item == "Energy Demand", 10] <- input[input$ID_Item == "Energy Demand", year_proj_col]
  
  y_dataset[y_dataset$ID_Item == "Efficiency Energy Demand", 10] <- input[input$ID_Item == "Efficiency Energy Demand", length(input)] - input[input$ID_Item == "Efficiency Energy Demand", year_proj_col]
  #y_dataset[y_dataset$ID_Item == "Efficiency Energy Demand", 1] <- "Efficiency Volume"
  
  y_dataset[y_dataset$ID_Item == "Activity Energy Demand", 10] <- input[input$ID_Item == "Activity Energy Demand", length(input)] - input[input$ID_Item == "Activity Energy Demand", year_proj_col]
  # y_dataset[y_dataset$ID_Item == "Activity Energy Demand", 1] <- "Activity Volume"
  
  y_dataset[y_dataset$ID_Item == "Substitution Energy Demand", 10] <- input[input$ID_Item == "Substitution Energy Demand", length(input)] - input[input$ID_Item == "Substitution Energy Demand", year_proj_col]
  # y_dataset[y_dataset$ID_Item == "Substitution Energy Demand", 1] <- "Substitution Volume"
  
  
  y_dataset <- as.data.frame(tapply(y_dataset[,10], y_dataset$ID_Item, sum))
  names(y_dataset) <- "Value"
  
  end_energy_demand_value <- data.frame(Value = sum(y_dataset$Value))
  y_dataset <- rbind(y_dataset, end_energy_demand_value)
  
  rownames(y_dataset) <- seq_along(y_dataset$Value) 
  
   y_dataset$end <- cumsum(y_dataset$Value)
   y_dataset$end <- c(head(y_dataset$end, -1), 0)
   y_dataset$start <- c(0, head(y_dataset$end, -1))
   y_dataset$id <- seq_along(y_dataset$Value)
   y_dataset$ID_Item <- c("Energy Demand in 2016", "Efficiency Volume", "Activity Volume", "Substitution Volume", "Energy Demand in 2050")
  
  return(y_dataset)
}

energy_color <- function(input_energy_choice)
{
  energy_color_palette = data.frame(Energy = c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"), Color = c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold"))
  
  energy_color_set = droplevels(subset(energy_color_palette, Energy %in% input_energy_choice))
  return(as.character(energy_color_set$Color))  
}

sector_color <- function(input_sector_choice)
{
  sector_color_palette = data.frame(Sector = c("AGR", "ESU", "IND", "NEU", "RES", "TER", "TRA"), Color = c("#00b050", "#8064a2", "#c0504d", "#4a452a","#f79646", "#fac090", "#4f81bd"))
  
  sector_color_set = droplevels(subset(sector_color_palette, Sector %in% input_sector_choice))
  return(as.character(sector_color_set$Color))  
}

energy_selected <- function(input_energy_choice)
{
  energy_color_palette = data.frame(Energy = c("biomass and waste", "coal", "direct hydrogen", "electricity", "final heat", "gas", "oil", "plug-in hybrid", "self-recharge hybrid", "total"), Color = c("#96be0f", "#8d8d8d", "#dc143c", "#7dceff","#f4aacd", "#0078be", "#f07d00", "#910f7d", "#007fbf", "gold"))
  
  energy_color_set = droplevels(subset(energy_color_palette, Energy %in% input_energy_choice))
  return(as.character(energy_color_set$Energy))  
}

hover_text <- function(input_display_choice, Flat_DF)
{
  if(input_display_choice == "Country") output = Flat_DF$Country
  if(input_display_choice == "ID_Item") output = Flat_DF$ID_Item
  if(input_display_choice == "Energy") output = Flat_DF$Energy
  if(input_display_choice == "Sector") output = Flat_DF$Sector
  if(input_display_choice == "Sub_Sector") output = Flat_DF$Sub_Sector
  if(input_display_choice == "Usage") output = Flat_DF$Usage
  if(input_display_choice == "Scenario") output = Flat_DF$Scenario
  
  return(output)
}

odysee_shares_checking <- function(Hist_Assumption, Hist_Energy_Demand)
{
  share_RES_TER_Usage <- Extract_Item(Hist_Assumption, "share_usage")
  share_RES_TER_Usage <- droplevels(subset(share_RES_TER_Usage, Energy!="direct hydrogen"))
  
  share_RES_TER_test <- data.frame(Value = rowMeans(share_RES_TER_Usage[,10:length(share_RES_TER_Usage)], na.rm = T))
  share_RES_TER_test <- cbind(share_RES_TER_Usage[,1:9], share_RES_TER_test)
  share_RES_TER_test <- share_RES_TER_test[share_RES_TER_test[,10] == 0,]
  share_RES_TER_test <- share_RES_TER_test[, -10] 
  
  share_RES_TER_to_modify <- unique(merge(share_RES_TER_Usage, share_RES_TER_test, by = c("ID_Item","Scenario", "Country","Energy", "Sector", "Sub_Sector", "Usage","Source", "Unit")))
  share_RES_TER_to_modify <- Extract_Usage(share_RES_TER_to_modify, "total") 
  share_RES_TER_to_modify$Sector <- tolower(share_RES_TER_to_modify$Sector) 
  
  column_type = c(rep("character", times = 9),rep("numeric", times = 51))
  Total_Eurostat_Data <-read.xlsx2("../../Data/Inputs/Benchmarks Data/Eurostat_Total_Energy_Demand.xlsx", sheetIndex = 2 , colClasses = column_type )
  Total_Eurostat_Data <- Extract_Sector(Total_Eurostat_Data, c("res", "ter"))


  Total_Eurostat_Data <- ktoe_to_TWh(Sort_By_Country(Total_Eurostat_Data))
  Total_Eurostat_Data <- Total_Eurostat_Data[order(Total_Eurostat_Data$Energy),]
  Total_Eurostat_Data <- Total_Eurostat_Data[order(Total_Eurostat_Data$Sector),]

  Total_Eurostat_Data_test <- unique(merge(share_RES_TER_to_modify, Total_Eurostat_Data, by = c("Country","Energy", "Sector"), suffixes = c(".x", "")))
  Total_Eurostat_Data_test <- Total_Eurostat_Data_test[,-(4:66)]
  Total_Eurostat_Data_test <- Total_Eurostat_Data_test[rowMeans(Total_Eurostat_Data_test[,4:length(Total_Eurostat_Data_test)], na.rm = T) != 0 ,]
  Total_Eurostat_Data_test_header <- Hist_Energy_Demand[1:nrow(Total_Eurostat_Data_test), 1:9]
  Total_Eurostat_Data_test_header$Country <-  Total_Eurostat_Data_test$Country
  Total_Eurostat_Data_test_header$Energy <-  Total_Eurostat_Data_test$Energy
  Total_Eurostat_Data_test_header$Sector <-  toupper(Total_Eurostat_Data_test$Sector)
  Total_Eurostat_Data_test_header$Sub_Sector <- "total"
  Total_Eurostat_Data_test_header$Usage <- "total"
  
  final_table <- cbind(Total_Eurostat_Data_test_header, Total_Eurostat_Data_test[,-(1:3)])
  
  return(final_table)
}

odysee_shares_fixing <- function(Hist_Assumption, Hist_Energy_Demand)
{
  list_shares_to_fix <- odysee_shares_checking(Hist_Assumption, Hist_Energy_Demand)
  list_shares_to_fix <- list_shares_to_fix[ rep( seq_len( nrow( list_shares_to_fix ) ), 2 ), ]
  list_shares_to_fix <- list_shares_to_fix[order(list_shares_to_fix$Sector),]
  list_shares_to_fix <- list_shares_to_fix[order(list_shares_to_fix$Energy),]
  list_shares_to_fix <- list_shares_to_fix[order(list_shares_to_fix$Country),]
  list_shares_to_fix$Usage <- rep(c("space heating", "water heating"), nrow(list_shares_to_fix)/2)
  list_shares_to_fix$Sub_Sector <- "na"
  list_shares_to_fix[list_shares_to_fix$Sector =="TER",]$Sub_Sector <- "Buildings"
  list_shares_to_fix$ID_Item <- "share_usage"
  
  for(i in 1:nrow(list_shares_to_fix))
  {
    row_to_fix <- which(Hist_Assumption$ID_Item == as.character(list_shares_to_fix$ID_Item[i]) & Hist_Assumption$Country == as.character(list_shares_to_fix$Country[i]) & Hist_Assumption$Energy == as.character(list_shares_to_fix$Energy[i]) & Hist_Assumption$Sector == as.character(list_shares_to_fix$Sector[i]) & Hist_Assumption$Usage == as.character(list_shares_to_fix$Usage[i]))
    
    if(as.character(Hist_Assumption[row_to_fix, "Usage"]) == "space heating")
    {
      Hist_Assumption[row_to_fix, 10:length(Hist_Assumption)] <- 2/3 
    }
    else # water heating
    {
      Hist_Assumption[row_to_fix, 10:length(Hist_Assumption)] <- 1/3 
    }
  }
  
  for(i in 1:nrow(list_shares_to_fix))
  { 
    #erreur ici hermence, pas de secteur RES en espagne pour i=1
    row_to_fix <- which(Hist_Energy_Demand$ID_Item == "Energy Demand" & Hist_Energy_Demand$Country == as.character(list_shares_to_fix$Country[i]) & Hist_Energy_Demand$Energy == as.character(list_shares_to_fix$Energy[i]) & Hist_Energy_Demand$Sector == as.character(list_shares_to_fix$Sector[i]) & Hist_Energy_Demand$Usage == "total")
    Hist_Energy_Demand[row_to_fix, 10:length(Hist_Energy_Demand)] <- list_shares_to_fix[i, 10:length(list_shares_to_fix)] 

    row_to_fix_Energy <- which(Hist_Energy_Demand$ID_Item == "Energy Demand" & Hist_Energy_Demand$Country == as.character(list_shares_to_fix$Country[i]) & Hist_Energy_Demand$Energy == as.character(list_shares_to_fix$Energy[i]) & Hist_Energy_Demand$Sector == as.character(list_shares_to_fix$Sector[i]) & Hist_Energy_Demand$Usage == as.character(list_shares_to_fix$Usage[i]))
    row_to_fix_Assumption <- which(Hist_Assumption$ID_Item == as.character(list_shares_to_fix$ID_Item[i]) & Hist_Assumption$Country == as.character(list_shares_to_fix$Country[i]) & Hist_Assumption$Energy == as.character(list_shares_to_fix$Energy[i]) & Hist_Assumption$Sector == as.character(list_shares_to_fix$Sector[i]) & Hist_Assumption$Usage == as.character(list_shares_to_fix$Usage[i]))
    Hist_Energy_Demand[row_to_fix_Energy, 10:length(Hist_Energy_Demand)] <- Hist_Energy_Demand[row_to_fix, 10:length(Hist_Energy_Demand)] * Hist_Assumption[row_to_fix_Assumption, 10:length(Hist_Assumption)] 
  }
  
  saveRDS(Hist_Assumption, "../../Data/temp/Hist_Assumption.Rds")
  saveRDS(Hist_Energy_Demand, "../../Data/temp/Hist_Energy_Demand.Rds")

}

list_country_modeling <- function(modeling_choice)
{
  country_classification <-  read.xlsx2("../../Data/Inputs/Nomenclature Data/Country Classification.xlsx", sheetIndex = 1)
  country_classification <- country_classification[country_classification$Country_Modelling == modeling_choice,]
  country_classification_name <- as.character(country_classification$Country_Name)
  
  country_classification_name_list <- country_classification_name[1]
  
  for(i in 2:length(country_classification_name))
  {
    country_classification_name_list <- paste0(country_classification_name_list, "</br>", country_classification_name[i])
  }
  
  return(country_classification_name_list)
}
