# Script pour compléter les tableaux de caractéristiques des PU et créer les tableaux de validation
# Author: Camille DERVILLEZ
# Date : 25/03/2025
# R version 4.4.3

rm(list=ls())

# CHARGEMENT DES PACKAGES NECESSAIRES
library(dplyr)
library(tidyr)
library(stringr)


# CREATION D'UN REPERTOIRE DE TRAVAIL
repertoire<-"Q:/Bénitiers Tahiti/R_REAO/INPUT"
output <- "Q:/Bénitiers Tahiti/R_REAO/OUTPUT"
setwd(repertoire)


#-----------------------------------------------------------------------------------------------------------------------------------------------
# IMPORTATION DES DONNEES
  
  # Df qui contiendra les densités & structure de taille dans chaque PU pour aout 2005 (données initiales)
  Stock_2005_lagon <- read.csv2("Autres excel/Tab_Spatial_REAO_1.csv", sep = ";", dec = ".", header = TRUE, fileEncoding = "latin1", stringsAsFactors = TRUE)
  #Stock_2005_lagon <- Stock_2005_lagon[,-c(4:5)]

  # Df qui contiendra toutes les caractéristiques de chaque PU (pour l'instant juste effort de pêche et aire)
  PU_Carac <- read.csv2("Autres excel/PU_Carac_1.csv", sep = ";", dec = ".", header = TRUE, fileEncoding = "latin1", stringsAsFactors = TRUE)

  # Suivi de D & ST de 2005 par GRIDCODE
  Stock2005 <- read.csv("Stocks/Stock_2005.csv", sep = ";", dec = ".", header = TRUE, fileEncoding = "latin1", stringsAsFactors = TRUE)

  # Suivis de D entre 2005 et 2024
  densite_B1 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_B1.csv")
  densite_B2 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_B2.csv")
  densite_B3 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_B3.csv")

  # Suivis de ST entre 2005 et 2024
  S_T_B1 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_B1.csv")
  S_T_B2 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_B2.csv")
  S_T_B3 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_B3.csv")
  
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  
# REMPLISSAGE DES CARACTERISTIQUES DES PU
  
  # 1 - Type Profondeur en fonction du GRIDCODE
  PU_Carac$Type_prof = NA
  PU_Carac <- PU_Carac %>%
    dplyr::select(1:3, Type_prof, everything())
  
  PU_Carac[PU_Carac$GRIDCODE == 11, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 30, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 14, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 15, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 20, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 16, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 21, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 29, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 100, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 101, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 102, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 103, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 104, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 105, "Type_prof"] <- "S"
  PU_Carac[PU_Carac$GRIDCODE == 106, "Type_prof"] <- "S"
  
  PU_Carac[PU_Carac$GRIDCODE == 18, "Type_prof"] <- "P"
  PU_Carac[PU_Carac$GRIDCODE == 108, "Type_prof"] <- "P"
  PU_Carac[PU_Carac$GRIDCODE == 24, "Type_prof"] <- "P"
  PU_Carac[PU_Carac$GRIDCODE == 107, "Type_prof"] <- "P"
  PU_Carac[PU_Carac$GRIDCODE == 108, "Type_prof"] <- "P"
  PU_Carac[PU_Carac$GRIDCODE == 109, "Type_prof"] <- "P"
  PU_Carac[PU_Carac$GRIDCODE == 110, "Type_prof"] <- "P"

  
  # 2 - Densités initiales dans les PU en fonction de l'habitat et du bassin
  PU_Carac$Type_dens = NA
  PU_Carac <- PU_Carac %>%
    dplyr::select(1:4, Type_dens, everything())
  
  PU_Carac[PU_Carac$GRIDCODE == 107 , "Type_dens"] <- "D1"
  PU_Carac[PU_Carac$GRIDCODE == 24 , "Type_dens"] <- "D2"
  PU_Carac[PU_Carac$GRIDCODE == 20 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D3"
  PU_Carac[PU_Carac$GRIDCODE == 18 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D4"
  PU_Carac[PU_Carac$GRIDCODE == 11 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D5"
  PU_Carac[PU_Carac$GRIDCODE == 16 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D6"
  PU_Carac[PU_Carac$GRIDCODE == 30 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D7"
  PU_Carac[PU_Carac$GRIDCODE == 15 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D8"
  PU_Carac[PU_Carac$GRIDCODE == 103 & PU_Carac$Bassin_shp == "B1", "Type_dens"] <- "D9"
  
  PU_Carac[PU_Carac$GRIDCODE == 108 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D10"
  PU_Carac[PU_Carac$GRIDCODE == 109 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D11"
  PU_Carac[PU_Carac$GRIDCODE == 106 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D12"
  PU_Carac[PU_Carac$GRIDCODE == 14 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D13"
  PU_Carac[PU_Carac$GRIDCODE == 110 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D14"
  PU_Carac[PU_Carac$GRIDCODE == 29 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D15"
  PU_Carac[PU_Carac$GRIDCODE == 100 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D16"
  PU_Carac[PU_Carac$GRIDCODE == 105 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D17"
  PU_Carac[PU_Carac$GRIDCODE == 21 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D18"
  PU_Carac[PU_Carac$GRIDCODE == 102 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D19"
  PU_Carac[PU_Carac$GRIDCODE == 104 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D20"
  PU_Carac[PU_Carac$GRIDCODE == 101 & (PU_Carac$Bassin_shp == "B2" | PU_Carac$Bassin_shp == "B3"), "Type_dens"] <- "D21"

  # réatribuer les PU à la limite entre deux bassins
  PU_Carac[PU_Carac$GRIDCODE == 16 & PU_Carac$Bassin_shp == "B2", "Type_dens"] <- "D6"
  PU_Carac[PU_Carac$GRIDCODE == 110 & (PU_Carac$Bassin_shp == "B1"), "Type_dens"] <- "D14"

write.csv2(PU_Carac, file = "Autres excel/PU_Carac_2.csv")
  
#-----------------------------------------------------------------------------------------------------------------------------------------------


# CREATION DU DF DES STOCKS INITIAUX DE 2005 POUR TOUTES LES PU
 
 # 1- Ajouter les types de densités de chaque PU au tableau qui contiendra les stocks initiaux et effort de peche
  Stock_2005_lagon <- merge(Stock_2005_lagon, PU_Carac[, c("PU", "Type_dens")], by.x = "PU", by.y = "PU", all.x = TRUE)

 # 2 - Ajouter les densités correspondantes pour chaque PU au tableau qui contiendra les stocks initiaux
  Stock_2005_lagon <- merge(Stock_2005_lagon, Stock2005[,c(1, 4, 6:28)], by = "Type_dens", all.x = TRUE)
  Stock_2005_lagon <- Stock_2005_lagon[c(2:8, 1, 9:ncol(Stock_2005_lagon))]
  
 # 3 - Renommer les colonnes uand c'est nécessaire
  colnames(Stock_2005_lagon)[colnames(Stock_2005_lagon) == "Densite_2005_mean..ind.m2."] <- "Dmean"
  colnames(Stock_2005_lagon)[colnames(Stock_2005_lagon) == "Densite_2005_IC...ind.m2."] <- "Dsd"

write.csv2(Stock_2005_lagon, file = "Stock_2005_REAO_2.csv", row.names= F)

#-----------------------------------------------------------------------------------------------------------------------------------------------


# CREATION DES TABLEAUX DE VALIDATION

  # 1 - Filtrage des PU suivies entre 2005 et 2024
  PU_validation <- PU_Carac
  
  PU_validation <- PU_Carac[PU_Carac$GRIDCODE == 101 & PU_Carac$Bassin_shp =="B3" |
                       PU_Carac$GRIDCODE == 21 & PU_Carac$Bassin_shp =="B2" |
                       PU_Carac$GRIDCODE == 107 & PU_Carac$Bassin_shp =="B2" |
                       PU_Carac$GRIDCODE == 105 & PU_Carac$Bassin_shp =="B2" |  
                       PU_Carac$GRIDCODE == 106 & PU_Carac$Bassin_shp =="B2" |  
                       PU_Carac$GRIDCODE == 110 & PU_Carac$Bassin_shp =="B2" |  
                       PU_Carac$GRIDCODE == 18 & PU_Carac$Bassin_shp =="B1" | 
                       PU_Carac$GRIDCODE == 30 & PU_Carac$Bassin_shp =="B1" |  
                       PU_Carac$GRIDCODE == 11 & PU_Carac$Bassin_shp =="B1", ]
  print(PU_validation$PU)
  
  
  # 2 - Attribution de la station de suivi correspondante à chaque PU suivie
  PU_validation$Stations <- NA
  PU_validation <- PU_validation[, c(1:3, ncol(PU_validation), 4:(ncol(PU_validation)-1))]
    
  PU_validation[PU_validation$GRIDCODE == 101 & PU_validation$Bassin_shp =="B3", "Stations"] <- "S12"
  PU_validation[PU_validation$GRIDCODE == 21 & PU_validation$Bassin_shp =="B2","Stations"] <-  "S26 & S33"
  PU_validation[PU_validation$GRIDCODE == 107 & PU_validation$Bassin_shp =="B2","Stations"] <-  "S30"                    
  PU_validation[PU_validation$GRIDCODE == 105 & PU_validation$Bassin_shp =="B2","Stations"] <-  "S39 & S41"
  PU_validation[PU_validation$GRIDCODE == 106 & PU_validation$Bassin_shp =="B2","Stations"] <-  "S39 & S41"
  PU_validation[PU_validation$GRIDCODE == 110 & PU_validation$Bassin_shp =="B2","Stations"] <-  "S43"
  PU_validation[PU_validation$GRIDCODE == 18 & PU_validation$Bassin_shp =="B1","Stations"] <-  "S44"
  PU_validation[PU_validation$GRIDCODE == 30 & PU_validation$Bassin_shp =="B1","Stations"] <-  "S45"
  PU_validation[PU_validation$GRIDCODE == 11 & PU_validation$Bassin_shp =="B1","Stations"] <-  "S48"
    
  PU_validation <- PU_validation[, c("PU", "AREA", "Stations", "GRIDCODE", "Zone_SST", "Bassin_shp"), drop = FALSE]


  # 3 - Préparation df des densités des stations suivies entre 2005 & 2024
    # Filtrer données inutiles ici
  densite_B1 <- densite_B1[, -((ncol(densite_B1)-2):ncol(densite_B1))]
  densite_B2 <- densite_B2[, -((ncol(densite_B2)-2):ncol(densite_B2))]
  densite_B3 <- densite_B3[, -((ncol(densite_B3)-2):ncol(densite_B3))]

    # Moyenne stations en double : stations S26 & S33
  for (campagne in unique(densite_B2$Campagnes)) {
    donnees_double <- subset(densite_B2, Campagnes == campagne & Stations %in% c("S26", "S33"))
    
    Dmean_mean <- mean(donnees_double$Dmean) 
    Dsd_mean <- mean(donnees_double$Dsd) 
     
    mean_row <- data.frame(Campagnes = campagne, Stations = "S26 & S33", Dmean = Dmean_mean, Dsd = Dsd_mean) 
      
    densite_B2 <- rbind(densite_B2, mean_row)
    densite_B2 <- subset(densite_B2, !(Campagnes == campagne & Stations %in% c("S26", "S33")))
  }

    # Moyenne stations en double : stations S39 & S41
  for (campagne in unique(densite_B2$Campagnes)) {
    donnees_double <- subset(densite_B2, Campagnes == campagne & Stations %in% c("S39", "S41"))
      
    Dmean_mean <- mean(donnees_double$Dmean) 
    Dsd_mean <- mean(donnees_double$Dsd) 
      
    mean_row <- data.frame(Campagnes = campagne, Stations = "S39 & S41", Dmean = Dmean_mean, Dsd = Dsd_mean) 
      
    densite_B2 <- rbind(densite_B2, mean_row)
    densite_B2 <- subset(densite_B2, !(Campagnes == campagne & Stations %in% c("S39", "S41")))
  }
  
  
   # 4 - Répartir les PU suivies selon leur bassin
 PU_B1 <- PU_validation[PU_validation$Bassin_shp =="B1",]
 PU_B2 <- PU_validation[PU_validation$Bassin_shp =="B2",]
 PU_B3 <- PU_validation[PU_validation$Bassin_shp =="B3",]

   # 5 - Ajout des densités aux PU suivies
 PU_B1_D <- merge(PU_B1, densite_B1, by = "Stations", all.x = TRUE)
 PU_B2_D <- merge(PU_B2, densite_B2, by = "Stations", all.x = TRUE)
 PU_B3_D <- merge(PU_B3, densite_B3, by = "Stations", all.x = TRUE)

   # 6 - Préparation df des fréquences de taille des stations suivies entre 2005 & 2024
 S_T_B1$NOMBRE <- NULL
 S_T_B2$NOMBRE <- NULL
 S_T_B3$NOMBRE <- NULL

 S_T_B1 <- S_T_B1 %>%
   mutate(FREQ = FREQ / 100) %>%  # Conversion en proportions
   pivot_wider(names_from = TAILLE, values_from = FREQ, names_prefix = "Frequence_Taille_")
 colnames(S_T_B1)[-1] <- paste0(colnames(S_T_B1)[-1], "cm")

 S_T_B2 <- S_T_B2 %>%
   mutate(FREQ = FREQ / 100) %>%  # Conversion en proportions
   pivot_wider(names_from = TAILLE, values_from = FREQ, names_prefix = "Frequence_Taille_")
 colnames(S_T_B2)[-1] <- paste0(colnames(S_T_B2)[-1], "cm")

 S_T_B3 <- S_T_B3 %>%
   mutate(FREQ = FREQ / 100) %>%  # Conversion en proportions
   pivot_wider(names_from = TAILLE, values_from = FREQ, names_prefix = "Frequence_Taille_")
 colnames(S_T_B3)[-1] <- paste0(colnames(S_T_B3)[-1], "cm")

 S_T_B1$Bassin_shp <-"B1"
 S_T_B2$Bassin_shp <-"B2"
 S_T_B3$Bassin_shp <-"B3"

  # 7 - Ajout des Structures de Taille aux PU suivies
 PU_B1_D_ST <- merge(PU_B1_D, S_T_B1, by = c("Bassin_shp", "Campagnes"), all.x = TRUE)
 PU_B2_D_ST <- merge(PU_B2_D, S_T_B2, by = c("Bassin_shp", "Campagnes"), all.x = TRUE)
 PU_B3_D_ST <- merge(PU_B3_D, S_T_B3, by = c("Bassin_shp", "Campagnes"), all.x = TRUE)

  # 8 - Supprimer les lignes pour aout 2005
 PU_B1_D_ST <- PU_B1_D_ST[PU_B1_D_ST$Campagnes != "aout 2005",]
 PU_B2_D_ST <- PU_B2_D_ST[PU_B2_D_ST$Campagnes != "aout 2005",]
 PU_B3_D_ST <- PU_B3_D_ST[PU_B3_D_ST$Campagnes != "aout 2005",]

  # 9 - Ajouter les données des Stocks 2005 pour la première campagne (et non pas des suivis des stations)
 Aout_2005 <- subset(Stock_2005_lagon, Stock_2005_lagon$PU %in% PU_validation$PU)

 Aout_2005 <- Aout_2005 %>%
  rename_with(
    .fn = ~ gsub("^Freq_(\\d+cm)_2005$", "Frequence_Taille_\\1", .x),
    .cols = starts_with("Freq_")
  )

  colnames(Aout_2005)[colnames(Aout_2005) == "Densite_2005_mean..ind.m2."] <- "Dmean"
  colnames(Aout_2005)[colnames(Aout_2005) == "Densite_2005_IC...ind.m2."] <- "Dsd"
  Aout_2005 <- Aout_2005[,-c(7,8)]

  Aout_2005$Campagnes <- "aout 2005"
  Aout_2005 <- Aout_2005[, c(1, ncol(Aout_2005), 2:(ncol(Aout_2005) - 1))]

  Aout_2005_B1 <- subset(Aout_2005, Aout_2005$PU %in% PU_B1$PU)
  Stock_valid_B1 <- bind_rows(PU_B1_D_ST, Aout_2005_B1)
  Stock_valid_B1 <- Stock_valid_B1[,-c(31:34)]
  
  Aout_2005_B2 <- subset(Aout_2005, Aout_2005$PU %in% PU_B2$PU)
  Stock_valid_B2 <- bind_rows(PU_B2_D_ST, Aout_2005_B2)
  Stock_valid_B2 <- Stock_valid_B2[,-c(31:34)]
  
  Aout_2005_B3 <- subset(Aout_2005, Aout_2005$PU %in% PU_B3$PU)
  Stock_valid_B3 <-  bind_rows(PU_B3_D_ST, Aout_2005_B3)
  Stock_valid_B3 <- Stock_valid_B3[,-c(31:34)]


  # 10 - Renommer les noms des campagnes
 
 v_campagnes <- c("AO2005", "J2010", "D2013", "AV2016", "S2016", "MAR2017", "D2017", "MAR2021", "AV2022", "O2022",  "MAI2023", "N2023", "MAR2024", "MAI2024", "S2024")
 
 for (bassin in 1:3){
   Tab <- get(paste0("Stock_valid_B", bassin))
   
   colnames(Tab)[colnames(Tab) == "Campagnes"] <- "CAMPAGNE"
   
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "aout 2005", "AO2005")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "juil-10", "J2010")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "decembre 2013", "D2013")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "avr-16", "AV2016")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "sept-16", "S2016")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "mars-17", "MAR2017")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "decembre 2017", "D2017")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "mars-21", "MAR2021")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "avr-22", "AV2022")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "oct-22", "O2022")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "mai-23", "MAI2023")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "oct-nov 2023", "N2023")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "mars-24", "MAR2024")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "mai-24", "MAI2024")
   Tab$CAMPAGNE <- str_replace_all(Tab$CAMPAGNE, "sept-24", "S2024")
   
   Tab$CAMPAGNE <- factor(Tab$CAMPAGNE, levels = v_campagnes, ordered = TRUE)
   Tab<- Tab[order(Tab$CAMPAGNE, Tab$PU), ]
   Tab <- Tab[, -1]
   Tab <- Tab[, c(4, setdiff(1:ncol(Tab), 4))]
   
   assign(paste0("Stock_valid_B", bassin), Tab)
 }
 
 
 # 10 - Réorganisation des colonnes
 Stock_valid_B1 <- Stock_valid_B1[, c(2, 4, 1, 5, 3, setdiff(1:ncol(Stock_valid_B1), c(1, 2, 3, 4, 5)))]
 Stock_valid_B2 <- Stock_valid_B2[, c(2, 4, 1, 5, 3, setdiff(1:ncol(Stock_valid_B1), c(1, 2, 3, 4, 5)))]
 Stock_valid_B3 <- Stock_valid_B3[, c(2, 4, 1, 5, 3, setdiff(1:ncol(Stock_valid_B1), c(1, 2, 3, 4, 5)))]
  
 
 # 11 - Sauvegarde des fichiers de validation par bassin
 write.csv2(Stock_valid_B1, file = "Tableau_validation_B1_REAO_2.csv")
 write.csv2(Stock_valid_B2, file = "Tableau_validation_B2_REAO_2.csv")
 write.csv2(Stock_valid_B3, file = "Tableau_validation_B3_REAO_2.csv")
 
 
 




