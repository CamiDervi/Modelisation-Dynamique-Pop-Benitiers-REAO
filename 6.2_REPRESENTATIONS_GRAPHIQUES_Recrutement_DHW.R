# Script de la modélisation des dynamiques de populations des bénitiers
# Author: Simon VAN WYNSBERGE
#         Camille DERVILLEZ
# Date : 25/03/2025
# R version 4.4.3

rm(list=ls())

# CHARGEMENT DES PACKAGES NECESSAIRES
library(doParallel)
library(MASS)
library(chron)
library(stringr)
library(ncdf4)
library(ggplot2)
library(foreach)


#CREATION D'UN REPERTOIRE DE TRAVAIL

repertoire<-"Q:/Bénitiers Tahiti/R_REAO/INPUT"
output <- "Q:/Bénitiers Tahiti/R_REAO/OUTPUT"
setwd(repertoire)

#PARAMETRISATION

#Paramètres généraux

Nstage  <- seq(from=0,to=20,by=1)    # Nombre de classes de tailles considérées 

load("dates_ech_completees.RData")   # Vecteur des dates de suivis des stocks (vraies dates de suivis + dates intermédiaires ajoutées)
dates_ech

time <- get(load("noms_dates_ech_completees.RData"))  # Vecteur des périodes     NB: 1ère date pour contenir les stocks initiaux
time

#model.type <- "STOCHASTIQUE"
model.type <- "DETERMINISTE"

if (model.type=="DETERMINISTE"){nb.simu <- 1}                             # Nombre de simulations par scenario
if (model.type=="STOCHASTIQUE"){nb.simu <- 10}

#Scenarios de dynamique naturelle

sf <- 2   # Paramètre de Fecondité
e <- 0.2
pB_list <- array(c(0.1,0.4,0.9,0.6,0,0), dim = c(2,3), 
                 dimnames = list(
                   periode = c("2005-2011", "2011-2024"),
                   bassin = c("B1", "B2", "B3")))

d.range <- c(0.005, 0.0001)     # Paramètres de Mortalité : 
#d <- 0.005
alpha <- 0.6279192                                      # mortalité liée à la température avec le paramètre d dans une fonction exponentielle (plus d est grand plus plus la mortalité augmente rapidement avec DHW)
beta <- 543.2006                                        # et mortalité liée au reste avec b  qui suit une loi Gamma de paramètres alpha et beta

mu <- 0.403498533333333                   # Paramètre de Croissance :
sigma <- 0.330429215415904                # c suit une loi Normale de paramètres mu et sigma

# Scenario de gestion de la pêche
T_capture.range <- c(NA)                  # Tailles de pêche règlementée. (NA = structure de taille pêchées telles qu'observées)
Peche.chair.range <- c(1)                 # Réduction d'effort de pêche pour la chair. (1 = tel qu'observée; 0 = interdiction totale de pêcher)
Peche.viv.range <- c(1)                   # Réduction d'effort de pêche de bénitiers vivants. (1 = tel qu'observée; 0 = interdiction totale de pêcher)

#Paramètres connus de dynamiques naturelle

DFG_P1P4_GL <- read.csv2("DFG_P1P4_goodlight.csv")               # Taux de croissance saison chaude dans des bonnes conditions lumineuses
DFG_P2P3_GL <- read.csv2("DFG_P2P3_goodlight.csv")               # Taux de croissance saison froide dans des bonnes conditions lumineuses
DFG_P1P4_LL <- read.csv2("DFG_P1P4_lowlight.csv")                # Taux de croissance saison froide dans des mauvaises conditions lumineuses
DFG_P2P3_LL <- read.csv2("DFG_P2P3_lowlight.csv")                # Taux de croissance saison chaude dans des mauvaises conditions lumineuses

CF <- as.matrix(read.csv2("CF_REAO.csv"))                        # Matrice de contribution relative des différentes classes de tailles à la reproduction


# Paramètres des PU

Tab <-read.csv2("Stock_2005_REAO_2.csv")
Tab <- Tab[order(Tab$PU), ]        # Trie selon PU
rownames(Tab) <- NULL    
Tab$CAPTURES_PER <- Tab$CAPTURES_PER/100

#Paramètres de dynamique des pêches             

# Pêche pour la chair

TQpc <- read.csv2("Qpc_REAO.csv")                                                         # Tableau des quantités de Chair pêchées de 2005 à 2025 : en Kg
TQpc$KG_PERIODE <- (TQpc$KG_PERIODE + (1617 * TQpc$NOMBRE_JOURS / 365))* 10^3             # Bénitiers consommés sur place : 1617 kg/an, proportionnel au nombre de jours
colnames(TQpc)[colnames(TQpc) == "KG_PERIODE"] <- "QPC"                                   # QPC valeurs en grammes
Qpc <- c(TQpc[,"QPC"])                                                                    # Vecteur des quantités de chair péchées 

Tpc <- read.csv2("T_REAO.csv")                                                            # Tailles des bénitiers visés pour la Chair, par les pêcheurs
colnames(Tpc) <- Nstage


# Pêche de bénitiers vivants

TQpv <- read.csv2("Qpv_REAO.csv")                                                #Tableau des quantités de bénitiers vivants pêchées de 2004 à 2015 : en nombre
colnames(TQpv)[colnames(TQpv) == "INDIV_PERIODE"] <- "QPV"
Qpv <- c(TQpv[,"QPV"]) 

Tpv <- read.csv2("T.vivant_REAO.csv")                                            #Tailles des bénitiers vivants visés, par les pêcheurs

colnames(Tpv) <- Nstage


# Relation Taille - Poids commercial en g

Bstage <- (0.03297565*(Nstage)^(2.775841))                                       # Estimées à partir de mes données de taille et poids commercial qui intègrent une variabilité spatiale & temporelle 

# Validation du modèle par suivis des populations

Tab.validation_B1 <- read.csv2("Tableau_validation_B1_REAO_2.csv")
Tab.validation_B1 <- Tab.validation_B1[, -c(1)]
Tab.validation_B2 <- read.csv2("Tableau_validation_B2_REAO_2.csv")
Tab.validation_B2 <- Tab.validation_B2[, -c(1)]
Tab.validation_B3 <- read.csv2("Tableau_validation_B3_REAO_2.csv")
Tab.validation_B3 <- Tab.validation_B3[, -c(1)]
Tab.validation <- rbind(Tab.validation_B1, Tab.validation_B2, Tab.validation_B3)

# Dates des périodes (pour les timeseries de temperature et les nc de DHW)
load("indices_ech_nc_day.RData")
index_ech_day

# Importation des timeseries de températures journalières

stations <- c("T2", "T4", "T6", "T10", "T11")

for (station in stations){
  assign(paste0("fluc_temp_daily_",station), read.csv(paste0("LongTimeseries_df_",station, "_temp_day.csv" )))
}

# Importation des timeseries de DHW journalières

for (station in stations){
  
  assign(paste0("NETCDF_DHW_",station), nc_open(paste0("dhw_sst_daily_",station, ".nc" )))
  
  NETCDF.TIME <-  ncvar_get(get(paste0("NETCDF_DHW_",station)),"time")
  NETCDF.TIME.UNIT <- ncatt_get(get(paste0("NETCDF_DHW_",station)), "time", "units")    
  
  assign(paste0("DWH_", station),ncvar_get(get(paste0("NETCDF_DHW_",station)),"DHW"))      
  nc_close(get(paste0("NETCDF_DHW_",station)))
  
  fluc_dhw <- data.frame(get(paste0("DWH_", station)),NETCDF.TIME)
  colnames(fluc_dhw) <- c("dhw", "time_days")
  
  assign(paste0("fluc_dhw_", station), fluc_dhw)
}
rm(fluc_dhw)

# Aires des trois bassins
Aire_B_list <- c(Aire_B1 = 189300.5, Aire_B2 = 796778.8, Aire_B3 = 60383.29)

#-------------------------------------------------------------------------------------------------------------------------------------------------------


resultats_recrut <- data.frame()


# Définition d'une fonction qui renvoie la matrice de R (Fertilité)
func.R <- function(t,cell,Aire_B_list,pB_list,CF,Tab,sf,e){
  
  # Aire de la PU
  Aire_Pu <- Tab[cell,"AREA"]
  
  # Type de période
  if (t <= 15){ periode = 1}    #Période de 2005 - 2011
  if (t > 15) { periode = 2}    #Période de 2012 - 2024
  
  # Paramétrisation en fonction du bassin, de la zone de SST et de la période
  if (Tab[cell,"ZONE_SURVIE"] == "B1_C") {
    Aire_B = Aire_B_list["Aire_B1"] ;
    pB <- pB_list[periode,"B1"] ;
    station <- "T10"}
  
  if (Tab[cell,"ZONE_SURVIE"] == "B1_F"){ 
    Aire_B = Aire_B_list["Aire_B1"] ;
    pB <- pB_list[periode,"B1"] ;
    station <- "T11"}
  
  if (Tab[cell,"ZONE_SURVIE"] == "B2"){ 
    Aire_B = Aire_B_list["Aire_B2"] ;
    pB <- pB_list[periode,"B2"] ;
    station <- "T2"}
  
  if (Tab[cell,"ZONE_SURVIE"] == "B3_F"){ 
    Aire_B = Aire_B_list["Aire_B3"] ;
    pB <- pB_list[periode,"B3"] ;
    station <- "T4"}
  
  if (Tab[cell,"ZONE_SURVIE"] == "B3_C"){ 
    Aire_B = Aire_B_list["Aire_B3"] ;
    pB <- pB_list[periode,"B3"] ;
    station <- "T6"}
  
  # Déterminer le DHW de la période
  # Extraire les indices de la période t  
  date_ini <- index_ech_day[t-1]
  date_fin <- index_ech_day[t] - 1
  
  #Données de DHW en fonction de la zone de SST
  fluc_dhw <- get(paste0("fluc_dhw_", station))    #extraire dhw de la période
  fluc_dhw <- fluc_dhw[date_ini:date_fin,]  
  
  dhw_max <- max(fluc_dhw$dhw)   #DHW max de la période
  
  # Calcul du recrutement de la PU pour la période données
  R <- sf * CF * exp(dhw_max * e) * pB * (Aire_Pu/Aire_B)
  rownames(R)<- Nstage
  colnames(R)<- Nstage
  
  return(R)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------

for (t in 2:length(time)) {
  for (cell in 1:nrow(Tab)) {
    # Calcule la matrice R
    R_mat <- func.R(t, cell, Aire_B_list, pB_list, CF, Tab, sf, e)
    
    # Récupère la zone de la cellule
    zone <- Tab[cell, "ZONE_SURVIE"]
    
    # Récupère le nom de la station correspondante
    station <- switch(zone,
                      "B1_C" = "T10",
                      "B1_F" = "T11",
                      "B2"   = "T2",
                      "B3_F" = "T4",
                      "B3_C" = "T6")
    
    # Récupère le DHW max de la période
    date_ini <- index_ech_day[t - 1]
    date_fin <- index_ech_day[t] - 1
    fluc_dhw <- get(paste0("fluc_dhw_", station))[date_ini:date_fin, ]
    dhw_max <- max(fluc_dhw$dhw, na.rm = TRUE)
    
    # Somme totale du recrutement (tu peux changer ça selon ce que tu veux tracer)
    R_total <- sum(R_mat)
    
    # Ajoute au df
    resultats_recrut <- rbind(resultats_recrut,
                              data.frame(DHW_max = dhw_max,
                                         Recrutement = R_total,
                                         Cellule = cell,
                                         Temps = t,
                                         Zone = zone))
  }
}

data_bassin1 <- resultats_recrut[resultats_recrut$Zone %in% c("B1_C", "B1_F"), ]
data_bassin1$Temps <- ifelse(data_bassin1$Temps == 17, "2014", "Autres années")

ggplot(data_bassin1, aes(x = DHW_max, y = Recrutement, color = Temps)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("2014" = "blue", "Autres années" = "red")) +
  labs(title = "Recrutement en fonction du DHW max",
       x = "DHW max",
       y = "Recrutement",
       color = "Période") +
  theme_minimal()






