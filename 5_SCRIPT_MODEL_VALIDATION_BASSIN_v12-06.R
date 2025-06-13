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

sf.range <- c(10)   # Paramètre de Fecondité
e <- 1
pB_list <- array(c(0,0.4,1,0.6,0,0), dim = c(2,3), 
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
# DEFINITION DES FONCTIONS INTERMEDIAIRES

# Definition d'une fonction qui calcule une température moyenne dans les 5 zones pour une période donnée
func.temp.periode <- function(t){
  
  temp_list <- c()
  
  for (station in stations){
    fluc_temp <- get(paste0("fluc_temp_daily_", station))
    date_ini <- index_ech_day[t-1]
    date_fin <- index_ech_day[t] - 1
    fluc_temp <- fluc_temp[date_ini:date_fin, ]
    
    temp_list[paste0("temp_", station)] <- mean(fluc_temp$TEMP_predite_mean)
  }
  
  return(temp_list)       # renvoie un vecteur avec les 5 températures des 5 zones
} 


# Définition d'une fonction qui renvoie le nombre de jours écoulés entre deux périodes
func.days <- function(t){
  
  date_ini <- index_ech_day[t-1]
  date_fin <- index_ech_day[t] - 1
  Njours <- as.numeric(date_fin - date_ini)
  
  return(Njours)
}


# Définition de la fonction qui calcule la mortalité liée au facteur DHW de deux manières différentes
func.morta.DHW <- function(t,d){
  
  #Extraire les indices de la période t  
  date_ini <- index_ech_day[t-1]
  date_fin <- index_ech_day[t] - 1
  
  # Mortalité calculée avec le DHW max #
    morta_dhw_max_list <- c()
   
    #Calcul de la mortalité liée au DHW max de la période t 
    for (station in stations){ 
      fluc_dhw <- get(paste0("fluc_dhw_", station))    #extraire dhw de la période
      fluc_dhw <- fluc_dhw[date_ini:date_fin,]          
      
      dhw_max <- max(fluc_dhw$dhw)
      morta_dhw_max <- exp(d * dhw_max) - 1 
      
      #Liste mortalités pour les différentes stations
      morta_dhw_max_list[paste0("morta_dhw_max_", station)] <- morta_dhw_max
    }
    
    return(morta_dhw_max = morta_dhw_max_list)
  }
  
  
# Définition de la fonction qui calcule la matrice de croissance 
Generate.G.matrix <- function(DF.G,temp,Njours){
  
  # Identification de la taille maximale (en cm) considérée dans les modèles à partir de la data.frame DF.G  
  Linf <- max(DF.G$ShellLength)/10
  
  # Initialisation de la matrice
  matrixG <- matrix(NA,nrow=(Linf)+1,ncol=(Linf)+1)
  rownames(matrixG) <- c(0:(Linf))
  colnames(matrixG) <- c(0:(Linf))
  
  # On remplie la matrice colonne par colonne
  for (size.start in colnames(matrixG)){
    
    # Simulation d'un portée de 1000 individus qui démarrent à la taille size.start
    DF.s <- data.frame(abs(runif(1000,as.numeric(size.start)-0.5,as.numeric(size.start)+0.5)))
    colnames(DF.s) <- "sizes.init"
    DF.s$sizes.final <- rep(NA,dim(DF.s)[1])
    
    # Calcul de leur taille finale 3 mois après à partir de la matrice de croissance fournie en input
    for (ind in 1:dim(DF.s)[1]){
      DF.s[ind,"sizes.final"] <- DF.s[ind,"sizes.init"]+((DF.G[(DF.G$Temperature==round(temp,1))&(DF.G$ShellLength==as.numeric(size.start)*10),"G"]*Njours)/10)
    }
    
    # Remplissage de la colonne de la matrice, ligne par ligne 
    for (size.end in rownames(matrixG)){
      matrixG[size.end,size.start] <- length(DF.s$sizes.final[(DF.s$sizes.final>=(as.numeric(size.end)-0.5))&(DF.s$sizes.final<(as.numeric(size.end)+0.5))])/length(DF.s$sizes.final)
    }
  } 
  
  # La fonction renvoie la matrix G en sortie 
  return(matrixG)
}  


# Definition d'une fonction qui renvoie les cinq matrices de croissance G pour les 5 zones
func.G.periode <- function(t,c){
  
  G_list <- list()
  
  # Matrice de la croissance
  if (grepl("P1|P4", time[t])) {
    DF.G.GL <- get("DFG_P1P4_GL")      #Récup taux de croissance periode P1_P4 en bonnes conditions lumineuses
    DF.G.LL <- get("DFG_P1P4_LL")}     #Récup taux de croissance periode P1_P4 en mauvaises conditions lumineuses
  
  if (grepl("P2|P3", time[t])) {
    DF.G.GL <- get("DFG_P2P3_GL")      #Récup taux de croissance periode P2_P3 en bonnes conditions lumineuses
    DF.G.LL <- get("DFG_P2P3_LL")}     #Récup taux de croissance periode P2_P3 en mauvaises conditions lumineuses
  
  temp_T2 <- func.temp.periode(t)[1]
  temp_T4 <- func.temp.periode(t)[2]
  temp_T6 <- func.temp.periode(t)[3]
  temp_T10 <- func.temp.periode(t)[4]
  temp_T11 <- func.temp.periode(t)[5]
  
  Njours <- func.days(t)
  
  for (station in stations){
    temp <- get(paste0("temp_", station))
    G.GL <- Generate.G.matrix(DF.G.GL,temp,Njours)        #Création de la matrice de croissance Goodlight
    G.LL <- Generate.G.matrix(DF.G.LL,temp,Njours)        #Création de la matrice de croissance Lowlight
    G <- c * G.GL + (1-c) * G.LL
    
    G_list[[paste0("G_", station)]] <- G
  }
  
  return(G_list)
} 


# Definition d'une fonction qui crée la matrice de la mortalité Y    
func.Y.periode <- function(t,b,d){
  
  Y_list_max <- list()
  
  # Initialisation de la matrice max
  matrixY_max <- matrix(NA, nrow = 21,ncol = 21)
  rownames(matrixY_max) <- c(0:20)
  colnames(matrixY_max) <- c(0:20)
  
  Njours <- func.days(t)
  morta_dhw_max_list <- func.morta.DHW(t,d)
  
  for (station in stations){
    morta_dhw_max <- morta_dhw_max_list[paste0("morta_dhw_max_", station)]
    
    for (l in 1:21){
      M <- b + morta_dhw_max
      S <- exp(-M * Njours)
      matrixY_max[, l] <- S 
    }
    Y_list_max[[paste0("Y_", station, "_max")]] <- matrixY_max
    
  }
  return(Y_list_max)    
} 


# Definition d'une fonction qui renvoie la matrice M (Croissance & Mortalité)
func.M.periode <- function(t,b,c,d){
  
  M_list_max <- list()
  G_list <- func.G.periode(t,c)
  Y_list_max <- func.Y.periode(t,b,d)
  
  for (station in stations){
    G <- G_list[[paste0("G_", station)]]
    Y <- Y_list_max[[paste0("Y_", station, "_max")]]
    M <- (G*Y)
    
    rownames(M)<-Nstage
    colnames(M)<-Nstage
    
    M_list_max[[paste0("M_", station, "_max")]] <- M
  }
  
  return(M_max = M_list_max)
} 


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


# Définition d'une fonction qui calcule le nombre d'individus pêchés pour la chair par classe de taille et par PU en fonction d'une valeur de biomasse pêchée
func.NPC <- function(Qpc,t,Tpc,Tab,cell,Bstage,T_capture){  
  
  NC <- array(NA,c(1,length(Nstage),(dim(Tab)[1])+1))
  dimnames(NC)[1] <- list(c("Nb"))
  dimnames(NC)[2] <- list(c(Nstage))
  dimnames(NC)[3] <- list(c(1:dim(Tab)[1],"Total"))
  
  NC["Nb",,"Total"] <- unlist((Qpc[t]/sum(Bstage*Tpc))*Tpc)   # Nombre d'individus pêchés par taille
  
  for (pu in 1:dim(Tab)[1]){
    NC[,,pu] <- NC[,,"Total"] * Tab[pu,"CAPTURES_PER"]
  }
  
  return(NC["Nb",,cell])
}


# Définition d'une fonction qui calcule le nombre de bénitiers pêchés vivants  par classe de pêcheur, par taille, et par PU 
func.NPV <- function (Qpv,t,Tpv,Tab,cell){    
  
  NV <- array(NA,c(1,length(Nstage),(dim(Tab)[1])+1))
  dimnames(NV)[1] <- list(c("Nb"))
  dimnames(NV)[2] <- list(c(Nstage))
  dimnames(NV)[3] <- list(c(1:(dim(Tab)[1]),"Total"))
  
  NV["Nb",,"Total"] <- unlist(Qpv[t] * Tpv)   # Nombre d'individus pêchés par taille
  
  for (pu in 1:(dim(Tab)[1])){
    NV[,,pu] <- NV[,,"Total"] * Tab[pu,"CAPTURES_PER"]
  }
  
  return(NV["Nb",,cell])
}



#-------------------------------------------------------------------------------------------------------------------------------------------------------
# DEFINITION FONCTION CALCUL DES EFFECTIFS  

# Création d'une fonction qui calcule les effectifs au cours du temps
func.dynam.max<-function(Tab,b,c,d,sf,e,CF,time,Qpc,Tpc,Qpv,Tpv,Bstage,Peche.chair,Peche.viv,T_capture){
  
  start_time <- Sys.time()
  
  #Création de la matrice de sortie
  
  Stock<-array(NA,c(length(Nstage)+1,length(time),dim(Tab)[1]+1))
  dimnames(Stock)[1] <- list(c(Nstage,"Total"))
  dimnames(Stock)[2] <- list(time)
  dimnames(Stock)[3] <- list(c(1:dim(Tab)[1],"Total"))
  
  #Calcul des effectifs initiaux No
  
  for (cell in 1:dim(Tab)[1]){
    if (model.type=="STOCHASTIQUE"){d.mean <- rnorm(1,Tab[cell,"Dmean"], Tab[cell,"Dsd"])}
    if (model.type=="DETERMINISTE"){d.mean <- Tab[cell,"Dmean"]}
    if (d.mean<0){d.mean<-0}
    
    Stock[,time[1],cell] <- c(Tab[cell,"Freq_0cm_2005"], Tab[cell,"Freq_1cm_2005"],Tab[cell,"Freq_2cm_2005"],Tab[cell,"Freq_3cm_2005"],Tab[cell,"Freq_4cm_2005"],Tab[cell,"Freq_5cm_2005"],
                              Tab[cell,"Freq_6cm_2005"],Tab[cell,"Freq_7cm_2005"],Tab[cell,"Freq_8cm_2005"],Tab[cell,"Freq_9cm_2005"],Tab[cell,"Freq_10cm_2005"],Tab[cell,"Freq_11cm_2005"],
                              Tab[cell,"Freq_12cm_2005"],Tab[cell,"Freq_13cm_2005"],Tab[cell,"Freq_14cm_2005"],Tab[cell,"Freq_15cm_2005"],Tab[cell,"Freq_16cm_2005"],Tab[cell,"Freq_17cm_2005"],
                              Tab[cell,"Freq_18cm_2005"],Tab[cell,"Freq_19cm_2005"],Tab[cell,"Freq_20cm_2005"],0) *d.mean*Tab[cell,"AREA"]
    Stock[dim(Stock)[1],1,cell] <- sum(Stock[,1,cell])
  }
  
  Stock[,,dim(Stock)[3]] <- apply(Stock[,,1:dim(Tab)[1]],c(1,2),sum,na.rm=T)  #na.rm=T
  
  #Calcul des effectifs au cours du temps
  
  for (t in 2:length(time)){                     #length(time)
    print(paste0("stock calculé : ",time[t]))
    
    M_list <- func.M.periode(t,b,c,d)
    
    for (cell in 1:dim(Tab)[1]){  
      print(paste0("cell : ", cell))
      
      R <- func.R(t,cell,Aire_B_list,pB_list,CF,Tab,sf,e)
      
      if (Tab[cell,"ZONE_SURVIE"] == "B2"){M <- M_list[[1]]}
      if (Tab[cell,"ZONE_SURVIE"] == "B3_F"){ M <- M_list[[2]]}
      if (Tab[cell,"ZONE_SURVIE"] == "B3_C"){ M <- M_list[[3]]}
      if (Tab[cell,"ZONE_SURVIE"] == "B1_C"){ M <- M_list[[4]]}
      if (Tab[cell,"ZONE_SURVIE"] == "B1_F"){ M <- M_list[[5]]}
      
      Stock[1:length(Nstage),time[t],cell] <- round((M %*% as.matrix(Stock[1:length(Nstage),time[t-1],cell]))
                                                    + apply(R %*% as.matrix(Stock[1:length(Nstage),time[t-1],"Total"]),1,FUN=rpois,n=1)
                                                    - func.NPC(Qpc,t,Tpc,Tab,cell,Bstage,T_capture) * Peche.chair
                                                    - func.NPV(Qpv,t,Tpv,Tab,cell) * Peche.viv,0)
    }                                                                                                                                                                                                                                                                                    
    
    Stock[Stock<0] <- 0                                                                                                      # Dans le cas où les effectifs passent en négatif, on les remet à 0
    Stock[dim(Stock)[1],time[t],1:dim(Tab)[1]]<-apply(Stock[1:length(Nstage),time[t],1:dim(Tab)[1]],2,sum)                   # Calcul des stock totaux par PU (total de bénitiers toutes tailles confondues par période)
    Stock[,,dim(Stock)[3]] <- apply(Stock[,,1:dim(Tab)[1], drop = FALSE],c(1,2),sum)                                         # Calcul des stock totaux par lagon (total bénitiers toutes tailles par période)
    
  }
  
  end_time <- Sys.time() 
  print(paste("Temps d'exécution :", round(difftime(end_time, start_time, units = "secs"), 2), "secondes"))
  
  return(Stock)}


if (model.type=="STOCHASTIQUE"){b.range <- rgamma(nb.simu, shape = alpha, rate = beta)}
if (model.type=="DETERMINISTE"){b.range <- rep(rgamma(1, shape = alpha, rate = beta),nb.simu)}

if (model.type=="STOCHASTIQUE"){c.range <- pmin(pmax(rnorm(nb.simu, mean = mu, sd = sigma), 0), 1)}
if (model.type=="DETERMINISTE"){c.range <- pmin(pmax(rnorm(nb.simu, mean = mu, sd = 0), 0), 1)}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
#SIMULATIONS : METHODE MAX

e.range = c(1)
sf.range = c(5,10,15)
d.range = c(0.001,0.005)

TAB_max <- expand.grid("sf"=sf.range,
                       "d"=d.range,
                       "e"=e.range,
                       "Peche_chair"=Peche.chair.range,
                       "Peche_viv"=Peche.viv.range,
                       "T_capture"=T_capture.range,
                       "SIMU"=1:nb.simu,
                       stringsAsFactors=FALSE)

TAB_max$b <- b.range[TAB_max$SIMU] 
TAB_max$c <- c.range[TAB_max$SIMU]
TAB <- TAB_max
TAB

#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Paramètres de la parallélisation
n.cores <- parallel::detectCores() - 1
cl <- makeCluster(n.cores)
registerDoParallel(cl)

# Lancement simulations en parallèle
system.time({
  all.sims.max <- foreach(x = 1:nrow(TAB), 
                          .packages = c("doParallel", "MASS", "chron", "stringr", "ncdf4", "ggplot2", "foreach"),
                          .export = c("DFG_P1P4_GL", "DFG_P1P4_LL", "DFG_P2P3_GL", "DFG_P2P3_LL", 
                                      "fluc_dhw_T2", "fluc_dhw_T4", "fluc_dhw_T6", "fluc_dhw_T10", "fluc_dhw_T11",
                                      "fluc_temp_daily_T2", "fluc_temp_daily_T4", "fluc_temp_daily_T6", "fluc_temp_daily_T10", "fluc_temp_daily_T11" )) %dopar% {
                                        func.dynam.max(Tab, TAB$b[x], TAB$c[x], TAB$d[x], TAB$sf[x], TAB$e[x], CF, time, Qpc, Tpc, Qpv, Tpv, Bstage,
                                                       TAB$Peche_chair[x], TAB$Peche_viv[x], TAB$T_capture[x])}})


#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Mise en forme des résultats des simulations

all.sims = all.sims.max

names(all.sims) <- paste("sf_",TAB$sf,"_e_",TAB$e, "_d_",TAB$d,"_Peche_chair_",TAB$Peche_chair,"_Peche_viv_",TAB$Peche_viv,"_T_capture_",TAB$T_capture,"_SIMU_",TAB$SIMU,sep="")[1:nrow(TAB)]
scenarios <- unique(paste("sf_",TAB$sf,"_e_",TAB$e, "_d_",TAB$d,"_Peche_chair_",TAB$Peche_chair,"_Peche_viv_",TAB$Peche_viv,"_T_capture_",TAB$T_capture,sep="")[1:nrow(TAB)])

#TRANSFORMATION DE LA LISTE DE RESULTATS EN MATRICE

result <- array(NA,c(length(Nstage)+2,length(time),(dim(Tab)[1])+1,nb.simu+3,length(all.sims)/nb.simu))
dimnames(result)[1] <- list(c(Nstage,"NTotal","BTotal"))
dimnames(result)[2] <- list(time)
dimnames(result)[3] <- list(c(1:((dim(Tab)[1])+1)))
dimnames(result)[4] <- list(c(1:nb.simu,"MEDIANE","Q5","Q95"))
dimnames(result)[5] <- list(scenarios)


for (s in scenarios){
  for (cell in 1:((dim(Tab)[1])+1)){
    for (simu in 1:nb.simu){                
      result[c(as.character(Nstage),"NTotal"),,cell,simu,s] <- as.matrix(as.data.frame(all.sims[paste(s,"_SIMU_",simu,sep="")]))[,(length(time)*(cell-1)+1):(length(time)*(cell-1)+length(time))]
    }}}

# CALCUL DE LA MEDIANE, QUARTILE 5% ET QUARTILE 95% POUR CHAQUE SCENARIO

for (s in scenarios){
  for (cell in 1:((dim(Tab)[1])+1)){
    
    result[,,cell,"MEDIANE",s] <- apply(result[,,cell,1:nb.simu,s],c(1,2),median,na.rm=T)
    result[,,cell,"Q95",s]     <- apply(result[,,cell,1:nb.simu,s],c(1,2),quantile,probs=c(0.95),na.rm=T)
    result[,,cell,"Q5",s]      <- apply(result[,,cell,1:nb.simu,s],c(1,2),quantile,probs=c(0.05),na.rm=T)
  }}

# CALCUL DE LA BIOMASSE

for (t in unlist(dimnames(result)[2])){
  for (cell in unlist(dimnames(result)[3])){
    for (simu in unlist(dimnames(result)[4])){
      for (s in unlist(dimnames(result)[5])){
        
        result["BTotal",t,cell,simu,s] <- sum(result[as.character(Nstage),t,cell,simu,s]*Bstage)
      }}}}

result_max <- result

stopCluster(cl)


# SAUVEGARDE DE L'ENVIRONNEMENT

save.image(paste0(output,"/ENVIRONNEMENTS/Last_Enviro_13-06_v1.RData"))











