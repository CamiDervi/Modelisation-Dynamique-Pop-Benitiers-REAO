# Script pour 
# Author: Camille DERVILLEZ
# Date : 04/04/2025
# R version 4.4.3

rm(list=ls())

library(dplyr)
library(tidyr)
library(doParallel)
library(MASS)
library(chron)
library(stringr)
library(ncdf4)
library(ggplot2)
library(stats)
library(DEoptim)
library(tibble)
library(tidyverse)
library(patchwork)


#-------------------------------------------------------------------------------------------------------------------------------------------------------
#CREATION D'UN REPERTOIRE DE TRAVAIL

repertoire<-"Q:/Bénitiers Tahiti/R_REAO/INPUT"
output <- "Q:/Bénitiers Tahiti/R_REAO/OUTPUT"
setwd(repertoire)


#-------------------------------------------------------------------------------------------------------------------------------------------------------
#PARAMETRISATION
# Paramètres généraux

Ile <- "REAO"
Nstage  <- seq(from=1,to=20,by=1)    # Nombre de classes de tailles considérées 


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# IMPORTATION DES DONNEES

#Paramètres connus de dynamiques naturelle

DFG_P1P4_GL <- read.csv2("DFG_P1P4_goodlight.csv")               # Taux de croissance saison chaude dans des bonnes conditions lumineuses
DFG_P2P3_GL <- read.csv2("DFG_P2P3_goodlight.csv")               # Taux de croissance saison froide dans des bonnes conditions lumineuses
DFG_P1P4_LL <- read.csv2("DFG_P1P4_lowlight.csv")                # Taux de croissance saison froide dans des mauvaises conditions lumineuses
DFG_P2P3_LL <- read.csv2("DFG_P2P3_lowlight.csv")                # Taux de croissance saison chaude dans des mauvaises conditions lumineuses

CF <- as.matrix(read.csv2("CF_REAO.csv"))                        # Matrice de contribution relative des différentes classes de tailles à la reproduction
CF <- CF[-nrow(CF), -1]

#Timeseries de températures journalières

fluc_temp_daily_T2 <- read.csv("LongTimeseries_df_T2_temp_day.csv")
fluc_temp_daily_T6 <- read.csv("LongTimeseries_df_T6_temp_day.csv")

# Vecteurs dates & indices dates échantillonages

dates_S20 <- c("2016-09-02", "2017-03-09",
               "2017-03-09", "2017-12-03", 
               "2022-10-29", "2023-05-05", 
               "2023-05-05", "2023-11-10")

load("indices_periodes_S20_day.RData")            #indices correspondants aux dates de début/fin des périodes d'interêt
index_S20

dates_S41 <- c("2017-12-05", "2019-07-09", 
               "2019-07-09", "2021-03-29", 
               "2022-04-04", "2022-10-29",
               "2022-10-29", "2023-05-05",
               "2023-05-05", "2023-11-10")

load("indices_periodes_S41_day.RData")   
index_S41

# Données de densités et de fréquences de taille
densite_S41 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_S41.csv")
densite_S41 <- densite_S41[, -((ncol(densite_S41)-2):ncol(densite_S41))]

densite_S20 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_S20prime.csv")
densite_S20 <- densite_S20[, -((ncol(densite_S20)-2):ncol(densite_S20))]
densite_S20$Stations <- gsub("S20'", "S20", densite_S20$Stations)

S_T_S41 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_S41.csv")
S_T_S41$FREQ <- as.numeric(S_T_S41$FREQ)
S_T_S20 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_S20prime.csv")


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# TRAITEMENTS PRELIMINAIRES

# Création df des densités moyennes par classe de taille
for (station in c("S41", "S20")){
  
  S_T <- get(paste0("S_T_", station))
  
  S_T <- S_T[S_T$TAILLE != 0,]
  S_T$NOMBRE <- NULL
  
  densite_S_T <- S_T %>%
    mutate(FREQ = FREQ / 100) %>%  # <- ici on divise toutes les fréquences
    pivot_wider(names_from = TAILLE, values_from = FREQ, names_prefix = "Frequence_Taille_")
  colnames(densite_S_T)[-c(1:2)] <- paste0(colnames(densite_S_T)[-c(1:2)], "cm")
  
  if (station == "S41"){ densite <- densite_S41}
  if (station == "S20"){ densite <- densite_S20}
  
  for (campagne in 1:nrow(densite_S_T)){
    n_campagne <- as.character(densite_S_T[campagne,"CAMPAGNE"])
    Dmean <- densite[densite$Campagnes == n_campagne & densite$Stations == station, "Dmean"]
    densite_S_T[campagne,3:ncol(densite_S_T)] <- round(densite_S_T[campagne,3:ncol(densite_S_T)] * Dmean, 5)
  }
  
  assign(paste0("densite_S_T_", station), densite_S_T)
  
  if (station == "S20"){
    stock <- as.matrix(densite_S_T[c(2:4,7:9),-c(1:2,23:ncol(densite_S_T))])     # Garder seulement les périodes & les classes de taille d'interêt
    colnames(stock)<-Nstage
    stock <- unlist(stock)
    
    ligne_mars_2017 <- stock[2, ]
    stock <- rbind(stock[1:2, ], ligne_mars_2017, stock[3:nrow(stock), ])
    
    ligne_mai_2023 <- stock[6, ]
    stock <- rbind(stock[1:6, ], ligne_mai_2023, stock[7:nrow(stock), ])}
  
  if (station == "S41"){
    stock <- as.matrix(densite_S_T[c(4:10),-c(1:2,23:ncol(densite_S_T))])     # Garder seulement les périodes & les classes de taille d'interêt
    colnames(stock)<-Nstage
    stock <- unlist(stock)
    
    ligne_juillet_2019 <- stock[2,]
    stock <- rbind(stock[1:2, ],  ligne_juillet_2019, stock[3:nrow(stock), ])
    
    ligne_oct_2022 <- stock[6,]
    stock <- rbind(stock[1:6, ],  ligne_oct_2022, stock[7:nrow(stock), ])
    
    ligne_mai_2023 <- stock[8,]
    stock <- rbind(stock[1:8, ],  ligne_mai_2023, stock[9:nrow(stock), ])
    }
  
  assign(paste0("stock_", station), stock)
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# DEFINITION DES FONCTIONS

# Definition d'une fonction qui calcule une température moyenne pour chaque période

func.temp <- function(t, station){
  
  if (station == "S41"){fluc_temp <- fluc_temp_daily_T2
  index_periodes_day <- index_S41}
  
  if (station == "S20"){fluc_temp <- fluc_temp_daily_T6
  index_periodes_day <- index_S20}
  
  #Extraire les données de température de la période t 
  date_ini <- index_periodes_day[2*t-1]                                    # Récupérer l'indice du premier jour de la période t
  date_fin <- index_periodes_day[2*t] - 1                                  # Récupérer l'indice du dernier jour de la période t
  fluc_temp <- fluc_temp[date_ini:date_fin,]
  
  temp <- mean(fluc_temp$TEMP_predite_mean)
  return(temp)
}


# Définition d'une fonction qui renvoie le nombre de jours écoulés entre deux dates d'échantillonage

func.days <- function(t, station){
  
  if (station == "S41"){
    dates <- dates_S41}
  
  if (station == "S20"){
    dates <- dates_S20}
  
  date_ini <- dates[2*t-1]
  date_fin <- dates[2*t]
  Njours <- as.numeric(difftime(date_fin, date_ini, units = "days"))
  
  return(Njours)
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


# Definition d'une fonction qui renvoie la matrice de la croissance G

func.G <- function(t,station,c){
  
  # Matrice de la croissance
  if (station == "S20"){
    if (t == 1 | t == 3 ) { 
      DF.G.GL <- get("DFG_P1P4_GL")      #Récup taux de croissance periode P1_P4 en bonnes conditions lumineuses
      DF.G.LL <- get("DFG_P1P4_LL")}     #Récup taux de croissance periode P1_P4 en mauvaises conditions lumineuses
    
    if (t == 2 | t == 4) { 
      DF.G.GL <- get("DFG_P2P3_GL")      #Récup taux de croissance periode P2_P3 en bonnes conditions lumineuses
      DF.G.LL <- get("DFG_P2P3_LL")}}    #Récup taux de croissance periode P2_P3 en mauvaises conditions lumineuses
  
  if (station == "S41"){
    if (t %in% c(1,3,6,8,10)) { 
      DF.G.GL <- get("DFG_P1P4_GL")      #Récup taux de croissance periode P1_P4 en bonnes conditions lumineuses
      DF.G.LL <- get("DFG_P1P4_LL")}     #Récup taux de croissance periode P1_P4 en mauvaises conditions lumineuses
    
    if (t %in% c(2,4,5,7,9,11)) { 
      DF.G.GL <- get("DFG_P2P3_GL")      #Récup taux de croissance periode P2_P3 en bonnes conditions lumineuses
      DF.G.LL <- get("DFG_P2P3_LL")}}    #Récup taux de croissance periode P2_P3 en mauvaises conditions lumineuses
  
  temp <- func.temp(t, station)
  Njours <- func.days(t, station)
  
  G.GL <- Generate.G.matrix(DF.G.GL,temp,Njours)        #Création de la matrice de croissance Goodlight
  G.LL <- Generate.G.matrix(DF.G.LL,temp,Njours)        #Création de la matrice de croissance Lowlight
  
  G <- c * G.GL + (1-c) * G.LL
  G <- G[-c(1), c(-1)]                 # On enlève la classe de taille 0 cm de la matrice
  
  return(G)
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# ESTIMATION DE LA MORTALITE

# Definition d'une fonction qui crée la matrice de la mortalité Y

Generate.Y.matrix <- function(a,b,t,station){
  
  # Initialisation de la matrice
  matrixY <- matrix(NA, nrow = 20,ncol = 20)
  rownames(matrixY) <- c(1:20)
  colnames(matrixY) <- c(1:20)
  
  Njours <- func.days(t,station)
  
  # On remplit la matrice colonne par colonne
  for (l in 1:20){
    
    M <- a * l + b
    
    S <- exp(-M * Njours)
    
    matrixY[, l] <- S 
  }
  
  return(matrixY)
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# TRAITEMENT DES PERIODES INTERMEDIAIRES DE LA STATION S41

# Ajout de périodes intermédiaires pour la station S41
dates_S41 <- c("2017-12-05", "2018-04-01",   # période 1.1 soit t=1
               "2018-04-01", "2018-10-01",   # période 1.2 soit t=2
               "2018-10-01", "2019-04-01",   # période 1.3 soit t=3
               "2019-04-01", "2019-07-09",   # période 1.4 soit t=4
               
               "2019-07-09", "2019-10-01",   # période 2.1 soit t=5
               "2019-10-01", "2020-04-01",   # période 2.2 soit t=6
               "2020-04-01",  "2020-10-01",  # période 2.3 soit t=7
               "2020-10-01", "2021-03-29",   # période 2.4 soit t=8
               
               "2022-04-04", "2022-10-29",   # période 3 soit t=9
               
               "2022-10-29", "2023-05-05",   # période 4 soit t=10
               
               "2023-05-05", "2023-11-10")   # période 5 soit t=11

index_S41 <- c(9105, 9222, 9222, 9405, 9405, 9587, 9587, 9686, 9686, 9770, 9770, 9953, 9953, 10136, 10136, 10315, 10686, 10894, 10894, 11082, 11082, 11271)

# ESTIMATION DES EFFECTIFS
# Définition d'une fonction qui estime les effectifs au temps t+1 pour les périodes intermédiaires de la station S41

func.Nest.inter <- function(a, b, c, t, stock, station){ 
  
  G <- func.G(t, station, c)
  Y <- Generate.Y.matrix(a, b, t, station)
  
  if (t %in% c(1:4)){                                          # période dec 2017 - juillet 2019
    Nest <- (G * Y) %*% as.numeric(stock[2*t - 1,])
    
  } else {                                                     # période juillet 2019 - mars 2021
    Nest <- (G * Y) %*% as.numeric(stock[2*(t - 4) - 1,])
  }
  
  return(Nest)
}



# CALCUL DIFFERENCE OBSERVE vs ESTIME
# Définition d'une fonction qui calcule la différence entre Nobs et Nest par classe de taille pour les périodes intermédiaires de la station S41

func.diff.inter <- function(a,b,c,t,station){ 
  
  if (t == 4){
    stock_est <- stock_S41[1, , drop = FALSE]
    
    for (inter in 1:4){
      Nest <- func.Nest.inter(a, b, c, inter, stock_est, station)
      stock_est <- rbind(stock_est, as.data.frame(t(Nest)), as.data.frame(t(Nest)))
    }
    
    stock_est <- stock_est[-nrow(stock_est), ]
  }
  
  if (t == 8){
    stock_est <- stock_S41[3, , drop = FALSE]
    
    for (inter in 5:8){
      Nest <- func.Nest.inter(a, b, c, inter, stock_est, station)
      stock_est <- rbind(stock_est, as.data.frame(t(Nest)), as.data.frame(t(Nest)))
    }
    
    stock_est <- stock_est[-nrow(stock_est), ]
  }
  
  # Calcul manuel de la différence ici, car on ne veut plus dépendre de get()
  diff <- matrix(NA, nrow = 20, ncol = 1)
  
  stock_obs <- stock_S41    #Nobs
  matrix.Nest <- as.matrix(t(stock_est[8, ]))    #Nest
  
  period_to_compare <- ifelse(t == 4, 2, 4)  # stock_S41[2,] ou stock_S41[3,]
  
  for (row in 1:20){
    diff[row,1] <- (as.matrix(stock_obs[period_to_compare, ])[row,1] - matrix.Nest[row,1])^2
  }
  
  sum_diff <- sum(diff, na.rm = TRUE)
  
  return(list(stock_est, sum_diff))
}


# Définition d'une fonction qui calcule la différence entre Nobs et Nest par classe de taille adaptée pour utiliser la fonction R DEoptim() 
# pour les périodes intermédiaires de la station S41

func.diff_optim.inter <- function(par){
  a <- par[1]
  b <- par[2]
  c <- par[3]
  
  t = 8
  station <- "S41"
  
  if (t == 4){
    stock_est <- stock_S41[1, , drop = FALSE]
    
    for (inter in 1:4){
      Nest <- func.Nest.inter(a, b, c, inter, stock_est, station)
      stock_est <- rbind(stock_est, as.data.frame(t(Nest)), as.data.frame(t(Nest)))
    }
    
    stock_est <- stock_est[-nrow(stock_est), ]
  }
  
  if (t == 8){
    stock_est <- stock_S41[3, , drop = FALSE]
    
    for (inter in 5:8){
      Nest <- func.Nest.inter(a, b, c, inter, stock_est, station)
      stock_est <- rbind(stock_est, as.data.frame(t(Nest)), as.data.frame(t(Nest)))
    }
    
    stock_est <- stock_est[-nrow(stock_est), ]
  }
  
  # Calcul manuel de la différence ici, car on ne veut plus dépendre de get()
  diff <- matrix(NA, nrow = 20, ncol = 1)
  
  stock_obs <- stock_S41    #Nobs
  matrix.Nest <- as.matrix(t(stock_est[8, ]))    #Nest
  
  period_to_compare <- ifelse(t == 4, 2, 4)  # stock_S41[2,] ou stock_S41[3,]
  
  for (row in 1:20){
    diff[row,1] <- (as.matrix(stock_obs[period_to_compare, ])[row,1] - matrix.Nest[row,1])^2
  }
  
  sum_diff <- sum(diff, na.rm = TRUE)
  
  return(sum_diff)
}



# FONCTIONS D'OPTIMISATION
# Fonction pour trouver valeurs de a, b et c pour minimiser func.diff_optim.inter

DEoptim(
  fn = func.diff_optim.inter,
  lower = c(-10^(-10), 0, 0),
  upper = c(0, 0.05, 1),
  control = DEoptim.control(trace = TRUE, itermax = 10))


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# VALEURS DES PARAMETRES a, b, c

a_values_S20 <- c(-3.891707e-11, -3.981444e-11, -8.331452e-11, -1.989161e-11)
b_values_S20 <- c(1.146567e-03, 1.818985e-04, 3.882136e-04, 1.088402e-03)
c_values_S20 <- c(0.7739128, 0.088411, 0.8106588, 0.7660652)

a_values_S41 <- c(-2.002333e-11, -2.217715e-11, -9.473334e-11, -6.306908e-11, -5.760036e-11)
b_values_S41 <- c(2.165147e-05, 1.174836e-04, 2.994807e-04, 1.454320e-03, 5.705648e-03)
c_values_S41 <- c(0.3281730, 0.043325, 0.007067, 0.560505, 0.253369)


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# MISE A JOUR DU STOCK_S41 AVEC PERIODES INTERMEDIAIRES

stock_est_inter_t1 <- func.diff.inter(a_values_S41[1], b_values_S41[1], c_values_S41[1], 4, "S41")[[1]]
stock_est_inter_t1
stock_obs_inter_t1 <- rbind(stock_est_inter_t1[-8,], stock_S41[2,])
stock_obs_inter_t1

stock_est_inter_t2 <- func.diff.inter(a_values_S41[2], b_values_S41[2], c_values_S41[2], 8, "S41")[[1]]
stock_est_inter_t2
stock_obs_inter_t2 <- rbind(stock_est_inter_t2[-8,], stock_S41[4,])
stock_obs_inter_t2

stock_inter <- rbind(stock_obs_inter_t1, stock_obs_inter_t2)
stock_inter

stock_S41_inter <- rbind(stock_inter, as.data.frame(stock_S41[5:10,]))
stock_S41_inter <- as.matrix(stock_S41_inter)
stock_S41_inter


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# TRAITEMENT DES AUTRES PERIODES

# ESTIMATION DES EFFECTIFS
# Définition d'une fonction qui estime les effectifs au temps t+1

func.Nest <- function(a,b,c,t,stock,station){
  
  G <- func.G(t,station,c)
  Y <- Generate.Y.matrix(a,b,t, station)
  
  Nest <- (G * Y) %*% as.numeric(stock[2*t-1,])
  
  return(Nest)
}


# CALCUL DIFFERENCE OBSERVE vs ESTIME
# Définition d'une fonction qui calcule la différence entre Nobs et Nest par classe de taille

func.diff <- function(a,b,c,t,station){
  
  diff <- matrix(NA, nrow = 20, ncol = 1)
  
  matrix.Nest <- func.Nest(a,b,c,t,station)
  stock <- get(paste0("stock_", station))
  
  for (row in 1:20){
    diff[row,1] <- (as.matrix(stock[2*t,])[row,1] - matrix.Nest[row,1])^2       
  }
  
  sum_diff <- sum(diff)
  
  return(sum_diff)}


# Définition d'une fonction qui calcule la différence entre Nobs et Nest par classe de taille adaptée pour utiliser la fonction R DEoptim()

func.diff_optim <- function(par){
  
  #Paramètres
  a <- par[1]
  b <- par[2]
  c <- par[3]
  
  station = "S41"    #A faire varier
  t = 11             #A faire varier
  
  diff <- matrix(NA, nrow = 20, ncol = 1)
  
  if (station == "S20"){
    stock <- stock_S20
    matrix.Nest <- func.Nest(a,b,c,t,stock,station)
  }
  
  if (station == "S41"){
    stock <- stock_S41_inter
    matrix.Nest <- func.Nest(a,b,c,t,stock,station)
  }
    
  for (row in 1:20){
    diff[row,1] <- (as.matrix(stock[2*t,])[row,1] - matrix.Nest[row,1])^2       
  }
  
  sum_diff <- sum(diff)
  
  return(sum_diff)}


 # FONCTIONS D'OPTIMISATION
# Fonction pour trouver valeurs de a, b et c pour minimiser func.diff_optim

DEoptim(
  fn = func.diff_optim,
  lower = c(-10^(-10), 0, 0),
  upper = c(0, 0.05, 1),
  control = DEoptim.control(trace = TRUE, itermax = 10))



#-------------------------------------------------------------------------------------------------------------------------------------------------------
# REPRESENTATIONS GRAPHIQUES

noms_periodes_S20 <- c("P_09/2016-03/2017_S20", 
                       "P_03/2017-12/2017_S20", 
                       "P_10/2022-05/2023_S20", 
                       "P_05/2023-11/2023_S20")

noms_periodes_S41 <- c("P_12/2017-07/2019", 
                       "P_07/2019-03/2021", 
                       "P_04/2022-10/2022", 
                       "P_10/2022-05/2023", 
                       "P_05/2023-11/2023")



# 1 - Courbes de mortalité par période et par station

# Vecteur l
l <- seq(1, 20, length.out = 200)  # commence à 0.1 pour éviter division par zéro

# Créer un dataframe long pour ggplot

df_S20 <- tibble()
  
for (i in 1:4) {
  df_S20 <- bind_rows(df_S20, tibble(
    l = l,
    f_l = a_values_S20[i] * l + b_values_S20[i],
    Periode = noms_periodes_S20[i]
  ))}


df_S41 <- tibble()
  
for (i in 1:5) {
  df_S41 <- bind_rows(df_S41, tibble(
    l = l,
    f_l = a_values_S41[i] * l + b_values_S41[i],
    Periode = noms_periodes_S41[i]
  ))}

# Forcer l’ordre des périodes dans les légendes
df_S20$Periode <- factor(df_S20$Periode, levels = noms_periodes_S20)
df_S41$Periode <- factor(df_S41$Periode, levels = noms_periodes_S41)


# Tracer avec ggplot
couleurs <- c("#a6dba0","#41ae76", "#006d2c", "#9e9ac8", "#3f007d")

p_S41 <- ggplot(df_S41, aes(x = l, y = f_l, color = Periode)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = couleurs) +
  theme_minimal(base_size = 14) +
  labs(x = "Taille",   y = expression(M[others]),
       color = "Périodes") +
  scale_x_continuous(breaks = seq(1, ceiling(max(df_S41$l)), by = 1)) +
  coord_cartesian(ylim = c(0, 0.003)) +
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Taille des valeurs sur l'axe X
        axis.text.y = element_text(size = 12),                         # Taille des valeurs sur l'axe Y
        axis.title.x = element_text(size = 18),                        # Taille du titre de l'axe X
        axis.title.y = element_text(size = 18),                        # Taille du titre de l'axe Y
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 11)
      )

p_S41 + plot_layout(guides = "collect") & theme(legend.position = "bottom")



couleurs <- c("#41ae76", "#006d2c", "#9e9ac8", "#3f007d")

# Graphique pour df_S20
p_S20 <- ggplot(df_S20, aes(x = l, y = f_l, color = Periode)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = couleurs) +
  theme_minimal(base_size = 14) +
  labs(title = "Station S20",
       x = "l", y = "f(l)",
       color = "Périodes") +
  scale_x_continuous(breaks = seq(1, ceiling(max(df_S20$l)), by = 1)) +
  coord_cartesian(ylim = c(0, 0.003)) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank())

p_S20 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

p_S20 + p_S41 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

#-------------------------------------------------------------------------------------------------------------------------------------------------------
# 2 - Histogrammes densité par classe de taille

tailles_obs_vs_est <-  data.frame(
  type = character(0),
  periode = character(0),
  station = character(0)
)


# Station S41
# Densités observées

resultats_nest <- list(stock_est_inter_t1[8,],
                    stock_est_inter_t2[8,],
                    t(func.Nest(a_values_S41[3], b_values_S41[3], c_values_S41[3], 9, stock_S41_inter, "S41")),
                    t(func.Nest(a_values_S41[4], b_values_S41[4], c_values_S41[4], 10, stock_S41_inter, "S41")),
                    t(func.Nest(a_values_S41[5], b_values_S41[5], c_values_S41[5], 11, stock_S41_inter,"S41")))


for (i in 1:5){
  
  ligne_1 <- data.frame(
    type = "Obs_init",
    periode = noms_periodes_S41[i],
    station = "S41")
  ligne_1 <- cbind(ligne_1, t(stock_S41[2*i-1,]))
  
  
  ligne_2 <- data.frame(
    type = "Obs_fin",
    periode = noms_periodes_S41[i],
    station = "S41")
  ligne_2 <- cbind(ligne_2, t(stock_S41[2*i,]))
  
  tailles_obs_vs_est <- bind_rows(tailles_obs_vs_est, ligne_1)
  tailles_obs_vs_est <- bind_rows(tailles_obs_vs_est, ligne_2)
  
  # Densités estimées
  nest <- resultats_nest[[i]]
  
  ligne <- data.frame(
    type = "Est",
    periode = noms_periodes_S41[i],
    station = "S41")
  
  ligne <- cbind(ligne, nest)
  tailles_obs_vs_est <- bind_rows(tailles_obs_vs_est, ligne)
}
tailles_obs_vs_est



tailles_long <- tailles_obs_vs_est %>%
  pivot_longer(
    cols = 4:23,
    names_to = "taille_cm",
    values_to = "frequence"
  ) %>%
  mutate(
    taille_cm = as.numeric(str_extract(taille_cm, "\\d+"))  # extrait le nombre de cm
  )

tailles_long$type <- factor(tailles_long$type, levels = c("Obs_init", "Obs_fin", "Est"))
tailles_long$periode <- factor(tailles_long$periode, levels = noms_periodes_S41)

# Filtrage par station
station_S41 <- tailles_long %>%
  filter(station == "S41", periode %in% noms_periodes_S41[4])


# Graphique pour la station S41
p_S41 <- ggplot(station_S41, aes(x = taille_cm, y = frequence, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ periode, ncol = 1, scales = "free_y") + 
  scale_fill_manual(values = c(
    "Obs_init" = "#66c2a4",
    "Obs_fin"  = "#006d2c",
    "Est"      = "#de2d26"
  )) +
  labs(
    title = "Station S41",
    x = "Taille (cm)",
    y = "Densités (ind/m²)",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Taille des valeurs sur l'axe X
    axis.text.y = element_text(size = 12),                         # Taille des valeurs sur l'axe Y
    axis.title.x = element_text(size = 18),                        # Taille du titre de l'axe X
    axis.title.y = element_text(size = 18),                        # Taille du titre de l'axe Y
    plot.title = element_text(hjust = 0.5, size = 16),            # Taille du titre principal
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

# Combiner les deux graphiques côte à côte
p_S41








tailles_obs_vs_est <-  data.frame(
  type = character(0),
  periode = character(0),
  station = character(0)
)

# Station S20
# Densités observées

for (i in 1:4){
  
  ligne_1 <- data.frame(
    type = "Obs_init",
    periode = noms_periodes_S20[i],
    station = "S20")
  ligne_1 <- cbind(ligne_1, t(stock_S20[2*i-1,]))
  
  
  ligne_2 <- data.frame(
    type = "Obs_fin",
    periode = noms_periodes_S20[i],
    station = "S20")
  ligne_2 <- cbind(ligne_2, t(stock_S20[2*i,]))
  
  tailles_obs_vs_est <- bind_rows(tailles_obs_vs_est, ligne_1)
  tailles_obs_vs_est <- bind_rows(tailles_obs_vs_est, ligne_2)
  
  # Densités estimées
  resultats_nest <- as.vector(func.Nest(a_values_S20[i], b_values_S20[i], c_values_S20[i], i, stock_S20, "S20"))
  
  ligne <- data.frame(
    type = "Est",
    periode = noms_periodes_S20[i],
    station = "S20")
  
  ligne <- cbind(ligne, t(resultats_nest))
  tailles_obs_vs_est <- bind_rows(tailles_obs_vs_est, ligne)
}

tailles_obs_vs_est

#save(tailles_obs_vs_est, file = "tableau comparatif effectifs Obs vs Est.RData")


tailles_long <- tailles_obs_vs_est %>%
  pivot_longer(
    cols = 4:22,
    names_to = "taille_cm",
    values_to = "frequence"
  ) %>%
  mutate(
    taille_cm = as.numeric(str_extract(taille_cm, "\\d+"))  # extrait le nombre de cm
  )

tailles_long$type <- factor(tailles_long$type, levels = c("Obs_init", "Obs_fin", "Est"))
tailles_long$periode <- factor(tailles_long$periode, levels = noms_periodes_S20)

station_S20 <- tailles_long %>%
  filter(station == "S20", periode %in% tail(noms_periodes_S20, 2))


# Graphique pour la station S20
p_S20 <- ggplot(station_S20, aes(x = taille_cm, y = frequence, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ periode, ncol = 1, scales = "free_y") + 
  scale_fill_manual(values = c(
    "Obs_init" = "#66c2a4",
    "Obs_fin"  = "#006d2c",
    "Est"      = "#de2d26"
  )) +
  labs(
    title = "Station S20",
    x = "Taille (cm)",
    y = "Densités (ind/m²)",
    fill = "Type"
  ) +
  theme_minimal(base_size = 14)

p_S20 

p_S20 + p_S41 + plot_layout(guides = "collect") & theme(legend.position = "bottom")








# --- 1. Vecteur l ---
l <- seq(1, 20, length.out = 200)

# --- 2. Création du dataframe long ---
df_S41 <- tibble()
for (i in 1:5) {
  df_S41 <- bind_rows(df_S41, tibble(
    l = l,
    f_l = a_values_S41[i] * l + b_values_S41[i],
    Periode = noms_periodes_S41[i]
  ))
}

# --- 3. Forcer l'ordre des périodes ---
df_S41$Periode <- factor(df_S41$Periode, levels = noms_periodes_S41)

# --- 4. Séparer les données avec ou sans légende ---
periode_a_masquer <- "P_05/2023-11/2023"

df_S41_main <- df_S41 %>% filter(Periode != periode_a_masquer)
df_S41_hidden <- df_S41 %>% filter(Periode == periode_a_masquer)

# --- 5. Définir les couleurs (4 seulement pour la légende) ---
couleurs <- c("#a6dba0", "#41ae76", "#006d2c", "#9e9ac8", "#3f007d")

# --- 6. Tracé ---
p_S41 <- ggplot() +
  # Courbes avec légende
  geom_line(data = df_S41_main, aes(x = l, y = f_l, color = Periode), linewidth = 1.3) +
  # Courbe sans légende
  geom_line(data = df_S41_hidden, aes(x = l, y = f_l), color = couleurs[5], linewidth = 1.3, show.legend = FALSE) +
  scale_color_manual(values = couleurs[1:4]) +
  scale_x_continuous(breaks = seq(1, ceiling(max(df_S41$l)), by = 1)) +
  coord_cartesian(ylim = c(0, 0.002)) +
  labs(
    x = "Taille",
    y = expression(M[others]),
    color = "Périodes"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 22),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 11)
  )

# --- 7. Affichage final avec légende en bas (facultatif) ---
p_S41 + plot_layout(guides = "collect") & theme(legend.position = "bottom")





## Calcul Biais moyen S41 P_10/2022-05/2023

biais_moyen <- c()

for (i in 1:20){
 biais <- abs(station_S41$frequence[40 + i] - station_S41$frequence[20 + i] ) 
 biais_moyen <- c(biais_moyen, biais)
  }

mean(biais_moyen)


## RMSE S41 P_10/2022-05/2023

RMSE <- c()

for (i in 1:20){
  diff <- abs(station_S41$frequence[40 + i] - station_S41$frequence[20 + i] ) 
  RMSE <- c(RMSE, diff)
}

sqrt(mean(RMSE^2))






























