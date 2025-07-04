
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

#INPUT

load("dates_ech_completees.RData")   # Vecteur des dates de suivis des stocks (vraies dates de suivis + dates intermédiaires ajoutées)
dates_ech

time <- get(load("noms_dates_ech_completees.RData"))  # Vecteur des périodes     NB: 1ère date pour contenir les stocks initiaux
time

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

#-------------------------------------------------------------------------------------------------------------------------------------------------------



# Définition de la fonction qui calcule la mortalité liée au facteur DHW de deux manières différentes
func.graphe.DHW <- function(t, d){
  
  date_ini <- index_ech_day[t-1]
  date_fin <- index_ech_day[t] - 1
  
  result <- data.frame(
    Station = character(),
    t = integer(),
    DHW_max = numeric(),
    Morta_DHW_max = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (station in stations){ 
    fluc_dhw <- get(paste0("fluc_dhw_", station))
    fluc_dhw <- fluc_dhw[date_ini:date_fin,]          
    
    dhw_max <- max(fluc_dhw$dhw, na.rm = TRUE)
    morta_dhw_max <- (exp(dhw_max) - 1) * d
    
    result <- rbind(result, data.frame(
      Station = station,
      t = t,
      DHW_max = dhw_max,
      Morta_DHW_max = morta_dhw_max
    ))
  }
  
  return(result)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------


d <- 1e-04  
b <- 4e-04
e <- 0.3

resultats_dhw <- data.frame()

for (t in 2:length(time)) {
  res <- func.graphe.DHW(t, d)
  resultats_dhw <- rbind(resultats_dhw, res)
}





ggplot(resultats_dhw, aes(x = DHW_max, y = Morta_DHW_max, color = Station)) +
  geom_point() +
  labs(title = "Mortalité en fonction du DHW max",
       x = "DHW max",
       y = "Mortalité exp(d * DHW) - 1") +
  theme_minimal()











