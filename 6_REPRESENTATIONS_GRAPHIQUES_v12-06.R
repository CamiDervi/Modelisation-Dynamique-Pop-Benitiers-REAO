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


# CHARGEMENT DE L'ENVIRONNEMENT
load("Q:/Bénitiers Tahiti/R_REAO/OUTPUT/ENVIRONNEMENTS/Last_Enviro_12-06_v1.RData")


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Correpondance entre Tab_Valid et Simulations
correspondance <- c(
  "AO2005" = "08/2005_P2_P3",
  "J2010" = "07/2010_P2_P3",
  "D2013" = "12/2013_P1_P4",
  "AV2016" = "04/2016_P1_P4",
  "S2016" = "09/2016_P2_P3",
  "MAR2017" = "03/2017_P1_P4",
  "D2017" = "12/2017_P1_P4",
  "MAR2021" = "03/2021_P1_P4",
  "AV2022" = "04/2022_P1_P4",
  "O2022" = "10/2022_P2_P3",
  "MAI2023" = "05/2023_P2_P3",
  "N2023" = "11/2023_P1_P4",
  "MAR2024" = "04/2024_P1_P4", 
  "MAI2024" = "05/2024_P2_P3",
  "S2024" = "09/2024_P2_P3")

cor.time.camp <- matrix(c(
  "AO2005",   "08/2005_P2_P3",
  "J2010",    "07/2010_P2_P3",
  "D2013",    "12/2013_P1_P4",
  "AV2016",   "04/2016_P1_P4",
  "S2016",    "09/2016_P2_P3",
  "MAR2017",  "03/2017_P1_P4",
  "D2017",    "12/2017_P1_P4",
  "MAR2021",  "03/2021_P1_P4",
  "AV2022",   "04/2022_P1_P4",
  "O2022",    "10/2022_P2_P3",
  "MAI2023",  "05/2023_P2_P3",
  "N2023",    "11/2023_P1_P4",
  "MAR2024",  "04/2024_P1_P4", 
  "MAI2024",  "05/2024_P2_P3",
  "S2024",    "09/2024_P2_P3"
), ncol = 2, byrow = TRUE)
colnames(cor.time.camp) <- c("CAMPAGNE", "TIME")

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Methode MAX : Utilisation du DHW max par période pour calcul mortalité liée à la température
  
scenarios <- dimnames(result_max)[[5]]

  # 1 - GRAPHIQUES CLASSES DE TAILLE 4 - 20 CM

  for (bassin in 1:2){
    
    #Calcul de la densité mean pour les 4 - 20 cm
    Tab_valid <- get(paste0("Tab.validation_B", bassin))
    Tab_valid$Dmean_4cm_et_plus <- Tab_valid$Dmean * rowSums(Tab_valid[,13:29])
    
    PU_list <- unique(Tab_valid$PU)
    Obs_PU <- Tab_valid
    Obs_PU <- Obs_PU[Obs_PU$CAMPAGNE %in% names(correspondance), ]
    Obs_PU$CAMPAGNE <- correspondance[Obs_PU$CAMPAGNE]
    campagnes_obs <- unique(Obs_PU$CAMPAGNE)
    
    for (s in scenarios) {
      
      # OBSERVATIONS
      d_MEAN_Obs <- numeric()
      d_sd_Obs <- numeric()
      d_upper_Obs <- numeric()
      d_lower_Obs <- numeric()
      
      for (campagne in campagnes_obs) {
        subset_data <-Obs_PU[Obs_PU$CAMPAGNE == campagne, ]
        d_MEAN_Obs <- c(d_MEAN_Obs,mean(subset_data$Dmean_4cm_et_plus, na.rm = TRUE))
        d_sd_Obs <- mean(subset_data$Dsd, na.rm = TRUE)
        d_upper_Obs <- d_MEAN_Obs + d_sd_Obs
        d_lower_Obs <- d_MEAN_Obs - d_sd_Obs
      }
      
      # ESTIMATIONS
      N_MEDIANE_Est <- apply(result_max[5:21, , PU_list, "MEDIANE", s],c(2,3),sum)   # Ab/PU/campagne
      N_Q5_Est <- apply(result_max[5:21, , PU_list, "Q5", s],c(2,3),sum)
      N_Q95_Est <- apply(result_max[5:21, , PU_list, "Q95", s],c(2,3),sum)
      
      N_sd_Est <- ((N_Q95_Est - N_Q5_Est)/2)/1.96                                       
      
      d_MEAN_Est <- array(NA, dim = dim(N_MEDIANE_Est), dimnames = dimnames(N_MEDIANE_Est))
      d_sd_Est <- array(NA, dim = dim(N_sd_Est), dimnames = dimnames(N_sd_Est))
      
      for (i in seq_along(PU_list)) {                                                # Ab/PU/campagne / Aire_PU = Dens/PU/campagne
        PU <- PU_list[i]
        area <- Tab$AREA[match(PU, Tab$PU)]
        d_MEAN_Est[, i] <- N_MEDIANE_Est[, i] / area
        d_sd_Est[, i] <- N_sd_Est[, i] / area
      }
      
      d_MEAN_Est <-  apply(d_MEAN_Est, 1, mean, na.rm = TRUE)                  # mean(Dens/PU/campagne) = Dens_mean_bassin/campagne
      d_sd_Est <- apply(d_sd_Est, 1, mean, na.rm = TRUE)
      d_upper_Est <- d_MEAN_Est + d_sd_Est
      d_lower_Est <- d_MEAN_Est - d_sd_Est
      
      ## GRAPHIQUES : DOIT ÊTRE ICI DANS LA BOUCLE SUR `s`
      setwd(output)
      setwd(paste0(output,"/GRAPHIQUES"))
      png(paste0("Densités_4cm_et_plus_Max_Bassin_", bassin,"_scenario_", s, ".png"), width = 800, height = 600)
      
      par(mar = c(10, 5, 4, 2))
      
      x_vals <- colnames(result_max)
      x_labels_tronques <- substr(x_vals, 1, nchar(x_vals) - 6)
      
      plot(1:length(x_vals), d_MEAN_Est, type = "l", col = "red", lwd = 3, xaxt = "n",
           xlab = "", ylab = "Densités (Ind/m²)", main = paste("Scénario", s),
           ylim = range(c(d_lower_Obs, d_upper_Obs, d_MEAN_Est), na.rm = TRUE))
      
      indices_obs <- match(campagnes_obs, x_vals)
      
      points(indices_obs, d_MEAN_Obs, col = "darkgreen", pch = 19)
      segments(indices_obs, d_lower_Obs, indices_obs, d_upper_Obs, col = "darkgreen", lwd = 2)
      
      axis(1, at = 1:length(x_vals), labels = FALSE)
      text(x = 1:length(x_vals), 
           y = par("usr")[3] - 0.06 * diff(par("usr")[3:4]),
           labels = x_labels_tronques, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
      
      mtext("Campagnes d'échantillonnage", side = 1, line = 4, cex = 1)
      
      legend("topright",
             legend = c("Médiane (Estimation)", "Médiane (Observation)", "Écart-type (Observation)"),
             col = c("red", "darkgreen", "darkgreen"),
             lty = c(1, NA, 1), pch = c(NA, 19, NA), lwd = c(3, NA, 2))
      
      dev.off()
    }
  }


  
  # 2 - GRAPHIQUES CLASSES DE TAILLE 12 - 20 CM
  
    for (bassin in 1:2){
      
      #Calcul de la densité mean pour les 12 - 20 cm
      Tab_valid <- get(paste0("Tab.validation_B", bassin))
      Tab_valid$Dmean_12cm_et_plus <- Tab_valid$Dmean * rowSums(Tab_valid[,21:29])
      
      PU_list <- unique(Tab_valid$PU)
      Obs_PU <- Tab_valid
      Obs_PU <- Obs_PU[Obs_PU$CAMPAGNE %in% names(correspondance), ]
      Obs_PU$CAMPAGNE <- correspondance[Obs_PU$CAMPAGNE]
      campagnes_obs <- unique(Obs_PU$CAMPAGNE)
        
        for (s in scenarios) {
          
          # OBSERVATIONS
          d_MEAN_Obs <- numeric()
          d_sd_Obs <- numeric()
          d_upper_Obs <- numeric()
          d_lower_Obs <- numeric()
          
          for (campagne in campagnes_obs) {
            subset_data <-Obs_PU[Obs_PU$CAMPAGNE == campagne, ]
            d_MEAN_Obs <- c(d_MEAN_Obs,mean(subset_data$Dmean_12cm_et_plus, na.rm = TRUE))
            d_sd_Obs <- mean(subset_data$Dsd, na.rm = TRUE)
            d_upper_Obs <- d_MEAN_Obs + d_sd_Obs
            d_lower_Obs <- d_MEAN_Obs - d_sd_Obs
          }
          
          # ESTIMATIONS
          N_MEDIANE_Est <- apply(result_max[13:21, , PU_list, "MEDIANE", s],c(2,3),sum)
          N_Q5_Est <- apply(result_max[13:21, , PU_list, "Q5", s],c(2,3),sum)
          N_Q95_Est <- apply(result_max[13:21, , PU_list, "Q95", s],c(2,3),sum)
          
          N_sd_Est <- ((N_Q95_Est - N_Q5_Est)/2)/1.96
          
          d_MEAN_Est <- array(NA, dim = dim(N_MEDIANE_Est), dimnames = dimnames(N_MEDIANE_Est))
          d_sd_Est <- array(NA, dim = dim(N_sd_Est), dimnames = dimnames(N_sd_Est))
          
          for (i in seq_along(PU_list)) {
            PU <- PU_list[i]
            area <- Tab$AREA[match(PU, Tab$PU)]
            d_MEAN_Est[, i] <- N_MEDIANE_Est[, i] / area
            d_sd_Est[, i] <- N_sd_Est[, i] / area
          }
          
          d_MEAN_Est <-  apply(d_MEAN_Est, 1, mean, na.rm = TRUE)
          d_sd_Est <- apply(d_sd_Est, 1, mean, na.rm = TRUE)
          d_upper_Est <- d_MEAN_Est + d_sd_Est
          d_lower_Est <- d_MEAN_Est - d_sd_Est
          
          ## GRAPHIQUES : DOIT ÊTRE ICI DANS LA BOUCLE SUR `s`
          setwd(output)
          setwd(paste0(output,"/GRAPHIQUES"))
          png(paste0("Densités_12cm_et_plus_Max_Bassin_", bassin,"_scenario_", s, ".png"), width = 800, height = 600)
          
          par(mar = c(10, 5, 4, 2))
          
          x_vals <- colnames(result_max)
          x_labels_tronques <- substr(x_vals, 1, nchar(x_vals) - 6)
          
          plot(1:length(x_vals), d_MEAN_Est, type = "l", col = "red", lwd = 3, xaxt = "n",
               xlab = "", ylab = "Densités (Ind/m²)", main = paste("Scénario", s),
               ylim = range(c(d_lower_Obs, d_upper_Obs), na.rm = TRUE))
          
          indices_obs <- match(campagnes_obs, x_vals)
          
          points(indices_obs, d_MEAN_Obs, col = "darkgreen", pch = 19)
          segments(indices_obs, d_lower_Obs, indices_obs, d_upper_Obs, col = "darkgreen", lwd = 2)
          
          axis(1, at = 1:length(x_vals), labels = FALSE)
          text(x = 1:length(x_vals), 
               y = par("usr")[3] - 0.06 * diff(par("usr")[3:4]),
               labels = x_labels_tronques, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
          
          mtext("Campagnes d'échantillonnage", side = 1, line = 4, cex = 1)
          
          legend("topright",
                 legend = c("Médiane (Estimation)", "Médiane (Observation)", "Écart-type (Observation)"),
                 col = c("red", "darkgreen", "darkgreen"),
                 lty = c(1, NA, 1), pch = c(NA, 19, NA), lwd = c(3, NA, 2))
          
          dev.off()
        }
      }



  # 3 - COMPARAISON DES STRUCTURES DE TAILLE

    # 3.1 - Détermination des paramètres par MAXIMUM LIKELIHOOD sur la base des structures de taille
    
      for (bassin in 1:2){ 
        
        SS.res <- rep(0,length(scenarios))
        names(SS.res) <- scenarios
        
        Tab_valid <- get(paste0("Tab.validation_B", bassin))
        
        for (s in scenarios){
          for (c in cor.time.camp[,"CAMPAGNE"]){
            for (sg in as.character(Nstage[-c(1:4)])){
              
              SS.res[s] <- SS.res[s] + ((sum(result_max[sg,cor.time.camp[cor.time.camp[,"CAMPAGNE"]==c,"TIME"],as.character(unique(Tab_valid$PU)),"MEDIANE",s])/sum(Tab[Tab$PU%in%as.character(unique(Tab_valid$PU)),"AREA"]))
                                        - (sum(Tab_valid[(Tab_valid$CAMPAGNE==c),paste("Frequence_Taille_",sg,"cm",sep="")]*Tab_valid[(Tab_valid$CAMPAGNE==c),"Dmean"]*Tab[Tab$PU%in%as.character(unique(Tab_valid$PU)),"AREA"])/sum(Tab[Tab$PU%in%as.character(unique(Tab_valid$PU)),"AREA"])))^2
              
            }}}
        
        print(paste0("Pour le bassin ", bassin, " le scénario adapté est : ",names(SS.res)[SS.res==min(SS.res)]," et est de : ", SS.res[names(SS.res)[SS.res==min(SS.res)]]))  # Paramètres qui maximisent la vraisemblance entre simulation et données observées
        
      }
    
    
    # 3.2 - Structure de taille simulées versus observées

      # Sélection du scénario optimal
      
      s.select = names(SS.res)[SS.res == min(SS.res)]
      s.select = "sf_20_e_0.7_d_3e-04_Peche_chair_1_Peche_viv_1_T_capture_NA"
      
      for (bassin in 1:2) { 
        
        # --- Données de validation pour le bassin ---
        Tab_valid <- get(paste0("Tab.validation_B", bassin))
        
        # --- Aire totale des PU validés ---
        AIRE_PU_VALID <- sum(Tab[as.character(Tab$PU) %in% as.character(unique(Tab_valid$PU)), "AREA"])
        
        # --- Matrice des aires (zone x classes de taille) ---
        mat.area <- matrix(
          rep(Tab[as.character(Tab$PU) %in% as.character(unique(Tab_valid$PU)), "AREA"], length(Nstage[-c(1:4)])),
          nrow = length(unique(Tab_valid$PU)),
          ncol = length(Nstage[-c(1:4)]),
          byrow = FALSE
        )
        colnames(mat.area) <- Nstage[-c(1:4)]
        rownames(mat.area) <- unique(Tab_valid$PU)
        
        # --- Nom du fichier PDF ---
        setwd(output)
        setwd(paste0(output, "/GRAPHIQUES"))
        pdf_filename <- paste0("Structure_Taille_Max_Bassin_", bassin,"_scenario_", s.select, ".pdf")
        pdf(pdf_filename, width = 12, height = 10)
        
        # --- Affichage par groupes de 6 campagnes ---
        n_panels_per_page <- 6
        n_campagnes <- length(cor.time.camp[, "CAMPAGNE"])
        
        for (i in seq(1, n_campagnes, by = n_panels_per_page)) {
          
          par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))  # marges ajustées
          
          campagnes_this_page <- cor.time.camp[i:min(i + n_panels_per_page - 1, n_campagnes), "CAMPAGNE"]
          
          for (c in campagnes_this_page) {
            
            time.sim <- cor.time.camp[cor.time.camp[, "CAMPAGNE"] == c, "TIME"]
            
            # --- Valeurs Simulation ---
            sim_vals <- apply(
              result_max[5:21, time.sim, as.character(unique(Tab_valid$PU)), "MEDIANE", s.select],1,sum) / AIRE_PU_VALID
            
            # --- Valeurs Observation ---
            obs_vals <- apply(
              (Tab_valid[Tab_valid$CAMPAGNE == c, 13:29]) *
                Tab_valid[Tab_valid$CAMPAGNE == c, 7] *
                Tab[Tab$PU %in% as.character(unique(Tab_valid$PU)), "AREA"],
              2,
              sum
            ) / AIRE_PU_VALID
            
            # --- Y-lim dynamique ---
            ymax <- max(c(sim_vals, obs_vals), na.rm = TRUE)
            
            # --- Barplot Simulation ---
            barplot(
              height = sim_vals,
              names.arg = Nstage[-c(1:4)],
              ylim = c(0, ymax),
              main = paste("Simulation -", c)
            )
            
            # --- Barplot Observation ---
            barplot(
              height = obs_vals,
              names.arg = Nstage[-c(1:4)],
              ylim = c(0, ymax),
              main = paste("Observation -", c)
            )
          }
        }
        
        dev.off()  # ferme le PDF du bassin
        cat("✅ Fichier PDF généré pour le bassin", bassin, ":", pdf_filename, "\n")
      }
          

    
#-------------------------------------------------------------------------------------------------------------------------------------------------------


# COMPARAISON DES DEUX METHODES

# scenarios à comparer entre les deux méthodes
scenario_pairs <- list(
  c("Sc_mean_1", "Sc_max_A"),
  c("Sc_mean_2", "Sc_max_B"),
  c("Sc_mean_3", "Sc_max_C")
)

# Calibration du modèle par bassin
for (bassin in 1:3) {
  
  Tab_valid <- get(paste0("Tab.validation_B", bassin))
  PU_list <- unique(Tab_valid$PU)
  Obs_PU <- Tab_valid
  Obs_PU <- Obs_PU[Obs_PU$CAMPAGNE %in% names(correspondance), ]
  Obs_PU$CAMPAGNE <- correspondance[Obs_PU$CAMPAGNE]
  campagnes_obs <- unique(Obs_PU$CAMPAGNE)
  
  for (pair in scenario_pairs) {
    
    s_mean <- pair[1]
    s_max <- pair[2]
    
    # OBSERVATIONS
    d_MEAN_Obs <- numeric()
    d_sd_Obs <- numeric()
    d_upper_Obs <- numeric()
    d_lower_Obs <- numeric()
    
    for (campagne in campagnes_obs) {
      subset_data <- Obs_PU[Obs_PU$CAMPAGNE == campagne, ]
      d_MEAN_Obs <- c(d_MEAN_Obs, mean(subset_data$Dmean, na.rm = TRUE))
      d_sd <- mean(subset_data$Dsd, na.rm = TRUE)
      d_sd_Obs <- c(d_sd_Obs, d_sd)
    }
    
    d_upper_Obs <- d_MEAN_Obs + d_sd_Obs
    d_lower_Obs <- d_MEAN_Obs - d_sd_Obs
    
    # --- ESTIMATIONS POUR result_mean ---
    N_MEDIANE_Est_mean <- result_mean["NTotal", , PU_list, "MEDIANE", s_mean]
    N_Q5_Est_mean <- result_mean["NTotal", , PU_list, "Q5", s_mean]
    N_Q95_Est_mean <- result_mean["NTotal", , PU_list, "Q95", s_mean]
    N_sd_Est_mean <- ((N_Q95_Est_mean - N_Q5_Est_mean) / 2) / 1.96
    
    d_MEAN_Est_mean <- array(NA, dim = dim(N_MEDIANE_Est_mean), dimnames = dimnames(N_MEDIANE_Est_mean))
    d_sd_Est_mean <- array(NA, dim = dim(N_sd_Est_mean), dimnames = dimnames(N_sd_Est_mean))
    
    # --- ESTIMATIONS POUR result_mean ---
    N_MEDIANE_Est_max <- result_mean["NTotal", , PU_list, "MEDIANE", s_max]
    N_Q5_Est_max <- result_mean["NTotal", , PU_list, "Q5", s_max]
    N_Q95_Est_max <- result_mean["NTotal", , PU_list, "Q95", s_max]
    N_sd_Est_max <- ((N_Q95_Est_max - N_Q5_Est_max) / 2) / 1.96
    
    d_MEAN_Est_max <- array(NA, dim = dim(N_MEDIANE_Est_max), dimnames = dimnames(N_MEDIANE_Est_max))
    d_sd_Est_max <- array(NA, dim = dim(N_sd_Est_max), dimnames = dimnames(N_sd_Est_max))
    
    for (i in seq_along(PU_list)) {
      PU <- PU_list[i]
      area <- Tab$AREA[match(PU, Tab$PU)]
      
      d_MEAN_Est_mean[, i] <- N_MEDIANE_Est_mean[, i] / area
      d_sd_Est_mean[, i] <- N_sd_Est_mean[, i] / area
      
      d_MEAN_Est_max[, i] <- N_MEDIANE_Est_max[, i] / area
      d_sd_Est_max[, i] <- N_sd_Est_max[, i] / area
    }
    
    d_MEAN_Est_mean <- apply(d_MEAN_Est_mean, 1, mean, na.rm = TRUE)
    d_sd_Est_mean <- apply(d_sd_Est_mean, 1, mean, na.rm = TRUE)
    d_upper_Est_mean <- d_MEAN_Est_mean + d_sd_Est_mean
    d_lower_Est_mean <- d_MEAN_Est_mean - d_sd_Est_mean
    
    d_MEAN_Est_max <- apply(d_MEAN_Est_max, 1, mean, na.rm = TRUE)
    d_sd_Est_max <- apply(d_sd_Est_max, 1, mean, na.rm = TRUE)
    d_upper_Est_max <- d_MEAN_Est_max + d_sd_Est_max
    d_lower_Est_max <- d_MEAN_Est_max - d_sd_Est_max
    
    ## --- GRAPHIQUES ---
    setwd(output)
    setwd(paste0(output, "/GRAPHIQUES"))
    png(paste0("Densités_Bassin_", bassin, "_", s_mean, "_vs_", s_max, ".png"), width = 800, height = 600)
    
    par(mar = c(10, 5, 4, 2))
    
    x_vals <- colnames(result_mean)
    x_labels_tronques <- substr(x_vals, 1, nchar(x_vals) - 6)
    
    plot(1:length(x_vals), d_MEAN_Est_mean, type = "l", col = "blue", lwd = 3, xaxt = "n",
         xlab = "", ylab = "Densités (Ind/m²)",
         main = paste("Comparaison Scénarios :", s_mean, "(mean) vs", s_max, "(max)"),
         ylim = range(c(d_lower_Obs, d_upper_Obs,
                        d_lower_Est_mean, d_upper_Est_mean,
                        d_lower_Est_max, d_upper_Est_max), na.rm = TRUE))
    
    lines(1:length(x_vals), d_MEAN_Est_max, col = "red", lwd = 3, lty = 2)
    
    indices_obs <- match(campagnes_obs, x_vals)
    
    points(indices_obs, d_MEAN_Obs, col = "darkgreen", pch = 19)
    segments(indices_obs, d_lower_Obs, indices_obs, d_upper_Obs, col = "darkgreen", lwd = 2)
    
    axis(1, at = 1:length(x_vals), labels = FALSE)
    text(x = 1:length(x_vals),
         y = par("usr")[3] - 0.06 * diff(par("usr")[3:4]),
         labels = x_labels_tronques, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
    
    mtext("Campagnes d'échantillonnage", side = 1, line = 4, cex = 1)
    
    legend("topright",
           legend = c("Médiane Estimation (mean)", "Médiane Estimation (max)",
                      "Médiane Observation", "Écart-type Observation"),
           col = c("blue", "red", "darkgreen", "darkgreen"),
           lty = c(1, 2, NA, 1), pch = c(NA, NA, 19, NA), lwd = c(3, 3, NA, 2))
    
    dev.off()
  }
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------




















