# Script pour calculer l'effort de pêche dans chacune des PU
# Author: Camille DERVILLEZ
# Date : 24/03/2025
# R version 4.4.3

rm(list=ls())

# CHARGEMENT DES PACKAGES NECESSAIRES
library(dplyr)


# CREATION D'UN REPERTOIRE DE TRAVAIL
repertoire<-"Q:/Bénitiers Tahiti/R_REAO/INPUT/Autres excel"
output <- "Q:/Bénitiers Tahiti/R_REAO/OUTPUT"
setwd(repertoire)


#-----------------------------------------------------------------------------------------------------------------------------------------------
# IMPORTATION DES DONNEES
Tab <- read.csv("Tab_Spatial_REAO_0.csv", sep = ";", dec = ".", header = TRUE, fileEncoding = "latin1", stringsAsFactors = TRUE)

capt_ID <- read.csv("Captures_Kg_ID.csv", sep = ";", dec = ".", header = TRUE, fileEncoding = "latin1", stringsAsFactors = TRUE)
capt_ID[capt_ID == ""] <- NA
capt_ID$Aire <- gsub(",", ".", as.character(capt_ID$Aire))
capt_ID <- subset(capt_ID, !is.na(Aire))
rownames(capt_ID) <- NULL

PU_Carac <- read.csv("PU_Carac_0.csv", sep = ";", dec = ".", header = TRUE, fileEncoding = "latin1", stringsAsFactors = TRUE)
PU_Carac[PU_Carac == ""] <- NA

#-----------------------------------------------------------------------------------------------------------------------------------------------


# 1 - Creation d'une chaine de caractère contenant les IDs des pecheurs pour chaque PU
PU_Carac <- PU_Carac %>%
  rowwise() %>%
  mutate(IDs = paste(na.omit(c_across(9:34)), collapse = ",")) %>%  # Concaténer les IDs
  ungroup() %>%
  select(-c(9:34)) %>%  # Supprimer les colonnes 4 à 29
  select(1:3, IDs, everything())



# 2 - Calcul aire de peche otale de chaque pecheur
capt_ID$Aire_calc = NA

for (ID in capt_ID$Identifiant) {              # pour chaque identifiant de pêcheur
  list_PU <- which(grepl(ID, PU_Carac$IDs))    # on récup la liste des PU comprises 
  
  Aire_ID = 0   
  for (PU in list_PU){                                            # pour chacune des PU du pêcheur on récup l'aire de la PU
    Aire_ID <- Aire_ID + as.numeric(PU_Carac[PU,"AREA"])}        # pour finalement sommer toutes les aires et obtenir l'aire totale du pêcheur
  
  row_ID <- which(capt_ID$Identifiant == ID)
  capt_ID$Aire_calc[row_ID] <- Aire_ID
}



# 3 - Calcul des Kg pechés pour chaque PU
Capt_lagon = sum(capt_ID$Captures_annuelle_BI_Be_Tm_USP_kg_mediane)   # Captures annuelle dans tout le lagon

PU_Carac$CAPTURES_PER <- NA
PU_Carac <- PU_Carac %>%
  select(1:4, CAPTURES_PER, everything())


for (row_PU in 1:nrow(PU_Carac)) {         # pour chaque PU du lagon
  sum_kg = 0                               # contiendra poids total pêché dans la PU concernée
  
  pecheurs <- strsplit(PU_Carac$IDs[row_PU], ",")[[1]]   # on identifie les ID des pecheurs de la PU
  
  for (pecheur in pecheurs) {
    Kg_pecheur <- capt_ID$Captures_annuelle_BI_Be_Tm_USP_kg_mediane[which(capt_ID$Identifiant == pecheur)]  # on récupère le poids pêché par le pêcheur
    Aire_pecheur <- as.numeric(capt_ID$Aire_calc[which(capt_ID$Identifiant == pecheur)])                    # on récupère l'aire de pêche totale du pêcheur
    
    Kg_pecheur_PU <- as.numeric((PU_Carac[row_PU,"AREA"] / Aire_pecheur) * Kg_pecheur)                      # on répartit le poids pêché dans la PU proportionellement à l'aire de la PU
    
    sum_kg <- sum_kg + Kg_pecheur_PU
  }
  
  PU_Carac[row_PU,"CAPTURES_KG"] <- sum_kg
  Tab[row_PU,"CAPTURES_KG"] <- sum_kg
  PU_Carac <- PU_Carac %>%
    select(1:4, CAPTURES_KG, everything())
  
  PU_Carac[row_PU,"CAPTURES_PER"] <- sum_kg/Capt_lagon * 100
  Tab[row_PU,"CAPTURES_PER"] <- sum_kg/Capt_lagon * 100
}

sum(PU_Carac$CAPTURES_KG)
sum(PU_Carac$CAPTURES_PER)

write.csv2(Tab, file = "Tab_Spatial_REAO_1.csv", row.names = F)
write.csv2(PU_Carac, file = "PU_Carac_1.csv", row.names = F)
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  