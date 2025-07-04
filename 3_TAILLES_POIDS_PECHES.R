# Script pour faire joitures entre les tableaux de données
# Author: Camille DERVILLEZ
# Date : 25/03/2025
# R version 4.4.3

rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)




# CREATION D'UN REPERTOIRE DE TRAVAIL
repertoire<-"Q:/Bénitiers Tahiti/R_REAO/INPUT"
output <- "Q:/Bénitiers Tahiti/R_REAO/OUTPUT"
setwd(repertoire)

# VECTEUR TIME
Nb.mois <- 229   
Astart <- 2005                       # Année initiale
P <- c("P1","P2","P3","P4")          # Vecteur de nom des périodes considérées
time <- paste(unlist(lapply(seq(from=Astart,to=Astart+Nb.mois/12,by=1),FUN=rep,length=4)),P,sep="_")
time <- time[-c(1, 2, length(time))]    # Commencer/finir le vecteur aux moments du début/fin des suivis de densités
time



# 0 - Data importation
tailles_pechees <- read.csv2("Q:/Bénitiers Tahiti/Data/Tailles_pechees_pechechair.csv")
data_peche <- read.csv2("Q:/Bénitiers Tahiti/Data/DATA_VOLUMES_EXPEDIES_DE_REAO.csv")


# ---------------------------------------------------------------------------------------------------------------------------
# 1 - Calcul fréquences de taille
taille_counts <- table(tailles_pechees)

taille_percentage <- (taille_counts / sum(taille_counts))

taille_df <- data.frame(TAILLE = names(taille_percentage), 
                        PERCENTAGE = as.numeric(taille_percentage))
taille_df$TAILLE <- factor(taille_df$TAILLE, levels = sort(as.numeric(taille_df$TAILLE)))

print(taille_df)

  # Créer un histogramme avec ggplot2
ggplot(taille_df, aes(x = TAILLE, y = PERCENTAGE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Histogramme des pourcentages par taille", 
       x = "Taille", 
       y = "Pourcentage (%)") +
  theme_minimal()


taille_df <- taille_df %>%
  pivot_wider(names_from = TAILLE, values_from = PERCENTAGE, names_prefix = "Taille_")

#write.csv2(taille_df, file = "T_REAO.csv")


# ---------------------------------------------------------------------------------------------------------------------------
# 2 - Calcul poids/individus pechés entre 2005 et 2025

  # Attribution des peches à une période

data_peche$PERIODE <- NA
data_peche <- data_peche %>%
  select(1:3, PERIODE, everything())

peche_jour <- data_peche[grep("^\\d{2}/\\d{2}/\\d{4}$", data_peche$DATE), ]
peche_mois <- data_peche[grep("^\\?{2}/\\d{2}/\\d{4}$", data_peche$DATE), ]
peche_mois$DATE <- sub("\\?\\?", "01", peche_mois$DATE)
peche_jour_mois <- rbind(peche_jour, peche_mois)
peche_jour_mois$DATE <- as.Date(peche_jour_mois$DATE, format = "%d/%m/%Y")

peche_jour_mois <- peche_jour_mois %>%
  mutate(PERIODE = case_when(
    month(DATE) >= 1 & month(DATE) <= 3 ~ paste0(year(DATE), "_P1"),     # P1: Janv, Févr, Mars
    month(DATE) >= 4 & month(DATE) <= 6 ~ paste0(year(DATE), "_P2"),     # P2: Avril, Mai, Juin
    month(DATE) >= 7 & month(DATE) <= 9 ~ paste0(year(DATE), "_P3"),     # P3: Juillet, Août, Septembre
    month(DATE) >= 10 & month(DATE) <= 12 ~ paste0(year(DATE), "_P4")    # P4: Octobre, Novembre, Décembre
  ))

peche_annee <- data_peche[grep("^\\?{2}/\\?{2}/\\d{4}$", data_peche$DATE), ]

unique(data_peche$PRODUIT)
data_vivant <- subset(peche_jour_mois, PRODUIT == "Vivant")
data_chair <- subset(peche_jour_mois, PRODUIT == "Chair")


  # QPC : Bénitiers pour la chair
    #Données d'expédition de chair de bénitiers
data_chair <- data_chair[order(data_chair$DATE), ]

data_chair_P <- data_chair %>%
  group_by(PERIODE) %>%
  summarise(
    DATE = first(DATE),                              # Conserver la première date pour chaque PERIODE
    KG_PERIODE = sum(VOLUME..KG., na.rm = TRUE)      # Somme des volumes (kg) pour chaque PERIODE
  ) %>%
  ungroup()  # Retirer le regroupement

data_chair_P <- data_chair_P[!(data_chair_P$PERIODE == "2012_P4" | data_chair_P$PERIODE == "2014_P2" | data_chair_P$PERIODE == "2023_P2"), ]  #Enlever les périodes avec des données incomplètes

    #Compléter les données manquantes
      #Période 2005_P3 - 2012_P4
duplicated_rows <- do.call(rbind, replicate(7, data_chair_P[1:4, ], simplify = FALSE))  # Dupliquer les données trimestrielles de 2013 (P1,P2,P3,P4)
duplicated_rows <- rbind(data_chair_P[3:4,], duplicated_rows)                           # pour compléter la période 2005_P3 - 2012_P4
duplicated_rows$PERIODE <- time[1:nrow(duplicated_rows)]      
duplicated_rows$DATE <- NA
data_chair_P <- rbind(duplicated_rows, data_chair_P)
data_chair_P$DATE <- as.Date(data_chair_P$DATE, format = "%d/%m/%Y")
data_chair_P <- as.data.frame(data_chair_P)
      
      #Période 2014_P2 - 2019_P4 
mean_P1 <- mean(c(
  data_chair_P[data_chair_P$PERIODE == "2014_P1", "KG_PERIODE"],     
  data_chair_P[data_chair_P$PERIODE == "2020_P1", "KG_PERIODE"]
), na.rm = TRUE)

mean_P4 <- mean(c(
  data_chair_P[data_chair_P$PERIODE == "2013_P4", "KG_PERIODE"],
  data_chair_P[data_chair_P$PERIODE == "2020_P4", "KG_PERIODE"]
), na.rm = TRUE)

mean_P3 <- mean(c(
  data_chair_P[data_chair_P$PERIODE == "2013_P3", "KG_PERIODE"],
  data_chair_P[data_chair_P$PERIODE == "2020_P3", "KG_PERIODE"]
), na.rm = TRUE)

mean_P2 <- mean(c(
  data_chair_P[data_chair_P$PERIODE == "2013_P2", "KG_PERIODE"],
  data_chair_P[data_chair_P$PERIODE == "2020_P2", "KG_PERIODE"]
), na.rm = TRUE)

vals_2014_2019 <-as.data.frame(rbind(mean_P1,mean_P2,mean_P3,mean_P4))  #complété sur excel
#data_chair_P <- read.csv2("Qpc_REAO.csv")


  
# QPV : Bénitiers vivants
data_vivant_P <- data_vivant %>%
  group_by(PERIODE) %>%
  summarise(
    DATE = first(DATE),
    INDIV_PERIODE = sum(NOMBRE..IND., na.rm = TRUE)
  ) %>% 
  ungroup()

write.csv2(data_vivant_P, file = "Qpv_REAO.csv")            















