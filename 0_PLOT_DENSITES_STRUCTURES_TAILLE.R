# Script pour faire les plots de densités et de structures de taille
# Author: Camille DERVILLEZ
# Date : 30/04/2025
# R version 4.4.3

rm(list=ls())

# CHARGEMENT DES PACKAGES NECESSAIRES

library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)


# CREATION D'UN REPERTOIRE DE TRAVAIL

repertoire<-"Q:/Bénitiers Tahiti/R_REAO/INPUT"
output <- "Q:/Bénitiers Tahiti/R_REAO/OUTPUT"
setwd(repertoire)


# IMPORTATION DATA
densite_B1 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_B1.csv")
densite_B2 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_B2.csv")
densite_B3 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/densite_B3.csv")

S_T_B1 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_B1.csv")
S_T_B2 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_B2.csv")
S_T_B3 <- read.csv2("Suivi_densite_et_structureTaille_2005_2024/Strucutres_tailles_B3.csv")

# CLEANING DATA

mois_fr <- c(
  "janv" = "01", "janvier" = "01",
  "fevr" = "02", "févr" = "02", "fevrier" = "02", "février" = "02",
  "mars" = "03",
  "avr" = "04", "avril" = "04",
  "mai" = "05",
  "juin" = "06",
  "juil" = "07", "juillet" = "07",
  "aout" = "08", "août" = "08",
  "sept" = "09", "septembre" = "09",
  "oct" = "10", "octobre" = "10",
  "nov" = "11", "novembre" = "11",
  "dec" = "12", "déc" = "12", "decembre" = "12", "décembre" = "12"
)

# Fonction de nettoyage
convert_campagne <- function(x) {
  x <- tolower(x)
  # Pour les cas comme "oct-nov 2023" → on garde le premier mois
  x <- str_replace(x, "^([a-zéûäô-]+)-[a-zéûäô]+", "\\1")
  
  # Extraire mois et année
  mois <- str_extract(x, "[a-zéûäô]+")
  annee <- str_extract(x, "\\d{2,4}")
  
  mois_num <- mois_fr[mois]
  
  # Ajuster l'année : 2 chiffres → 4 chiffres
  annee <- ifelse(nchar(annee) == 2, paste0("20", annee), annee)
  
  # Format final
  result <- ifelse(!is.na(mois_num) & !is.na(annee),
                   paste0(mois_num, "/", annee),
                   NA)
  return(result)
}

# Appliquer à ta colonne
densite_B1$Campagne_clean <- convert_campagne(densite_B1$Campagnes)
densite_B2$Campagne_clean <- convert_campagne(densite_B2$Campagnes)
densite_B3$Campagne_clean <- convert_campagne(densite_B3$Campagnes)

S_T_B1$Campagne_clean <- convert_campagne(S_T_B1$Campagnes)
S_T_B2$Campagne_clean <- convert_campagne(S_T_B2$Campagnes)
S_T_B3$Campagne_clean <- convert_campagne(S_T_B3$Campagnes)
#-------------------------------------------------------------------------------------------------------------------------------------

# COURBES 
# Ajouter une colonne pour chaque bassin
densite_B1_tot <- densite_B1 %>%
  filter(Stations == "Total") %>%
  mutate(Bassin = "B1")

densite_B2_tot <- densite_B2 %>%
  filter(Stations == "Total") %>%
  mutate(Bassin = "B2")

densite_B3_tot <- densite_B3 %>%
  filter(Stations == "Total") %>%
  mutate(Bassin = "B3")

# Fusionner les trois jeux de données
densite_all <- bind_rows(densite_B1_tot, densite_B2_tot, densite_B3_tot)

# S'assurer que Campagnes est bien ordonné si nécessaire
densite_all$Campagne_clean<- factor(densite_all$Campagne_clean, levels = unique(densite_all$Campagne_clean))


# --- 1. Vecteur des dates exactes ---
dates_ech <- as.Date(c("2005-08-15", "2010-07-02", "2013-12-11", "2016-04-04", "2016-09-04", 
                       "2017-03-10", "2017-12-04", "2021-03-28", "2022-04-03", "2022-10-30",
                       "2023-05-06", "2023-11-10", "2024-04-01", "2024-05-05", "2024-09-29"))

# --- 2. Associer dates aux campagnes ---
dates_df <- data.frame(
  Campagne_clean = unique(densite_all$Campagne_clean),
  date_campagne = dates_ech
)

# --- 3. Fusion dates et calcul des intervalles ---
densite_all <- densite_all %>%
  left_join(dates_df, by = "Campagne_clean") %>%
  mutate(
    ymin = Dmean - Dsd,
    ymax = Dmean + Dsd
  )

# --- 4. Graphique final avec écart-type ---
# Retirer certaines detes des labels
dates_to_exclude <- as.Date(c("2016-09-04", "2022-10-30", "2023-11-10", "2024-05-05", "2024-04-01"))
dates_ech_filtered <- dates_ech[!dates_ech %in% dates_to_exclude]

ggplot(densite_all, aes(x = date_campagne, y = Dmean, color = Bassin, group = Bassin)) +
  # Rubans uniquement pour B1 et B2, avec alpha augmenté
  geom_ribbon(
    data = filter(densite_all, Bassin %in% c("B1", "B2")),
    aes(ymin = ymin, ymax = ymax, fill = Bassin),
    alpha = 0.25,
    color = NA,
    na.rm = TRUE
  ) +
  # Courbes et points pour tous les bassins
  geom_line(size = 1.3, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_x_date(
    breaks = sort(dates_ech_filtered),
    labels = format(sort(dates_ech_filtered), "%m/%Y")
  ) +
  scale_color_manual(values = c("B1" = "#2b83ba", "B2" = "#fdae61", "B3" = "#abdda4")) +
  scale_fill_manual(values = c("B1" = "#2b83ba", "B2" = "#fdae61", "B3" = "#abdda4")) +
  labs(
    title = "Évolution des densités moyennes par bassin (± écart-type)",
    x = "Date de campagne",
    y = "Densité moyenne (ind/m²)",
    color = "Bassin"  # seulement pour les lignes
    # Ne pas inclure fill ici
  ) +
  guides(fill = "none") +  # Supprime la légende des rubans
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Taille des valeurs sur l'axe X
    axis.text.y = element_text(size = 12),                         # Taille des valeurs sur l'axe Y
    axis.title.x = element_text(size = 16),                        # Taille du titre de l'axe X
    axis.title.y = element_text(size = 16),                        # Taille du titre de l'axe Y
    plot.title = element_text(hjust = 0.75, size = 16),            # Taille du titre principal
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )




































# HISTOGRAMMES
library(viridis)
colors <- viridis(60) 
colors <- colors[15:56]
colors <- colors[seq(1, length(colors), by = 2)]

ggplot(S_T_B1, aes(x = Campagnes_clean, y = FREQ, fill = factor(TAILLE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colors) +
  labs(title = "Fréquence par taille et par campagne",
       x = "Campagnes", y = "Fréquence",
       fill = "Taille") +
  theme(
    panel.background = element_rect(fill = "#d1e5f0", color = NA),
    plot.background = element_rect(fill = "#d1e5f0", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# TAILLE MOYENNE COURBES

### Sans écart type

# --- 1. Vecteur des dates exactes des campagnes ---
dates_ech <- as.Date(c("2005-08-15", "2010-07-02", "2013-12-11", "2016-04-04", "2016-09-04", 
                       "2017-03-10", "2017-12-04", "2021-03-28", "2022-04-03", "2022-10-30",
                       "2023-05-06", "2023-11-10", "2024-04-01", "2024-05-05", "2024-09-29"))

# --- 2. Associer les dates exactes aux campagnes ---
stopifnot(length(unique(S_T_B1$Campagne_clean)) == length(dates_ech))

dates_df <- data.frame(
  Campagne_clean = unique(S_T_B1$Campagne_clean),
  date_campagne = dates_ech
)

S_T_B1 <- S_T_B1 %>%
  left_join(dates_df, by = "Campagne_clean")

# --- 3. Calcul des tailles moyennes pondérées (en divisant FREQ par 100) ---
taille_moyenne_df <- S_T_B1 %>%
  group_by(date_campagne) %>%
  summarise(taille_moyenne = sum(TAILLE * (FREQ / 100)), .groups = "drop")

# --- 4. Graphique : abscisses = uniquement les dates de taille_moyenne_df ---
ggplot(taille_moyenne_df, aes(x = date_campagne, y = taille_moyenne)) +
  geom_point(color = "#1f2d86", size = 2) +
  geom_line(color = "#e74c3c", linewidth = 1) +
  scale_x_date(
    breaks = taille_moyenne_df$date_campagne,  # seulement les dates avec données
    labels = format(taille_moyenne_df$date_campagne, "%d/%m/%Y")
  ) +
  labs(title = "Taille moyenne pondérée par date exacte de campagne",
       x = "Date de campagne",
       y = "Taille moyenne pondérée") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### Avec écart type

# --- 1. Vecteur des dates exactes des campagnes ---
dates_ech <- as.Date(c("2005-08-15", "2010-07-02", "2013-12-11", "2016-04-04", "2016-09-04", 
                       "2017-03-10", "2017-12-04", "2021-03-28", "2022-04-03", "2022-10-30",
                       "2023-05-06", "2023-11-10", "2024-04-01", "2024-05-05", "2024-09-29"))

# --- 2. Associer les dates exactes aux campagnes ---
stopifnot(length(unique(S_T_B1$Campagne_clean)) == length(dates_ech))

dates_df <- data.frame(
  Campagne_clean = unique(S_T_B1$Campagne_clean),
  date_campagne = dates_ech
)

S_T_B1 <- S_T_B1 %>%
  left_join(dates_df, by = "Campagne_clean")

# --- 3. Calcul moyenne + écart-type pondéré (avec FREQ en pourcentage) ---
stats_df <- S_T_B1 %>%
  mutate(FREQ_rel = FREQ / 100) %>%
  group_by(date_campagne) %>%
  summarise(
    taille_moyenne = sum(TAILLE * FREQ_rel),
    ecart_type = sqrt(sum(FREQ_rel * (TAILLE - sum(TAILLE * FREQ_rel))^2)),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = taille_moyenne - ecart_type,
    ymax = taille_moyenne + ecart_type
  )

# --- 4. Graphique avec bande ± écart-type ---
ggplot(stats_df, aes(x = date_campagne, y = taille_moyenne)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "#d6604d", alpha = 0.4) +
  geom_line(color = "#b2182b", linewidth = 1) +
  geom_point(color = "#67001f", size = 2) +
  scale_x_date(
    breaks = stats_df$date_campagne[format(stats_df$date_campagne, "%m/%Y") != "04/2024"],
    labels = format(stats_df$date_campagne[format(stats_df$date_campagne, "%m/%Y") != "04/2024"], "%m/%Y")
  ) +
  labs(
    title = "Taille moyenne des bénitiers du bassin 1",
    x = "Date de campagne",
    y = "Taille moyenne ± écart-type (cm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## MOYENNE TAILLE BOITE A MOUSTACHE

# --- 1. Ajouter les dates exactes à chaque campagne ---
dates_ech <- as.Date(c("2005-08-15", "2010-07-02", "2013-12-11", "2016-04-04", "2016-09-04", 
                       "2017-03-10", "2017-12-04", "2021-03-28", "2022-04-03", "2022-10-30",
                       "2023-05-06", "2023-11-10", "2024-04-01", "2024-05-05", "2024-09-29"))

# Vérification
stopifnot(length(unique(S_T_B1$Campagne_clean)) == length(dates_ech))

# Associer date à campagne
dates_df <- data.frame(
  Campagne_clean = unique(S_T_B1$Campagne_clean),
  date_campagne = dates_ech
)

S_T_B1 <- S_T_B1 %>%
  left_join(dates_df, by = "Campagne_clean")

# --- 2. Réplication de chaque taille selon le nombre d’individus (NOMBRE) ---
S_T_B1_rep <- S_T_B1 %>%
  filter(NOMBRE > 0) %>%         # garder uniquement les tailles observées
  uncount(weights = NOMBRE)      # répète chaque ligne selon NOMBRE

# --- 3. Graphique en boîtes à moustaches avec labels %m/%Y ---
S_T_B1_rep <- S_T_B1_rep %>%
  mutate(date_label = format(date_campagne, "%m/%Y")) %>%
  mutate(date_label = factor(date_label, levels = format(sort(unique(date_campagne)), "%m/%Y")))

ggplot(S_T_B1_rep, aes(x = date_label, y = TAILLE)) +
  geom_boxplot(fill = "#f4a582", color = "#67001f", outlier.color = "#e74c3c") +
  labs(
    title = "Distribution des tailles de bénitiers par campagne",
    x = "Date de campagne",
    y = "Taille des individus (cm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Taille des valeurs sur l'axe X
    axis.text.y = element_text(size = 12),                         # Taille des valeurs sur l'axe Y
    axis.title.x = element_text(size = 16),                        # Taille du titre de l'axe X
    axis.title.y = element_text(size = 16),                        # Taille du titre de l'axe Y
    plot.title = element_text(hjust = 0.75, size = 16),            # Taille du titre principal
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

