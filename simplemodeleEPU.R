library(tidyverse)
library(zoo)
library(insee)
library(timeSeries)


data_PIB <- read_excel("Desktop/data_PIB.xlsx")
vintage <-"oui"

# Création d’une variable de trimestre
data_PIB <- data_PIB %>%
  mutate(
    quarter = ceiling(month / 3)
  ) |> 
  fill(EPU, .direction = "down")

# Moyenne trimestrielle des séries F1, F2, F3, EPU
df_quarterly <- data_PIB |>
  mutate(quarter = ceiling(month / 3)) |>
  group_by(year, quarter) |>
  summarise(
    F1 = mean(as.numeric(F1), na.rm = TRUE),
    F2 = mean(as.numeric(F2), na.rm = TRUE),
    F3 = mean(as.numeric(F3), na.rm = TRUE),
    EPU = mean(as.numeric(EPU), na.rm = TRUE),
    .groups = "drop"  
  )

# Optionnel : créer une variable "date" au format Année-Trimestre
df_quarterly <- df_quarterly %>%
  mutate(
    date = paste0(year, "-Q", quarter),
    EPU= log(EPU)
  )

df_quarterly <- df_quarterly |>
  mutate(
    year = as.numeric(unlist(year)),
    quarter = as.numeric(unlist(quarter))
  )

##### J'enlève ce qui est après 2025Q2
df_travail <- df_quarterly[-c(123:127),]


if(vintage=="non"){
dataset <- get_dataset_list()
coucou <- get_insee_dataset("CNT-2020-PIB-EQB-RF")
PIB_vol <- get_insee_idbank("011794844") |> 
          arrange(DATE) |>   
          select(TIME_PERIOD, OBS_VALUE) |> 
  dplyr::rename("date"=TIME_PERIOD, 
                "PIB"=OBS_VALUE)
}else{
  PIB_vol <- read_csv("Desktop/PIB_millesime.csv")
  PIB_vol <- PIB_vol |>
  mutate(date = paste0(year(DATE), "-Q", quarter(DATE)))
}


df_travail_final <- df_travail |> 
  left_join(PIB_vol, by = "date")

## Je fais à partir de 2010 (par contre je laisse jusqu'à 2025 pour avoir suffisemment d'observations.)
#' Je mettrais des dummies après observations des résiuds
df_travail_final <- df_travail_final[-c(1:60),]

modele_ols <- lm(PIB ~ F1 + F2 + F3 , data = df_travail_final)

# Résumé complet
summary(modele_ols)
residus <- resid(modele_ols)

plot(resid(modele_ols), type = "l",
     main = "Résidus du modèle OLS", xlab = "Date", ylab = "Résidus")

## Ajout des dummmies
df_travail_final <- df_travail_final |>
  mutate(
    D102 = ifelse(row_number() == 41, 1, 0),
    D103 = ifelse(row_number() == 42, 1, 0),
    D104 = ifelse(row_number() == 43, 1, 0),
  )

#' Modèle OLS, 3 facteurs 3 dummies
modele_ols <- lm(PIB ~ F1 + F2 + F3+ D102 + D103  + D104, data = df_travail_final)

summary(modele_ols)
prevT3 <- coef(modele_ols)[1]*1+coef(modele_ols)[2]*df_quarterly$F1[which(df_quarterly$date=="2025-Q3")]+coef(modele_ols)[3]*df_quarterly$F2[which(df_quarterly$date=="2025-Q3")]

#' Même modèle avec ajout de l'EPU.
modele_ols_avecEPU <- lm(PIB ~ F1 + F2 +F3 + EPU+ D102 + D103  + D104, data = df_travail_final)
summary(modele_ols_avecEPU)

prevT3_avecEPU <- coef(modele_ols_avecEPU)[1]*1+coef(modele_ols_avecEPU )[2]*df_quarterly$F1[which(df_quarterly$date=="2025-Q3")]+coef(modele_ols_avecEPU)[3]*df_quarterly$F2[which(df_quarterly$date=="2025-Q3")]+coef(modele_ols_avecEPU)[4]*df_quarterly$F3[which(df_quarterly$date=="2025-Q3")]+coef(modele_ols_avecEPU)[5]*df_quarterly$EPU[which(df_quarterly$date=="2025-Q3")]

cat(paste0("Le modèle avec les 3 facteurs indiquent une croissance du PIB au T4 2025 de ", round(prevT3, 2), ", tandis quel l'ajout de l'EPU dans l'équation et la simulation du modèle avec en hypothèse un niveau maintenu au dernier connu ampute la prévision de la croissance du PIB du T4 2025 de ", round(prevT3 -prevT3_avecEPU, 2), " point."))

