library(tidyverse)
library(zoo)
library(insee)
library(tseries)
library(readxl)
library(dplyr)

data_PIB <- read_excel("data_PIB.xlsx")
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


if(vintage=="non"){
dataset <- get_dataset_list()
coucou <- get_insee_dataset("CNT-2020-PIB-EQB-RF")
PIB_vol <- get_insee_idbank("011794844") |> 
          arrange(DATE) |>   
          select(TIME_PERIOD, OBS_VALUE) |> 
  dplyr::rename("date"=TIME_PERIOD, 
                "PIB"=OBS_VALUE)
}else{
  PIB_vol <- read_csv("PIB_millesime.csv", show_col_types = FALSE) |> 
    dplyr::rename(date=DATE)
}

df_quarterly <- df_quarterly |> 
  mutate(date=yq(paste0(year, ":Q", quarter))) |> 
  left_join(PIB_vol, by = "date") |> 
  mutate(lagEPU = dplyr::lag(EPU,1), 
         dEPU = EPU - dplyr::lag(EPU,1))


plot(df_quarterly$dEPU, type="l")

df_travail <- df_quarterly |> 
na.omit()

df_travail2010 <- df_travail |> 
  dplyr::filter(date>="2009-07-01") |> 
  mutate(date=as.Date(date))

adf.test(df_travail2010$EPU)
adf.test(df_travail2010$dEPU)


moy <- mean(df_travail2010$EPU, na.rm = TRUE)
ecart <- sd(df_travail2010$EPU, na.rm = TRUE)
moydEPU <- mean(df_travail2010$dEPU, na.rm = TRUE)
ecartdEPU <- sd(df_travail2010$dEPU, na.rm = TRUE)
# Création de la dummy
df_travail2010$dummy_EPU <- ifelse(df_travail2010$EPU > moy +  ecart, df_travail2010$EPU, 0)
df_travail2010$dummy_dEPU <- ifelse(df_travail2010$dEPU > moydEPU +  ecartdEPU, df_travail2010$EPU, 0)
#' le problème de cette variable est qu'elle nous indique une non stationnarité
#' 
df_travail2010sans2020 <- df_travail2010 %>%
  mutate(date = as.Date(date)) %>%
  dplyr::filter(
    !(year(date) == 2020 | (year(date) == 2021 & quarter(date) %in% c(1, 2, 3)))
  )


#' Modèle OLS, 3 facteurs 3 dummies
modele_ols <- lm(PIB ~ F1 + F2 + F3 , data = df_travail2010sans2020)
summary(modele_ols)
df_travail2010sans2020$res_modele_ols <- resid(modele_ols)


plot(resid(modele_ols), type = "l",
     main = "Résidus du modèle OLS", xlab = "Date", ylab = "Résidus")

prevT4 <- coef(modele_ols)[1]*1+coef(modele_ols)[2]*df_quarterly$F1[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols)[3]*df_quarterly$F2[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols)[4]*df_quarterly$F3[which(df_quarterly$date=="2025-10-01")]

#' Modèle OLS, 3 facteurs EPU
modele_ols_EPU <- lm(PIB ~ F1 + F2 + F3 + EPU, data = df_travail2010sans2020)
summary(modele_ols_EPU)

prevT4_EPU <- coef(modele_ols_EPU)[1]*1+coef(modele_ols_EPU)[2]*df_quarterly$F1[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_EPU)[3]*df_quarterly$F2[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_EPU)[4]*df_quarterly$F3[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_EPU)[5]*df_quarterly$EPU[which(df_quarterly$date=="2025-10-01")]

#' Même modèle avec ajout du lagEPU.
modele_ols_lagEPU <- lm(PIB ~ F1 + F2 +F3 + EPU + lagEPU, data = df_travail2010sans2020)
summary(modele_ols_lagEPU)

prevT4_lagEPU <- coef(modele_ols_EPU)[1]*1+coef(modele_ols_lagEPU)[2]*df_quarterly$F1[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_lagEPU)[3]*df_quarterly$F2[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_lagEPU)[4]*df_quarterly$F3[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_lagEPU)[5]*df_quarterly$EPU[which(df_quarterly$date=="2025-10-01")]+coef(modele_ols_lagEPU)[6]*df_quarterly$lagEPU[which(df_quarterly$date=="2025-10-01")]



modele_ols_dum1EPU <- lm(PIB ~ F1 + F2 +F3 + dummy_EPU, data = df_travail2010sans2020)
summary(modele_ols_dum1EPU)


modele_ols_dum2EPU <- lm(PIB ~ F1 + F2 +F3 + dummy_dEPU, data = df_travail2010sans2020)
summary(modele_ols_dum2EPU)
