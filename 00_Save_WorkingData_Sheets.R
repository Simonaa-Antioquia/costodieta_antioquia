library(tidyverse)
library(googlesheets4)
library(stringdist)
lk <- "https://docs.google.com/spreadsheets/d/1195oJCW_rj1jxiGj2Pb4o8VKSZ717HA7E1GhYoVwlqI/edit?usp=sharing"

## OPen DataBase
TCAC <- range_read(lk, sheet = "TCAC")
MAYORISTA <- range_read(lk, sheet = "MAYORISTA")


# Calcular la matriz de distancias
distancias <- stringdistmatrix(MAYORISTA$Alimento,TCAC$Alimento_TCAC, method = "jw")

# Encontrar el índice de la cadena más cercana en MAYORISTA$Alimento para cada cadena en TCAC$Alimento_TCAC
indices <- apply(distancias, 1, which.min)

# Crear una nueva columna en TCAC con el Alimento correspondiente de MAYORISTA
MAYORISTA$Alimento_TCAC <- TCAC$Alimento_TCAC[indices]

help()