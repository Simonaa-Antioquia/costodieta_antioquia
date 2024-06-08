#Proyecto Plasa Colombia - Capitulo Antioquia

# 
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo, Juan cArlos Muoz
#Fecha de creacion: 01/04/2024
#Fecha de ultima modificacion: 01/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl);library(writexl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
options(scipen = 999)
################################################################################-


### Using SIPSA
sipsa <- read_rds("output/SIPSA.rds")

get_foodprice <- function(date){
  foodprice <- sipsa[,c("food","food_tcac",paste0("precio",date))]
  names(foodprice) <- c("food","food_tcac","price_g")
  write_xlsx(foodprice,paste0("output/foodprice_",date,".xlsx"))
}
date <- "202311"
get_foodprice("202311")

### Using SABA
saba_comercios <- read_rds("output/SABA_Comercios.rds")
saba_precios <- read_rds("output/SABA_Precios.rds")
saba <- rbind(saba_comercios,saba_precios) %>%
    mutate(p_compra=as.numeric(p_compra),p_venta=as.numeric(p_venta),
    margen=as.numeric(margen)) %>%
    group_by(food,food_tcac) %>% summarise_all(mean)

write_xlsx(saba,paste0("output/foodprice_SABA.xlsx"))