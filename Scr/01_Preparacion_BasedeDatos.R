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
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
options(scipen = 999)
################################################################################-

#### Llaves para TCAC
# Tabla de Composición de Alimentos Colombianos
llave_sipsa <- read_delim("input/llave_sipsa.csv", 
                           delim = ";", escape_double = FALSE,
                           col_types = cols(producto_tcac = col_skip()), 
                           trim_ws = TRUE)%>%
  mutate(producto=producto.x, producto_tcac=producto.y)


llave_saba <- read_excel("input/llave_dieta.xlsx")%>%
janitor::clean_names()%>%
  mutate(producto=str_to_lower(producto))%>%unique()


### --- SIPSA ---- ###
## Cargamos la base de datos de origen cantidades 
sipsa_abast <- read_rds("input/SIPSA_Abastecimiento.rds") %>% 
  left_join(llave_sipsa, by = "producto")%>%
  mutate(food=producto,food_tcac=cod_tcac) %>% ungroup() %>%
  select(food,food_tcac,cantidad20231:cantidad20239)

sipsa_precio <- read_rds("input/SIPSA_Precios.rds") %>% 
  left_join(llave_sipsa, by = c("producto"="producto.x"))%>%
  mutate(food=producto,food_tcac=cod_tcac) %>% ungroup() %>%
  select(food,food_tcac,precio20231:precio20239)

sipsa <- sipsa_precio %>%
          left_join(sipsa_abast, by = c("food", "food_tcac"))

write_rds(sipsa, "output/SIPSA.rds")

### --- SABA---- ###
## Comercios
saba_comercios <- read_rds("input/SABA_Comercios.rds") %>%
  left_join(llave_saba, by = c("rub_prodc"="producto")) %>%
  mutate(food=alimento_sipsa,food_tcac=cod_tcac) %>%
  select(food,food_tcac,p_compra:margen)

write_rds(saba_comercios, "output/SABA_Comercios.rds")

## Tracking precios
saba_precios <- read_rds("input/SABA_Precios.rds") %>%
  left_join(llave_saba, by = c("rubros"="producto")) %>%
  mutate(food=alimento_sipsa,food_tcac=cod_tcac) %>%
  select(food,food_tcac,p_compra:margen)

write_rds(saba_precios, "output/SABA_Precios.rds")
