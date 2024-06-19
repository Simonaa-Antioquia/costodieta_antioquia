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


#### --- SIPSA ABASTACIMEINTO -- ####
##### ---  Pre-procesamos los datos - Por su tamaño no se realiza directamente en el script principal. --- #####
cantidades <- read_csv("/Users/jcmunoz/Library/CloudStorage/OneDrive-SharedLibraries-UniversidadEAFIT/Laura Maria Quintero Vasquez - FAO/01_Analisis_Empirico/01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_microdatos_antioquia.csv")%>%
  filter(mpio_destino == "Medellín")%>%
  rename(producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(fecha, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto=str_to_lower(producto), suma_kg=cantidad_kg/1000)

cantidades$anio <- year(cantidades$mes_y_ano)
cantidades$mes <- month(cantidades$mes_y_ano)

cantidades <- cantidades %>%
  filter(anio == 2023) %>%
  mutate(fecha = paste("cantidad", anio, mes, sep = "")) %>%
  group_by(producto, fecha) %>%
  summarise(suma_kg = sum(suma_kg, na.rm = TRUE)) %>%
  spread(key = fecha, value = suma_kg)

write_rds(cantidades,"input/SIPSA_Abastecimiento.rds")

#### --- SIPSA PRECIOS-- ####
##### ---  Pre-procesamos los datos - Por su tamaño no se realiza directamente en el script principal. --- #####
## Precios 

precios<-read_excel("/Users/jcmunoz/Library/CloudStorage/OneDrive-SharedLibraries-UniversidadEAFIT/Laura Maria Quintero Vasquez - FAO/01_Analisis_Empirico/01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_mes_filtrados.xlsx")%>%
  filter(ciudad == "Medellín")%>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto=str_to_lower(producto))

precios$anio <- year(precios$mes_y_ano)
precios$mes <- month(precios$mes_y_ano)

precios <- precios %>%
  filter(anio == 2023) %>%
  mutate(fecha = paste("precio", anio, mes, sep = "")) %>%
  group_by(producto, fecha) %>%
  summarise(precio_prom = mean(precio_prom, na.rm = TRUE)) %>%
  spread(key = fecha, value = precio_prom)

  write_rds(precios,"input/SIPSA_Precios.rds")

#### --- SABA - Comercialización-- ####
##### ---  Pre-procesamos los datos - Por su tamaño no se realiza directamente en el script principal. --- #####

saba_comercios <- read_excel("/Users/jcmunoz/Library/CloudStorage/OneDrive-SharedLibraries-UniversidadEAFIT/Laura Maria Quintero Vasquez - FAO/01_Analisis_Empirico/02_Indices/TCAC/04_DietaSaludable/00_data/Caracterización de Comercios/Caracterización de Comercios.xlsx", 
                                           sheet = "Data Procesamiento SONDEO", 
                                           col_types = c("date", "date", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text"))%>%
  janitor::clean_names()%>%drop_na(fec_inic)%>%
  mutate(rub_prodc=str_to_lower(rub_prodc))%>% unique() %>%
  ### Select variables
  transmute(rub_prodc,mpio,tip_est,var_r,p_compra=prec_ajust_kg,p_venta=prec_vent_kg_hoy_26,margen=marg_ccio)

    write_rds(saba_comercios,"input/SABA_Comercios.rds")

#### --- SABA - Tracking-- ####
##### ---  Pre-procesamos los datos - Por su tamaño no se realiza directamente en el script principal. --- #####

saba_precios <- read_excel("/Users/jcmunoz/Library/CloudStorage/OneDrive-SharedLibraries-UniversidadEAFIT/Laura Maria Quintero Vasquez - FAO/01_Analisis_Empirico/02_Indices/TCAC/04_DietaSaludable/00_data/Tracking - Precios/Tracking  - Precios.xlsx", 
                               sheet = "Consolidado")%>%
  janitor::clean_names()%>%
  mutate(rubros=str_to_lower(rubros))%>%unique() %>%
  transmute(rubros,rubros,p_compra=a_que_precio_compro_la_ultima_vez_el_producto,
  p_venta=precio_de_venta_del_kilogramo_el_dia_de_hoy,margen=margen_en_percent_comercio)

write_rds(saba_precios,"input/SABA_Precios.rds")