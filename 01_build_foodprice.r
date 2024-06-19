library(tidyverse)
library(Foodprice)
library(googlesheets4)
library(dplyr)
source("aux_funciones.R")

### <------ SIPSA_P ------>
    lk <- "https://docs.google.com/spreadsheets/d/1195oJCW_rj1jxiGj2Pb4o8VKSZ717HA7E1GhYoVwlqI/edit?usp=sharing"
    ss <- gs4_get(lk)
    sipsa_p_id <- range_read(lk, sheet = "SIPSA_P",skip=1) %>% select(SIPSA_P_ID,Cod_TCAC,TCAC_Int,Cod_TCAC_In,Diferencias)
    sipsa_p_c <- sipsa_p_id %>% transmute(SIPSA_P_ID,TCAC=
                                            ### Misma clasificación
                                            ifelse(Diferencias=="Igual",Cod_TCAC,
                                            ifelse(TCAC_Int=="No Existe",Cod_TCAC_In,Cod_TCAC))) %>%
                                            drop_na(TCAC)

### <------ IPC - Deflactores ------>
    IPC <- range_read(lk, sheet = "IPC_DANE") %>%
            mutate_all(as.numeric) %>% 
            distinct(ano,mes, .keep_all = TRUE)

### <------ TCAC ------>
    # TCAC - Raw data
    TCAC_raw <- range_read(lk, sheet = "TCAC")
    TCAC_raw <- TCAC_raw[!duplicated(TCAC_raw$Cod_TCAC),] # In case of repeated
    # Tablas de alimentos
    Grupos_food <- range_read(lk, sheet = "Grupos_Alimentos") 
    Grupos_food_small <- Grupos_food[,c("Grupo_GABAS","Subgrupo_GABAS","Group","Subgroup")]
    Markup <- Grupos_food[,c("Group","Subgroup","markup_min","markup_max")] %>%
                        mutate(markup_mean=(markup_min+markup_max)/2)
    
    # Tabla de intercambios
    Tabla_Int <- range_read(lk, sheet = "Tabla_Intercambio") %>% 
      transmute(TCAC=Cod_TCAC,
                Serving_g=Porcion_en_100_g) 
    
    Tabla_Int <- Tabla_Int[!duplicated(Tabla_Int$TCAC),] # Fix datos repetidos
    
    # Tablas de alimentos
    TCAC_dt <- TCAC_raw[, c(1,3,4,6,8:24)]
    nam <- names(data_example)
    names(TCAC_dt)[5:21] <- nam[8:24]
    names(TCAC_dt)[1:4] <- c("TCAC","Grupo_GABAS","Subgrupo_GABAS","pc")
    TCAC_dt$pc <- as.numeric(TCAC_dt$pc)/100

    # Incluir Serving (valor de referencia en gramos de porción comestible (100 gramos para todos los alimentos).
    TCAC_dt$Serving <- 100

    TCAC_dt <- TCAC_dt %>% left_join(Grupos_food_small) %>%  
                            select(-Grupo_GABAS,-Subgrupo_GABAS)
    # Fixing serving (% de parte comestible)
    TCAC_dt$Serving <- as.numeric(TCAC_dt$Serving)

    ## Sin categora ("NUECES Y SEMILLAS SECAS", "DULCES Y POSTRES","SIN CATEGORIA")
    TCAC_dt <- TCAC_dt[!(TCAC_dt$Subgroup=="Sin Categoria"),]

    ## Serving G
    TCAC_dt <- merge(TCAC_dt,Tabla_Int)
    TCAC_dt <- TCAC_dt[!duplicated(TCAC_dt$TCAC),]

### <------ SIPSA ------>

# Datos SIPSA
    sipsa <- fix_sipsa_uni(readRDS("output/sipsa_dieta_antioquia.rds"))  %>%
                left_join(IPC) %>%
                mutate(precio_cons=(precio/ipc)*100)

# CoCA
    get_ds_end_CoCA <- function(an) {return(
      do.call(rbind,lapply(unique(sipsa[sipsa$ano==an,]$mes),
                           create_CoCA_year_m,an=an,sipsa=sipsa)))}
    
    CoCA_out <- do.call(rbind,lapply(unique(sipsa$ano),get_ds_end_CoCA))
    CoCA_out$diet <- "CoCA"
    CoCA_out <- CoCA_out[,c("year","mt","Demo_Group","Sex",
                            "cost_day","Cost_1000kcal",
                            "diet","price_type")]
                           
# CoNA                
    get_ds_end_CoNA <- function(an) {return(
      do.call(rbind,lapply(unique(sipsa[sipsa$ano==an,]$mes),
                           create_CoNA_year_m,an=an,sipsa=sipsa)))}
    
    CoNA_out <- do.call(rbind,lapply(unique(sipsa$ano),get_ds_end_CoNA))
    CoNA_out$diet <- "CoNA"
    CoNA_out <- CoNA_out[,c("year","mt","Demo_Group","Sex",
                            "cost_day","Cost_1000kcal",
                            "diet","price_type")]
    

# CoRD 
    get_ds_end_CoRD  <- function(an) {return(
      do.call(rbind,lapply(unique(sipsa[sipsa$ano==an,]$mes),
                           create_CoRD_year_m,an=an,sipsa=sipsa)))}
    
    CoRD_out <- do.call(rbind,lapply(unique(sipsa$ano),get_ds_end_CoRD))
    CoRD_out$diet <- "CoRD"
    
    CoRD_out <- CoRD_out[,c("year","mt","Demo_Group","Sex",
                            "cost_day","Cost_1000kcal",
                            "diet","price_type")]
    
### <------ END ------>
    diet_out <- rbind(CoCA_out,CoNA_out)
    diet_out <- rbind(diet_out,CoRD_out)
    
    diet_out <- diet_out %>% mutate(precios=ifelse(price_type %in% 
                                                c("cons_min",
                                                      "cons_max",
                                                      "cons_mean"),
                                    "Constantes (Base=2018)",
                                    "Corrientes"))
    
    sheet_write(ss, data = diet_out, sheet = "CostoDieta_Antioquia_long")
    
    -dei
    
    dieat_out1 <- diet_out[diet_out$price_type %in% c("cons_max","corr_max"),]
    dieat_out1 <- dieat_out1 %>% rename(
                          "cost_day_max"="cost_day",
                          "Cost_1000kcal_max"="Cost_1000kcal") %>% 
        select(-price_type,)
    
    dieat_out2 <- diet_out[diet_out$price_type %in% c("cons_min","corr_min"),]
    dieat_out2 <- dieat_out2 %>% rename(
      "cost_day_min"="cost_day",
      "Cost_1000kcal_min"="Cost_1000kcal") %>% 
      select(-price_type,)
    
    dieat_out3 <- diet_out[diet_out$price_type %in% c("cons_mean","corr_mean"),]
    dieat_out3 <- dieat_out3 %>% rename(
      "cost_day_mean"="cost_day",
      "Cost_1000kcal_mean"="Cost_1000kcal") %>% 
      select(-price_type,)
    
    out <- merge(dieat_out3,dieat_out2,all.x=TRUE,all.y=TRUE)
    out <- merge(out,dieat_out1,all.x=TRUE,all.y=TRUE)
    
    # Sheets 
    sheet_write(ss, data = out, sheet = "CostoDieta_Antioquia_wide")