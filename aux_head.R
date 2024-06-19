pacman::p_load("tidyverse","googlesheets4","dplyr","linprog")
source("aux_funciones.R")
library(Foodprice)

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
    names(TCAC_dt)[1:4] <- c("TCAC","Grupo_GABAS","Subgrupo_GABAS","Serving")
    TCAC_dt <- TCAC_dt %>% left_join(Grupos_food_small) %>%  
                            select(-Grupo_GABAS,-Subgrupo_GABAS)
    # Fixing serving (% de parte comestible)
    TCAC_dt$Serving <- as.numeric(TCAC_dt$Serving)

    ## Sin categora ("NUECES Y SEMILLAS SECAS", "DULCES Y POSTRES","SIN CATEGORIA")
    TCAC_dt <- TCAC_dt[!(TCAC_dt$Subgroup=="Sin Categoria"),]

    ## Serving G
    TCAC_dt <- merge(TCAC_dt,Tabla_Int)
    TCAC_dt <- TCAC_dt[!duplicated(TCAC_dt$TCAC),]