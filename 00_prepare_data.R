library(tidyverse)
library(googlesheets4)

### -- Open Equivalent Tables
lk <- "https://docs.google.com/spreadsheets/d/1195oJCW_rj1jxiGj2Pb4o8VKSZ717HA7E1GhYoVwlqI/edit?usp=sharing"
ss <- gs4_get(lk)

### -- Loading IDS - Equivalent tables
sipsa_p_id <- range_read(lk, sheet = "SIPSA_P",skip=1) %>%
            transmute(alimento=limpiar_texto(Alimento),
            SIPSA_P_ID,SIPSA_A_ID)
sipsa_p_id <- sipsa_p_id[!duplicated(sipsa_p_id$alimento),]
sipsa_a_id <- range_read(lk, sheet = "SIPSA_A") %>% 
            transmute(alimento=limpiar_texto(Alimento),SIPSA_A_ID)

mayorista_id <- range_read(lk, sheet = "Mayorista") %>% 
                    transmute(alimento=Alimento,Mayorista_ID,SIPSA_P_ID)
mayorista_id <- mayorista_id[!duplicated(mayorista_id$alimento),]

### -- SIPSA_A --> 

    # OPen 
    sipsa_a <- readRDS('input/base_abastecimiento_mensual_no_outliers.rds') 

    ### --- Solo datos que llegan a Medellin
    sipsa_a <- sipsa_a[sipsa_a$mpio_destino == "Medellín",]

    ### ---- Append SIPSA_A
    sipsa_a <- sipsa_a %>% mutate(ano=year(fecha),mes=month(fecha),
                                alimento=limpiar_texto(alimento),
                                cantidad_kg=as.numeric(cantidad_kg)) %>%
                        group_by(ano,mes,alimento) %>%
                        summarize(cantidad_kg=sum(cantidad_kg,na.rm=TRUE))
    
    ### ---- Check disjoint 
    disjoint_data <- anti_join(sipsa_a, sipsa_a_id, by = "alimento")
    ### "bananos otros", "carnes frias y embutidos", "cebolla cabezona"
    ### "hortalizas de hoja", "limones otros", "naranja valencia", "productos de panaderia"

    ### ---- Merge SIPSA_P - ID
    sipsa_a <- sipsa_a %>%
        group_by(ano,mes) %>%
        arrange(desc(cantidad_kg)) %>%
        mutate(cumulative_sum = cumsum(cantidad_kg)) %>%
        mutate(percentage = cumulative_sum / sum(cantidad_kg)) %>%
        filter(percentage <= 0.95)
    
    ### ---- Merge SIPSA_P - ID
    sipsa_a <- merge(sipsa_a,sipsa_a_id,by="alimento")

    ### ---- Prepare ID
    sipsa_a <- sipsa_a %>% mutate(ID_M=paste0(ano,"-",mes,"-",SIPSA_A_ID))

### <-- SIPSA_A --> 

    # OPen 
    sipsa_p <- readRDS('input/SIPSA_P_antioquia.rds') 

    ### -- Total abastecimiento -- por los mercados
    sipsa_p <- sipsa_p %>% filter(mpio_destino=="Medellín") %>% 
                            ungroup() %>%
                            mutate(ano=year(fecha),
                            mes=month(fecha),
                            precio=as.numeric(precio)) %>%
                            group_by(ano,mes,alimento) %>%
                            summarise(precio=mean(precio,na.rm=TRUE))
    
    ### --- Merge data
    sipsa_p <- inner_join(sipsa_p,sipsa_p_id, by = "alimento")

   sipsa_p <- sipsa_p %>% mutate(ID_M=paste0(ano,"-",mes,"-",SIPSA_A_ID))
    
### -- Final Data Set - SIPSA --> Keep only the docment
    sipsa <- sipsa_p[sipsa_p$ID_M %in% sipsa_a$ID_M,]
    saveRDS(sipsa, "output/sipsa_dieta_antioquia.rds")

    sipsa <- sipsa %>% mutate(ID_P=paste0(ano,"-",mes,"-",SIPSA_P_ID))

### <-- Mayorista --> 

    # OPen 
    mayorista <- readRDS('input/Mayorista_antioquia.rds') %>% 
                            transmute(alimento,
                            ano=year(date),
                            mes=month(date),
                            precio=as.numeric(alimentoprecio))

    ## Merge
    mayorista <- inner_join(mayorista,mayorista_id, by = "alimento")
    mayorista <- mayorista %>% mutate(ID_P=paste0(ano,"-",mes,"-",SIPSA_P_ID))

    mayorista <- mayorista[mayorista$ID_P %in% sipsa$ID_P,]
    saveRDS(mayorista, "output/mayorista_dieta_antioquia.rds")


